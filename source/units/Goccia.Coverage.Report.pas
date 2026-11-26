unit Goccia.Coverage.Report;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Coverage;

procedure PrintCoverageSummary(const ATracker: TGocciaCoverageTracker);
{ When APreloadedSource is supplied, use it as the source instead of
  reading AFilePath from disk.  Callers can pass the in-memory source
  for virtual filenames (multifile sections, stdin) that have no
  on-disk backing.  Caller retains ownership of APreloadedSource.
  When nil, falls back to reading AFilePath from disk; if that file
  does not exist, the function exits without printing detail. }
procedure PrintCoverageDetail(const ATracker: TGocciaCoverageTracker;
  const AFilePath: string;
  const APreloadedSource: TStringList = nil);
procedure WriteCoverageLcov(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);
procedure WriteCoverageJSON(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);

implementation

uses
  SysUtils,

  BaseMap,
  HostOutputFiles,
  OrderedStringMap,
  StringBuffer,
  TextSemantics,

  Goccia.JSON.Utils,
  Goccia.SourceMap,
  Goccia.TextFiles;

type
  TFileCovPair = TBaseMap<string, TGocciaFileCoverage>.TKeyValuePair;

{ A location through the file's source map, when it has one: the start and the end each translate on their own
  (the map's positions are 1-based, Istanbul's columns 0-based). }
function TranslateLoc(const ASrcMap: TGocciaSourceMap;
  const ALoc: TGocciaCoverageLoc): TGocciaCoverageLoc;
var
  OrigLine, OrigCol: Integer;
begin
  Result := ALoc;
  if (ASrcMap = nil) or (ALoc.StartLine <= 0) then Exit;
  if ASrcMap.Translate(ALoc.StartLine, ALoc.StartColumn + 1, OrigLine, OrigCol) then
  begin
    Result.StartLine := OrigLine;
    Result.StartColumn := OrigCol - 1;
  end;
  if ASrcMap.Translate(ALoc.EndLine, ALoc.EndColumn + 1, OrigLine, OrigCol) then
  begin
    Result.EndLine := OrigLine;
    Result.EndColumn := OrigCol - 1;
  end;
end;

{ Istanbul's name of a function: its own identifier, else (anonymous_<index>) with its index in the file's
  function map, as its newFunction names it. }
function FunctionName(const AFunc: TGocciaCoverageFunction;
  const AKey: Integer): string;
begin
  Result := AFunc.Name;
  if Result = '' then
    Result := '(anonymous_' + IntToStr(AKey) + ')';
end;

{ Console Summary }

function Percent(const AHit, ATotal: Integer): Double;
begin
  if ATotal > 0 then
    Result := (AHit / ATotal) * 100.0
  else
    Result := 100.0;
end;

procedure PrintCoverageSummary(const ATracker: TGocciaCoverageTracker);
var
  Pair: TFileCovPair;
  FileCov: TGocciaFileCoverage;
  StatementsHit, StatementsTotal, LinesHit, LinesTotal, BranchesHit, BranchesTotal: Integer;
  FunctionsHit, FunctionsTotal: Integer;
  TotalStatementsHit, TotalStatementsTotal, TotalLinesHit, TotalLinesTotal: Integer;
  TotalBranchesHit, TotalBranchesTotal, TotalFunctionsHit, TotalFunctionsTotal: Integer;
begin
  TotalStatementsHit := 0;
  TotalStatementsTotal := 0;
  TotalLinesHit := 0;
  TotalLinesTotal := 0;
  TotalBranchesHit := 0;
  TotalBranchesTotal := 0;
  TotalFunctionsHit := 0;
  TotalFunctionsTotal := 0;

  WriteLn;
  WriteLn('Coverage Summary:');
  WriteLn(Format('  %-52s %-18s %-18s %-18s %s',
    ['File', 'Statements', 'Lines', 'Branches', 'Functions']));

  for Pair in ATracker.Files do
  begin
    FileCov := Pair.Value;
    StatementsHit := FileCov.StatementsHit;
    StatementsTotal := FileCov.StatementsFound;
    LinesHit := FileCov.LinesHit;
    LinesTotal := FileCov.LinesFound;
    BranchesHit := FileCov.BranchesHit;
    BranchesTotal := FileCov.BranchesFound;
    FunctionsHit := FileCov.FunctionsHit;
    FunctionsTotal := FileCov.FunctionsFound;

    WriteLn(Format('  %-52s %d/%d (%5.1f%%)  %d/%d (%5.1f%%)  %d/%d (%5.1f%%)  %d/%d (%5.1f%%)',
      [FileCov.FileName,
       StatementsHit, StatementsTotal, Percent(StatementsHit, StatementsTotal),
       LinesHit, LinesTotal, Percent(LinesHit, LinesTotal),
       BranchesHit, BranchesTotal, Percent(BranchesHit, BranchesTotal),
       FunctionsHit, FunctionsTotal, Percent(FunctionsHit, FunctionsTotal)]));

    Inc(TotalStatementsHit, StatementsHit);
    Inc(TotalStatementsTotal, StatementsTotal);
    Inc(TotalLinesHit, LinesHit);
    Inc(TotalLinesTotal, LinesTotal);
    Inc(TotalBranchesHit, BranchesHit);
    Inc(TotalBranchesTotal, BranchesTotal);
    Inc(TotalFunctionsHit, FunctionsHit);
    Inc(TotalFunctionsTotal, FunctionsTotal);
  end;

  WriteLn(Format('  %-52s %-18s %-18s %-18s %s', [
    '----------------------------------------------------',
    '------------------', '------------------', '------------------',
    '------------------']));
  WriteLn(Format('  %-52s %d/%d (%5.1f%%)  %d/%d (%5.1f%%)  %d/%d (%5.1f%%)  %d/%d (%5.1f%%)',
    ['Total',
     TotalStatementsHit, TotalStatementsTotal, Percent(TotalStatementsHit, TotalStatementsTotal),
     TotalLinesHit, TotalLinesTotal, Percent(TotalLinesHit, TotalLinesTotal),
     TotalBranchesHit, TotalBranchesTotal, Percent(TotalBranchesHit, TotalBranchesTotal),
     TotalFunctionsHit, TotalFunctionsTotal, Percent(TotalFunctionsHit, TotalFunctionsTotal)]));
end;

{ Line-by-Line Detail }

procedure PrintCoverageDetail(const ATracker: TGocciaCoverageTracker;
  const AFilePath: string;
  const APreloadedSource: TStringList);
var
  FileCov: TGocciaFileCoverage;
  SourceLines: TStringList;
  OwnsSource: Boolean;
  Lines: TGocciaCoverageLineArray;
  { per source line: -1 no statement starts on it, else the line's count }
  Counts: array of Int64;
  I, LineWidth: Integer;
  Gutter, SourcePath: string;
begin
  FileCov := ATracker.GetFileCoverage(AFilePath);
  if not Assigned(FileCov) then Exit;

  if Assigned(APreloadedSource) then
  begin
    SourceLines := APreloadedSource;
    OwnsSource := False;
  end
  else
  begin
    { Read through the resolved on-disk path — the record's own name is
      the canonical report key, which is repo-relative and need not
      resolve against the working directory. }
    SourcePath := ATracker.ResolvedSourcePath(FileCov.FileName);
    if not FileExists(SourcePath) then Exit;
    SourceLines := CreateFileTextLines(ReadUTF8FileText(SourcePath));
    OwnsSource := True;
  end;

  try
    if SourceLines.Count = 0 then Exit;

    { the lines Istanbul lists (a statement starts on them) and their counts }
    SetLength(Counts, SourceLines.Count);
    for I := 0 to High(Counts) do
      Counts[I] := -1;
    Lines := FileCov.LineCoverage;
    for I := 0 to High(Lines) do
      if (Lines[I].Line >= 1) and (Lines[I].Line <= SourceLines.Count) then
        Counts[Lines[I].Line - 1] := Lines[I].HitCount;

    // Determine gutter width from line count
    LineWidth := Length(IntToStr(SourceLines.Count));
    if LineWidth < 3 then
      LineWidth := 3;

    WriteLn;
    { Print the canonical key so the detail header matches the file column
      of the summary table and the SF:/JSON keys. }
    WriteLn(FileCov.FileName + ':');

    for I := 0 to SourceLines.Count - 1 do
    begin
      if Counts[I] > 0 then
        Gutter := Format('%*dx', [LineWidth + 3, Counts[I]])
      else if Counts[I] = 0 then
        Gutter := Format('%*s', [LineWidth + 2, '']) + '!!'
      else
        Gutter := Format('%*s', [LineWidth + 4, '']);
      WriteLn(Format('  %s | %*d | %s',
        [Gutter, LineWidth, I + 1, SourceLines[I]]));
    end;
  finally
    if OwnsSource then
      SourceLines.Free;
  end;
end;

{ lcov Format (istanbul-reports' lcovonly: TN, SF, FN, FNF, FNH, FNDA, DA, LF, LH, BRDA, BRF, BRH) }

procedure WriteCoverageLcov(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);
var
  Output: TStringList;
  Pair: TFileCovPair;
  FileCov: TGocciaFileCoverage;
  SrcMap: TGocciaSourceMap;
  FunctionOrder, BranchOrder: TGocciaCoverageIndexArray;
  Lines: TGocciaCoverageLineArray;
  I, J, LinesHitCount: Integer;
  Func: TGocciaCoverageFunction;
  Branch: TGocciaCoverageBranch;
  Decl, BranchLoc: TGocciaCoverageLoc;
begin
  Output := TStringList.Create;
  try
    Output.Add('TN:');

    for Pair in ATracker.Files do
    begin
      FileCov := Pair.Value;
      { SF: carries the canonical key — repo-relative with '/' separators,
        which is what Codecov matches against and what genhtml resolves. }
      Output.Add('SF:' + FileCov.FileName);
      SrcMap := ATracker.GetSourceMap(FileCov.FileName);

      { functions: the declaration's line and the name, in Istanbul's order }
      FunctionOrder := FileCov.FunctionOrder;
      for I := 0 to High(FunctionOrder) do
      begin
        Func := FileCov.Functions[FunctionOrder[I]];
        Decl := TranslateLoc(SrcMap, Func.Decl);
        Output.Add(Format('FN:%d,%s', [Decl.StartLine, FunctionName(Func, I)]));
      end;
      Output.Add(Format('FNF:%d', [FileCov.FunctionsFound]));
      Output.Add(Format('FNH:%d', [FileCov.FunctionsHit]));
      for I := 0 to High(FunctionOrder) do
      begin
        Func := FileCov.Functions[FunctionOrder[I]];
        Output.Add(Format('FNDA:%d,%s', [Func.HitCount, FunctionName(Func, I)]));
      end;

      { lines: each line a statement starts on, with the most hits among those statements }
      Lines := FileCov.LineCoverage(SrcMap);
      LinesHitCount := 0;
      for I := 0 to High(Lines) do
      begin
        Output.Add(Format('DA:%d,%d', [Lines[I].Line, Lines[I].HitCount]));
        if Lines[I].HitCount > 0 then
          Inc(LinesHitCount);
      end;
      Output.Add(Format('LF:%d', [Length(Lines)]));
      Output.Add(Format('LH:%d', [LinesHitCount]));

      { branches: BRDA:<line of the branch>,<branch's key in the map>,<path>,<hits> }
      BranchOrder := FileCov.BranchOrder;
      for I := 0 to High(BranchOrder) do
      begin
        Branch := FileCov.Branches[BranchOrder[I]];
        BranchLoc := TranslateLoc(SrcMap, Branch.Loc);
        for J := 0 to High(Branch.HitCounts) do
          Output.Add(Format('BRDA:%d,%d,%d,%d',
            [BranchLoc.StartLine, I, J, Branch.HitCounts[J]]));
      end;
      Output.Add(Format('BRF:%d', [FileCov.BranchesFound]));
      Output.Add(Format('BRH:%d', [FileCov.BranchesHit]));
      Output.Add('end_of_record');
    end;

    SaveStringsToHostFile(Output, AOutputPath);
  finally
    Output.Free;
  end;
end;

{ JSON Format: Istanbul's coverage map (istanbul-lib-coverage's FileCoverage data per file) }

function JsonLocation(const ALoc: TGocciaCoverageLoc): string;
begin
  { a location with no start line is Istanbul's undefined one: the empty alternate of an `if` without `else` }
  if ALoc.StartLine <= 0 then
    Exit('{"start":{},"end":{}}');
  Result := Format('{"start":{"line":%d,"column":%d},"end":{"line":%d,"column":%d}}',
    [ALoc.StartLine, ALoc.StartColumn, ALoc.EndLine, ALoc.EndColumn]);
end;

procedure WriteCoverageJSON(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);
var
  Buf: TStringBuffer;
  Pair: TFileCovPair;
  FileCov: TGocciaFileCoverage;
  SrcMap: TGocciaSourceMap;
  StatementOrder, FunctionOrder, BranchOrder: TGocciaCoverageIndexArray;
  I, J: Integer;
  FirstFile, Wrote: Boolean;
  Output: TStringList;
  Stmt: TGocciaCoverageStatement;
  Func: TGocciaCoverageFunction;
  Branch: TGocciaCoverageBranch;
  Loc, FuncLoc: TGocciaCoverageLoc;
begin
  Buf := TStringBuffer.Create(4096);
  Buf.Append('{');

  FirstFile := True;
  for Pair in ATracker.Files do
  begin
    FileCov := Pair.Value;
    if not FirstFile then
      Buf.AppendChar(',');
    FirstFile := False;
    SrcMap := ATracker.GetSourceMap(FileCov.FileName);
    StatementOrder := FileCov.StatementOrder;
    FunctionOrder := FileCov.FunctionOrder;
    BranchOrder := FileCov.BranchOrder;

    Buf.Append(#10'  "');
    Buf.Append(EscapeJSONString(FileCov.FileName));
    Buf.Append('": {');

    // path
    Buf.Append(#10'    "path": "');
    Buf.Append(EscapeJSONString(FileCov.FileName));
    Buf.Append('",');

    // statementMap
    Buf.Append(#10'    "statementMap": {');
    for I := 0 to High(StatementOrder) do
    begin
      if I > 0 then
        Buf.AppendChar(',');
      Stmt := FileCov.Statements[StatementOrder[I]];
      Buf.Append(Format(#10'      "%d": %s',
        [I, JsonLocation(TranslateLoc(SrcMap, Stmt.Loc))]));
    end;
    if Length(StatementOrder) > 0 then
      Buf.Append(#10'    ');
    Buf.Append('},');

    // fnMap
    Buf.Append(#10'    "fnMap": {');
    for I := 0 to High(FunctionOrder) do
    begin
      if I > 0 then
        Buf.AppendChar(',');
      Func := FileCov.Functions[FunctionOrder[I]];
      Loc := TranslateLoc(SrcMap, Func.Decl);
      FuncLoc := TranslateLoc(SrcMap, Func.Loc);
      Buf.Append(Format(#10'      "%d": {"name":"%s","decl":%s,"loc":%s,"line":%d}',
        [I, EscapeJSONString(FunctionName(Func, I)), JsonLocation(Loc),
         JsonLocation(FuncLoc), FuncLoc.StartLine]));
    end;
    if Length(FunctionOrder) > 0 then
      Buf.Append(#10'    ');
    Buf.Append('},');

    // branchMap
    Buf.Append(#10'    "branchMap": {');
    { a branch with no paths (an empty switch) is pruned, as SourceCoverage.freeze does: the keys after it keep
      their numbers }
    Wrote := False;
    for I := 0 to High(BranchOrder) do
    begin
      Branch := FileCov.Branches[BranchOrder[I]];
      if Length(Branch.Locations) = 0 then
        Continue;
      if Wrote then
        Buf.AppendChar(',');
      Wrote := True;
      Loc := TranslateLoc(SrcMap, Branch.Loc);
      Buf.Append(Format(#10'      "%d": {"loc":%s,"type":"%s","locations":[',
        [I, JsonLocation(Loc), EscapeJSONString(Branch.BranchType)]));
      for J := 0 to High(Branch.Locations) do
      begin
        if J > 0 then
          Buf.AppendChar(',');
        Buf.Append(JsonLocation(TranslateLoc(SrcMap, Branch.Locations[J])));
      end;
      Buf.Append(Format('],"line":%d}', [Loc.StartLine]));
    end;
    if Wrote then
      Buf.Append(#10'    ');
    Buf.Append('},');

    // s (statement hit counts)
    Buf.Append(#10'    "s": {');
    for I := 0 to High(StatementOrder) do
    begin
      if I > 0 then
        Buf.AppendChar(',');
      Buf.Append(Format('"%d":%d',
        [I, FileCov.Statements[StatementOrder[I]].HitCount]));
    end;
    Buf.Append('},');

    // f (function hit counts)
    Buf.Append(#10'    "f": {');
    for I := 0 to High(FunctionOrder) do
    begin
      if I > 0 then
        Buf.AppendChar(',');
      Buf.Append(Format('"%d":%d',
        [I, FileCov.Functions[FunctionOrder[I]].HitCount]));
    end;
    Buf.Append('},');

    // b (the hit counts of each branch's paths)
    Buf.Append(#10'    "b": {');
    Wrote := False;
    for I := 0 to High(BranchOrder) do
    begin
      Branch := FileCov.Branches[BranchOrder[I]];
      if Length(Branch.Locations) = 0 then
        Continue;
      if Wrote then
        Buf.AppendChar(',');
      Wrote := True;
      Buf.Append(Format('"%d":[', [I]));
      for J := 0 to High(Branch.HitCounts) do
      begin
        if J > 0 then
          Buf.AppendChar(',');
        Buf.Append(IntToStr(Branch.HitCounts[J]));
      end;
      Buf.AppendChar(']');
    end;
    Buf.Append('}');

    Buf.Append(#10'  }');
  end;

  Buf.Append(#10'}' + #10);

  Output := TStringList.Create;
  try
    Output.Text := Buf.ToString;
    SaveStringsToHostFile(Output, AOutputPath);
  finally
    Output.Free;
  end;
end;

end.

unit Goccia.Coverage.Report;

{$I Goccia.inc}

interface

uses
  Goccia.Coverage;

procedure PrintCoverageSummary(const ATracker: TGocciaCoverageTracker);
procedure PrintCoverageDetail(const ATracker: TGocciaCoverageTracker;
  const AFilePath: string);
procedure WriteCoverageLcov(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);
procedure WriteCoverageJSON(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);

implementation

uses
  Classes,
  SysUtils,

  BaseMap,
  OrderedStringMap,
  StringBuffer,
  TextSemantics,

  Goccia.SourceMap,
  Goccia.TextFiles;

type
  TFileCovPair = TBaseMap<string, TGocciaFileCoverage>.TKeyValuePair;

{ Build an array mapping original source lines (1-based index) to aggregated
  hit counts by translating each transformed line through the source map. }
procedure BuildTranslatedLineHits(const AFileCov: TGocciaFileCoverage;
  const ASrcMap: TGocciaSourceMap; const AOrigLineCount: Integer;
  var AHits: array of Integer);
var
  I, HitCount, OrigLine, OrigCol: Integer;
begin
  for I := 0 to High(AHits) do
    AHits[I] := 0;
  for I := 1 to AFileCov.LineHitCount - 1 do
  begin
    HitCount := AFileCov.GetLineHitCount(I);
    if HitCount > 0 then
      if ASrcMap.Translate(I, 0, OrigLine, OrigCol) then
        if (OrigLine >= 1) and (OrigLine <= AOrigLineCount) then
          Inc(AHits[OrigLine], HitCount);
  end;
end;

{ Console Summary }

procedure PrintCoverageSummary(const ATracker: TGocciaCoverageTracker);
var
  Pair: TFileCovPair;
  FileCov: TGocciaFileCoverage;
  LinesHit, LinesTotal, BranchesHit, BranchesTotal: Integer;
  TotalLinesHit, TotalLinesTotal, TotalBranchesHit, TotalBranchesTotal: Integer;
  LinePct, BranchPct: Double;
begin
  TotalLinesHit := 0;
  TotalLinesTotal := 0;
  TotalBranchesHit := 0;
  TotalBranchesTotal := 0;

  WriteLn;
  WriteLn('Coverage Summary:');
  WriteLn(Format('  %-60s %-18s %s', ['File', 'Lines', 'Branches']));

  for Pair in ATracker.Files do
  begin
    FileCov := Pair.Value;
    LinesHit := FileCov.LinesHit;
    LinesTotal := LinesHit;
    if FileCov.ExecutableLines > LinesTotal then
      LinesTotal := FileCov.ExecutableLines;
    BranchesHit := FileCov.BranchesHit;
    BranchesTotal := FileCov.BranchesFound;

    if LinesTotal > 0 then
      LinePct := (LinesHit / LinesTotal) * 100.0
    else
      LinePct := 100.0;

    if BranchesTotal > 0 then
      BranchPct := (BranchesHit / BranchesTotal) * 100.0
    else
      BranchPct := 100.0;

    WriteLn(Format('  %-60s %d/%d (%5.1f%%)  %d/%d (%5.1f%%)',
      [FileCov.FileName, LinesHit, LinesTotal, LinePct,
       BranchesHit, BranchesTotal, BranchPct]));

    TotalLinesHit := TotalLinesHit + LinesHit;
    TotalLinesTotal := TotalLinesTotal + LinesTotal;
    TotalBranchesHit := TotalBranchesHit + BranchesHit;
    TotalBranchesTotal := TotalBranchesTotal + BranchesTotal;
  end;

  if TotalLinesTotal > 0 then
    LinePct := (TotalLinesHit / TotalLinesTotal) * 100.0
  else
    LinePct := 100.0;
  if TotalBranchesTotal > 0 then
    BranchPct := (TotalBranchesHit / TotalBranchesTotal) * 100.0
  else
    BranchPct := 100.0;

  WriteLn(Format('  %-60s %-18s %s', [
    '----------------------------------------------------------------',
    '------------------', '------------------']));
  WriteLn(Format('  %-60s %d/%d (%5.1f%%)  %d/%d (%5.1f%%)',
    ['Total', TotalLinesHit, TotalLinesTotal, LinePct,
     TotalBranchesHit, TotalBranchesTotal, BranchPct]));
end;

{ Line-by-Line Detail }

procedure PrintCoverageDetail(const ATracker: TGocciaCoverageTracker;
  const AFilePath: string);
var
  FileCov: TGocciaFileCoverage;
  SourceLines: TStringList;
  ExecutableFlags: array of Boolean;
  I, HitCount, LineWidth: Integer;
  Gutter: string;
begin
  FileCov := ATracker.GetFileCoverage(AFilePath);
  if not Assigned(FileCov) then Exit;
  if not FileExists(AFilePath) then Exit;

  SourceLines := CreateUTF8FileTextLines(ReadUTF8FileText(AFilePath));
  try
    if SourceLines.Count = 0 then Exit;

    // Build per-line executable flags using the shared scanner
    SetLength(ExecutableFlags, SourceLines.Count);
    BuildExecutableLineFlags(SourceLines, ExecutableFlags);

    // Determine gutter width from line count
    LineWidth := Length(IntToStr(SourceLines.Count));
    if LineWidth < 3 then
      LineWidth := 3;

    WriteLn;
    WriteLn(AFilePath + ':');

    for I := 0 to SourceLines.Count - 1 do
    begin
      HitCount := FileCov.GetLineHitCount(I + 1);

      if HitCount > 0 then
        Gutter := Format('%*dx', [LineWidth + 3, HitCount])
      else if ExecutableFlags[I] then
        Gutter := Format('%*s', [LineWidth + 2, '']) + '!!'
      else
        Gutter := Format('%*s', [LineWidth + 4, '']);
      WriteLn(Format('  %s | %*d | %s',
        [Gutter, LineWidth, I + 1, SourceLines[I]]));
    end;
  finally
    SourceLines.Free;
  end;
end;

{ lcov Format }

procedure WriteCoverageLcov(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);
var
  Output: TStringList;
  SourceLines: TStringList;
  ExecutableFlags: array of Boolean;
  TranslatedHits: array of Integer;
  Pair: TFileCovPair;
  FileCov: TGocciaFileCoverage;
  I, HitCount, LineCount, LinesHitCount, BranchCount, BranchHitCount: Integer;
  Branch: TGocciaCoverageBranch;
  BranchBlockIndex: Integer;
  PrevLine, PrevColumn: Integer;
  HasSource: Boolean;
  SrcMap: TGocciaSourceMap;
  OrigLine, OrigCol: Integer;
  BranchLine, BranchCol: Integer;
begin
  Output := TStringList.Create;
  try
    Output.Add('TN:');

    for Pair in ATracker.Files do
    begin
      FileCov := Pair.Value;
      Output.Add('SF:' + FileCov.FileName);
      SrcMap := ATracker.GetSourceMap(FileCov.FileName);

      // Load source to identify executable lines for zero-hit entries
      HasSource := False;
      SourceLines := nil;
      try
        if FileExists(FileCov.FileName) then
        begin
          SourceLines := CreateUTF8FileTextLines(ReadUTF8FileText(FileCov.FileName));
          if SourceLines.Count > 0 then
          begin
            SetLength(ExecutableFlags, SourceLines.Count);
            BuildExecutableLineFlags(SourceLines, ExecutableFlags);
            HasSource := True;
          end;
        end;

        // Line coverage data — emit DA: for all executable lines (hit or zero)
        LineCount := 0;
        LinesHitCount := 0;
        if HasSource and Assigned(SrcMap) then
        begin
          // JSX file: pre-build translated hit counts, then emit
          SetLength(TranslatedHits, SourceLines.Count + 1);
          BuildTranslatedLineHits(FileCov, SrcMap, SourceLines.Count,
            TranslatedHits);
          for I := 0 to SourceLines.Count - 1 do
            if ExecutableFlags[I] then
            begin
              HitCount := TranslatedHits[I + 1];
              Output.Add(Format('DA:%d,%d', [I + 1, HitCount]));
              Inc(LineCount);
              if HitCount > 0 then
                Inc(LinesHitCount);
            end;
        end
        else if HasSource then
        begin
          for I := 0 to SourceLines.Count - 1 do
            if ExecutableFlags[I] then
            begin
              HitCount := FileCov.GetLineHitCount(I + 1);
              Output.Add(Format('DA:%d,%d', [I + 1, HitCount]));
              Inc(LineCount);
              if HitCount > 0 then
                Inc(LinesHitCount);
            end;
        end
        else
          for I := 1 to FileCov.LineHitCount - 1 do
          begin
            HitCount := FileCov.GetLineHitCount(I);
            if HitCount > 0 then
            begin
              if Assigned(SrcMap) and SrcMap.Translate(I, 0, OrigLine, OrigCol) then
                Output.Add(Format('DA:%d,%d', [OrigLine, HitCount]))
              else
                Output.Add(Format('DA:%d,%d', [I, HitCount]));
              Inc(LineCount);
              Inc(LinesHitCount);
            end;
          end;
      finally
        SourceLines.Free;
      end;

      // Branch coverage data
      // BRDA format: BRDA:line,block,branch,hit_count
      BranchCount := 0;
      BranchHitCount := 0;
      BranchBlockIndex := 0;
      PrevLine := -1;
      PrevColumn := -1;
      for I := 0 to FileCov.Branches.Count - 1 do
      begin
        Branch := FileCov.Branches[I];
        // Translate branch position through source map if available
        if Assigned(SrcMap) and
           SrcMap.Translate(Branch.Line, Branch.Column, OrigLine, OrigCol) then
        begin
          BranchLine := OrigLine;
          BranchCol := OrigCol;
        end
        else
        begin
          BranchLine := Branch.Line;
          BranchCol := Branch.Column;
        end;
        if (BranchLine <> PrevLine) or (BranchCol <> PrevColumn) then
        begin
          Inc(BranchBlockIndex);
          PrevLine := BranchLine;
          PrevColumn := BranchCol;
        end;
        if Branch.HitCount > 0 then
          Output.Add(Format('BRDA:%d,%d,%d,%d',
            [BranchLine, BranchBlockIndex, Branch.BranchIndex,
             Branch.HitCount]))
        else
          Output.Add(Format('BRDA:%d,%d,%d,-',
            [BranchLine, BranchBlockIndex, Branch.BranchIndex]));
        Inc(BranchCount);
        if Branch.HitCount > 0 then
          Inc(BranchHitCount);
      end;

      Output.Add(Format('BRF:%d', [BranchCount]));
      Output.Add(Format('BRH:%d', [BranchHitCount]));
      Output.Add(Format('LF:%d', [LineCount]));
      Output.Add(Format('LH:%d', [LinesHitCount]));
      Output.Add('end_of_record');
    end;

    Output.SaveToFile(AOutputPath);
  finally
    Output.Free;
  end;
end;

{ JSON Format (Istanbul-compatible) }

function EscapeJSONStr(const S: string): string;
begin
  Result := StringReplace(S, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

procedure WriteCoverageJSON(const ATracker: TGocciaCoverageTracker;
  const AOutputPath: string);
var
  Buf: TStringBuffer;
  SourceLines: TStringList;
  ExecutableFlags: array of Boolean;
  TranslatedHits: array of Integer;
  Pair: TFileCovPair;
  FileCov: TGocciaFileCoverage;
  I, HitCount, StatementIndex, BranchBlockIndex: Integer;
  Branch: TGocciaCoverageBranch;
  PrevLine, PrevColumn: Integer;
  FirstFile, FirstEntry: Boolean;
  HasSource: Boolean;
  Output: TStringList;
  SrcMap: TGocciaSourceMap;
  OrigLine, OrigCol: Integer;
  BranchLine, BranchCol: Integer;
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

    Buf.Append(#10'  "');
    Buf.Append(EscapeJSONStr(FileCov.FileName));
    Buf.Append('": {');

    // path
    Buf.Append(#10'    "path": "');
    Buf.Append(EscapeJSONStr(FileCov.FileName));
    Buf.Append('",');

    // Load source to identify executable lines for zero-hit entries
    HasSource := False;
    SourceLines := nil;
    try
      if FileExists(FileCov.FileName) then
      begin
        SourceLines := CreateUTF8FileTextLines(ReadUTF8FileText(FileCov.FileName));
        if SourceLines.Count > 0 then
        begin
          SetLength(ExecutableFlags, SourceLines.Count);
          BuildExecutableLineFlags(SourceLines, ExecutableFlags);
          HasSource := True;
        end;
      end;

      // s (statement hit counts) — emit for all executable lines
      Buf.Append(#10'    "s": {');
      StatementIndex := 0;
      FirstEntry := True;
      if HasSource and Assigned(SrcMap) then
      begin
        // JSX file: pre-build translated hit counts, then emit
        SetLength(TranslatedHits, SourceLines.Count + 1);
        BuildTranslatedLineHits(FileCov, SrcMap, SourceLines.Count,
          TranslatedHits);
        for I := 0 to SourceLines.Count - 1 do
          if ExecutableFlags[I] then
          begin
            if not FirstEntry then
              Buf.AppendChar(',');
            FirstEntry := False;
            Inc(StatementIndex);
            Buf.Append(Format('"%d":%d', [StatementIndex, TranslatedHits[I + 1]]));
          end;
      end
      else if HasSource then
      begin
        for I := 0 to SourceLines.Count - 1 do
          if ExecutableFlags[I] then
          begin
            if not FirstEntry then
              Buf.AppendChar(',');
            FirstEntry := False;
            Inc(StatementIndex);
            Buf.Append(Format('"%d":%d',
              [StatementIndex, FileCov.GetLineHitCount(I + 1)]));
          end;
      end
      else
        for I := 1 to FileCov.LineHitCount - 1 do
        begin
          HitCount := FileCov.GetLineHitCount(I);
          if HitCount > 0 then
          begin
            if not FirstEntry then
              Buf.AppendChar(',');
            FirstEntry := False;
            Inc(StatementIndex);
            Buf.Append(Format('"%d":%d', [StatementIndex, HitCount]));
          end;
        end;
      Buf.Append('},');

      // statementMap — emit for all executable lines
      Buf.Append(#10'    "statementMap": {');
      StatementIndex := 0;
      FirstEntry := True;
      if HasSource then
      begin
        for I := 0 to SourceLines.Count - 1 do
          if ExecutableFlags[I] then
          begin
            if not FirstEntry then
              Buf.AppendChar(',');
            FirstEntry := False;
            Inc(StatementIndex);
            Buf.Append(Format(
              '"%d":{"start":{"line":%d,"column":0},"end":{"line":%d,"column":0}}',
              [StatementIndex, I + 1, I + 1]));
          end;
      end
      else
        for I := 1 to FileCov.LineHitCount - 1 do
        begin
          HitCount := FileCov.GetLineHitCount(I);
          if HitCount > 0 then
          begin
            if not FirstEntry then
              Buf.AppendChar(',');
            FirstEntry := False;
            Inc(StatementIndex);
            Buf.Append(Format(
              '"%d":{"start":{"line":%d,"column":0},"end":{"line":%d,"column":0}}',
              [StatementIndex, I, I]));
          end;
        end;
      Buf.Append('},');
    finally
      SourceLines.Free;
    end;

    // b (branch hit counts)
    Buf.Append(#10'    "b": {');
    BranchBlockIndex := 0;
    PrevLine := -1;
    PrevColumn := -1;
    FirstEntry := True;
    I := 0;
    while I < FileCov.Branches.Count do
    begin
      Branch := FileCov.Branches[I];
      if (Branch.Line <> PrevLine) or (Branch.Column <> PrevColumn) then
      begin
        if not FirstEntry then
          Buf.AppendChar(']');
        if BranchBlockIndex > 0 then
          Buf.AppendChar(',');
        Inc(BranchBlockIndex);
        PrevLine := Branch.Line;
        PrevColumn := Branch.Column;
        FirstEntry := False;
        Buf.Append(Format('"%d":[', [BranchBlockIndex]));
        Buf.Append(Format('%d', [Branch.HitCount]));
      end
      else
      begin
        Buf.AppendChar(',');
        Buf.Append(Format('%d', [Branch.HitCount]));
      end;
      Inc(I);
    end;
    if not FirstEntry then
      Buf.AppendChar(']');
    Buf.Append('},');

    // branchMap — translate positions through source map if available
    Buf.Append(#10'    "branchMap": {');
    BranchBlockIndex := 0;
    PrevLine := -1;
    PrevColumn := -1;
    I := 0;
    while I < FileCov.Branches.Count do
    begin
      Branch := FileCov.Branches[I];
      if (Branch.Line <> PrevLine) or (Branch.Column <> PrevColumn) then
      begin
        if BranchBlockIndex > 0 then
          Buf.AppendChar(',');
        Inc(BranchBlockIndex);
        PrevLine := Branch.Line;
        PrevColumn := Branch.Column;
        // Translate position for display
        if Assigned(SrcMap) and
           SrcMap.Translate(Branch.Line, Branch.Column, OrigLine, OrigCol) then
        begin
          BranchLine := OrigLine;
          BranchCol := OrigCol;
        end
        else
        begin
          BranchLine := Branch.Line;
          BranchCol := Branch.Column;
        end;
        Buf.Append(Format('"%d":{"type":"branch","loc":{"start":{"line":%d,"column":%d},"end":{"line":%d,"column":%d}},"locations":[',
          [BranchBlockIndex, BranchLine, BranchCol,
           BranchLine, BranchCol]));
        Buf.Append(Format('{"start":{"line":%d,"column":%d},"end":{"line":%d,"column":%d}}',
          [BranchLine, BranchCol, BranchLine, BranchCol]));
      end
      else
      begin
        // Same block — use same translated position
        if Assigned(SrcMap) and
           SrcMap.Translate(Branch.Line, Branch.Column, OrigLine, OrigCol) then
        begin
          BranchLine := OrigLine;
          BranchCol := OrigCol;
        end
        else
        begin
          BranchLine := Branch.Line;
          BranchCol := Branch.Column;
        end;
        Buf.AppendChar(',');
        Buf.Append(Format('{"start":{"line":%d,"column":%d},"end":{"line":%d,"column":%d}}',
          [BranchLine, BranchCol, BranchLine, BranchCol]));
      end;
      // Check if next branch is same block
      if (I + 1 >= FileCov.Branches.Count) or
         (FileCov.Branches[I + 1].Line <> PrevLine) or
         (FileCov.Branches[I + 1].Column <> PrevColumn) then
        Buf.Append(']}');
      Inc(I);
    end;
    Buf.Append('},');

    // f and fnMap (function coverage — not tracked yet, emit empty)
    Buf.Append(#10'    "f": {},');
    Buf.Append(#10'    "fnMap": {}');

    Buf.Append(#10'  }');
  end;

  Buf.Append(#10'}' + #10);

  Output := TStringList.Create;
  try
    Output.Text := Buf.ToString;
    Output.SaveToFile(AOutputPath);
  finally
    Output.Free;
  end;
end;

end.

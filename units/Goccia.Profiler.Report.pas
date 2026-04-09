unit Goccia.Profiler.Report;

{$I Goccia.inc}

interface

uses
  Goccia.Profiler;

procedure PrintOpcodeProfile(const AProfiler: TGocciaProfiler);
procedure PrintOpcodePairProfile(const AProfiler: TGocciaProfiler);
procedure PrintScalarHitRate(const AProfiler: TGocciaProfiler);
procedure PrintFunctionProfile(const AProfiler: TGocciaProfiler);
procedure WriteProfileJSON(const AProfiler: TGocciaProfiler;
  const AOutputPath: string);
procedure WriteCollapsedStacks(const AProfiler: TGocciaProfiler;
  const AOutputPath: string);

implementation

uses
  Classes,
  Math,
  SysUtils,

  BaseMap,
  StringBuffer,
  TimingUtils,

  Goccia.Bytecode.OpCodeNames;

const
  MAX_OPCODE_PAIRS_DISPLAYED = 20;

type
  TOpcodeEntry = record
    Op: UInt8;
    Count: Int64;
  end;

  TOpcodePairEntry = record
    Prev: UInt8;
    Cur: UInt8;
    Count: Int64;
  end;

  TFunctionEntry = record
    Index: Integer;
    SelfTimeNanoseconds: Int64;
  end;

function EscapeJSONString(const AValue: string): string;
begin
  Result := StringReplace(AValue, '\', '\\', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '\"', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #13, '\r', [rfReplaceAll]);
  Result := StringReplace(Result, #9, '\t', [rfReplaceAll]);
end;

{ Console: Opcode Profile }

procedure PrintOpcodeProfile(const AProfiler: TGocciaProfiler);
var
  Entries: array of TOpcodeEntry;
  EntryCount: Integer;
  I, J: Integer;
  Total: Int64;
  Pct: Double;
  Temp: TOpcodeEntry;
  Count: Int64;
begin
  EntryCount := 0;
  SetLength(Entries, 256);
  Total := 0;

  for I := 0 to MAX_OPCODE_ORDINAL do
  begin
    Count := AProfiler.GetOpcodeCount(I);
    if Count > 0 then
    begin
      Entries[EntryCount].Op := I;
      Entries[EntryCount].Count := Count;
      Inc(EntryCount);
      Total := Total + Count;
    end;
  end;

  if EntryCount = 0 then
  begin
    WriteLn;
    WriteLn('Opcode Profile: (no instructions executed)');
    Exit;
  end;

  // Sort descending by count (insertion sort — small N)
  for I := 1 to EntryCount - 1 do
  begin
    Temp := Entries[I];
    J := I - 1;
    while (J >= 0) and (Entries[J].Count < Temp.Count) do
    begin
      Entries[J + 1] := Entries[J];
      Dec(J);
    end;
    Entries[J + 1] := Temp;
  end;

  WriteLn;
  WriteLn('Opcode Profile:');
  WriteLn(Format('  %-40s %15s %8s', ['Opcode', 'Count', '%']));

  for I := 0 to EntryCount - 1 do
  begin
    if Total > 0 then
      Pct := (Entries[I].Count * 1.0 / Total) * 100.0
    else
      Pct := 0.0;
    WriteLn(Format('  %-40s %15d %7.1f%%', [
      OpCodeName(Entries[I].Op),
      Entries[I].Count,
      Pct]));
  end;

  WriteLn(Format('  %-40s %15d', ['Total', Total]));
end;

{ Console: Opcode Pair Profile }

procedure PrintOpcodePairProfile(const AProfiler: TGocciaProfiler);
var
  Entries: array of TOpcodePairEntry;
  EntryCount: Integer;
  Prev, Cur: Integer;
  I, J: Integer;
  Total: Int64;
  Pct: Double;
  Temp: TOpcodePairEntry;
  Count: Int64;
begin
  EntryCount := 0;
  SetLength(Entries, 65536);
  Total := 0;

  for Prev := 0 to MAX_OPCODE_ORDINAL do
    for Cur := 0 to MAX_OPCODE_ORDINAL do
    begin
      Count := AProfiler.GetOpcodePairCount(Prev, Cur);
      if Count > 0 then
      begin
        Entries[EntryCount].Prev := Prev;
        Entries[EntryCount].Cur := Cur;
        Entries[EntryCount].Count := Count;
        Inc(EntryCount);
        Total := Total + Count;
      end;
    end;

  if EntryCount = 0 then
  begin
    WriteLn;
    WriteLn('Opcode Pairs: (no data)');
    Exit;
  end;

  // Sort descending by count (insertion sort)
  for I := 1 to EntryCount - 1 do
  begin
    Temp := Entries[I];
    J := I - 1;
    while (J >= 0) and (Entries[J].Count < Temp.Count) do
    begin
      Entries[J + 1] := Entries[J];
      Dec(J);
    end;
    Entries[J + 1] := Temp;
  end;

  WriteLn;
  WriteLn('Opcode Pairs (top ', MAX_OPCODE_PAIRS_DISPLAYED, '):');
  WriteLn(Format('  %-40s %-4s %-40s %15s %8s', [
    'Previous', '', 'Current', 'Count', '%']));

  for I := 0 to Min(EntryCount, MAX_OPCODE_PAIRS_DISPLAYED) - 1 do
  begin
    if Total > 0 then
      Pct := (Entries[I].Count * 1.0 / Total) * 100.0
    else
      Pct := 0.0;
    WriteLn(Format('  %-40s  -> %-40s %15d %7.1f%%', [
      OpCodeName(Entries[I].Prev),
      OpCodeName(Entries[I].Cur),
      Entries[I].Count,
      Pct]));
  end;
end;

{ Console: Scalar Fast-Path Hit Rate }

procedure PrintScalarHitRate(const AProfiler: TGocciaProfiler);
var
  Hits, Misses, Total: Int64;
  HitPct: Double;
begin
  Hits := AProfiler.ScalarHits;
  Misses := AProfiler.ScalarMisses;
  Total := Hits + Misses;

  if Total = 0 then
  begin
    WriteLn;
    WriteLn('Scalar Fast-Path: (no generic arithmetic executed)');
    Exit;
  end;

  HitPct := (Hits * 1.0 / Total) * 100.0;

  WriteLn;
  WriteLn('Scalar Fast-Path:');
  WriteLn(Format('  Hits:   %12d (%5.1f%%)', [Hits, HitPct]));
  WriteLn(Format('  Misses: %12d (%5.1f%%)', [Misses, 100.0 - HitPct]));
  WriteLn(Format('  Total:  %12d', [Total]));
end;

{ Console: Function Profile }

procedure PrintFunctionProfile(const AProfiler: TGocciaProfiler);
var
  Entries: array of TFunctionEntry;
  EntryCount: Integer;
  I, J: Integer;
  Profile: TGocciaFunctionProfile;
  Temp: TFunctionEntry;
  DisplayName, Location: string;
begin
  EntryCount := 0;
  SetLength(Entries, AProfiler.FunctionProfileCount);

  for I := 0 to AProfiler.FunctionProfileCount - 1 do
  begin
    Profile := AProfiler.GetFunctionProfile(I);
    if Profile.CallCount > 0 then
    begin
      Entries[EntryCount].Index := I;
      Entries[EntryCount].SelfTimeNanoseconds := Profile.SelfTimeNanoseconds;
      Inc(EntryCount);
    end;
  end;

  if EntryCount = 0 then
  begin
    WriteLn;
    WriteLn('Function Profile: (no functions called)');
    Exit;
  end;

  // Sort descending by self-time (insertion sort — small N)
  for I := 1 to EntryCount - 1 do
  begin
    Temp := Entries[I];
    J := I - 1;
    while (J >= 0) and (Entries[J].SelfTimeNanoseconds < Temp.SelfTimeNanoseconds) do
    begin
      Entries[J + 1] := Entries[J];
      Dec(J);
    end;
    Entries[J + 1] := Temp;
  end;

  WriteLn;
  WriteLn('Function Profile:');
  WriteLn(Format('  %-12s %-12s %8s %10s  %-30s %s', [
    'Self Time', 'Total Time', 'Calls', 'Allocs', 'Function', 'Location']));

  for I := 0 to EntryCount - 1 do
  begin
    Profile := AProfiler.GetFunctionProfile(Entries[I].Index);
    if Profile.TemplateName = '' then
      DisplayName := '<anonymous>'
    else
      DisplayName := Profile.TemplateName;
    if (Profile.SourceFile <> '') and (Profile.Line > 0) then
      Location := Format('%s:%d', [Profile.SourceFile, Profile.Line])
    else if Profile.SourceFile <> '' then
      Location := Profile.SourceFile
    else
      Location := '';
    WriteLn(Format('  %-12s %-12s %8d %10d  %-30s %s', [
      FormatDuration(Profile.SelfTimeNanoseconds),
      FormatDuration(Profile.TotalTimeNanoseconds),
      Profile.CallCount,
      Profile.Allocations,
      DisplayName,
      Location]));
  end;
end;

{ JSON Export }

procedure WriteProfileJSON(const AProfiler: TGocciaProfiler;
  const AOutputPath: string);
var
  Buf: TStringBuffer;
  I, Prev, Cur: Integer;
  Count: Int64;
  Total: Int64;
  Pct: Double;
  Profile: TGocciaFunctionProfile;
  Hits, Misses, ScalarTotal: Int64;
  FirstEntry: Boolean;
  Output: TStringList;
  InvariantFormat: TFormatSettings;
begin
  InvariantFormat := DefaultFormatSettings;
  InvariantFormat.DecimalSeparator := '.';
  Buf := TStringBuffer.Create(8192);
  Buf.Append('{');

  // Opcodes section
  Buf.Append(#10'  "opcodes": [');
  Total := 0;
  for I := 0 to MAX_OPCODE_ORDINAL do
    Total := Total + AProfiler.GetOpcodeCount(I);

  FirstEntry := True;
  for I := 0 to MAX_OPCODE_ORDINAL do
  begin
    Count := AProfiler.GetOpcodeCount(I);
    if Count > 0 then
    begin
      if not FirstEntry then
        Buf.AppendChar(',');
      FirstEntry := False;
      if Total > 0 then
        Pct := (Count * 1.0 / Total) * 100.0
      else
        Pct := 0.0;
      Buf.Append(Format(#10'    {"opcode": "%s", "count": %d, "percentage": %.1f}', [
        EscapeJSONString(OpCodeName(I)), Count, Pct], InvariantFormat));
    end;
  end;
  Buf.Append(#10'  ],');

  // Opcode pairs section
  Buf.Append(#10'  "opcodePairs": [');
  FirstEntry := True;
  for Prev := 0 to MAX_OPCODE_ORDINAL do
    for Cur := 0 to MAX_OPCODE_ORDINAL do
    begin
      Count := AProfiler.GetOpcodePairCount(Prev, Cur);
      if Count > 0 then
      begin
        if not FirstEntry then
          Buf.AppendChar(',');
        FirstEntry := False;
        Buf.Append(Format(#10'    {"prev": "%s", "cur": "%s", "count": %d}', [
          EscapeJSONString(OpCodeName(Prev)),
          EscapeJSONString(OpCodeName(Cur)),
          Count]));
      end;
    end;
  Buf.Append(#10'  ],');

  // Scalar fast-path section
  Hits := AProfiler.ScalarHits;
  Misses := AProfiler.ScalarMisses;
  ScalarTotal := Hits + Misses;
  Buf.Append(Format(#10'  "scalarFastPath": {"hits": %d, "misses": %d, "total": %d', [
    Hits, Misses, ScalarTotal]));
  if ScalarTotal > 0 then
    Buf.Append(Format(', "hitRate": %.1f', [Hits * 100.0 / ScalarTotal],
      InvariantFormat));
  Buf.Append('},');

  // Functions section
  Buf.Append(#10'  "functions": [');
  FirstEntry := True;
  for I := 0 to AProfiler.FunctionProfileCount - 1 do
  begin
    Profile := AProfiler.GetFunctionProfile(I);
    if Profile.CallCount > 0 then
    begin
      if not FirstEntry then
        Buf.AppendChar(',');
      FirstEntry := False;
      Buf.Append(Format(#10'    {"name": "%s", "sourceFile": "%s", "line": %d, ' +
        '"calls": %d, "selfTimeNs": %d, "totalTimeNs": %d, "allocations": %d}', [
        EscapeJSONString(Profile.TemplateName),
        EscapeJSONString(Profile.SourceFile),
        Profile.Line,
        Profile.CallCount,
        Profile.SelfTimeNanoseconds,
        Profile.TotalTimeNanoseconds,
        Profile.Allocations]));
    end;
  end;
  Buf.Append(#10'  ]');

  Buf.Append(#10'}' + #10);

  Output := TStringList.Create;
  try
    Output.Text := Buf.ToString;
    Output.SaveToFile(AOutputPath);
  finally
    Output.Free;
  end;
end;

{ Collapsed Stack Export (Flame Graph) }

procedure WriteCollapsedStacks(const AProfiler: TGocciaProfiler;
  const AOutputPath: string);
var
  Output: TStringList;
  Pair: TBaseMap<string, Int64>.TKeyValuePair;
begin
  Output := TStringList.Create;
  try
    for Pair in AProfiler.CollapsedStacks do
      Output.Add(Pair.Key + ' ' + IntToStr(Pair.Value));
    Output.SaveToFile(AOutputPath);
  finally
    Output.Free;
  end;
end;

end.

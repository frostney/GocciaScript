unit Goccia.Builtins.Testing.Snapshots;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections;

type
  TGocciaSnapshotUpdateMode = (sumNone, sumNew, sumAll);

  TGocciaInlineSnapshotEdit = record
    SourcePath: string;
    Line: Integer;
    Column: Integer;
    SnapshotArgumentIndex: Integer;
    Snapshot: string;
  end;

  IGocciaSnapshotHost = interface
    ['{896A22E7-8111-4AA2-B6B6-9B1D37BE71D4}']
    function HasPersistentSource: Boolean;
    function ReadSnapshotFile(out AContent: string): Boolean;
    procedure WriteSnapshotFile(const AContent: string);
    procedure DeleteSnapshotFile;
    procedure QueueInlineSnapshot(const AEdit: TGocciaInlineSnapshotEdit);
    procedure FlushInlineSnapshots;
  end;

  { In-memory host adapter for embedders and focused tests. }
  TGocciaMemorySnapshotHost = class(TInterfacedObject, IGocciaSnapshotHost)
  private
    FHasPersistentSource: Boolean;
    FSnapshotContent: string;
    FSnapshotExists: Boolean;
    FInlineEdits: array of TGocciaInlineSnapshotEdit;
    FFlushed: Boolean;
  public
    constructor Create(const AHasPersistentSource: Boolean = True);

    function HasPersistentSource: Boolean;
    function ReadSnapshotFile(out AContent: string): Boolean;
    procedure WriteSnapshotFile(const AContent: string);
    procedure DeleteSnapshotFile;
    procedure QueueInlineSnapshot(const AEdit: TGocciaInlineSnapshotEdit);
    procedure FlushInlineSnapshots;

    function InlineEditCount: Integer;
    function InlineEdit(const AIndex: Integer): TGocciaInlineSnapshotEdit;
    property SnapshotContent: string read FSnapshotContent;
    property SnapshotExists: Boolean read FSnapshotExists;
    property Flushed: Boolean read FFlushed;
  end;

  TGocciaSnapshotState = class
  private
    FHost: IGocciaSnapshotHost;
    FUpdateMode: TGocciaSnapshotUpdateMode;
    FSnapshots: TDictionary<string, string>;
    FUncheckedKeys: TDictionary<string, Boolean>;
    FCounters: TDictionary<string, Integer>;
    FSnapshotFileExists: Boolean;
    FDirty: Boolean;

    procedure LoadSnapshotFile;
    function ResolveKey(const ATestName, AHint: string): string;
    procedure MarkChecked(const AKey: string);
    function Reconcile(const AKey, AReceived, AExpected: string;
      const AHasSnapshot, APropertyMatched: Boolean;
      out AFailureMessage: string): Boolean;
    function SerializeSnapshotFile: string;
  public
    constructor Create(const AHost: IGocciaSnapshotHost;
      const AUpdateMode: TGocciaSnapshotUpdateMode);
    destructor Destroy; override;

    function MatchExternal(const ATestName, AHint, AReceived: string;
      const APropertyMatched: Boolean; out AFailureMessage: string): Boolean;
    function MatchInline(const ATestName, AHint, AReceived,
      AInlineSnapshot: string; const AHasInlineSnapshot,
      APropertyMatched: Boolean; const ASourcePath: string;
      const ALine, AColumn,
      ASnapshotArgumentIndex: Integer; out AFailureMessage: string): Boolean;
    procedure PreserveTestSnapshots(const ATestName: string);
    procedure Finish(const AErrors: TStrings;
      const AProcessUnchecked: Boolean = True);
  end;

function AddSnapshotLineBreaks(const AValue: string): string;
function RemoveSnapshotLineBreaks(const AValue: string): string;

implementation

uses
  Generics.Defaults,
  SysUtils;

const
  SNAPSHOT_HEADER =
    '// Vitest Snapshot v1, https://vitest.dev/guide/snapshot.html';

function AddSnapshotLineBreaks(const AValue: string): string;
begin
  if Pos(#10, AValue) > 0 then
    Result := #10 + AValue + #10
  else
    Result := AValue;
end;

function NormalizeSnapshotNewlines(const AValue: string): string;
begin
  Result := StringReplace(AValue, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
end;

function RemoveSnapshotLineBreaks(const AValue: string): string;
begin
  if (Length(AValue) > 2) and (AValue[1] = #10) and
     (AValue[Length(AValue)] = #10) then
    Result := Copy(AValue, 2, Length(AValue) - 2)
  else
    Result := AValue;
end;

function EscapeBacktickString(const AValue: string): string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(AValue) do
  begin
    if (AValue[I] = '`') or (AValue[I] = '\') or
       ((AValue[I] = '$') and (I < Length(AValue)) and
        (AValue[I + 1] = '{')) then
      Result := Result + '\';
    Result := Result + AValue[I];
    Inc(I);
  end;
end;

function ReadBacktickString(const AContent: string; var AIndex: Integer;
  out AValue: string): Boolean;
var
  Escaped: Boolean;
begin
  Result := False;
  AValue := '';
  if (AIndex > Length(AContent)) or (AContent[AIndex] <> '`') then
    Exit;

  Inc(AIndex);
  Escaped := False;
  while AIndex <= Length(AContent) do
  begin
    if Escaped then
    begin
      AValue := AValue + AContent[AIndex];
      Escaped := False;
    end
    else if AContent[AIndex] = '\' then
      Escaped := True
    else if AContent[AIndex] = '`' then
    begin
      Inc(AIndex);
      Exit(True);
    end
    else
      AValue := AValue + AContent[AIndex];
    Inc(AIndex);
  end;
end;

function FindNextSnapshotExport(const AContent: string;
  var AIndex: Integer): Boolean;
const
  EXPORT_PREFIX = 'exports[';
var
  Ignored: string;
  Quote: Char;
  Closed: Boolean;
begin
  while AIndex <= Length(AContent) do
  begin
    if (AIndex < Length(AContent)) and (AContent[AIndex] = '/') and
       (AContent[AIndex + 1] = '/') then
    begin
      Inc(AIndex, 2);
      while (AIndex <= Length(AContent)) and
            not (AContent[AIndex] in [#10, #13]) do
        Inc(AIndex);
      Continue;
    end;
    if (AIndex < Length(AContent)) and (AContent[AIndex] = '/') and
       (AContent[AIndex + 1] = '*') then
    begin
      Inc(AIndex, 2);
      Closed := False;
      while AIndex < Length(AContent) do
      begin
        if (AContent[AIndex] = '*') and
           (AContent[AIndex + 1] = '/') then
        begin
          Inc(AIndex, 2);
          Closed := True;
          Break;
        end;
        Inc(AIndex);
      end;
      if not Closed then
        raise Exception.Create('Invalid snapshot file: unterminated comment');
      Continue;
    end;
    if AContent[AIndex] in ['''', '"'] then
    begin
      Quote := AContent[AIndex];
      Inc(AIndex);
      Closed := False;
      while AIndex <= Length(AContent) do
        if AContent[AIndex] = '\' then
          Inc(AIndex, 2)
        else if AContent[AIndex] = Quote then
        begin
          Inc(AIndex);
          Closed := True;
          Break;
        end
        else
          Inc(AIndex);
      if not Closed then
        raise Exception.Create('Invalid snapshot file: unterminated string');
      Continue;
    end;
    if AContent[AIndex] = '`' then
    begin
      if not ReadBacktickString(AContent, AIndex, Ignored) then
        raise Exception.Create(
          'Invalid snapshot file: unterminated template literal');
      Continue;
    end;
    if Copy(AContent, AIndex, Length(EXPORT_PREFIX)) = EXPORT_PREFIX then
    begin
      Inc(AIndex, Length(EXPORT_PREFIX));
      Exit(True);
    end;
    Inc(AIndex);
  end;
  Result := False;
end;

function NaturalSortCode(const ACharacter: Char): Integer; inline;
var
  CharacterCode: Integer;
begin
  CharacterCode := Ord(ACharacter);
  if (CharacterCode < 45) or (CharacterCode > 127) then
    Exit(CharacterCode);
  if CharacterCode < 46 then
    Exit(65); // -
  if CharacterCode < 48 then
    Exit(CharacterCode - 1);
  if CharacterCode < 58 then
    Exit(CharacterCode + 18); // 0-9
  if CharacterCode < 65 then
    Exit(CharacterCode - 11);
  if CharacterCode < 91 then
    Exit(CharacterCode + 11); // A-Z
  if CharacterCode < 97 then
    Exit(CharacterCode - 37);
  if CharacterCode < 123 then
    Exit(CharacterCode + 5); // a-z
  Result := CharacterCode - 63;
end;

function NaturalCompareText(constref ALeft, ARight: string): Integer;
var
  LeftIndex, RightIndex: Integer;
  LeftStart, RightStart, LeftSignificant, RightSignificant: Integer;
  LeftDigits, RightDigits: Integer;
  LeftCode, RightCode: Integer;
  LeftChar, RightChar: Char;
begin
  LeftIndex := 1;
  RightIndex := 1;
  while (LeftIndex <= Length(ALeft)) and (RightIndex <= Length(ARight)) do
  begin
    LeftChar := ALeft[LeftIndex];
    RightChar := ARight[RightIndex];
    if (LeftChar in ['0'..'9']) and (RightChar in ['0'..'9']) then
    begin
      LeftStart := LeftIndex;
      RightStart := RightIndex;
      while (LeftIndex <= Length(ALeft)) and
            (ALeft[LeftIndex] in ['0'..'9']) do
        Inc(LeftIndex);
      while (RightIndex <= Length(ARight)) and
            (ARight[RightIndex] in ['0'..'9']) do
        Inc(RightIndex);
      LeftSignificant := LeftStart;
      while (LeftSignificant < LeftIndex) and
            (ALeft[LeftSignificant] = '0') do
        Inc(LeftSignificant);
      RightSignificant := RightStart;
      while (RightSignificant < RightIndex) and
            (ARight[RightSignificant] = '0') do
        Inc(RightSignificant);
      LeftDigits := LeftIndex - LeftSignificant;
      RightDigits := RightIndex - RightSignificant;
      if LeftDigits < RightDigits then
        Exit(-1);
      if LeftDigits > RightDigits then
        Exit(1);
      while (LeftSignificant < LeftIndex) and
            (RightSignificant < RightIndex) do
      begin
        if ALeft[LeftSignificant] < ARight[RightSignificant] then
          Exit(-1);
        if ALeft[LeftSignificant] > ARight[RightSignificant] then
          Exit(1);
        Inc(LeftSignificant);
        Inc(RightSignificant);
      end;
      if (LeftIndex - LeftStart) < (RightIndex - RightStart) then
        Exit(-1);
      if (LeftIndex - LeftStart) > (RightIndex - RightStart) then
        Exit(1);
      Continue;
    end;

    LeftCode := NaturalSortCode(LeftChar);
    RightCode := NaturalSortCode(RightChar);
    if LeftCode < RightCode then
      Exit(-1);
    if LeftCode > RightCode then
      Exit(1);
    Inc(LeftIndex);
    Inc(RightIndex);
  end;

  Result := Length(ALeft) - Length(ARight);
end;

{ TGocciaMemorySnapshotHost }

constructor TGocciaMemorySnapshotHost.Create(
  const AHasPersistentSource: Boolean);
begin
  inherited Create;
  FHasPersistentSource := AHasPersistentSource;
  FSnapshotContent := '';
  FSnapshotExists := False;
  FFlushed := False;
end;

function TGocciaMemorySnapshotHost.HasPersistentSource: Boolean;
begin
  Result := FHasPersistentSource;
end;

function TGocciaMemorySnapshotHost.ReadSnapshotFile(
  out AContent: string): Boolean;
begin
  AContent := FSnapshotContent;
  Result := FSnapshotExists;
end;

procedure TGocciaMemorySnapshotHost.WriteSnapshotFile(
  const AContent: string);
begin
  FSnapshotContent := AContent;
  FSnapshotExists := True;
end;

procedure TGocciaMemorySnapshotHost.DeleteSnapshotFile;
begin
  FSnapshotContent := '';
  FSnapshotExists := False;
end;

procedure TGocciaMemorySnapshotHost.QueueInlineSnapshot(
  const AEdit: TGocciaInlineSnapshotEdit);
var
  Index: Integer;
begin
  Index := Length(FInlineEdits);
  SetLength(FInlineEdits, Index + 1);
  FInlineEdits[Index] := AEdit;
end;

procedure TGocciaMemorySnapshotHost.FlushInlineSnapshots;
begin
  FFlushed := True;
end;

function TGocciaMemorySnapshotHost.InlineEditCount: Integer;
begin
  Result := Length(FInlineEdits);
end;

function TGocciaMemorySnapshotHost.InlineEdit(
  const AIndex: Integer): TGocciaInlineSnapshotEdit;
begin
  Result := FInlineEdits[AIndex];
end;

{ TGocciaSnapshotState }

constructor TGocciaSnapshotState.Create(const AHost: IGocciaSnapshotHost;
  const AUpdateMode: TGocciaSnapshotUpdateMode);
begin
  inherited Create;
  if not Assigned(AHost) then
    raise Exception.Create('Snapshot state requires a host adapter');
  FHost := AHost;
  FUpdateMode := AUpdateMode;
  FSnapshots := TDictionary<string, string>.Create;
  FUncheckedKeys := TDictionary<string, Boolean>.Create;
  FCounters := TDictionary<string, Integer>.Create;
  LoadSnapshotFile;
end;

destructor TGocciaSnapshotState.Destroy;
begin
  FCounters.Free;
  FUncheckedKeys.Free;
  FSnapshots.Free;
  FHost := nil;
  inherited;
end;

procedure TGocciaSnapshotState.LoadSnapshotFile;
var
  Content, Key, Value: string;
  Index: Integer;
begin
  FSnapshotFileExists := FHost.ReadSnapshotFile(Content);
  if not FSnapshotFileExists then
    Exit;
  try
    Index := 1;
    while Index <= Length(Content) do
    begin
      if not FindNextSnapshotExport(Content, Index) then
        Break;
      if not ReadBacktickString(Content, Index, Key) then
        raise Exception.Create('Invalid snapshot file: expected snapshot key');
      Key := NormalizeSnapshotNewlines(Key);
      while (Index <= Length(Content)) and (Content[Index] <> '=') do
        Inc(Index);
      if Index > Length(Content) then
        raise Exception.Create('Invalid snapshot file: expected "="');
      Inc(Index);
      while (Index <= Length(Content)) and
            (Content[Index] in [' ', #9, #10, #13]) do
        Inc(Index);
      if not ReadBacktickString(Content, Index, Value) then
        raise Exception.Create(
          'Invalid snapshot file: expected snapshot value');
      Value := NormalizeSnapshotNewlines(Value);
      FSnapshots.AddOrSetValue(Key, Value);
      FUncheckedKeys.AddOrSetValue(Key, True);
    end;
  except
    FSnapshots.Clear;
    FUncheckedKeys.Clear;
  end;
end;

function TGocciaSnapshotState.ResolveKey(const ATestName,
  AHint: string): string;
var
  BaseKey: string;
  Count: Integer;
begin
  if AHint <> '' then
    BaseKey := ATestName + ' > ' + AHint
  else
    BaseKey := ATestName;

  if not FCounters.TryGetValue(BaseKey, Count) then
    Count := 0;
  Inc(Count);
  FCounters.AddOrSetValue(BaseKey, Count);
  Result := BaseKey + ' ' + IntToStr(Count);
end;

procedure TGocciaSnapshotState.MarkChecked(const AKey: string);
begin
  FUncheckedKeys.Remove(AKey);
end;

procedure TGocciaSnapshotState.PreserveTestSnapshots(
  const ATestName: string);
var
  Key, Suffix: string;
begin
  for Key in FUncheckedKeys.Keys.ToArray do
    if Copy(Key, 1, Length(ATestName)) = ATestName then
    begin
      Suffix := Copy(Key, Length(ATestName) + 1, MaxInt);
      if ((Length(Suffix) > 1) and (Suffix[1] = ' ') and
          (Suffix[2] in ['0'..'9'])) or
         (Copy(Suffix, 1, 3) = ' > ') then
        MarkChecked(Key);
    end;
end;

function TGocciaSnapshotState.Reconcile(const AKey, AReceived,
  AExpected: string; const AHasSnapshot, APropertyMatched: Boolean;
  out AFailureMessage: string): Boolean;
var
  Matches: Boolean;
begin
  AFailureMessage := '';
  if not APropertyMatched then
  begin
    AFailureMessage := 'Snapshot properties mismatched';
    Exit(False);
  end;

  Matches := AHasSnapshot and (Trim(AExpected) = Trim(AReceived));
  if Matches then
    Exit(True);

  if (AHasSnapshot and (FUpdateMode = sumAll)) or
     ((not AHasSnapshot) and (FUpdateMode in [sumNew, sumAll])) then
    Exit(True);

  if AHasSnapshot then
    AFailureMessage := 'Snapshot `' + AKey + '` mismatched' + LineEnding +
      'Expected: ' + RemoveSnapshotLineBreaks(AExpected) + LineEnding +
      'Received: ' + RemoveSnapshotLineBreaks(AReceived)
  else
    AFailureMessage := 'Snapshot `' + AKey + '` is missing';
  Result := False;
end;

function TGocciaSnapshotState.MatchExternal(const ATestName, AHint,
  AReceived: string; const APropertyMatched: Boolean;
  out AFailureMessage: string): Boolean;
var
  Key, Expected: string;
  HasSnapshot: Boolean;
begin
  if not FHost.HasPersistentSource then
  begin
    AFailureMessage := 'External snapshots cannot be used from stdin';
    Exit(False);
  end;
  Key := ResolveKey(ATestName, AHint);
  MarkChecked(Key);
  HasSnapshot := FSnapshots.TryGetValue(Key, Expected);
  Result := Reconcile(Key, AReceived, Expected, HasSnapshot,
    APropertyMatched, AFailureMessage);
  if not Result or not APropertyMatched then
    Exit;

  if (not HasSnapshot) or
     ((FUpdateMode = sumAll) and (Trim(Expected) <> Trim(AReceived))) then
  begin
    FSnapshots.AddOrSetValue(Key, AReceived);
    FDirty := True;
  end
  else
    { Refresh escaping when another snapshot makes the file dirty. }
    FSnapshots.AddOrSetValue(Key, AReceived);
end;

function TGocciaSnapshotState.MatchInline(const ATestName, AHint,
  AReceived, AInlineSnapshot: string; const AHasInlineSnapshot,
  APropertyMatched: Boolean; const ASourcePath: string;
  const ALine, AColumn,
  ASnapshotArgumentIndex: Integer; out AFailureMessage: string): Boolean;
var
  Edit: TGocciaInlineSnapshotEdit;
  Key: string;
begin
  Key := ResolveKey(ATestName, AHint);
  { An inline assertion supersedes an old external snapshot with the same key. }
  if not FSnapshots.ContainsKey(Key) then
    MarkChecked(Key);

  Result := Reconcile(Key, AReceived, AInlineSnapshot,
    AHasInlineSnapshot, APropertyMatched, AFailureMessage);
  if not Result or not APropertyMatched then
    Exit;

  if AHasInlineSnapshot and
     ((FUpdateMode <> sumAll) or
      (Trim(AInlineSnapshot) = Trim(AReceived))) then
    Exit;

  if not FHost.HasPersistentSource then
  begin
    AFailureMessage :=
      'Inline snapshots cannot be created or updated from stdin';
    Exit(False);
  end;

  Edit.SourcePath := ASourcePath;
  Edit.Line := ALine;
  Edit.Column := AColumn;
  Edit.SnapshotArgumentIndex := ASnapshotArgumentIndex;
  Edit.Snapshot := AReceived;
  FHost.QueueInlineSnapshot(Edit);
end;

function TGocciaSnapshotState.SerializeSnapshotFile: string;
var
  I: Integer;
  Key: string;
  Keys: TList<string>;
begin
  Keys := TList<string>.Create;
  try
    for Key in FSnapshots.Keys do
      Keys.Add(Key);
    Keys.Sort(TComparer<string>.Construct(NaturalCompareText));

    Result := SNAPSHOT_HEADER + #10#10;
    for I := 0 to Keys.Count - 1 do
    begin
      if I > 0 then
        Result := Result + #10#10;
      Key := Keys[I];
      Result := Result + 'exports[`' + EscapeBacktickString(Key) +
        '`] = `' + EscapeBacktickString(FSnapshots[Key]) + '`;';
    end;
    Result := Result + #10;
  finally
    Keys.Free;
  end;
end;

procedure TGocciaSnapshotState.Finish(const AErrors: TStrings;
  const AProcessUnchecked: Boolean);
var
  Key: string;
  UncheckedKeys: TArray<string>;
begin
  UncheckedKeys := FUncheckedKeys.Keys.ToArray;
  if AProcessUnchecked and (Length(UncheckedKeys) > 0) then
  begin
    if FUpdateMode = sumAll then
    begin
      for Key in UncheckedKeys do
        FSnapshots.Remove(Key);
      FDirty := True;
    end
    else if FUpdateMode = sumNone then
      for Key in UncheckedKeys do
        AErrors.Add('Obsolete snapshot: ' + Key);
  end;

  if (FUpdateMode = sumAll) and FSnapshotFileExists and
     (FSnapshots.Count = 0) then
    FHost.DeleteSnapshotFile
  else if FDirty then
  begin
    FHost.WriteSnapshotFile(SerializeSnapshotFile);
  end;
  FHost.FlushInlineSnapshots;
end;

end.

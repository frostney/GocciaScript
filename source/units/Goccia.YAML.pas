unit Goccia.YAML;

{ YAML 1.2.2 (yaml.org/spec/1.2.2) into a Goccia.DataTree tree, composed from
  the events of Goccia.YAML.Events. An anchor names the node it is on, and an
  alias is that node again, so the tree shares it wherever an alias stands,
  its own content included.

  Plain scalars resolve by the YAML 1.2 core schema (10.3), with the
  extensions Bun's YAML also reads: digit separators ("1_000"), "0b" binary
  integers and "<<" merge keys. A tag outside the core schema, and the
  !!timestamp and !!binary tags, keep their tag on the node as a tagged node.
  A mapping's keys become strings: a scalar key its ECMAScript string, an
  empty plain key "", and a collection key a canonical JSON-like string. }

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.DataTree,
  Goccia.YAML.Events,
  Goccia.YAML.Scanner;

type
  EGocciaYAMLParseError = Goccia.YAML.Scanner.EGocciaYAMLParseError;

  TGocciaYAMLParser = class
  private
    FAnchors: TGocciaDataNodeMap;
    FEvents: TGocciaYAMLEventParser;
    FTree: TGocciaDataTree;

    procedure RaiseError(const AMark: TGocciaYAMLMarker; const AMessage: string);
    function NextEvent: TGocciaYAMLEvent;
    procedure RegisterAnchor(const AEvent: TGocciaYAMLEvent;
      const ANode: TGocciaDataNode);
    function ResolvePlainScalar(const AText: string): TGocciaDataNode;
    function ComposeScalar(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
    function ApplyCollectionTag(const AEvent: TGocciaYAMLEvent;
      const ANode: TGocciaDataNode): TGocciaDataNode;
    function ComposeSequence(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
    function ComposeMapping(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
    function ComposeNode(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
    procedure MergeMappingValue(const ATarget, AValue: TGocciaDataNode;
      const AMark: TGocciaYAMLMarker);
    function KeyName(const AEvent: TGocciaYAMLEvent;
      const AKey: TGocciaDataNode): string;
    function ParseStream(const AText: string;
      out AExplicitDocumentStart: Boolean): TGocciaDataTree;
  public
    constructor Create;
    destructor Destroy; override;

    { the stream's value: null for no document, the document for one without a
      "---" marker, otherwise an array of the documents }
    function Parse(const AText: string): TGocciaDataTree;
    { an array of the stream's documents }
    function ParseDocuments(const AText: string): TGocciaDataTree;
  end;

implementation

uses
  Math,

  NumericText,

  Goccia.Base64,
  Goccia.Temporal.Utils;

const
  TAG_NON_SPECIFIC = '!';
  TAG_PREFIX_STANDARD = 'tag:yaml.org,2002:';
  TAG_STANDARD_NULL = TAG_PREFIX_STANDARD + 'null';
  TAG_STANDARD_BOOL = TAG_PREFIX_STANDARD + 'bool';
  TAG_STANDARD_INT = TAG_PREFIX_STANDARD + 'int';
  TAG_STANDARD_FLOAT = TAG_PREFIX_STANDARD + 'float';
  TAG_STANDARD_STR = TAG_PREFIX_STANDARD + 'str';
  TAG_STANDARD_SEQ = TAG_PREFIX_STANDARD + 'seq';
  TAG_STANDARD_MAP = TAG_PREFIX_STANDARD + 'map';
  TAG_STANDARD_TIMESTAMP = TAG_PREFIX_STANDARD + 'timestamp';
  TAG_STANDARD_BINARY = TAG_PREFIX_STANDARD + 'binary';
  MERGE_KEY = '<<';
  BASE64_BLOCK_LENGTH = 4;
  DATA_DEPTH_OPERATION = 'YAML parsing';

function RemoveNumericSeparators(const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
    if AText[I] <> '_' then
      Result := Result + AText[I];
end;

function IsDigitForBase(const AChar: Char; const ABase: Integer): Boolean;
begin
  case AChar of
    '0'..'9':
      Result := (Ord(AChar) - Ord('0')) < ABase;
    'a'..'f':
      Result := (Ord(AChar) - Ord('a') + 10) < ABase;
    'A'..'F':
      Result := (Ord(AChar) - Ord('A') + 10) < ABase;
  else
    Result := False;
  end;
end;

function IsValidDigitRun(const AText: string; const ABase: Integer;
  const AAllowEmpty: Boolean = False): Boolean;
var
  I: Integer;
begin
  if AText = '' then
    Exit(AAllowEmpty);
  if (AText[1] = '_') or (AText[Length(AText)] = '_') then
    Exit(False);

  for I := 1 to Length(AText) do
  begin
    if AText[I] = '_' then
    begin
      if (I = 1) or (I = Length(AText)) then
        Exit(False);
      if (AText[I - 1] = '_') or (AText[I + 1] = '_') then
        Exit(False);
      if not IsDigitForBase(AText[I - 1], ABase) or
         not IsDigitForBase(AText[I + 1], ABase) then
        Exit(False);
      Continue;
    end;

    if not IsDigitForBase(AText[I], ABase) then
      Exit(False);
  end;

  Result := True;
end;

function ParseUnsignedIntegerBase(const ADigits: string; const ABase: Integer;
  out AValue: Int64): Boolean;
var
  DigitValue, I: Integer;
  Value: Int64;
begin
  Value := 0;
  if ADigits = '' then
    Exit(False);

  for I := 1 to Length(ADigits) do
  begin
    case ADigits[I] of
      '0'..'9':
        DigitValue := Ord(ADigits[I]) - Ord('0');
      'a'..'f':
        DigitValue := Ord(ADigits[I]) - Ord('a') + 10;
      'A'..'F':
        DigitValue := Ord(ADigits[I]) - Ord('A') + 10;
    else
      Exit(False);
    end;

    if DigitValue >= ABase then
      Exit(False);
    Value := Value * ABase + DigitValue;
  end;

  AValue := Value;
  Result := True;
end;

function TryParseYAMLInteger(const AText: string; out AValue: Double): Boolean;
var
  Digits, Signless, Sanitized: string;
  Sign: Int64;
  IntegerValue: Int64;
begin
  Sanitized := RemoveNumericSeparators(AText);
  if Sanitized = '' then
    Exit(False);

  Sign := 1;
  Signless := AText;
  if (Signless[1] = '+') or (Signless[1] = '-') then
  begin
    if Signless[1] = '-' then
      Sign := -1;
    Delete(Signless, 1, 1);
  end;

  if Signless = '' then
    Exit(False);

  if (Length(Signless) > 2) and (LowerCase(Copy(Signless, 1, 2)) = '0x') then
  begin
    Digits := Copy(Signless, 3, MaxInt);
    if not IsValidDigitRun(Digits, 16) then
      Exit(False);
    Digits := RemoveNumericSeparators(Digits);
    if not ParseUnsignedIntegerBase(Digits, 16, IntegerValue) then
      Exit(False);
    AValue := Sign * IntegerValue;
    Exit(True);
  end;

  if (Length(Signless) > 2) and (LowerCase(Copy(Signless, 1, 2)) = '0o') then
  begin
    Digits := Copy(Signless, 3, MaxInt);
    if not IsValidDigitRun(Digits, 8) then
      Exit(False);
    Digits := RemoveNumericSeparators(Digits);
    if not ParseUnsignedIntegerBase(Digits, 8, IntegerValue) then
      Exit(False);
    AValue := Sign * IntegerValue;
    Exit(True);
  end;

  if (Length(Signless) > 2) and (LowerCase(Copy(Signless, 1, 2)) = '0b') then
  begin
    Digits := Copy(Signless, 3, MaxInt);
    if not IsValidDigitRun(Digits, 2) then
      Exit(False);
    Digits := RemoveNumericSeparators(Digits);
    if not ParseUnsignedIntegerBase(Digits, 2, IntegerValue) then
      Exit(False);
    AValue := Sign * IntegerValue;
    Exit(True);
  end;

  if not IsValidDigitRun(Signless, 10) then
    Exit(False);

  if TryStrToInt64(Sanitized, IntegerValue) then
  begin
    AValue := IntegerValue;
    Exit(True);
  end;

  Result := False;
end;

function TryParseYAMLFloat(const AText: string; out AValue: Double): Boolean;
var
  ExponentText, FractionalText, IntegerText, Lowered, MantissaText, Sanitized,
    Signless: string;
  DotIndex, ExponentIndex, I: Integer;
begin
  Sanitized := RemoveNumericSeparators(AText);
  Lowered := LowerCase(Sanitized);

  if (Lowered = '.inf') or (Lowered = '+.inf') then
  begin
    AValue := Infinity;
    Exit(True);
  end;

  if Lowered = '-.inf' then
  begin
    AValue := -Infinity;
    Exit(True);
  end;

  if (Lowered = '.nan') or (Lowered = '+.nan') or (Lowered = '-.nan') then
  begin
    AValue := NaN;
    Exit(True);
  end;

  Signless := AText;
  if (Signless <> '') and ((Signless[1] = '+') or (Signless[1] = '-')) then
    Delete(Signless, 1, 1);
  if Signless = '' then
    Exit(False);

  ExponentIndex := 0;
  for I := 1 to Length(Signless) do
    if Signless[I] in ['e', 'E'] then
    begin
      if ExponentIndex <> 0 then
        Exit(False);
      ExponentIndex := I;
    end;

  if ExponentIndex > 0 then
  begin
    MantissaText := Copy(Signless, 1, ExponentIndex - 1);
    ExponentText := Copy(Signless, ExponentIndex + 1, MaxInt);
    if ExponentText = '' then
      Exit(False);
    if (ExponentText[1] = '+') or (ExponentText[1] = '-') then
      Delete(ExponentText, 1, 1);
    if not IsValidDigitRun(ExponentText, 10) then
      Exit(False);
  end
  else
  begin
    MantissaText := Signless;
    ExponentText := '';
  end;

  DotIndex := Pos('.', MantissaText);
  if DotIndex = 0 then
  begin
    if ExponentIndex = 0 then
      Exit(False);
    if not IsValidDigitRun(MantissaText, 10) then
      Exit(False);
  end
  else
  begin
    if Pos('.', Copy(MantissaText, DotIndex + 1, MaxInt)) > 0 then
      Exit(False);
    IntegerText := Copy(MantissaText, 1, DotIndex - 1);
    FractionalText := Copy(MantissaText, DotIndex + 1, MaxInt);
    if not IsValidDigitRun(IntegerText, 10, True) then
      Exit(False);
    if not IsValidDigitRun(FractionalText, 10, True) then
      Exit(False);
    if (IntegerText = '') and (FractionalText = '') then
      Exit(False);
    if (IntegerText = '') and (FractionalText <> '') then
    begin
      if FractionalText[1] = '_' then
        Exit(False);
    end;
    if (FractionalText = '') and (IntegerText = '') then
      Exit(False);
  end;

  AValue := DecimalTextToNumber(Sanitized);
  Result := not IsNan(AValue);
end;

function IsValidYAMLTimestamp(const AText: string): Boolean;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
begin
  Result := TryParseISODate(AText, DateRec) or TryParseISODateTime(AText, DateRec, TimeRec);
end;

function IsValidBase64Text(const AText: string): Boolean;
var
  Bytes: TBytes;
  Compact: string;
  I: Integer;
begin
  Compact := '';
  for I := 1 to Length(AText) do
    if not CharInSet(AText[I], [' ', #9, #10, #13]) then
      Compact := Compact + AText[I];
  Result := (Compact <> '') and ((Length(Compact) mod BASE64_BLOCK_LENGTH) = 0) and
    TryDecodeBase64Standard(Compact, Bytes);
end;

function EscapeCompositeKeyString(const AText: string): string;
var
  I: Integer;
begin
  Result := '"';
  for I := 1 to Length(AText) do
    case AText[I] of
      '\':
        Result := Result + '\\';
      '"':
        Result := Result + '\"';
      #10:
        Result := Result + '\n';
      #13:
        Result := Result + '\r';
      #9:
        Result := Result + '\t';
    else
      Result := Result + AText[I];
    end;
  Result := Result + '"';
end;

{ a key that is a collection as a stable JSON-like string }
function CanonicalizeKeyValue(const AValue: TGocciaDataNode;
  const AInComposite: Boolean = False): string;
var
  I: Integer;
  KeyNames: TArray<string>;
begin
  if AValue.Kind = dnkTagged then
    Exit(CanonicalizeKeyValue(AValue.TaggedValue, AInComposite));

  if AValue.IsContainer then
  begin
    // a key that contains itself has no string: the depth guard stops it
    EnterDataDepth(DATA_DEPTH_OPERATION);
    try
      if AValue.Kind = dnkArray then
      begin
        Result := '[';
        for I := 0 to AValue.Count - 1 do
        begin
          if I > 0 then
            Result := Result + ', ';
          Result := Result + CanonicalizeKeyValue(AValue.Items[I], True);
        end;
        Result := Result + ']';
      end
      else
      begin
        Result := '{';
        KeyNames := AValue.OwnKeys;
        for I := 0 to Length(KeyNames) - 1 do
        begin
          if I > 0 then
            Result := Result + ', ';
          Result := Result + EscapeCompositeKeyString(KeyNames[I]) + ': ' +
            CanonicalizeKeyValue(AValue.GetProperty(KeyNames[I]), True);
        end;
        Result := Result + '}';
      end;
    finally
      LeaveDataDepth;
    end;
    Exit;
  end;

  Result := AValue.ToText;
  if AInComposite and (AValue.Kind = dnkString) then
    Result := EscapeCompositeKeyString(Result);
end;

function IsCoreScalarTag(const ATag: string): Boolean;
begin
  Result := SameText(ATag, TAG_STANDARD_STR) or SameText(ATag, TAG_STANDARD_NULL) or
    SameText(ATag, TAG_STANDARD_BOOL) or SameText(ATag, TAG_STANDARD_INT) or
    SameText(ATag, TAG_STANDARD_FLOAT) or SameText(ATag, TAG_STANDARD_TIMESTAMP) or
    SameText(ATag, TAG_STANDARD_BINARY);
end;

{ TGocciaYAMLParser }

constructor TGocciaYAMLParser.Create;
begin
  inherited Create;
  FAnchors := TGocciaDataNodeMap.Create;
end;

destructor TGocciaYAMLParser.Destroy;
begin
  FAnchors.Free;
  inherited;
end;

procedure TGocciaYAMLParser.RaiseError(const AMark: TGocciaYAMLMarker;
  const AMessage: string);
begin
  raise EGocciaYAMLParseError.CreateFmt('Line %d, column %d: %s',
    [AMark.Line, AMark.Column + 1, AMessage]);
end;

function TGocciaYAMLParser.NextEvent: TGocciaYAMLEvent;
begin
  CheckDataWork;
  Result := FEvents.NextEvent;
end;

procedure TGocciaYAMLParser.RegisterAnchor(const AEvent: TGocciaYAMLEvent;
  const ANode: TGocciaDataNode);
begin
  if AEvent.Anchor <> '' then
    FAnchors.AddOrSetValue(AEvent.Anchor, ANode);
end;

{ a plain scalar by the core schema and its extensions }
function TGocciaYAMLParser.ResolvePlainScalar(const AText: string): TGocciaDataNode;
var
  NumericValue: Double;
begin
  if (AText = '') or SameText(AText, 'null') or (AText = '~') then
    Result := FTree.NewNull
  else if SameText(AText, 'true') then
    Result := FTree.NewBoolean(True)
  else if SameText(AText, 'false') then
    Result := FTree.NewBoolean(False)
  else if TryParseYAMLInteger(AText, NumericValue) or
          TryParseYAMLFloat(AText, NumericValue) then
    Result := FTree.NewNumber(NumericValue)
  else
    Result := FTree.NewString(AText);
end;

function TGocciaYAMLParser.ComposeScalar(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
var
  NumericValue: Double;
  Tag, Text: string;
begin
  Text := AEvent.Value;
  if not AEvent.HasTag then
  begin
    if AEvent.Style = yssPlain then
      Exit(ResolvePlainScalar(Text));
    Exit(FTree.NewString(Text));
  end;

  Tag := AEvent.Tag;
  // the non-specific tag "!" makes any scalar a string (6.9.1)
  if (Tag = TAG_NON_SPECIFIC) or SameText(Tag, TAG_STANDARD_STR) then
    Exit(FTree.NewString(Text));

  if SameText(Tag, TAG_STANDARD_NULL) then
  begin
    if (Text <> '') and not SameText(Text, 'null') and (Text <> '~') then
      RaiseError(AEvent.StartMark,
        'The !!null tag requires an empty scalar, "null", or "~".');
    Exit(FTree.NewNull);
  end;

  if SameText(Tag, TAG_STANDARD_BOOL) then
  begin
    if SameText(Text, 'true') then
      Exit(FTree.NewBoolean(True));
    if SameText(Text, 'false') then
      Exit(FTree.NewBoolean(False));
    RaiseError(AEvent.StartMark, 'The !!bool tag requires "true" or "false".');
  end;

  if SameText(Tag, TAG_STANDARD_INT) then
  begin
    if not TryParseYAMLInteger(Text, NumericValue) then
      RaiseError(AEvent.StartMark, 'The !!int tag requires a valid integer scalar.');
    Exit(FTree.NewNumber(NumericValue));
  end;

  if SameText(Tag, TAG_STANDARD_FLOAT) then
  begin
    if not (TryParseYAMLInteger(Text, NumericValue) or
            TryParseYAMLFloat(Text, NumericValue)) then
      RaiseError(AEvent.StartMark, 'The !!float tag requires a valid numeric scalar.');
    Exit(FTree.NewNumber(NumericValue));
  end;

  if SameText(Tag, TAG_STANDARD_TIMESTAMP) then
  begin
    if not IsValidYAMLTimestamp(Text) then
      RaiseError(AEvent.StartMark,
        'The !!timestamp tag requires a valid ISO date or date-time scalar.');
    Exit(FTree.NewTagged(Tag, FTree.NewString(Text)));
  end;

  // the base64 text itself: decoded bytes have no string form
  if SameText(Tag, TAG_STANDARD_BINARY) then
  begin
    if not IsValidBase64Text(Text) then
      RaiseError(AEvent.StartMark, 'The !!binary tag requires valid base64 content.');
    Exit(FTree.NewTagged(Tag, FTree.NewString(Text)));
  end;

  if SameText(Tag, TAG_STANDARD_SEQ) then
    RaiseError(AEvent.StartMark, 'The !!seq tag requires a sequence value.');
  if SameText(Tag, TAG_STANDARD_MAP) then
    RaiseError(AEvent.StartMark, 'The !!map tag requires a mapping value.');

  if AEvent.Style = yssPlain then
    Result := FTree.NewTagged(Tag, ResolvePlainScalar(Text))
  else
    Result := FTree.NewTagged(Tag, FTree.NewString(Text));
end;

function TGocciaYAMLParser.ApplyCollectionTag(const AEvent: TGocciaYAMLEvent;
  const ANode: TGocciaDataNode): TGocciaDataNode;
begin
  if not AEvent.HasTag or (AEvent.Tag = TAG_NON_SPECIFIC) then
    Exit(ANode);
  if SameText(AEvent.Tag, TAG_STANDARD_SEQ) then
  begin
    if ANode.Kind <> dnkArray then
      RaiseError(AEvent.StartMark, 'The !!seq tag requires a sequence value.');
    Exit(ANode);
  end;
  if SameText(AEvent.Tag, TAG_STANDARD_MAP) then
  begin
    if ANode.Kind <> dnkObject then
      RaiseError(AEvent.StartMark, 'The !!map tag requires a mapping value.');
    Exit(ANode);
  end;
  if IsCoreScalarTag(AEvent.Tag) then
    RaiseError(AEvent.StartMark,
      'Scalar tags cannot be applied to mappings or sequences.');
  Result := FTree.NewTagged(AEvent.Tag, ANode);
end;

function TGocciaYAMLParser.ComposeSequence(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
var
  Item: TGocciaYAMLEvent;
begin
  EnterDataDepth(DATA_DEPTH_OPERATION);
  try
    Result := FTree.NewArray;
    // the anchor names the sequence before its entries, which may alias it
    RegisterAnchor(AEvent, Result);
    while True do
    begin
      Item := NextEvent;
      if Item.Kind = yekSequenceEnd then
        Break;
      Result.Add(ComposeNode(Item));
    end;
    Result := ApplyCollectionTag(AEvent, Result);
    RegisterAnchor(AEvent, Result);
  finally
    LeaveDataDepth;
  end;
end;

function TGocciaYAMLParser.ComposeMapping(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
var
  KeyEvent: TGocciaYAMLEvent;
  Key, Value: TGocciaDataNode;
begin
  EnterDataDepth(DATA_DEPTH_OPERATION);
  try
    Result := FTree.NewObject;
    RegisterAnchor(AEvent, Result);
    while True do
    begin
      KeyEvent := NextEvent;
      if KeyEvent.Kind = yekMappingEnd then
        Break;
      Key := ComposeNode(KeyEvent);
      Value := ComposeNode(NextEvent);
      if (KeyEvent.Kind = yekScalar) and (KeyEvent.Style = yssPlain) and
         not KeyEvent.HasTag and (KeyEvent.Value = MERGE_KEY) then
        MergeMappingValue(Result, Value, KeyEvent.StartMark)
      else
        Result.SetProperty(KeyName(KeyEvent, Key), Value);
    end;
    Result := ApplyCollectionTag(AEvent, Result);
    RegisterAnchor(AEvent, Result);
  finally
    LeaveDataDepth;
  end;
end;

function TGocciaYAMLParser.ComposeNode(const AEvent: TGocciaYAMLEvent): TGocciaDataNode;
begin
  case AEvent.Kind of
    yekAlias:
      if not FAnchors.TryGetValue(AEvent.Value, Result) then
        RaiseError(AEvent.StartMark, 'Unknown alias *' + AEvent.Value + '.');
    yekScalar:
      begin
        Result := ComposeScalar(AEvent);
        RegisterAnchor(AEvent, Result);
      end;
    yekSequenceStart:
      Result := ComposeSequence(AEvent);
    yekMappingStart:
      Result := ComposeMapping(AEvent);
  else
    RaiseError(AEvent.StartMark, 'Expected a YAML node.');
  end;
end;

{ "<<": the mapping, or each mapping of a sequence, adds the keys the target
  does not have yet (yaml.org/type/merge.html). A sequence holds mappings
  only, so a sequence that holds itself through an alias is no cycle here. }
procedure TGocciaYAMLParser.MergeMappingValue(const ATarget, AValue: TGocciaDataNode;
  const AMark: TGocciaYAMLMarker);

  procedure MergeMapping(const AMapping: TGocciaDataNode);
  var
    Key: string;
    Source: TGocciaDataNode;
  begin
    Source := AMapping;
    if Source.Kind = dnkTagged then
      Source := Source.TaggedValue;
    if Source.Kind <> dnkObject then
      RaiseError(AMark, 'Merge keys must reference a mapping or a sequence of mappings.');
    for Key in Source.OwnKeys do
      if not ATarget.HasProperty(Key) then
        ATarget.SetProperty(Key, Source.GetProperty(Key));
  end;

var
  I: Integer;
  Source: TGocciaDataNode;
begin
  Source := AValue;
  if Source.Kind = dnkTagged then
    Source := Source.TaggedValue;
  if Source.Kind = dnkArray then
  begin
    for I := 0 to Source.Count - 1 do
      MergeMapping(Source.Items[I]);
    Exit;
  end;
  MergeMapping(Source);
end;

function TGocciaYAMLParser.KeyName(const AEvent: TGocciaYAMLEvent;
  const AKey: TGocciaDataNode): string;
begin
  if (AEvent.Kind = yekScalar) and (AEvent.Style = yssPlain) and
     not AEvent.HasTag and (AEvent.Value = '') then
    Exit('');
  Result := CanonicalizeKeyValue(AKey);
end;

function TGocciaYAMLParser.ParseStream(const AText: string;
  out AExplicitDocumentStart: Boolean): TGocciaDataTree;
var
  Event: TGocciaYAMLEvent;
begin
  AExplicitDocumentStart := False;
  Result := TGocciaDataTree.Create;
  try
    Result.Root := Result.NewArray;
    FTree := Result;
    FEvents := TGocciaYAMLEventParser.Create(AText);
    try
      NextEvent;
      while True do
      begin
        Event := NextEvent;
        if Event.Kind = yekStreamEnd then
          Break;
        if Event.Explicit then
          AExplicitDocumentStart := True;
        // anchors name nodes of their own document only
        FAnchors.Clear;
        Result.Root.Add(ComposeNode(NextEvent));
        NextEvent;
      end;
    finally
      FreeAndNil(FEvents);
      FTree := nil;
      // the last document's anchors would otherwise name nodes of a tree the
      // caller may free
      FAnchors.Clear;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TGocciaYAMLParser.Parse(const AText: string): TGocciaDataTree;
var
  Documents: TGocciaDataNode;
  ExplicitDocumentStart: Boolean;
begin
  Result := ParseStream(AText, ExplicitDocumentStart);
  Documents := Result.Root;
  if Documents.Count = 0 then
    Result.Root := Result.NewNull
  else if not ExplicitDocumentStart then
    Result.Root := Documents.Items[0];
end;

function TGocciaYAMLParser.ParseDocuments(const AText: string): TGocciaDataTree;
var
  ExplicitDocumentStart: Boolean;
begin
  Result := ParseStream(AText, ExplicitDocumentStart);
end;

end.

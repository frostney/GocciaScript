unit Goccia.YAML;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  EGocciaYAMLParseError = class(Exception);

  TGocciaYAMLLine = record
    RawText: string;
    Number: Integer;
    Indent: Integer;
    IsIgnorable: Boolean;
    Text: string;
  end;

  TGocciaYAMLParser = class
  private
    FAnchors: TGocciaValueMap;
    FExplicitTagHandles: TStringStringMap;
    FTagHandles: TStringStringMap;
    FLines: array of TGocciaYAMLLine;
    FLineCount: Integer;
    FIndex: Integer;
    FYAMLVersion: string;

    procedure InitializeTagHandles;
    procedure ParseDirectiveLine(const ALineText: string; const ALineNumber: Integer);
    function TryParseAnchorPrefix(const AText: string; out AAnchorName: string;
      out ARemainingText: string): Boolean;
    function TryParseTagPrefix(const AText: string; out ATagName: string;
      out ARemainingText: string; const ALineNumber: Integer = 0): Boolean;
    procedure ParseNodeProperties(const AText: string; out ATagName,
      AAnchorName, ARemainingText: string; const ALineNumber: Integer = 0);
    function ApplyTag(const ATagName: string; const AValue: TGocciaValue;
      const ALineNumber: Integer = 0): TGocciaValue;
    procedure RegisterAnchor(const AAnchorName: string; const AValue: TGocciaValue;
      const ALineNumber: Integer = 0);
    function ResolveAliasValue(const AAliasName: string;
      const ALineNumber: Integer = 0): TGocciaValue;
    procedure MergeMappingValue(const ATarget: TGocciaObjectValue;
      const AValue: TGocciaValue; const ALineNumber: Integer);
    procedure SkipIgnorableLines;
    procedure Reset;
    procedure AddLine(const ALineNumber, AIndent: Integer; const ARawText,
      AText: string; const AIsIgnorable: Boolean);
    procedure LoadLines(const AText: string);
    function CurrentLine: TGocciaYAMLLine;
    function HasCurrentLine: Boolean;
    procedure RaiseParseError(const AMessage: string; const ALineNumber: Integer = 0);
    function IsBlockScalarHeader(const AText: string): Boolean;
    function ParseBlockScalar(const AParentIndent: Integer;
      const AHeaderText: string; const ALineNumber: Integer): TGocciaValue;
    function ParseNode(const AIndent: Integer): TGocciaValue;
    function ParseNestedNode(const AParentIndent: Integer): TGocciaValue;
    function ParseExplicitNestedNode(const AParentIndent: Integer): TGocciaValue;
    function ParseMapping(const AIndent: Integer): TGocciaValue;
    procedure ParseMappingInto(const AIndent: Integer;
      const AObject: TGocciaObjectValue);
    function ParseSequence(const AIndent: Integer): TGocciaValue;
    procedure ParseSequenceInto(const AIndent: Integer;
      const AArray: TGocciaArrayValue);
    function ParseMappingValueText(const AValueText: string; const ALineIndent,
      ALineNumber: Integer): TGocciaValue;
    procedure ParseMappingEntry(const AContainer: TGocciaObjectValue;
      const ALineText: string; const ALineIndent, ALineNumber: Integer);
    procedure ParseExplicitMappingEntry(const AContainer: TGocciaObjectValue;
      const ALineText: string; const ALineIndent, ALineNumber: Integer);
    function ParseSequenceEntryMapping(const ASequenceIndent: Integer;
      const AEntryText: string; const ALineNumber: Integer): TGocciaValue;
    procedure ParseSequenceEntryMappingInto(const ASequenceIndent: Integer;
      const AEntryText: string; const ALineNumber: Integer;
      const AObject: TGocciaObjectValue);
    function ParseNodeValue(const AValueText: string; const ALineIndent,
      ALineNumber: Integer; const ATagName, AAnchorName: string;
      const AAdvanceBeforeNested, ASequenceEntryInlineMapping,
      AStopAtIndentedSequenceEntry, AAllowSameIndentNestedNode,
      ARequireIndentedQuotedContinuation,
      AAllowSameIndentScalarContinuation: Boolean): TGocciaValue;
    function ParseInlineValue(const AText: string): TGocciaValue;
    function ParseInlineValueWithContinuations(const AText: string;
      const AParentIndent, ALineNumber: Integer;
      const AStopAtIndentedSequenceEntry: Boolean = False;
      const ARequireIndentedQuotedContinuations: Boolean = False;
      const AAllowSameIndentScalarContinuations: Boolean = False): TGocciaValue;
    function ParseFlowSequence(const AText: string): TGocciaValue;
    procedure ParseFlowSequenceInto(const AText: string;
      const AArray: TGocciaArrayValue);
    function ParseFlowMapping(const AText: string): TGocciaValue;
    procedure ParseFlowMappingEntry(const AObject: TGocciaObjectValue;
      const AItemText: string; const ALineNumber: Integer = 0);
    procedure ParseFlowMappingInto(const AText: string;
      const AObject: TGocciaObjectValue);
    procedure ParseInlineSequenceInto(const AFirstEntryText: string;
      const ASequenceIndent, ALineNumber: Integer;
      const AArray: TGocciaArrayValue);
    function CollectFlowCollectionText(const AInitialText: string;
      const ALineNumber: Integer): string;
    function CanonicalizeKeyValue(const AValue: TGocciaValue;
      const AInComposite: Boolean = False): string;
    function ParseKeyName(const AText: string;
      const ALineNumber: Integer = 0): string;
    function ParseSingleDocument(const AText: string): TGocciaValue;
  public
    constructor Create;
    destructor Destroy; override;

    function Parse(const AText: string): TGocciaValue;
    function ParseDocuments(const AText: string): TGocciaArrayValue;
  end;

implementation

uses
  Classes,
  Math,

  base64,

  Goccia.Constants.PropertyNames,
  Goccia.Temporal.Utils,
  Goccia.TextFiles;

type
  TGocciaYAMLTaggedValue = class(TGocciaValue)
  private
    FTagName: string;
    FValue: TGocciaValue;
  public
    constructor Create(const ATagName: string; const AValue: TGocciaValue);
    procedure MarkReferences; override;
    function RuntimeCopy: TGocciaValue; override;
    function IsPrimitive: Boolean; override;
    function IsCallable: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    property TagName: string read FTagName;
    property Value: TGocciaValue read FValue;
  end;

const
  DOCUMENT_START_MARKER = '---';
  DOCUMENT_END_MARKER = '...';
  DOUBLE_QUOTE = '"';
  SINGLE_QUOTE = '''';
  TAG_HANDLE_LOCAL = '!';
  TAG_HANDLE_STANDARD = '!!';
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

constructor TGocciaYAMLTaggedValue.Create(const ATagName: string;
  const AValue: TGocciaValue);
begin
  inherited Create;
  FTagName := ATagName;
  FValue := AValue;
end;

procedure TGocciaYAMLTaggedValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FValue) then
    FValue.MarkReferences;
end;

function TGocciaYAMLTaggedValue.RuntimeCopy: TGocciaValue;
begin
  Result := TGocciaYAMLTaggedValue.Create(FTagName, FValue.RuntimeCopy);
end;

function TGocciaYAMLTaggedValue.IsPrimitive: Boolean;
begin
  Result := FValue.IsPrimitive;
end;

function TGocciaYAMLTaggedValue.IsCallable: Boolean;
begin
  Result := FValue.IsCallable;
end;

function TGocciaYAMLTaggedValue.TypeName: string;
begin
  Result := FValue.TypeName;
end;

function TGocciaYAMLTaggedValue.TypeOf: string;
begin
  Result := FValue.TypeOf;
end;

function TGocciaYAMLTaggedValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := FValue.ToBooleanLiteral;
end;

function TGocciaYAMLTaggedValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := FValue.ToNumberLiteral;
end;

function TGocciaYAMLTaggedValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := FValue.ToStringLiteral;
end;

function TGocciaYAMLTaggedValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_TAG_NAME then
    Exit(TGocciaStringLiteralValue.Create(FTagName));
  if AName = PROP_VALUE then
    Exit(FValue);
  Result := FValue.GetProperty(AName);
end;

procedure TGocciaYAMLTaggedValue.SetProperty(const AName: string;
  const AValue: TGocciaValue);
begin
  FValue.SetProperty(AName, AValue);
end;

function IsWhitespace(const AChar: Char): Boolean;
begin
  Result := (AChar = ' ') or (AChar = #9);
end;

function IsEscapedByBackslashes(const AText: string; const AIndex: Integer): Boolean;
var
  BackslashCount, Cursor: Integer;
begin
  BackslashCount := 0;
  Cursor := AIndex - 1;
  while (Cursor >= 1) and (AText[Cursor] = '\') do
  begin
    Inc(BackslashCount);
    Dec(Cursor);
  end;
  Result := (BackslashCount mod 2) = 1;
end;

function IsFlowDelimiter(const AChar: Char): Boolean;
begin
  Result := (AChar = ',') or (AChar = ']') or (AChar = '}');
end;

function CanStartQuotedScalar(const AText: string; const AIndex: Integer): Boolean;
var
  PreviousChar: Char;
begin
  if (AIndex < 1) or (AIndex > Length(AText)) then
    Exit(False);

  if AIndex = 1 then
    Exit(True);

  PreviousChar := AText[AIndex - 1];
  Result := IsWhitespace(PreviousChar) or
    (PreviousChar in ['[', '{', ',', ':']);
end;

function CountIndentation(const AText: string): Integer;
begin
  Result := 0;
  while (Result < Length(AText)) and (AText[Result + 1] = ' ') do
    Inc(Result);
end;

function HasInvalidTabAfterBlockIndicator(const AText: string): Boolean;
var
  Cursor: Integer;
begin
  Result := False;
  if (AText = '') or not (AText[1] in ['-', '?', ':']) then
    Exit;

  if (Length(AText) > 1) and not (AText[2] in [' ', #9]) then
    Exit;

  Cursor := 2;
  while (Cursor <= Length(AText)) and (AText[Cursor] in [' ', #9]) do
  begin
    if AText[Cursor] = #9 then
      Exit(True);
    Inc(Cursor);
  end;
end;

function StripComment(const AText: string): string;
var
  I, BracketDepth, BraceDepth: Integer;
  InSingleQuote, InDoubleQuote, InVerbatimTag: Boolean;
begin
  InSingleQuote := False;
  InDoubleQuote := False;
  InVerbatimTag := False;
  BracketDepth := 0;
  BraceDepth := 0;
  I := 1;

  while I <= Length(AText) do
  begin
    case AText[I] of
      SINGLE_QUOTE:
        if not InDoubleQuote then
        begin
          if InSingleQuote and (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
            Inc(I)
          else if not InSingleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else
            InSingleQuote := not InSingleQuote;
        end;
      DOUBLE_QUOTE:
        if not InSingleQuote then
        begin
          if not InDoubleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else if not IsEscapedByBackslashes(AText, I) then
            InDoubleQuote := not InDoubleQuote;
        end;
      '<':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (I > 1) and (AText[I - 1] = '!') then
          InVerbatimTag := True;
      '>':
        if InVerbatimTag then
          InVerbatimTag := False;
      '[':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BracketDepth);
      ']':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth > 0) then
          Dec(BracketDepth);
      '{':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BraceDepth);
      '}':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BraceDepth > 0) then
          Dec(BraceDepth);
      '#':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth = 0) and
           (BraceDepth = 0) and ((I = 1) or IsWhitespace(AText[I - 1])) then
          Exit(TrimRight(Copy(AText, 1, I - 1)));
    end;
    Inc(I);
  end;

  Result := TrimRight(AText);
end;

function FindTopLevelMappingSeparator(const AText: string): Integer;
var
  I, BracketDepth, BraceDepth: Integer;
  InSingleQuote, InDoubleQuote, InVerbatimTag: Boolean;
begin
  Result := 0;
  InSingleQuote := False;
  InDoubleQuote := False;
  InVerbatimTag := False;
  BracketDepth := 0;
  BraceDepth := 0;
  I := 1;

  while I <= Length(AText) do
  begin
    case AText[I] of
      SINGLE_QUOTE:
        if not InDoubleQuote then
        begin
          if InSingleQuote and (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
            Inc(I)
          else if not InSingleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else
            InSingleQuote := not InSingleQuote;
        end;
      DOUBLE_QUOTE:
        if not InSingleQuote then
        begin
          if not InDoubleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else if not IsEscapedByBackslashes(AText, I) then
            InDoubleQuote := not InDoubleQuote;
        end;
      '<':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (I > 1) and (AText[I - 1] = '!') then
          InVerbatimTag := True;
      '>':
        if InVerbatimTag then
          InVerbatimTag := False;
      '[':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BracketDepth);
      ']':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth > 0) then
          Dec(BracketDepth);
      '{':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BraceDepth);
      '}':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BraceDepth > 0) then
          Dec(BraceDepth);
      ':':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth = 0) and
           (BraceDepth = 0) and
           ((I = Length(AText)) or IsWhitespace(AText[I + 1]) or
            IsFlowDelimiter(AText[I + 1])) then
          Exit(I);
    end;
    Inc(I);
  end;
end;

function FindTopLevelFlowMappingSeparator(const AText: string): Integer;
var
  I, BracketDepth, BraceDepth: Integer;
  InSingleQuote, InDoubleQuote, InVerbatimTag: Boolean;
  KeyText: string;
begin
  Result := 0;
  InSingleQuote := False;
  InDoubleQuote := False;
  InVerbatimTag := False;
  BracketDepth := 0;
  BraceDepth := 0;
  I := 1;

  while I <= Length(AText) do
  begin
    case AText[I] of
      SINGLE_QUOTE:
        if not InDoubleQuote then
        begin
          if InSingleQuote and (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
            Inc(I)
          else if not InSingleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else
            InSingleQuote := not InSingleQuote;
        end;
      DOUBLE_QUOTE:
        if not InSingleQuote then
        begin
          if not InDoubleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else if not IsEscapedByBackslashes(AText, I) then
            InDoubleQuote := not InDoubleQuote;
        end;
      '<':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (I > 1) and (AText[I - 1] = '!') then
          InVerbatimTag := True;
      '>':
        if InVerbatimTag then
          InVerbatimTag := False;
      '[':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BracketDepth);
      ']':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth > 0) then
          Dec(BracketDepth);
      '{':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BraceDepth);
      '}':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BraceDepth > 0) then
          Dec(BraceDepth);
      ':':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth = 0) and
           (BraceDepth = 0) then
        begin
          if (I = Length(AText)) or IsWhitespace(AText[I + 1]) or
             IsFlowDelimiter(AText[I + 1]) then
            Exit(I);

          KeyText := Trim(Copy(AText, 1, I - 1));
          if (KeyText <> '') and (KeyText[1] in [SINGLE_QUOTE, DOUBLE_QUOTE, '[', '{',
              '!', '&', '*']) then
            Exit(I);
        end;
    end;
    Inc(I);
  end;
end;

function FindTopLevelFlowCollectionClose(const AText: string): Integer;
var
  BracketDepth, BraceDepth, I: Integer;
  InSingleQuote, InDoubleQuote, InVerbatimTag: Boolean;
  RootDelimiter: Char;
begin
  Result := 0;
  if AText = '' then
    Exit;

  RootDelimiter := AText[1];
  if (RootDelimiter <> '[') and (RootDelimiter <> '{') then
    Exit;

  InSingleQuote := False;
  InDoubleQuote := False;
  InVerbatimTag := False;
  BracketDepth := 0;
  BraceDepth := 0;
  if RootDelimiter = '[' then
    BracketDepth := 1
  else
    BraceDepth := 1;
  I := 2;

  while I <= Length(AText) do
  begin
    case AText[I] of
      SINGLE_QUOTE:
        if not InDoubleQuote then
        begin
          if InSingleQuote and (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
            Inc(I)
          else if not InSingleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else
            InSingleQuote := not InSingleQuote;
        end;
      DOUBLE_QUOTE:
        if not InSingleQuote then
        begin
          if not InDoubleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else if not IsEscapedByBackslashes(AText, I) then
            InDoubleQuote := not InDoubleQuote;
        end;
      '<':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (I > 1) and (AText[I - 1] = '!') then
          InVerbatimTag := True;
      '>':
        if InVerbatimTag then
          InVerbatimTag := False;
      '[':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BracketDepth);
      ']':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
        begin
          Dec(BracketDepth);
          if (BracketDepth = 0) and (BraceDepth = 0) and (RootDelimiter = '[') then
            Exit(I);
        end;
      '{':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BraceDepth);
      '}':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
        begin
          Dec(BraceDepth);
          if (BracketDepth = 0) and (BraceDepth = 0) and (RootDelimiter = '{') then
            Exit(I);
        end;
    end;
    Inc(I);
  end;
end;

function StartsWithDocumentMarkerToken(const AText: string): Boolean;
var
  MarkerLength: Integer;
begin
  if Copy(AText, 1, Length(DOCUMENT_START_MARKER)) = DOCUMENT_START_MARKER then
    MarkerLength := Length(DOCUMENT_START_MARKER)
  else if Copy(AText, 1, Length(DOCUMENT_END_MARKER)) = DOCUMENT_END_MARKER then
    MarkerLength := Length(DOCUMENT_END_MARKER)
  else
    Exit(False);

  Result := (Length(AText) = MarkerLength) or
    IsWhitespace(AText[MarkerLength + 1]) or
    IsFlowDelimiter(AText[MarkerLength + 1]);
end;

function DocumentMarkerRemainder(const AText: string): string;
var
  MarkerLength: Integer;
begin
  if Copy(AText, 1, Length(DOCUMENT_START_MARKER)) = DOCUMENT_START_MARKER then
    MarkerLength := Length(DOCUMENT_START_MARKER)
  else if Copy(AText, 1, Length(DOCUMENT_END_MARKER)) = DOCUMENT_END_MARKER then
    MarkerLength := Length(DOCUMENT_END_MARKER)
  else
    Exit(AText);

  Result := TrimLeft(Copy(AText, MarkerLength + 1, MaxInt));
end;

function StartsSequenceEntry(const AText: string): Boolean;
begin
  Result := (AText <> '') and (AText[1] = '-') and
    ((Length(AText) = 1) or IsWhitespace(AText[2]));
end;

function StartsExplicitKeyEntry(const AText: string): Boolean;
begin
  Result := (AText <> '') and (AText[1] = '?') and
    ((Length(AText) = 1) or IsWhitespace(AText[2]));
end;

function StartsExplicitValueEntry(const AText: string): Boolean;
begin
  Result := (AText <> '') and (AText[1] = ':') and
    ((Length(AText) = 1) or IsWhitespace(AText[2]));
end;

function LooksLikeBlockMapping(const AText: string): Boolean;
begin
  Result := (AText <> '') and (AText[1] <> '[') and (AText[1] <> '{') and
    (FindTopLevelMappingSeparator(AText) > 0);
end;

function StartsBlockMappingEntry(const AText: string): Boolean;
begin
  Result := StartsExplicitKeyEntry(AText) or LooksLikeBlockMapping(AText);
end;

function IsBlockScalarHeaderText(const AText: string): Boolean;
var
  HeaderText: string;
  I: Integer;
begin
  HeaderText := Trim(AText);
  if (HeaderText = '') or ((HeaderText[1] <> '|') and (HeaderText[1] <> '>')) then
    Exit(False);

  for I := 2 to Length(HeaderText) do
    if not (HeaderText[I] in ['+', '-', '1'..'9']) then
      Exit(False);
  Result := True;
end;

function TryParseAnchorPrefixText(const AText: string; out ARemainingText: string): Boolean;
var
  I: Integer;
  TrimmedText: string;
begin
  TrimmedText := Trim(AText);
  ARemainingText := TrimmedText;

  if (TrimmedText = '') or (TrimmedText[1] <> '&') then
    Exit(False);

  I := 2;
  while (I <= Length(TrimmedText)) and not IsWhitespace(TrimmedText[I]) do
    Inc(I);

  if Copy(TrimmedText, 2, I - 2) = '' then
    Exit(False);

  ARemainingText := Trim(Copy(TrimmedText, I, MaxInt));
  Result := True;
end;

function TryParseTagPrefixText(const AText: string; out ARemainingText: string): Boolean;
var
  HandleEnd, PrefixEnd, WhitespaceIndex: Integer;
  TrimmedText: string;
begin
  TrimmedText := Trim(AText);
  ARemainingText := TrimmedText;
  if (TrimmedText = '') or (TrimmedText[1] <> '!') then
    Exit(False);

  if (Length(TrimmedText) >= 3) and (TrimmedText[2] = '<') then
  begin
    PrefixEnd := Pos('>', TrimmedText);
    if PrefixEnd = 0 then
      Exit(False);
    ARemainingText := Trim(Copy(TrimmedText, PrefixEnd + 1, MaxInt));
    Exit(True);
  end;

  if (Length(TrimmedText) = 1) or IsWhitespace(TrimmedText[2]) then
  begin
    ARemainingText := Trim(Copy(TrimmedText, 2, MaxInt));
    Exit(True);
  end;

  if TrimmedText[2] = '!' then
  begin
    HandleEnd := 2;
    while (HandleEnd < Length(TrimmedText)) and not IsWhitespace(TrimmedText[HandleEnd + 1]) do
      Inc(HandleEnd);
    ARemainingText := Trim(Copy(TrimmedText, HandleEnd + 1, MaxInt));
    Exit(True);
  end;

  HandleEnd := 2;
  while (HandleEnd <= Length(TrimmedText)) and not IsWhitespace(TrimmedText[HandleEnd]) do
  begin
    if TrimmedText[HandleEnd] = '!' then
    begin
      WhitespaceIndex := HandleEnd + 1;
      while (WhitespaceIndex <= Length(TrimmedText)) and
            not IsWhitespace(TrimmedText[WhitespaceIndex]) do
        Inc(WhitespaceIndex);
      ARemainingText := Trim(Copy(TrimmedText, WhitespaceIndex, MaxInt));
      Exit(True);
    end;
    Inc(HandleEnd);
  end;

  ARemainingText := Trim(Copy(TrimmedText, HandleEnd, MaxInt));
  Result := True;
end;

function StartsBlockScalarLine(const AText: string): Boolean;
var
  CandidateText, RemainingText: string;
  SeparatorIndex: Integer;
begin
  CandidateText := Trim(AText);
  if CandidateText = '' then
    Exit(False);

  if StartsSequenceEntry(CandidateText) then
    CandidateText := Trim(Copy(CandidateText, 2, MaxInt));

  SeparatorIndex := FindTopLevelMappingSeparator(CandidateText);
  if (SeparatorIndex > 0) and (CandidateText[1] <> '[') and (CandidateText[1] <> '{') then
    CandidateText := Trim(Copy(CandidateText, SeparatorIndex + 1, MaxInt));

  while True do
  begin
    if TryParseAnchorPrefixText(CandidateText, RemainingText) or
       TryParseTagPrefixText(CandidateText, RemainingText) then
    begin
      if RemainingText = '' then
        Exit(False);
      CandidateText := RemainingText;
      Continue;
    end;
    Break;
  end;

  Result := IsBlockScalarHeaderText(CandidateText);
end;

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

function YAMLFormatSettings: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
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

  Result := TryStrToFloat(Sanitized, AValue, YAMLFormatSettings);
end;

function CodePointToUTF8(const ACodePoint: Cardinal): string;
begin
  if ACodePoint <= $7F then
    Result := Chr(ACodePoint)
  else if ACodePoint <= $7FF then
    Result := Chr($C0 or (ACodePoint shr 6)) + Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $FFFF then
    Result := Chr($E0 or (ACodePoint shr 12)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else if ACodePoint <= $10FFFF then
    Result := Chr($F0 or (ACodePoint shr 18)) +
              Chr($80 or ((ACodePoint shr 12) and $3F)) +
              Chr($80 or ((ACodePoint shr 6) and $3F)) +
              Chr($80 or (ACodePoint and $3F))
  else
    raise EGocciaYAMLParseError.Create('Invalid Unicode code point in double-quoted scalar.');
end;

function IsHexDigitChar(const AChar: Char): Boolean;
begin
  Result := AChar in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

function UnescapeDoubleQuotedString(const AText: string): string;
var
  CodePoint: Cardinal;
  DigitCount, I, J: Integer;
  HexDigits: string;
  ParsedCodePoint: QWord;
begin
  Result := '';
  I := 1;
  while I <= Length(AText) do
  begin
    if (AText[I] = '\') and (I < Length(AText)) then
    begin
      Inc(I);
      case AText[I] of
        '"', '\', '/':
          Result := Result + AText[I];
        '0':
          Result := Result + #0;
        'a':
          Result := Result + #7;
        'b':
          Result := Result + #8;
        't':
          Result := Result + #9;
        'n':
          Result := Result + #10;
        'v':
          Result := Result + #11;
        'f':
          Result := Result + #12;
        'r':
          Result := Result + #13;
        'e':
          Result := Result + #27;
        ' ':
          Result := Result + ' ';
        #9:
          Result := Result + #9;
        'N':
          Result := Result + CodePointToUTF8($85);
        '_':
          Result := Result + CodePointToUTF8($A0);
        'L':
          Result := Result + CodePointToUTF8($2028);
        'P':
          Result := Result + CodePointToUTF8($2029);
        'x', 'u', 'U':
          begin
            case AText[I] of
              'x':
                DigitCount := 2;
              'u':
                DigitCount := 4;
            else
              DigitCount := 8;
            end;

            if I + DigitCount > Length(AText) then
              raise EGocciaYAMLParseError.Create('Invalid Unicode escape in double-quoted scalar.');
            HexDigits := Copy(AText, I + 1, DigitCount);
            for J := 1 to Length(HexDigits) do
              if not IsHexDigitChar(HexDigits[J]) then
                raise EGocciaYAMLParseError.Create('Invalid Unicode escape in double-quoted scalar.');
            if not TryStrToQWord('$' + HexDigits, ParsedCodePoint) then
              raise EGocciaYAMLParseError.Create('Invalid Unicode escape in double-quoted scalar.');
            CodePoint := ParsedCodePoint;
            Result := Result + CodePointToUTF8(CodePoint);
            Inc(I, Length(HexDigits));
          end;
        #10, #13:
          while (I < Length(AText)) and ((AText[I + 1] in [#10, #13]) or
                IsWhitespace(AText[I + 1])) do
            Inc(I);
      else
        raise EGocciaYAMLParseError.Create('Invalid escape sequence in double-quoted scalar.');
      end;
    end
    else
      Result := Result + AText[I];
    Inc(I);
  end;
end;

function UnescapeSingleQuotedString(const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(AText) do
  begin
    if (AText[I] = SINGLE_QUOTE) and (I < Length(AText)) and
       (AText[I + 1] = SINGLE_QUOTE) then
    begin
      Result := Result + SINGLE_QUOTE;
      Inc(I, 2);
      Continue;
    end;
    Result := Result + AText[I];
    Inc(I);
  end;
end;

function SplitTopLevelItems(const AText: string): TStringList;
var
  I, ItemStart, BracketDepth, BraceDepth: Integer;
  InSingleQuote, InDoubleQuote, InVerbatimTag: Boolean;
  ItemText: string;
begin
  Result := TStringList.Create;
  InSingleQuote := False;
  InDoubleQuote := False;
  InVerbatimTag := False;
  BracketDepth := 0;
  BraceDepth := 0;
  ItemStart := 1;
  I := 1;

  while I <= Length(AText) do
  begin
    case AText[I] of
      SINGLE_QUOTE:
        if not InDoubleQuote then
        begin
          if InSingleQuote and (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
            Inc(I)
          else if not InSingleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else
            InSingleQuote := not InSingleQuote;
        end;
      DOUBLE_QUOTE:
        if not InSingleQuote then
        begin
          if not InDoubleQuote and not CanStartQuotedScalar(AText, I) then
          begin
          end
          else if not IsEscapedByBackslashes(AText, I) then
            InDoubleQuote := not InDoubleQuote;
        end;
      '<':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (I > 1) and (AText[I - 1] = '!') then
          InVerbatimTag := True;
      '>':
        if InVerbatimTag then
          InVerbatimTag := False;
      '[':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BracketDepth);
      ']':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth > 0) then
          Dec(BracketDepth);
      '{':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag then
          Inc(BraceDepth);
      '}':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BraceDepth > 0) then
          Dec(BraceDepth);
      ',':
        if not InSingleQuote and not InDoubleQuote and not InVerbatimTag and
           (BracketDepth = 0) and
           (BraceDepth = 0) then
        begin
          ItemText := Trim(Copy(AText, ItemStart, I - ItemStart));
          if ItemText = '' then
            raise EGocciaYAMLParseError.Create('Invalid flow collection entry.');
          Result.Add(ItemText);
          ItemStart := I + 1;
        end;
    end;
    Inc(I);
  end;

  ItemText := Trim(Copy(AText, ItemStart, MaxInt));
  if ItemText <> '' then
    Result.Add(ItemText);
end;

function SplitYAMLDocuments(const AText: string): TStringList;
var
  DocumentOpen, HasContent, InBlockScalar: Boolean;
  BlockScalarIndent, I, LineIndent: Integer;
  LineText, MarkerText: string;
  Source, CurrentDocument: TStringList;

  procedure FlushCurrentDocument(const AForce: Boolean);
  begin
    if AForce or HasContent then
      Result.Add(StringListToLFText(CurrentDocument));
    CurrentDocument.Clear;
    DocumentOpen := False;
    HasContent := False;
  end;

begin
  Result := TStringList.Create;
  Source := CreateUTF8StringList(AText);
  CurrentDocument := TStringList.Create;
  try
    DocumentOpen := False;
    HasContent := False;
    InBlockScalar := False;
    BlockScalarIndent := 0;

    for I := 0 to Source.Count - 1 do
    begin
      LineText := Source[I];
      if (I = 0) and (LineText <> '') and (LineText[1] = #$FEFF) then
        Delete(LineText, 1, 1);

      LineIndent := CountIndentation(LineText);
      if InBlockScalar then
      begin
        if (Trim(LineText) = '') or (LineIndent > BlockScalarIndent) then
        begin
          CurrentDocument.Add(LineText);
          if Trim(LineText) <> '' then
          begin
            DocumentOpen := True;
            HasContent := True;
          end;
          Continue;
        end;
        InBlockScalar := False;
      end;

      MarkerText := Trim(StripComment(LineText));
      if (LineIndent = 0) and (MarkerText <> '') and (MarkerText[1] = '%') then
      begin
        if not HasContent and (DocumentOpen or HasContent) then
          FlushCurrentDocument(True);
        CurrentDocument.Add(LineText);
        Continue;
      end;

      if (LineIndent = 0) and StartsWithDocumentMarkerToken(MarkerText) and
         (Copy(MarkerText, 1, Length(DOCUMENT_START_MARKER)) = DOCUMENT_START_MARKER) then
      begin
        if DocumentOpen or HasContent then
          FlushCurrentDocument(True);
        DocumentOpen := True;
        MarkerText := DocumentMarkerRemainder(MarkerText);
        if MarkerText <> '' then
        begin
          CurrentDocument.Add(MarkerText);
          HasContent := True;
        end;
        Continue;
      end;

      if (LineIndent = 0) and StartsWithDocumentMarkerToken(MarkerText) and
         (Copy(MarkerText, 1, Length(DOCUMENT_END_MARKER)) = DOCUMENT_END_MARKER) then
      begin
        if DocumentOpen or HasContent then
          FlushCurrentDocument(True);
        Continue;
      end;

      CurrentDocument.Add(LineText);
      if Trim(MarkerText) <> '' then
      begin
        DocumentOpen := True;
        HasContent := True;
        if StartsBlockScalarLine(MarkerText) then
        begin
          InBlockScalar := True;
          BlockScalarIndent := LineIndent;
        end;
      end;
    end;

    if DocumentOpen or HasContent then
      FlushCurrentDocument(DocumentOpen);
  finally
    CurrentDocument.Free;
    Source.Free;
  end;
end;

function HasExplicitDocumentStartMarker(const AText: string): Boolean;
var
  I, BlockScalarIndent, LineIndent: Integer;
  InBlockScalar: Boolean;
  LineText: string;
  Source: TStringList;
begin
  Source := CreateUTF8StringList(AText);
  try
    InBlockScalar := False;
    BlockScalarIndent := 0;
    for I := 0 to Source.Count - 1 do
    begin
      LineText := Source[I];
      if (I = 0) and (LineText <> '') and (LineText[1] = #$FEFF) then
        Delete(LineText, 1, 1);

      LineIndent := CountIndentation(LineText);
      if InBlockScalar then
      begin
        if (Trim(LineText) = '') or (LineIndent > BlockScalarIndent) then
          Continue;
        InBlockScalar := False;
      end;

      LineText := Trim(StripComment(LineText));
      if (LineIndent = 0) and (LineText <> '') and (LineText[1] = '%') then
        Continue;
      if StartsWithDocumentMarkerToken(LineText) and
         (Copy(LineText, 1, Length(DOCUMENT_START_MARKER)) = DOCUMENT_START_MARKER) then
        Exit(True);
      if StartsBlockScalarLine(LineText) then
      begin
        InBlockScalar := True;
        BlockScalarIndent := LineIndent;
      end;
    end;
    Result := False;
  finally
    Source.Free;
  end;
end;

function EndsWithLineFeed(const AText: string): Boolean;
begin
  Result := (AText <> '') and (AText[Length(AText)] = #10);
end;

function TrimTrailingLineFeeds(const AText: string): string;
begin
  Result := AText;
  while EndsWithLineFeed(Result) do
    Delete(Result, Length(Result), 1);
end;

function RemoveAllWhitespace(const AText: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AText) do
    if not IsWhitespace(AText[I]) and (AText[I] <> #10) and (AText[I] <> #13) then
      Result := Result + AText[I];
end;

function IsValidTagHandle(const AHandleName: string): Boolean;
var
  I: Integer;
begin
  Result := (AHandleName = TAG_HANDLE_LOCAL) or
    ((Length(AHandleName) >= 2) and (AHandleName[1] = '!') and
    (AHandleName[Length(AHandleName)] = '!'));
  if not Result then
    Exit(False);

  for I := 2 to Length(AHandleName) - 1 do
    if IsWhitespace(AHandleName[I]) then
      Exit(False);
end;

function IsValidYAMLVersion(const AVersion: string): Boolean;
var
  DotIndex, MajorVersion, MinorVersion: Integer;
begin
  DotIndex := Pos('.', AVersion);
  if (DotIndex <= 1) or (DotIndex >= Length(AVersion)) then
    Exit(False);
  if not TryStrToInt(Copy(AVersion, 1, DotIndex - 1), MajorVersion) then
    Exit(False);
  if not TryStrToInt(Copy(AVersion, DotIndex + 1, MaxInt), MinorVersion) then
    Exit(False);
  Result := (MajorVersion = 1) and (MinorVersion >= 0);
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
  EqualsIndex, I: Integer;
begin
  if (AText = '') or ((Length(AText) mod 4) <> 0) then
    Exit(False);

  EqualsIndex := Pos('=', AText);
  if EqualsIndex > 0 then
    for I := EqualsIndex to Length(AText) do
      if AText[I] <> '=' then
        Exit(False);

  for I := 1 to Length(AText) do
    if not (AText[I] in ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '=']) then
      Exit(False);

  Result := True;
end;

function EscapeCompositeKeyString(const AText: string): string;
var
  I: Integer;
begin
  Result := DOUBLE_QUOTE;
  for I := 1 to Length(AText) do
    case AText[I] of
      '\':
        Result := Result + '\\';
      DOUBLE_QUOTE:
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
  Result := Result + DOUBLE_QUOTE;
end;

function IsQuotedScalarText(const AText: string; out AQuoteChar: Char): Boolean;
begin
  Result := AText <> '';
  if not Result then
    Exit(False);
  AQuoteChar := AText[1];
  Result := (AQuoteChar = DOUBLE_QUOTE) or (AQuoteChar = SINGLE_QUOTE);
end;

function FindClosingQuotePosition(const AText: string; const AQuoteChar: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 2;
  while I <= Length(AText) do
  begin
    if AText[I] = AQuoteChar then
    begin
      if AQuoteChar = DOUBLE_QUOTE then
      begin
        if not IsEscapedByBackslashes(AText, I) then
        begin
          Result := I;
          Exit;
        end;
      end
      else
      begin
        if (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
          Inc(I)
        else
        begin
          Result := I;
          Exit;
        end;
      end;
    end;
    Inc(I);
  end;
end;

function FindContinuationQuotePosition(const AText: string;
  const AQuoteChar: Char): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(AText) do
  begin
    if AText[I] = AQuoteChar then
    begin
      if AQuoteChar = DOUBLE_QUOTE then
      begin
        if not IsEscapedByBackslashes(AText, I) then
          Exit(I);
      end
      else
      begin
        if (I < Length(AText)) and (AText[I + 1] = SINGLE_QUOTE) then
          Inc(I)
        else
          Exit(I);
      end;
    end;
    Inc(I);
  end;
end;

function IsValidQuotedContinuationTail(const AText: string;
  const AQuotePosition: Integer): Boolean;
var
  RawTail, RemainingText: string;
begin
  RawTail := Copy(AText, AQuotePosition + 1, MaxInt);
  if RawTail = '' then
    Exit(True);

  if RawTail[1] = '#' then
    Exit(False);

  if not IsWhitespace(RawTail[1]) then
    Exit(False);

  RemainingText := TrimLeft(RawTail);
  Result := (RemainingText = '') or (RemainingText[1] = '#');
end;

function FoldMultilineScalarFragments(const AFragments: TStringList): string;
var
  Fragment: string;
  FragmentIndex, PendingBlankLines: Integer;
begin
  Result := '';
  PendingBlankLines := 0;
  for FragmentIndex := 0 to AFragments.Count - 1 do
  begin
    Fragment := Trim(AFragments[FragmentIndex]);
    if Fragment = '' then
    begin
      Inc(PendingBlankLines);
      Continue;
    end;

    if Result <> '' then
    begin
      if PendingBlankLines > 0 then
        Result := Result + StringOfChar(#10, PendingBlankLines)
      else
        Result := Result + ' ';
    end;
    Result := Result + Fragment;
    PendingBlankLines := 0;
  end;
end;

function EndsWithEscapedLineBreakMarker(const AText: string): Boolean;
var
  BackslashCount, Cursor: Integer;
begin
  BackslashCount := 0;
  Cursor := Length(AText);
  while (Cursor >= 1) and (AText[Cursor] = '\') do
  begin
    Inc(BackslashCount);
    Dec(Cursor);
  end;
  Result := (BackslashCount mod 2) = 1;
end;

function FoldMultilineDoubleQuotedFragments(const AFragments: TStringList): string;
var
  Fragment: string;
  FragmentIndex, PendingBlankLines: Integer;
  SuppressSeparator: Boolean;
begin
  Result := '';
  PendingBlankLines := 0;
  SuppressSeparator := False;
  for FragmentIndex := 0 to AFragments.Count - 1 do
  begin
    Fragment := Trim(AFragments[FragmentIndex]);
    if Fragment = '' then
    begin
      Inc(PendingBlankLines);
      Continue;
    end;

    if Result <> '' then
    begin
      if not SuppressSeparator then
      begin
        if PendingBlankLines > 0 then
          Result := Result + StringOfChar(#10, PendingBlankLines)
        else
          Result := Result + ' ';
      end;
    end;

    if EndsWithEscapedLineBreakMarker(Fragment) then
    begin
      Delete(Fragment, Length(Fragment), 1);
      SuppressSeparator := True;
    end
    else
      SuppressSeparator := False;

    Result := Result + Fragment;
    PendingBlankLines := 0;
  end;
end;

{ TGocciaYAMLParser }

constructor TGocciaYAMLParser.Create;
begin
  inherited Create;
  FAnchors := TGocciaValueMap.Create;
  FExplicitTagHandles := TStringStringMap.Create;
  FTagHandles := TStringStringMap.Create;
  InitializeTagHandles;
end;

destructor TGocciaYAMLParser.Destroy;
begin
  FTagHandles.Free;
  FExplicitTagHandles.Free;
  FAnchors.Free;
  inherited;
end;

procedure TGocciaYAMLParser.InitializeTagHandles;
begin
  FExplicitTagHandles.Clear;
  FTagHandles.Clear;
  FTagHandles.AddOrSetValue(TAG_HANDLE_LOCAL, TAG_HANDLE_LOCAL);
  FTagHandles.AddOrSetValue(TAG_HANDLE_STANDARD, TAG_PREFIX_STANDARD);
  FYAMLVersion := '';
end;

procedure TGocciaYAMLParser.ParseDirectiveLine(const ALineText: string;
  const ALineNumber: Integer);
var
  DirectiveName, DirectiveText, DirectiveValue, HandleName, PrefixText: string;
  SpaceIndex: Integer;
begin
  DirectiveText := Trim(StripComment(ALineText));
  if (DirectiveText = '') or (DirectiveText[1] <> '%') then
    Exit;

  Delete(DirectiveText, 1, 1);
  SpaceIndex := Pos(' ', DirectiveText);
  if SpaceIndex > 0 then
  begin
    DirectiveName := UpperCase(Copy(DirectiveText, 1, SpaceIndex - 1));
    DirectiveValue := Trim(Copy(DirectiveText, SpaceIndex + 1, MaxInt));
  end
  else
  begin
    DirectiveName := UpperCase(DirectiveText);
    DirectiveValue := '';
  end;

  if DirectiveName = 'YAML' then
  begin
    if FYAMLVersion <> '' then
      RaiseParseError('The %YAML directive may only appear once per document.',
        ALineNumber);
    if DirectiveValue = '' then
      RaiseParseError('The %YAML directive requires a version number.', ALineNumber);
    if not IsValidYAMLVersion(DirectiveValue) then
      RaiseParseError('Unsupported YAML version "' + DirectiveValue + '".',
        ALineNumber);
    FYAMLVersion := DirectiveValue;
    Exit;
  end;

  if DirectiveName = 'TAG' then
  begin
    SpaceIndex := Pos(' ', DirectiveValue);
    if SpaceIndex = 0 then
      RaiseParseError('The %TAG directive requires a handle and prefix.', ALineNumber);
    HandleName := Copy(DirectiveValue, 1, SpaceIndex - 1);
    PrefixText := Trim(Copy(DirectiveValue, SpaceIndex + 1, MaxInt));
    if (HandleName = '') or (PrefixText = '') then
      RaiseParseError('The %TAG directive requires a handle and prefix.', ALineNumber);
    if not IsValidTagHandle(HandleName) then
      RaiseParseError('Invalid YAML tag handle "' + HandleName + '".',
        ALineNumber);
    if FExplicitTagHandles.ContainsKey(HandleName) then
      RaiseParseError('The %TAG directive may only appear once per handle in the same document.',
        ALineNumber);
    FExplicitTagHandles.AddOrSetValue(HandleName, PrefixText);
    FTagHandles.AddOrSetValue(HandleName, PrefixText);
    Exit;
  end;
end;

function TGocciaYAMLParser.TryParseAnchorPrefix(const AText: string;
  out AAnchorName: string; out ARemainingText: string): Boolean;
var
  I: Integer;
  TrimmedText: string;
begin
  TrimmedText := Trim(AText);
  AAnchorName := '';
  ARemainingText := TrimmedText;

  if (TrimmedText = '') or (TrimmedText[1] <> '&') then
    Exit(False);

  I := 2;
  while (I <= Length(TrimmedText)) and not IsWhitespace(TrimmedText[I]) do
    Inc(I);

  AAnchorName := Copy(TrimmedText, 2, I - 2);
  if AAnchorName = '' then
    RaiseParseError('Anchor names cannot be empty.');

  ARemainingText := Trim(Copy(TrimmedText, I, MaxInt));
  Result := True;
end;

function TGocciaYAMLParser.TryParseTagPrefix(const AText: string; out ATagName: string;
  out ARemainingText: string; const ALineNumber: Integer): Boolean;
var
  HandleName, PrefixText, SuffixText, TrimmedText: string;
  HandleEnd, PrefixEnd, WhitespaceIndex: Integer;
begin
  TrimmedText := Trim(AText);
  ATagName := '';
  ARemainingText := TrimmedText;
  if (TrimmedText = '') or (TrimmedText[1] <> '!') then
    Exit(False);

  if (Length(TrimmedText) >= 3) and (TrimmedText[2] = '<') then
  begin
    PrefixEnd := Pos('>', TrimmedText);
    if PrefixEnd = 0 then
      RaiseParseError('Verbatim tags must end with ">".', ALineNumber);
    ATagName := Copy(TrimmedText, 3, PrefixEnd - 3);
    ARemainingText := Trim(Copy(TrimmedText, PrefixEnd + 1, MaxInt));
    Exit(True);
  end;

  if (Length(TrimmedText) = 1) or IsWhitespace(TrimmedText[2]) then
  begin
    ATagName := TAG_HANDLE_LOCAL;
    ARemainingText := Trim(Copy(TrimmedText, 2, MaxInt));
    Exit(True);
  end;

  HandleName := TAG_HANDLE_LOCAL;
  SuffixText := '';
  if TrimmedText[2] = '!' then
  begin
    WhitespaceIndex := 3;
    while (WhitespaceIndex <= Length(TrimmedText)) and
          not IsWhitespace(TrimmedText[WhitespaceIndex]) do
      Inc(WhitespaceIndex);
    HandleName := TAG_HANDLE_STANDARD;
    SuffixText := Copy(TrimmedText, 3, WhitespaceIndex - 3);
    ARemainingText := Trim(Copy(TrimmedText, WhitespaceIndex, MaxInt));
  end
  else
  begin
    HandleEnd := 2;
    while (HandleEnd <= Length(TrimmedText)) and not IsWhitespace(TrimmedText[HandleEnd]) do
    begin
      if TrimmedText[HandleEnd] = '!' then
      begin
        HandleName := Copy(TrimmedText, 1, HandleEnd);
        WhitespaceIndex := HandleEnd + 1;
        while (WhitespaceIndex <= Length(TrimmedText)) and
              not IsWhitespace(TrimmedText[WhitespaceIndex]) do
          Inc(WhitespaceIndex);
        SuffixText := Copy(TrimmedText, HandleEnd + 1, WhitespaceIndex - HandleEnd - 1);
        ARemainingText := Trim(Copy(TrimmedText, WhitespaceIndex, MaxInt));
        Break;
      end;
      Inc(HandleEnd);
    end;

	    if SuffixText = '' then
	    begin
	      WhitespaceIndex := 2;
	      while (WhitespaceIndex <= Length(TrimmedText)) and
	            not IsWhitespace(TrimmedText[WhitespaceIndex]) do
	        Inc(WhitespaceIndex);
	      SuffixText := Copy(TrimmedText, 2, WhitespaceIndex - 2);
	      ARemainingText := Trim(Copy(TrimmedText, WhitespaceIndex, MaxInt));
	    end;
	  end;

  if not FTagHandles.TryGetValue(HandleName, PrefixText) then
    RaiseParseError('Unknown tag handle ' + HandleName + '.', ALineNumber);
  ATagName := PrefixText + SuffixText;
  Result := True;
end;

procedure TGocciaYAMLParser.ParseNodeProperties(const AText: string; out ATagName,
  AAnchorName, ARemainingText: string; const ALineNumber: Integer);
var
  ParsedTagName, ParsedAnchorName, RemainingText: string;
begin
  ATagName := '';
  AAnchorName := '';
  ARemainingText := Trim(AText);
  while ARemainingText <> '' do
  begin
    if TryParseAnchorPrefix(ARemainingText, ParsedAnchorName, RemainingText) then
    begin
      if AAnchorName <> '' then
        RaiseParseError('Duplicate anchor property on the same YAML node.', ALineNumber);
      AAnchorName := ParsedAnchorName;
      ARemainingText := RemainingText;
      Continue;
    end;

    if TryParseTagPrefix(ARemainingText, ParsedTagName, RemainingText, ALineNumber) then
    begin
      if ATagName <> '' then
        RaiseParseError('Duplicate tag property on the same YAML node.', ALineNumber);
      ATagName := ParsedTagName;
      ARemainingText := RemainingText;
      Continue;
    end;
    Break;
  end;
end;

function TGocciaYAMLParser.ApplyTag(const ATagName: string; const AValue: TGocciaValue;
  const ALineNumber: Integer): TGocciaValue;
var
  ScalarText: string;
  NumericValue: Double;
begin
  if (ATagName = '') or (ATagName = TAG_HANDLE_LOCAL) then
    Exit(AValue);

  if not SameText(ATagName, TAG_STANDARD_SEQ) and
     not SameText(ATagName, TAG_STANDARD_MAP) and
     not SameText(ATagName, TAG_STANDARD_STR) and
     not SameText(ATagName, TAG_STANDARD_NULL) and
     not SameText(ATagName, TAG_STANDARD_BOOL) and
     not SameText(ATagName, TAG_STANDARD_INT) and
     not SameText(ATagName, TAG_STANDARD_FLOAT) and
     not SameText(ATagName, TAG_STANDARD_TIMESTAMP) and
     not SameText(ATagName, TAG_STANDARD_BINARY) then
    Exit(TGocciaYAMLTaggedValue.Create(ATagName, AValue));

  if SameText(ATagName, TAG_STANDARD_SEQ) then
  begin
    if not (AValue is TGocciaArrayValue) then
      RaiseParseError('The !!seq tag requires a sequence value.', ALineNumber);
    Exit(AValue);
  end;

  if SameText(ATagName, TAG_STANDARD_MAP) then
  begin
    if not (AValue is TGocciaObjectValue) then
      RaiseParseError('The !!map tag requires a mapping value.', ALineNumber);
    Exit(AValue);
  end;

  if (AValue is TGocciaArrayValue) or (AValue is TGocciaObjectValue) then
    RaiseParseError('Scalar tags cannot be applied to mappings or sequences.',
      ALineNumber);

  if AValue is TGocciaStringLiteralValue then
    ScalarText := TGocciaStringLiteralValue(AValue).Value
  else
    ScalarText := AValue.ToStringLiteral.Value;

  if SameText(ATagName, TAG_STANDARD_STR) then
    Exit(TGocciaStringLiteralValue.Create(ScalarText));

  if SameText(ATagName, TAG_STANDARD_NULL) then
  begin
    if (ScalarText <> '') and not SameText(ScalarText, 'null') and (ScalarText <> '~') then
      RaiseParseError('The !!null tag requires an empty scalar, "null", or "~".',
        ALineNumber);
    Exit(TGocciaNullLiteralValue.NullValue);
  end;

  if SameText(ATagName, TAG_STANDARD_BOOL) then
  begin
    if SameText(ScalarText, 'true') then
      Exit(TGocciaBooleanLiteralValue.TrueValue);
    if SameText(ScalarText, 'false') then
      Exit(TGocciaBooleanLiteralValue.FalseValue);
    RaiseParseError('The !!bool tag requires "true" or "false".', ALineNumber);
  end;

  if SameText(ATagName, TAG_STANDARD_INT) then
  begin
    if not TryParseYAMLInteger(ScalarText, NumericValue) then
      RaiseParseError('The !!int tag requires a valid integer scalar.', ALineNumber);
    Exit(TGocciaNumberLiteralValue.Create(NumericValue));
  end;

  if SameText(ATagName, TAG_STANDARD_FLOAT) then
  begin
    if not (TryParseYAMLInteger(ScalarText, NumericValue) or
            TryParseYAMLFloat(ScalarText, NumericValue)) then
      RaiseParseError('The !!float tag requires a valid numeric scalar.',
        ALineNumber);
    Exit(TGocciaNumberLiteralValue.Create(NumericValue));
  end;

  if SameText(ATagName, TAG_STANDARD_TIMESTAMP) then
  begin
    if not IsValidYAMLTimestamp(ScalarText) then
      RaiseParseError('The !!timestamp tag requires a valid ISO date or date-time scalar.',
        ALineNumber);
    Exit(TGocciaYAMLTaggedValue.Create(ATagName,
      TGocciaStringLiteralValue.Create(ScalarText)));
  end;

  if SameText(ATagName, TAG_STANDARD_BINARY) then
  begin
    ScalarText := RemoveAllWhitespace(ScalarText);
    if not IsValidBase64Text(ScalarText) then
      RaiseParseError('The !!binary tag requires valid base64 content.',
        ALineNumber);
    try
      ScalarText := DecodeStringBase64(ScalarText);
    except
      on E: Exception do
        RaiseParseError('The !!binary tag requires valid base64 content.',
          ALineNumber);
    end;
    Exit(TGocciaYAMLTaggedValue.Create(ATagName,
      TGocciaStringLiteralValue.Create(ScalarText)));
  end;

  Result := AValue;
end;

procedure TGocciaYAMLParser.RegisterAnchor(const AAnchorName: string;
  const AValue: TGocciaValue; const ALineNumber: Integer);
begin
  if AAnchorName = '' then
    Exit;
  if not Assigned(AValue) then
    RaiseParseError('Anchors must reference a value.', ALineNumber);
  FAnchors.AddOrSetValue(AAnchorName, AValue);
end;

function TGocciaYAMLParser.ResolveAliasValue(const AAliasName: string;
  const ALineNumber: Integer): TGocciaValue;
begin
  if not FAnchors.TryGetValue(AAliasName, Result) then
    RaiseParseError('Unknown alias *' + AAliasName + '.', ALineNumber);
end;

procedure TGocciaYAMLParser.MergeMappingValue(const ATarget: TGocciaObjectValue;
  const AValue: TGocciaValue; const ALineNumber: Integer);
var
  I: Integer;
  Key: string;
  SourceArray: TGocciaArrayValue;
  SourceObject: TGocciaObjectValue;
  SourceValue: TGocciaValue;
begin
  if AValue is TGocciaArrayValue then
  begin
    SourceArray := TGocciaArrayValue(AValue);
    for I := 0 to SourceArray.Elements.Count - 1 do
      MergeMappingValue(ATarget, SourceArray.Elements[I], ALineNumber);
    Exit;
  end;

  if not (AValue is TGocciaObjectValue) then
    RaiseParseError('Merge keys must reference a mapping or a sequence of mappings.',
      ALineNumber);

  SourceObject := TGocciaObjectValue(AValue);
  for Key in SourceObject.GetOwnPropertyKeys do
    if not ATarget.HasOwnProperty(Key) then
    begin
      SourceValue := SourceObject.GetProperty(Key);
      ATarget.AssignProperty(Key, SourceValue);
    end;
end;

procedure TGocciaYAMLParser.Reset;
begin
  FAnchors.Clear;
  InitializeTagHandles;
  SetLength(FLines, 0);
  FLineCount := 0;
  FIndex := 0;
end;

procedure TGocciaYAMLParser.SkipIgnorableLines;
begin
  while HasCurrentLine and CurrentLine.IsIgnorable do
    Inc(FIndex);
end;

procedure TGocciaYAMLParser.AddLine(const ALineNumber, AIndent: Integer;
  const ARawText, AText: string; const AIsIgnorable: Boolean);
begin
  if FLineCount = Length(FLines) then
    SetLength(FLines, FLineCount + 32);
  FLines[FLineCount].RawText := ARawText;
  FLines[FLineCount].Number := ALineNumber;
  FLines[FLineCount].Indent := AIndent;
  FLines[FLineCount].IsIgnorable := AIsIgnorable;
  FLines[FLineCount].Text := AText;
  Inc(FLineCount);
end;

procedure TGocciaYAMLParser.LoadLines(const AText: string);
var
  I, Indent: Integer;
  ContentText, LineText, ParsedLine, TrimmedLine: string;
  SeenContent: Boolean;
  Source: TStringList;
begin
  Reset;
  Source := CreateUTF8StringList(AText);
  try
    SeenContent := False;
    for I := 0 to Source.Count - 1 do
    begin
      LineText := Source[I];
      if (I = 0) and (LineText <> '') and (LineText[1] = #$FEFF) then
        Delete(LineText, 1, 1);

      if (LineText <> '') and (LineText[1] = #9) then
        RaiseParseError('Tabs are not allowed in YAML indentation.', I + 1);

      ParsedLine := StripComment(LineText);
      TrimmedLine := TrimRight(ParsedLine);
      Indent := CountIndentation(LineText);
      ContentText := Copy(TrimmedLine, Indent + 1, MaxInt);
      if HasInvalidTabAfterBlockIndicator(ContentText) then
        RaiseParseError('Tabs are not allowed in YAML indentation.', I + 1);

      if (Indent = 0) and (Trim(TrimmedLine) <> '') and (Trim(TrimmedLine)[1] = '%') then
      begin
        if SeenContent then
          RaiseParseError('YAML directives must appear before document content.',
            I + 1);
        ParseDirectiveLine(LineText, I + 1);
        Continue;
      end;

      if Trim(TrimmedLine) = '' then
        AddLine(I + 1, Indent, LineText, '', True)
      else
      begin
        AddLine(I + 1, Indent, LineText, ContentText, False);
        SeenContent := True;
      end;
    end;
  finally
    Source.Free;
  end;
end;

function TGocciaYAMLParser.CurrentLine: TGocciaYAMLLine;
begin
  Result := FLines[FIndex];
end;

function TGocciaYAMLParser.HasCurrentLine: Boolean;
begin
  Result := FIndex < FLineCount;
end;

procedure TGocciaYAMLParser.RaiseParseError(const AMessage: string;
  const ALineNumber: Integer);
begin
  if ALineNumber > 0 then
    raise EGocciaYAMLParseError.CreateFmt('Line %d: %s', [ALineNumber, AMessage]);
  raise EGocciaYAMLParseError.Create(AMessage);
end;

function TGocciaYAMLParser.ParseSingleDocument(const AText: string): TGocciaValue;
var
  RootIndent: Integer;
begin
  LoadLines(AText);
  SkipIgnorableLines;
  if FLineCount = 0 then
    Exit(TGocciaNullLiteralValue.NullValue);
  if not HasCurrentLine then
    Exit(TGocciaNullLiteralValue.NullValue);

  RootIndent := CurrentLine.Indent;
  Result := ParseNode(RootIndent);
  SkipIgnorableLines;
  if HasCurrentLine then
    RaiseParseError('Unexpected trailing content.', CurrentLine.Number);
end;

function TGocciaYAMLParser.Parse(const AText: string): TGocciaValue;
var
  Documents: TGocciaArrayValue;
  HasExplicitDocumentStart: Boolean;
  SingleDocument: TGocciaValue;
begin
  HasExplicitDocumentStart := HasExplicitDocumentStartMarker(AText);
  Documents := ParseDocuments(AText);
  try
    if Documents.Elements.Count = 0 then
      Exit(TGocciaNullLiteralValue.NullValue);
    if HasExplicitDocumentStart then
      Exit(Documents);
    SingleDocument := Documents.Elements[0];
    Documents.Elements.Extract(SingleDocument);
    Result := SingleDocument;
  finally
    if Result <> Documents then
      Documents.Free;
  end;
end;

function TGocciaYAMLParser.ParseDocuments(const AText: string): TGocciaArrayValue;
var
  DocumentIndex: Integer;
  DocumentText: TStringList;
begin
  Result := TGocciaArrayValue.Create;
  DocumentText := SplitYAMLDocuments(AText);
  try
    for DocumentIndex := 0 to DocumentText.Count - 1 do
      Result.Elements.Add(ParseSingleDocument(DocumentText[DocumentIndex]));
  finally
    DocumentText.Free;
  end;
end;

function TGocciaYAMLParser.ParseNode(const AIndent: Integer): TGocciaValue;
var
  AnchorName, RemainingText, TagName: string;
  LineNumber: Integer;
begin
  SkipIgnorableLines;
  if not HasCurrentLine then
    Exit(TGocciaNullLiteralValue.NullValue);

  if CurrentLine.Indent <> AIndent then
    RaiseParseError('Invalid indentation.', CurrentLine.Number);
  LineNumber := CurrentLine.Number;

  ParseNodeProperties(CurrentLine.Text, TagName, AnchorName, RemainingText, LineNumber);
  Result := ParseNodeValue(RemainingText, AIndent, LineNumber, TagName,
    AnchorName, True, False, False, (RemainingText = '') and
    ((TagName <> '') or (AnchorName <> '')), False, True);
end;

function TGocciaYAMLParser.ParseNestedNode(const AParentIndent: Integer): TGocciaValue;
begin
  SkipIgnorableLines;
  if not HasCurrentLine or (CurrentLine.Indent <= AParentIndent) then
    Exit(TGocciaNullLiteralValue.NullValue);
  Result := ParseNode(CurrentLine.Indent);
end;

function TGocciaYAMLParser.ParseExplicitNestedNode(const AParentIndent: Integer): TGocciaValue;
begin
  SkipIgnorableLines;
  if not HasCurrentLine or (CurrentLine.Indent < AParentIndent) then
    Exit(TGocciaNullLiteralValue.NullValue);
  if (CurrentLine.Indent = AParentIndent) and
     not StartsSequenceEntry(CurrentLine.Text) and
     not StartsBlockMappingEntry(CurrentLine.Text) and
     ((CurrentLine.Text = '') or
      ((CurrentLine.Text[1] <> '[') and (CurrentLine.Text[1] <> '{'))) then
    Exit(TGocciaNullLiteralValue.NullValue);
  Result := ParseNode(CurrentLine.Indent);
end;

function TGocciaYAMLParser.IsBlockScalarHeader(const AText: string): Boolean;
begin
  Result := IsBlockScalarHeaderText(AText);
end;

function TGocciaYAMLParser.ParseBlockScalar(const AParentIndent: Integer;
  const AHeaderText: string; const ALineNumber: Integer): TGocciaValue;
var
  BuiltText: string;
  ChompMode, Style: Char;
  Content, HeaderText: string;
  ContentIndent, HeaderIndex, I, LineIndex: Integer;
  HasContentLines: Boolean;
  IndentIndicator: Integer;
  IsBlankLine: Boolean;
  Lines: TStringList;
  PendingBlankLines: Integer;
begin
  HeaderText := Trim(AHeaderText);
  Style := HeaderText[1];
  ChompMode := #0;
  IndentIndicator := 0;
  for HeaderIndex := 2 to Length(HeaderText) do
    if HeaderText[HeaderIndex] in ['+', '-'] then
      ChompMode := HeaderText[HeaderIndex]
    else if HeaderText[HeaderIndex] in ['1'..'9'] then
      IndentIndicator := Ord(HeaderText[HeaderIndex]) - Ord('0')
    else
      RaiseParseError('Invalid block scalar header.', ALineNumber);

  Inc(FIndex);
  Lines := TStringList.Create;
  try
    while HasCurrentLine do
    begin
      if not CurrentLine.IsIgnorable and (CurrentLine.Indent <= AParentIndent) then
        Break;
      Lines.AddObject(CurrentLine.RawText, TObject(PtrInt(CurrentLine.Number)));
      Inc(FIndex);
    end;

    if IndentIndicator > 0 then
      ContentIndent := AParentIndent + IndentIndicator
    else
    begin
      ContentIndent := MaxInt;
      for LineIndex := 0 to Lines.Count - 1 do
      begin
        Content := Lines[LineIndex];
        if Trim(Content) = '' then
          Continue;
        I := CountIndentation(Content);
        if I < ContentIndent then
          ContentIndent := I;
      end;
      if ContentIndent = MaxInt then
        ContentIndent := AParentIndent + 1;
    end;

    BuiltText := '';
    HasContentLines := Lines.Count > 0;
    PendingBlankLines := 0;
    for LineIndex := 0 to Lines.Count - 1 do
    begin
      Content := Lines[LineIndex];
      if Trim(Content) = '' then
        Content := ''
      else
      begin
        I := CountIndentation(Content);
        if I < ContentIndent then
          RaiseParseError('Block scalar content must be indented.',
            PtrInt(Lines.Objects[LineIndex]));
        Content := Copy(Content, ContentIndent + 1, MaxInt);
      end;

      IsBlankLine := Content = '';
      if Style = '|' then
      begin
        if LineIndex > 0 then
          BuiltText := BuiltText + #10
      end
      else
      begin
        if IsBlankLine then
          Inc(PendingBlankLines)
        else
        begin
          if BuiltText <> '' then
          begin
            if PendingBlankLines > 0 then
              BuiltText := BuiltText + StringOfChar(#10, PendingBlankLines)
            else
              BuiltText := BuiltText + ' ';
          end
          else if PendingBlankLines > 0 then
            BuiltText := BuiltText + StringOfChar(#10, PendingBlankLines);
          PendingBlankLines := 0;
        end;
      end;

      if Style = '|' then
        BuiltText := BuiltText + Content
      else if not IsBlankLine then
        BuiltText := BuiltText + Content;
    end;

    if (Style = '>') and (PendingBlankLines > 0) then
      BuiltText := BuiltText + StringOfChar(#10, PendingBlankLines);

    case ChompMode of
      '-':
        BuiltText := TrimTrailingLineFeeds(BuiltText);
      '+':
        if HasContentLines and not EndsWithLineFeed(BuiltText) then
          BuiltText := BuiltText + #10;
    else
      begin
        BuiltText := TrimTrailingLineFeeds(BuiltText);
        if HasContentLines then
          BuiltText := BuiltText + #10;
      end;
    end;
    Result := TGocciaStringLiteralValue.Create(BuiltText);
  finally
    Lines.Free;
  end
end;

function TGocciaYAMLParser.CanonicalizeKeyValue(const AValue: TGocciaValue;
  const AInComposite: Boolean): string;
var
  I: Integer;
  KeyNames: TArray<string>;
  Obj: TGocciaObjectValue;
  TaggedValue: TGocciaYAMLTaggedValue;
begin
  if AValue is TGocciaYAMLTaggedValue then
  begin
    TaggedValue := TGocciaYAMLTaggedValue(AValue);
    Exit(CanonicalizeKeyValue(TaggedValue.Value, AInComposite));
  end;

  if AValue is TGocciaArrayValue then
  begin
    Result := '[';
    for I := 0 to TGocciaArrayValue(AValue).Elements.Count - 1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      Result := Result + CanonicalizeKeyValue(
        TGocciaArrayValue(AValue).Elements[I], True);
    end;
    Exit(Result + ']');
  end;

  if AValue is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AValue);
    Result := '{';
    KeyNames := Obj.GetOwnPropertyKeys;
    for I := 0 to Length(KeyNames) - 1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      Result := Result + EscapeCompositeKeyString(KeyNames[I]) + ': ' +
        CanonicalizeKeyValue(Obj.GetProperty(KeyNames[I]), True);
    end;
    Exit(Result + '}');
  end;

  Result := AValue.ToStringLiteral.Value;
  if AInComposite and (AValue is TGocciaStringLiteralValue) then
    Result := EscapeCompositeKeyString(Result);
end;

function TGocciaYAMLParser.ParseKeyName(const AText: string;
  const ALineNumber: Integer): string;
var
  AnchorName, KeyText, RemainingText, TagName: string;
  KeyValue: TGocciaValue;
begin
  KeyText := Trim(AText);
  ParseNodeProperties(KeyText, TagName, AnchorName, RemainingText);
  if RemainingText = '' then
    KeyValue := TGocciaStringLiteralValue.Create('')
  else
    KeyValue := ParseInlineValue(RemainingText);

  KeyValue := ApplyTag(TagName, KeyValue);
  RegisterAnchor(AnchorName, KeyValue, ALineNumber);
  Result := CanonicalizeKeyValue(KeyValue);
end;

function TGocciaYAMLParser.ParseMappingValueText(const AValueText: string;
  const ALineIndent, ALineNumber: Integer): TGocciaValue;
var
  AnchorName, RemainingText, TagName, ValueText: string;
begin
  ValueText := AValueText;
  ParseNodeProperties(ValueText, TagName, AnchorName, RemainingText, ALineNumber);
  Result := ParseNodeValue(RemainingText, ALineIndent, ALineNumber, TagName,
    AnchorName, True, False, False, False, True, False);
end;

procedure TGocciaYAMLParser.ParseMappingEntry(const AContainer: TGocciaObjectValue;
  const ALineText: string; const ALineIndent, ALineNumber: Integer);
var
  KeyText, ValueText: string;
  SeparatorIndex: Integer;
  Value: TGocciaValue;
begin
  SeparatorIndex := FindTopLevelMappingSeparator(ALineText);
  if SeparatorIndex = 0 then
    RaiseParseError('Expected a mapping entry.', ALineNumber);

  KeyText := Trim(Copy(ALineText, 1, SeparatorIndex - 1));
  ValueText := Trim(Copy(ALineText, SeparatorIndex + 1, MaxInt));
  Value := ParseMappingValueText(ValueText, ALineIndent, ALineNumber);
  KeyText := ParseKeyName(KeyText, ALineNumber);
  if KeyText = '<<' then
    MergeMappingValue(AContainer, Value, ALineNumber)
  else
    AContainer.AssignProperty(KeyText, Value);
end;

procedure TGocciaYAMLParser.ParseExplicitMappingEntry(
  const AContainer: TGocciaObjectValue; const ALineText: string;
  const ALineIndent, ALineNumber: Integer);
var
  KeyAnchorName, KeyRemainingText, KeyTagName: string;
  KeyLineText, ValueLineText: string;
  KeyValue, Value: TGocciaValue;
begin
  KeyLineText := Trim(Copy(ALineText, 2, MaxInt));
  if KeyLineText = '' then
  begin
    Inc(FIndex);
    KeyValue := ParseExplicitNestedNode(ALineIndent);
  end
  else
  begin
    ParseNodeProperties(KeyLineText, KeyTagName, KeyAnchorName, KeyRemainingText,
      ALineNumber);
    if KeyRemainingText = '' then
    begin
      KeyValue := TGocciaStringLiteralValue.Create('');
      Inc(FIndex);
    end
    else
      KeyValue := ParseInlineValueWithContinuations(KeyRemainingText,
        ALineIndent, ALineNumber, False, True);
    KeyValue := ApplyTag(KeyTagName, KeyValue, ALineNumber);
    RegisterAnchor(KeyAnchorName, KeyValue, ALineNumber);
  end;

  SkipIgnorableLines;
  if not HasCurrentLine or (CurrentLine.Indent <> ALineIndent) or
     not StartsExplicitValueEntry(CurrentLine.Text) then
  begin
    Value := TGocciaNullLiteralValue.NullValue;
    KeyLineText := CanonicalizeKeyValue(KeyValue);
    if KeyLineText = '<<' then
      MergeMappingValue(AContainer, Value, ALineNumber)
    else
      AContainer.AssignProperty(KeyLineText, Value);
    Exit;
  end;

  ValueLineText := Trim(Copy(CurrentLine.Text, 2, MaxInt));
  if ValueLineText = '' then
  begin
    Inc(FIndex);
    Value := ParseExplicitNestedNode(ALineIndent);
  end
  else
    Value := ParseMappingValueText(ValueLineText, ALineIndent, CurrentLine.Number);
  KeyLineText := CanonicalizeKeyValue(KeyValue);
  if KeyLineText = '<<' then
    MergeMappingValue(AContainer, Value, CurrentLine.Number)
  else
    AContainer.AssignProperty(KeyLineText, Value);
end;

function TGocciaYAMLParser.ParseMapping(const AIndent: Integer): TGocciaValue;
var
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  ParseMappingInto(AIndent, Obj);
  Result := Obj;
end;

procedure TGocciaYAMLParser.ParseMappingInto(const AIndent: Integer;
  const AObject: TGocciaObjectValue);
begin
  while True do
  begin
    SkipIgnorableLines;
    if not (HasCurrentLine and (CurrentLine.Indent = AIndent) and
            StartsBlockMappingEntry(CurrentLine.Text)) then
      Break;
    if StartsExplicitKeyEntry(CurrentLine.Text) then
      ParseExplicitMappingEntry(AObject, CurrentLine.Text, CurrentLine.Indent,
        CurrentLine.Number)
    else
      ParseMappingEntry(AObject, CurrentLine.Text, CurrentLine.Indent,
        CurrentLine.Number);
  end;
end;

function TGocciaYAMLParser.ParseSequenceEntryMapping(
  const ASequenceIndent: Integer; const AEntryText: string;
  const ALineNumber: Integer): TGocciaValue;
var
  MappingIndent: Integer;
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  ParseSequenceEntryMappingInto(ASequenceIndent, AEntryText, ALineNumber, Obj);
  Result := Obj;
end;

procedure TGocciaYAMLParser.ParseSequenceEntryMappingInto(
  const ASequenceIndent: Integer; const AEntryText: string;
  const ALineNumber: Integer; const AObject: TGocciaObjectValue);
var
  MappingIndent: Integer;
begin
  if StartsExplicitKeyEntry(AEntryText) then
    ParseExplicitMappingEntry(AObject, AEntryText, ASequenceIndent, ALineNumber)
  else
    ParseMappingEntry(AObject, AEntryText, ASequenceIndent, ALineNumber);
  if HasCurrentLine and (CurrentLine.Indent > ASequenceIndent) then
  begin
    MappingIndent := CurrentLine.Indent;
    while True do
    begin
      SkipIgnorableLines;
      if not (HasCurrentLine and (CurrentLine.Indent = MappingIndent) and
              StartsBlockMappingEntry(CurrentLine.Text)) then
        Break;
      if StartsExplicitKeyEntry(CurrentLine.Text) then
        ParseExplicitMappingEntry(AObject, CurrentLine.Text, CurrentLine.Indent,
          CurrentLine.Number)
      else
        ParseMappingEntry(AObject, CurrentLine.Text, CurrentLine.Indent,
          CurrentLine.Number);
    end;
  end;
end;

function TGocciaYAMLParser.ParseNodeValue(const AValueText: string;
  const ALineIndent, ALineNumber: Integer; const ATagName, AAnchorName: string;
  const AAdvanceBeforeNested, ASequenceEntryInlineMapping,
  AStopAtIndentedSequenceEntry, AAllowSameIndentNestedNode,
  ARequireIndentedQuotedContinuation,
  AAllowSameIndentScalarContinuation: Boolean): TGocciaValue;
var
  FlowText: string;
  Obj: TGocciaObjectValue;
  Arr: TGocciaArrayValue;
begin
  if AValueText = '' then
  begin
    if AAdvanceBeforeNested then
      Inc(FIndex);
    SkipIgnorableLines;
    if not HasCurrentLine or (CurrentLine.Indent < ALineIndent) or
       ((CurrentLine.Indent = ALineIndent) and
        (((not AAllowSameIndentNestedNode) and
          not StartsSequenceEntry(CurrentLine.Text) and
          not StartsBlockMappingEntry(CurrentLine.Text) and
          ((CurrentLine.Text = '') or
           ((CurrentLine.Text[1] <> '[') and (CurrentLine.Text[1] <> '{')))) or
         (ASequenceEntryInlineMapping and StartsSequenceEntry(CurrentLine.Text)))) then
      Result := TGocciaNullLiteralValue.NullValue
    else if StartsSequenceEntry(CurrentLine.Text) then
    begin
      Arr := TGocciaArrayValue.Create;
      RegisterAnchor(AAnchorName, Arr, ALineNumber);
      ParseSequenceInto(CurrentLine.Indent, Arr);
      Result := Arr;
    end
    else if StartsBlockMappingEntry(CurrentLine.Text) then
    begin
      Obj := TGocciaObjectValue.Create;
      RegisterAnchor(AAnchorName, Obj, ALineNumber);
      ParseMappingInto(CurrentLine.Indent, Obj);
      Result := Obj;
    end
    else if (CurrentLine.Text <> '') and (CurrentLine.Text[1] = '[') then
    begin
      Arr := TGocciaArrayValue.Create;
      RegisterAnchor(AAnchorName, Arr, ALineNumber);
      FlowText := CollectFlowCollectionText(CurrentLine.Text, CurrentLine.Number);
      ParseFlowSequenceInto(FlowText, Arr);
      Result := Arr;
    end
    else if (CurrentLine.Text <> '') and (CurrentLine.Text[1] = '{') then
    begin
      Obj := TGocciaObjectValue.Create;
      RegisterAnchor(AAnchorName, Obj, ALineNumber);
      FlowText := CollectFlowCollectionText(CurrentLine.Text, CurrentLine.Number);
      ParseFlowMappingInto(FlowText, Obj);
      Result := Obj;
    end
    else
      Result := ParseNode(CurrentLine.Indent);
  end
  else if StartsSequenceEntry(AValueText) then
  begin
    Arr := TGocciaArrayValue.Create;
    RegisterAnchor(AAnchorName, Arr, ALineNumber);
    ParseInlineSequenceInto(AValueText, ALineIndent, ALineNumber, Arr);
    Result := Arr;
  end
  else if StartsBlockMappingEntry(AValueText) then
  begin
    Obj := TGocciaObjectValue.Create;
    RegisterAnchor(AAnchorName, Obj, ALineNumber);
    if ASequenceEntryInlineMapping then
      ParseSequenceEntryMappingInto(ALineIndent, AValueText, ALineNumber, Obj)
    else
      ParseMappingInto(ALineIndent, Obj);
    Result := Obj;
  end
  else if IsBlockScalarHeader(AValueText) then
    Result := ParseBlockScalar(ALineIndent, AValueText, ALineNumber)
  else if AValueText[1] = '[' then
  begin
    Arr := TGocciaArrayValue.Create;
    RegisterAnchor(AAnchorName, Arr, ALineNumber);
    FlowText := CollectFlowCollectionText(AValueText, ALineNumber);
    ParseFlowSequenceInto(FlowText, Arr);
    Result := Arr;
  end
  else if AValueText[1] = '{' then
  begin
    Obj := TGocciaObjectValue.Create;
    RegisterAnchor(AAnchorName, Obj, ALineNumber);
    FlowText := CollectFlowCollectionText(AValueText, ALineNumber);
    ParseFlowMappingInto(FlowText, Obj);
    Result := Obj;
  end
  else
    Result := ParseInlineValueWithContinuations(AValueText, ALineIndent,
      ALineNumber, AStopAtIndentedSequenceEntry,
      ARequireIndentedQuotedContinuation,
      AAllowSameIndentScalarContinuation);

  Result := ApplyTag(ATagName, Result, ALineNumber);
  RegisterAnchor(AAnchorName, Result, ALineNumber);
end;

function TGocciaYAMLParser.ParseSequence(const AIndent: Integer): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue.Create;
  ParseSequenceInto(AIndent, Arr);
  Result := Arr;
end;

procedure TGocciaYAMLParser.ParseSequenceInto(const AIndent: Integer;
  const AArray: TGocciaArrayValue);
var
  AnchorName, RemainingText, TagName: string;
  EntryText: string;
  LineNumber: Integer;
  Value: TGocciaValue;
begin
  while True do
  begin
    SkipIgnorableLines;
    if not (HasCurrentLine and (CurrentLine.Indent = AIndent) and
            StartsSequenceEntry(CurrentLine.Text)) then
      Break;
    LineNumber := CurrentLine.Number;
    EntryText := Trim(Copy(CurrentLine.Text, 2, MaxInt));
    ParseNodeProperties(EntryText, TagName, AnchorName, RemainingText, LineNumber);
    Value := ParseNodeValue(RemainingText, AIndent, LineNumber, TagName,
      AnchorName, True, True, True, False, True, False);
    AArray.Elements.Add(Value);
  end;
end;

function TGocciaYAMLParser.ParseFlowSequence(const AText: string): TGocciaValue;
var
  Arr: TGocciaArrayValue;
begin
  Arr := TGocciaArrayValue.Create;
  ParseFlowSequenceInto(AText, Arr);
  Result := Arr;
end;

function TGocciaYAMLParser.ParseFlowMapping(const AText: string): TGocciaValue;
var
  Obj: TGocciaObjectValue;
begin
  Obj := TGocciaObjectValue.Create;
  ParseFlowMappingInto(AText, Obj);
  Result := Obj;
end;

procedure TGocciaYAMLParser.ParseInlineSequenceInto(const AFirstEntryText: string;
  const ASequenceIndent, ALineNumber: Integer; const AArray: TGocciaArrayValue);
var
  AnchorName, EntryText, RemainingText, TagName: string;
  Value: TGocciaValue;
begin
  EntryText := Trim(Copy(AFirstEntryText, 2, MaxInt));
  ParseNodeProperties(EntryText, TagName, AnchorName, RemainingText, ALineNumber);
  Value := ParseNodeValue(RemainingText, ASequenceIndent, ALineNumber, TagName,
    AnchorName, True, True, True, False, True, False);
  AArray.Elements.Add(Value);

  SkipIgnorableLines;
  if HasCurrentLine and (CurrentLine.Indent >= ASequenceIndent) and
     StartsSequenceEntry(CurrentLine.Text) then
    ParseSequenceInto(CurrentLine.Indent, AArray);
end;

function TGocciaYAMLParser.CollectFlowCollectionText(const AInitialText: string;
  const ALineNumber: Integer): string;
var
  FragmentText: string;
begin
  Result := Trim(AInitialText);
  Inc(FIndex);

  while FindTopLevelFlowCollectionClose(Result) = 0 do
  begin
    if not HasCurrentLine then
      RaiseParseError('Unterminated flow collection.', ALineNumber);
    if CurrentLine.IsIgnorable then
    begin
      Inc(FIndex);
      Continue;
    end;

    FragmentText := Trim(CurrentLine.Text);
    if StartsWithDocumentMarkerToken(FragmentText) then
      RaiseParseError('Document markers are not allowed inside flow collections.',
        CurrentLine.Number);
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + FragmentText;
    Inc(FIndex);
  end;
end;

procedure TGocciaYAMLParser.ParseFlowSequenceInto(const AText: string;
  const AArray: TGocciaArrayValue);
var
  InnerText: string;
  ItemIndex: Integer;
  Items: TStringList;
  Obj: TGocciaObjectValue;
begin
  if (Length(AText) < 2) or (AText[1] <> '[') or
     (AText[Length(AText)] <> ']') then
    RaiseParseError('Invalid flow sequence.');

  InnerText := Trim(Copy(AText, 2, Length(AText) - 2));
  if InnerText = '' then
    Exit;

  Items := SplitTopLevelItems(InnerText);
  try
    for ItemIndex := 0 to Items.Count - 1 do
      if FindTopLevelFlowMappingSeparator(Items[ItemIndex]) > 0 then
      begin
        Obj := TGocciaObjectValue.Create;
        ParseFlowMappingEntry(Obj, Items[ItemIndex]);
        AArray.Elements.Add(Obj);
      end
      else
        AArray.Elements.Add(ParseInlineValue(Items[ItemIndex]));
  finally
    Items.Free;
  end;
end;

procedure TGocciaYAMLParser.ParseFlowMappingEntry(
  const AObject: TGocciaObjectValue; const AItemText: string;
  const ALineNumber: Integer);
var
  KeyText, ValueText: string;
  SeparatorIndex: Integer;
  Value: TGocciaValue;
begin
  SeparatorIndex := FindTopLevelFlowMappingSeparator(AItemText);
  if SeparatorIndex = 0 then
  begin
    KeyText := ParseKeyName(Trim(AItemText), ALineNumber);
    if KeyText = '<<' then
      RaiseParseError('Merge keys must include a value in flow mappings.',
        ALineNumber);
    AObject.AssignProperty(KeyText, TGocciaNullLiteralValue.NullValue);
    Exit;
  end;

  KeyText := Trim(Copy(AItemText, 1, SeparatorIndex - 1));
  ValueText := Trim(Copy(AItemText, SeparatorIndex + 1, MaxInt));
  KeyText := ParseKeyName(KeyText, ALineNumber);
  Value := ParseInlineValue(ValueText);
  if KeyText = '<<' then
    MergeMappingValue(AObject, Value, ALineNumber)
  else
    AObject.AssignProperty(KeyText, Value);
end;

procedure TGocciaYAMLParser.ParseFlowMappingInto(const AText: string;
  const AObject: TGocciaObjectValue);
var
  InnerText: string;
  ItemIndex: Integer;
  Items: TStringList;
begin
  if (Length(AText) < 2) or (AText[1] <> '{') or
     (AText[Length(AText)] <> '}') then
    RaiseParseError('Invalid flow mapping.');
  InnerText := Trim(Copy(AText, 2, Length(AText) - 2));
  if InnerText = '' then
    Exit;

  Items := SplitTopLevelItems(InnerText);
  try
    for ItemIndex := 0 to Items.Count - 1 do
      ParseFlowMappingEntry(AObject, Items[ItemIndex]);
  finally
    Items.Free;
  end;
end;

function TGocciaYAMLParser.ParseInlineValue(const AText: string): TGocciaValue;
var
  AliasName: string;
  AnchorName, TagName: string;
  Arr: TGocciaArrayValue;
  NumericValue: Double;
  Obj: TGocciaObjectValue;
  RemainingText: string;
  TrimmedText: string;
begin
  TrimmedText := Trim(AText);
  if TrimmedText = '' then
    Exit(TGocciaNullLiteralValue.NullValue);

  ParseNodeProperties(TrimmedText, TagName, AnchorName, RemainingText);
  if (AnchorName <> '') or (TagName <> '') then
  begin
    if RemainingText = '' then
      RaiseParseError('Inline node properties must be followed by a value.');
    TrimmedText := RemainingText;
  end;

  if TrimmedText[1] = '*' then
  begin
    AliasName := Trim(Copy(TrimmedText, 2, MaxInt));
    if AliasName = '' then
      RaiseParseError('Aliases must reference an anchor name.');
    Result := ResolveAliasValue(AliasName);
  end
  else if TrimmedText[1] = '[' then
  begin
    Arr := TGocciaArrayValue.Create;
    RegisterAnchor(AnchorName, Arr);
    ParseFlowSequenceInto(TrimmedText, Arr);
    Result := Arr;
  end
  else if TrimmedText[1] = '{' then
  begin
    Obj := TGocciaObjectValue.Create;
    RegisterAnchor(AnchorName, Obj);
    ParseFlowMappingInto(TrimmedText, Obj);
    Result := Obj;
  end
  else if (Length(TrimmedText) >= 2) and (TrimmedText[1] = DOUBLE_QUOTE) and
          (TrimmedText[Length(TrimmedText)] = DOUBLE_QUOTE) then
    Result := TGocciaStringLiteralValue.Create(
      UnescapeDoubleQuotedString(Copy(TrimmedText, 2, Length(TrimmedText) - 2)))
  else if (Length(TrimmedText) >= 2) and (TrimmedText[1] = SINGLE_QUOTE) and
          (TrimmedText[Length(TrimmedText)] = SINGLE_QUOTE) then
    Result := TGocciaStringLiteralValue.Create(
      UnescapeSingleQuotedString(Copy(TrimmedText, 2, Length(TrimmedText) - 2)))
  else if SameText(TrimmedText, 'null') or (TrimmedText = '~') then
    Result := TGocciaNullLiteralValue.NullValue
  else if SameText(TrimmedText, 'true') then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else if SameText(TrimmedText, 'false') then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else if TryParseYAMLInteger(TrimmedText, NumericValue) or
          TryParseYAMLFloat(TrimmedText, NumericValue) then
    Result := TGocciaNumberLiteralValue.Create(NumericValue)
  else
    Result := TGocciaStringLiteralValue.Create(TrimmedText);

  Result := ApplyTag(TagName, Result);
  RegisterAnchor(AnchorName, Result);
end;

function TGocciaYAMLParser.ParseInlineValueWithContinuations(const AText: string;
  const AParentIndent, ALineNumber: Integer;
  const AStopAtIndentedSequenceEntry,
  ARequireIndentedQuotedContinuations,
  AAllowSameIndentScalarContinuations: Boolean): TGocciaValue;
var
  ClosingQuoteIndex: Integer;
  FoldedText, FragmentText, InitialText: string;
  Fragments: TStringList;
  QuoteChar: Char;
  QuotedScalarClosed: Boolean;
begin
  InitialText := Trim(AText);
  Fragments := TStringList.Create;
  try
    Fragments.Add(InitialText);
    if IsQuotedScalarText(InitialText, QuoteChar) then
    begin
      Inc(FIndex);
      ClosingQuoteIndex := FindClosingQuotePosition(InitialText, QuoteChar);
      QuotedScalarClosed := (ClosingQuoteIndex > 0) and
        IsValidQuotedContinuationTail(InitialText, ClosingQuoteIndex);
      while not QuotedScalarClosed do
      begin
        if not HasCurrentLine then
          RaiseParseError('Unterminated quoted scalar.', ALineNumber);
        if CurrentLine.IsIgnorable then
        begin
          Fragments.Add('');
          Inc(FIndex);
          Continue;
        end;
        if ARequireIndentedQuotedContinuations and
           (CurrentLine.Indent <= AParentIndent) then
          RaiseParseError('Unterminated quoted scalar.', ALineNumber);
        FragmentText := CurrentLine.Text;
        ClosingQuoteIndex := FindContinuationQuotePosition(FragmentText, QuoteChar);
        if ClosingQuoteIndex > 0 then
        begin
          if not IsValidQuotedContinuationTail(FragmentText, ClosingQuoteIndex) then
            RaiseParseError('Unexpected trailing content after quoted scalar.',
              CurrentLine.Number);
          Fragments.Add(Copy(FragmentText, 1, ClosingQuoteIndex - 1));
          QuotedScalarClosed := True;
        end
        else
          Fragments.Add(FragmentText);
        Inc(FIndex);
      end;

      Fragments[0] := Copy(Fragments[0], 2, MaxInt);
      if Fragments.Count = 1 then
      begin
        ClosingQuoteIndex := FindClosingQuotePosition(Fragments[0], QuoteChar);
        Fragments[0] := Copy(Fragments[0], 1, ClosingQuoteIndex - 1);
      end;
      if QuoteChar = DOUBLE_QUOTE then
        FoldedText := FoldMultilineDoubleQuotedFragments(Fragments)
      else
        FoldedText := FoldMultilineScalarFragments(Fragments);
      if QuoteChar = DOUBLE_QUOTE then
        Result := TGocciaStringLiteralValue.Create(
          UnescapeDoubleQuotedString(FoldedText))
      else
        Result := TGocciaStringLiteralValue.Create(
          UnescapeSingleQuotedString(FoldedText));
      Exit;
    end;

    Inc(FIndex);
    while HasCurrentLine do
    begin
      if CurrentLine.IsIgnorable then
      begin
        Fragments.Add('');
        Inc(FIndex);
        Continue;
      end;
      if CurrentLine.Indent < AParentIndent then
        Break;
      if not AAllowSameIndentScalarContinuations and
         (CurrentLine.Indent = AParentIndent) then
        Break;
      if AStopAtIndentedSequenceEntry and StartsSequenceEntry(CurrentLine.Text) then
        Break;
      Fragments.Add(Trim(CurrentLine.Text));
      Inc(FIndex);
    end;

    while Fragments.Count > 1 do
    begin
      if Trim(Fragments[Fragments.Count - 1]) <> '' then
        Break;
      Fragments.Delete(Fragments.Count - 1);
    end;

    if Fragments.Count = 1 then
      Result := ParseInlineValue(InitialText)
    else
    begin
      FoldedText := FoldMultilineScalarFragments(Fragments);
      Result := TGocciaStringLiteralValue.Create(FoldedText);
    end;
  finally
    Fragments.Free;
  end;
end;

end.

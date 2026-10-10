unit Goccia.Values.Formatting;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

const
  DEFAULT_INSPECT_DEPTH = 5;
  MAX_INSPECT_DEPTH = 64;

procedure SetInspectDepth(const ADepth: Integer);
function FormatForDisplay(const AValue: TGocciaValue): string;
{ FormatForDisplay as console output renders it, following Node's
  util.inspect: a function reads `[Function: name]`, a class
  `[class K extends B]` and an Error its stack, each followed by its own
  enumerable properties. A container with a multi-line entry, such as an
  Error's stack, puts one entry on each line, indented by two spaces. }
function FormatForConsole(const AValue: TGocciaValue): string;

implementation

uses
  Generics.Collections,
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Constants.PropertyNames,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue,
  Goccia.Values.SymbolValue;

type
  { How one rendering goes. AConsole selects FormatForConsole's shapes for
    functions, classes and errors; FormatForDisplay leaves them as objects. }
  TFormatOptions = record
    Console: Boolean;
  end;

var
  GInspectDepth: Integer = DEFAULT_INSPECT_DEPTH;

procedure SetInspectDepth(const ADepth: Integer);
begin
  GInspectDepth := Min(MAX_INSPECT_DEPTH, Max(1, ADepth));
end;

function FormatRecursive(const AValue: TGocciaValue; const ANested: Boolean;
  const ADepth: Integer; const AOptions: TFormatOptions;
  out AMultiLine: Boolean): string; forward;

{ Joins a container's rendered entries between AOpen and AClose: on one line,
  or, when an entry spans lines, one entry per line indented by two spaces,
  as util.inspect does once an entry contains a line break. A non-empty APrefix
  (a Set's or Map's size, a function's or error's own rendering) comes first. }
function JoinEntries(const APrefix, AOpen, AClose: string;
  const AEntries: array of string; const AMultiLine: Boolean): string;
var
  SB: TStringBuffer;
  I: Integer;
begin
  SB := TStringBuffer.Create;
  if APrefix <> '' then
  begin
    SB.Append(APrefix);
    SB.AppendChar(' ');
  end;
  SB.Append(AOpen);
  if AMultiLine then
  begin
    for I := 0 to High(AEntries) do
    begin
      if I > 0 then
        SB.AppendChar(',');
      SB.Append(#10'  ');
      SB.Append(StringReplace(AEntries[I], #10, #10'  ', [rfReplaceAll]));
    end;
    SB.AppendChar(#10);
  end
  else
  begin
    for I := 0 to High(AEntries) do
    begin
      if I > 0 then
        SB.Append(', ')
      else
        SB.AppendChar(' ');
      SB.Append(AEntries[I]);
    end;
    SB.AppendChar(' ');
  end;
  SB.Append(AClose);
  Result := SB.ToString;
end;

function FormatArray(const AArr: TGocciaArrayValue; const ADepth: Integer;
  const AOptions: TFormatOptions; out AMultiLine: Boolean): string;
var
  Entries: array of string;
  I: Integer;
  EntryMultiLine: Boolean;
begin
  AMultiLine := False;
  if ADepth >= GInspectDepth then
  begin
    Result := '[Array]';
    Exit;
  end;
  if AArr.Elements.Count = 0 then
  begin
    Result := '[]';
    Exit;
  end;
  SetLength(Entries, AArr.Elements.Count);
  for I := 0 to AArr.Elements.Count - 1 do
  begin
    Entries[I] := FormatRecursive(AArr.Elements[I], True, ADepth + 1,
      AOptions, EntryMultiLine);
    AMultiLine := AMultiLine or EntryMultiLine;
  end;
  Result := JoinEntries('', '[', ']', Entries, AMultiLine);
end;

function FormatSet(const ASet: TGocciaSetValue; const ADepth: Integer;
  const AOptions: TFormatOptions; out AMultiLine: Boolean): string;
var
  Entries: array of string;
  Count, Cursor: Integer;
  Item: TGocciaValue;
  EntryMultiLine: Boolean;
begin
  AMultiLine := False;
  if ADepth >= GInspectDepth then
  begin
    Result := '[Set]';
    Exit;
  end;
  if ASet.Count = 0 then
  begin
    Result := 'Set(0) {}';
    Exit;
  end;
  SetLength(Entries, ASet.Count);
  Count := 0;
  Cursor := 0;
  ASet.RetainIterator;
  try
    while ASet.NextItem(Cursor, Item) do
    begin
      if Count = Length(Entries) then
        SetLength(Entries, Count * 2);
      Entries[Count] := FormatRecursive(Item, True, ADepth + 1, AOptions,
        EntryMultiLine);
      AMultiLine := AMultiLine or EntryMultiLine;
      Inc(Count);
    end;
  finally
    ASet.ReleaseIterator;
  end;
  SetLength(Entries, Count);
  Result := JoinEntries('Set(' + IntToStr(ASet.Count) + ')', '{', '}',
    Entries, AMultiLine);
end;

function FormatMap(const AMap: TGocciaMapValue; const ADepth: Integer;
  const AOptions: TFormatOptions; out AMultiLine: Boolean): string;
var
  Entries: array of string;
  Count, Cursor: Integer;
  Key, Value: TGocciaValue;
  KeyText: string;
  EntryMultiLine: Boolean;
begin
  AMultiLine := False;
  if ADepth >= GInspectDepth then
  begin
    Result := '[Map]';
    Exit;
  end;
  if AMap.Count = 0 then
  begin
    Result := 'Map(0) {}';
    Exit;
  end;
  SetLength(Entries, AMap.Count);
  Count := 0;
  Cursor := 0;
  AMap.RetainIterator;
  try
    while AMap.NextEntry(Cursor, Key, Value) do
    begin
      if Count = Length(Entries) then
        SetLength(Entries, Count * 2);
      KeyText := FormatRecursive(Key, True, ADepth + 1, AOptions,
        EntryMultiLine);
      AMultiLine := AMultiLine or EntryMultiLine;
      Entries[Count] := KeyText + ' => ' + FormatRecursive(Value, True,
        ADepth + 1, AOptions, EntryMultiLine);
      AMultiLine := AMultiLine or EntryMultiLine;
      Inc(Count);
    end;
  finally
    AMap.ReleaseIterator;
  end;
  SetLength(Entries, Count);
  Result := JoinEntries('Map(' + IntToStr(AMap.Count) + ')', '{', '}',
    Entries, AMultiLine);
end;

{ AObj's own enumerable properties as `key: value` entries, leaving out the
  names in ASkip. }
function FormatPropertyEntries(const AObj: TGocciaObjectValue;
  const ADepth: Integer; const AOptions: TFormatOptions;
  const ASkip: array of string; var AMultiLine: Boolean): TArray<string>;
var
  Entries: TArray<TPair<string, TGocciaValue>>;
  Entry: TPair<string, TGocciaValue>;
  Count, I: Integer;
  Skipped, EntryMultiLine: Boolean;
begin
  Entries := AObj.GetEnumerablePropertyEntries;
  SetLength(Result, Length(Entries));
  Count := 0;
  for Entry in Entries do
  begin
    Skipped := False;
    for I := 0 to High(ASkip) do
      if Entry.Key = ASkip[I] then
        Skipped := True;
    if Skipped then
      Continue;
    Result[Count] := Entry.Key + ': ' + FormatRecursive(Entry.Value, True,
      ADepth + 1, AOptions, EntryMultiLine);
    AMultiLine := AMultiLine or EntryMultiLine;
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

function FormatObject(const AObj: TGocciaObjectValue; const ADepth: Integer;
  const AOptions: TFormatOptions; out AMultiLine: Boolean): string;
var
  Entries: TArray<string>;
begin
  AMultiLine := False;
  if ADepth >= GInspectDepth then
  begin
    Result := '[Object]';
    Exit;
  end;
  Entries := FormatPropertyEntries(AObj, ADepth, AOptions, [], AMultiLine);
  if Length(Entries) = 0 then
    Result := '{}'
  else
    Result := JoinEntries('', '{', '}', Entries, AMultiLine);
end;

function StringPropertyOrEmpty(const AObj: TGocciaObjectValue;
  const AName: string): string;
var
  Value: TGocciaValue;
begin
  Value := AObj.GetProperty(AName);
  if Value is TGocciaStringLiteralValue then
    Result := TGocciaStringLiteralValue(Value).Value
  else
    Result := '';
end;

{ util.inspect's base for a function: `[class K extends B]` for a class whose
  source text is a class, else `[Function: name]`, `[AsyncFunction: name]`
  and so on, with `(anonymous)` for an empty name. }
function FunctionBaseText(const AObj: TGocciaObjectValue): string;
var
  Kind, Name, SuperName: string;
  Tag: TGocciaValue;
begin
  if (AObj is TGocciaClassValue) and
     (Copy(TGocciaClassValue(AObj).GetSourceText, 1, 5) = 'class') then
  begin
    Name := '';
    if AObj.HasOwnProperty(PROP_NAME) then
      Name := StringPropertyOrEmpty(AObj, PROP_NAME);
    if Name = '' then
      Name := '(anonymous)';
    Result := '[class ' + Name;
    if Assigned(AObj.Prototype) then
    begin
      SuperName := StringPropertyOrEmpty(AObj.Prototype, PROP_NAME);
      if SuperName <> '' then
        Result := Result + ' extends ' + SuperName;
    end;
    Result := Result + ']';
    Exit;
  end;

  Kind := 'Function';
  Tag := AObj.GetSymbolProperty(TGocciaSymbolValue.WellKnownToStringTag);
  if (Tag is TGocciaStringLiteralValue) and
     ((TGocciaStringLiteralValue(Tag).Value = 'AsyncFunction') or
      (TGocciaStringLiteralValue(Tag).Value = 'GeneratorFunction') or
      (TGocciaStringLiteralValue(Tag).Value = 'AsyncGeneratorFunction')) then
    Kind := TGocciaStringLiteralValue(Tag).Value;
  Name := StringPropertyOrEmpty(AObj, PROP_NAME);
  if Name = '' then
    Result := '[' + Kind + ' (anonymous)]'
  else
    Result := '[' + Kind + ': ' + Name + ']';
end;

function FormatFunction(const AObj: TGocciaObjectValue; const ADepth: Integer;
  const AOptions: TFormatOptions; out AMultiLine: Boolean): string;
var
  Entries: TArray<string>;
begin
  AMultiLine := False;
  Result := FunctionBaseText(AObj);
  if ADepth >= GInspectDepth then
    Exit;
  Entries := FormatPropertyEntries(AObj, ADepth, AOptions, [], AMultiLine);
  if Length(Entries) > 0 then
    Result := JoinEntries(Result, '{', '}', Entries, AMultiLine);
end;

{ util.inspect's rendering of an Error: its stack, or `name: message` as
  Error.prototype.toString builds it when it has none, in brackets when it
  lists no frames. Its own enumerable properties follow, without a name,
  message or stack the stack already shows, and with `[cause]` and an
  AggregateError's `[errors]` when they are not enumerable. }
function FormatError(const AObj: TGocciaObjectValue; const ADepth: Integer;
  const AOptions: TFormatOptions; out AMultiLine: Boolean): string;
var
  Stack, Name, Message: string;
  Skip: array of string;
  Entries: TArray<string>;
  Extra: TGocciaValue;
  EntryMultiLine: Boolean;

  procedure SkipShown(const AName: string);
  var
    Value: string;
  begin
    if not AObj.HasOwnProperty(AName) then
      Exit;
    Value := StringPropertyOrEmpty(AObj, AName);
    if (Value <> '') and (Pos(Value, Stack) > 0) then
    begin
      SetLength(Skip, Length(Skip) + 1);
      Skip[High(Skip)] := AName;
    end;
  end;

  // Whether AName is among the own enumerable properties already listed.
  function ListsEnumerable(const AName: string): Boolean;
  var
    Descriptor: TGocciaPropertyDescriptor;
  begin
    Descriptor := AObj.GetOwnPropertyDescriptor(AName);
    Result := Assigned(Descriptor) and Descriptor.Enumerable;
  end;

  procedure AddEntry(const AText: string);
  begin
    SetLength(Entries, Length(Entries) + 1);
    Entries[High(Entries)] := AText;
  end;

begin
  AMultiLine := False;
  Stack := StringPropertyOrEmpty(AObj, PROP_STACK);
  if Stack = '' then
  begin
    if AObj.GetProperty(PROP_NAME) is TGocciaUndefinedLiteralValue then
      Name := 'Error'
    else
      Name := StringPropertyOrEmpty(AObj, PROP_NAME);
    Message := StringPropertyOrEmpty(AObj, PROP_MESSAGE);
    if Name = '' then
      Stack := Message
    else if Message = '' then
      Stack := Name
    else
      Stack := Name + ': ' + Message;
  end;
  if Pos(#10'    at', Stack) = 0 then
    Stack := '[' + Stack + ']';
  Result := Stack;
  AMultiLine := Pos(#10, Stack) > 0;
  if ADepth >= GInspectDepth then
    Exit;

  Skip := nil;
  SkipShown(PROP_NAME);
  SkipShown(PROP_MESSAGE);
  SkipShown(PROP_STACK);
  Entries := FormatPropertyEntries(AObj, ADepth, AOptions, Skip, AMultiLine);
  if AObj.HasProperty(PROP_CAUSE) and not ListsEnumerable(PROP_CAUSE) then
  begin
    AddEntry('[' + PROP_CAUSE + ']: ' + FormatRecursive(
      AObj.GetProperty(PROP_CAUSE), True, ADepth + 1, AOptions,
      EntryMultiLine));
    AMultiLine := AMultiLine or EntryMultiLine;
  end;
  Extra := AObj.GetProperty(PROP_ERRORS);
  if (Extra is TGocciaArrayValue) and not ListsEnumerable(PROP_ERRORS) then
  begin
    AddEntry('[' + PROP_ERRORS + ']: ' + FormatRecursive(Extra, True,
      ADepth + 1, AOptions, EntryMultiLine));
    AMultiLine := AMultiLine or EntryMultiLine;
  end;
  if Length(Entries) > 0 then
    Result := JoinEntries(Result, '{', '}', Entries, AMultiLine);
end;

function QuoteString(const AStr: string): string;
begin
  if Pos('''', AStr) = 0 then
    Result := '''' + AStr + ''''
  else if Pos('"', AStr) = 0 then
    Result := '"' + AStr + '"'
  else
    Result := '`' + AStr + '`';
end;

function FormatRecursive(const AValue: TGocciaValue; const ANested: Boolean;
  const ADepth: Integer; const AOptions: TFormatOptions;
  out AMultiLine: Boolean): string;
begin
  AMultiLine := False;
  if not Assigned(AValue) then
  begin
    Result := 'undefined';
    Exit;
  end;

  if AValue is TGocciaSymbolValue then
    Result := TGocciaSymbolValue(AValue).ToDisplayString.Value
  else if AValue is TGocciaArrayValue then
    Result := FormatArray(TGocciaArrayValue(AValue), ADepth, AOptions,
      AMultiLine)
  else if AValue is TGocciaSetValue then
    Result := FormatSet(TGocciaSetValue(AValue), ADepth, AOptions, AMultiLine)
  else if AValue is TGocciaMapValue then
    Result := FormatMap(TGocciaMapValue(AValue), ADepth, AOptions, AMultiLine)
  else if AOptions.Console and ((AValue is TGocciaFunctionBase) or
          (AValue is TGocciaClassValue)) then
    Result := FormatFunction(TGocciaObjectValue(AValue), ADepth, AOptions,
      AMultiLine)
  else if AOptions.Console and (AValue is TGocciaObjectValue) and
          TGocciaObjectValue(AValue).HasErrorData then
    Result := FormatError(TGocciaObjectValue(AValue), ADepth, AOptions,
      AMultiLine)
  else if AValue is TGocciaObjectValue then
    Result := FormatObject(TGocciaObjectValue(AValue), ADepth, AOptions,
      AMultiLine)
  else if ANested and (AValue is TGocciaStringLiteralValue) then
    Result := QuoteString(AValue.ToStringLiteral.Value)
  else
    Result := AValue.ToStringLiteral.Value;
end;

function FormatForDisplay(const AValue: TGocciaValue): string;
var
  Options: TFormatOptions;
  MultiLine: Boolean;
begin
  Options.Console := False;
  Result := FormatRecursive(AValue, False, 0, Options, MultiLine);
end;

function FormatForConsole(const AValue: TGocciaValue): string;
var
  Options: TFormatOptions;
  MultiLine: Boolean;
begin
  Options.Console := True;
  Result := FormatRecursive(AValue, False, 0, Options, MultiLine);
end;

end.

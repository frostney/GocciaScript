unit Goccia.Builtins.CSV;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.CSV,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaCSVBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaCSVParser;

    procedure ReadOptions(const AArgs: TGocciaArgumentsCollection;
      const AOptionsIndex: Integer; out ADelimiter: Char;
      out AHeaders: Boolean; out ASkipEmptyLines: Boolean);
  published
    function CSVParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CSVParseChunk(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function CSVStringify(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback;
      const ADefineGlobalBinding: Boolean = True);
    destructor Destroy; override;
  end;

implementation

uses
  Goccia.Builtins.DelimitedText,
  Goccia.GarbageCollector,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FStaticMembers, 0);
end;

constructor TGocciaCSVBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const ADefineGlobalBinding: Boolean = True);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaCSVParser.Create;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('parse', CSVParse, 1, gmkStaticMethod);
    Members.AddNamedMethod('parseChunk', CSVParseChunk, 1, gmkStaticMethod);
    Members.AddNamedMethod('stringify', CSVStringify, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('CSV'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  if ADefineGlobalBinding then
    AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet, True);
end;

destructor TGocciaCSVBuiltin.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure TGocciaCSVBuiltin.ReadOptions(
  const AArgs: TGocciaArgumentsCollection; const AOptionsIndex: Integer;
  out ADelimiter: Char; out AHeaders: Boolean;
  out ASkipEmptyLines: Boolean);
var
  Obj: TGocciaObjectValue;
  Prop: TGocciaValue;
begin
  ADelimiter := ',';
  AHeaders := True;
  ASkipEmptyLines := False;

  if AArgs.Length <= AOptionsIndex then
    Exit;

  if not (AArgs.GetElement(AOptionsIndex) is TGocciaObjectValue) then
    Exit;

  Obj := TGocciaObjectValue(AArgs.GetElement(AOptionsIndex));

  Prop := Obj.GetProperty('delimiter');
  if Assigned(Prop) and (Prop is TGocciaStringLiteralValue) then
  begin
    if Length(Prop.ToStringLiteral.Value) > 0 then
      ADelimiter := Prop.ToStringLiteral.Value[1];
  end;

  Prop := Obj.GetProperty('headers');
  if Assigned(Prop) and (Prop is TGocciaBooleanLiteralValue) then
    AHeaders := TGocciaBooleanLiteralValue(Prop).Value;

  Prop := Obj.GetProperty('skipEmptyLines');
  if Assigned(Prop) and (Prop is TGocciaBooleanLiteralValue) then
    ASkipEmptyLines := TGocciaBooleanLiteralValue(Prop).Value;
end;

function TGocciaCSVBuiltin.CSVParse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Context: TGocciaObjectValue;
  Delimiter: Char;
  FieldValue: string;
  FieldInfoRows: TArray<TArray<TGocciaCSVFieldInfo>>;
  Headers: Boolean;
  HeaderRow: TArray<TGocciaCSVFieldInfo>;
  I, J: Integer;
  Key: string;
  Obj: TGocciaObjectValue;
  ParsedResult: TGocciaArrayValue;
  Reviver: TGocciaValue;
  ReviverResult: TGocciaValue;
  Row: TGocciaArrayValue;
  SkipEmptyLines: Boolean;
  Text: string;
  ParsedResultRoot: TGocciaTempRoot;
  RowRoot: TGocciaTempRoot;
  ContextRoot: TGocciaTempRoot;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'CSV.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('CSV.parse: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  ReadOptions(AArgs, 1, Delimiter, Headers, SkipEmptyLines);
  Reviver := GetDelimitedTextCallback(AArgs, 2);

  { The reviver is arbitrary JS and every field string is charged against the
    memory ceiling, so both are GC safe points; the result array, the
    in-flight row, and the context object are reachable only from this frame
    until stored, and need temp roots. }
  InitializeTempRoot(ParsedResultRoot);
  InitializeTempRoot(RowRoot);
  InitializeTempRoot(ContextRoot);
  try
  try
    if Assigned(Reviver) then
    begin
      FieldInfoRows := FParser.ParseWithFieldInfo(Text, Delimiter, False,
        SkipEmptyLines);

      ParsedResult := TGocciaArrayValue.Create;
      AddTempRootIfNeeded(ParsedResultRoot, ParsedResult);
      if Length(FieldInfoRows) = 0 then
      begin
        Result := ParsedResult;
        Exit;
      end;

      if Headers then
      begin
        HeaderRow := FieldInfoRows[0];
        for I := 1 to Length(FieldInfoRows) - 1 do
        begin
          Obj := TGocciaObjectValue.Create;
          AddTempRootIfNeeded(RowRoot, Obj);
          for J := 0 to Length(HeaderRow) - 1 do
          begin
            Key := HeaderRow[J].Value;
            Context := TGocciaObjectValue.Create;
            AddTempRootIfNeeded(ContextRoot, Context);
            Context.AssignProperty('quoted',
              TGocciaBooleanLiteralValue.Create(
                (J < Length(FieldInfoRows[I])) and
                FieldInfoRows[I][J].Quoted));
            Context.AssignProperty('row',
              TGocciaNumberLiteralValue.Create(I - 1));
            Context.AssignProperty('column',
              TGocciaNumberLiteralValue.Create(J));

            if J < Length(FieldInfoRows[I]) then
              FieldValue := FieldInfoRows[I][J].Value
            else
              FieldValue := '';
            ReviverResult := InvokeDelimitedTextReviver(Reviver,
              TGocciaStringLiteralValue.Create(Key), FieldValue, Context);
            Obj.AssignProperty(Key, ReviverResult);
          end;
          ParsedResult.Elements.Add(Obj);
        end;
      end
      else
      begin
        for I := 0 to Length(FieldInfoRows) - 1 do
        begin
          Row := TGocciaArrayValue.Create;
          AddTempRootIfNeeded(RowRoot, Row);
          for J := 0 to Length(FieldInfoRows[I]) - 1 do
          begin
            Context := TGocciaObjectValue.Create;
            AddTempRootIfNeeded(ContextRoot, Context);
            Context.AssignProperty('quoted',
              TGocciaBooleanLiteralValue.Create(FieldInfoRows[I][J].Quoted));
            Context.AssignProperty('row',
              TGocciaNumberLiteralValue.Create(I));
            Context.AssignProperty('column',
              TGocciaNumberLiteralValue.Create(J));

            ReviverResult := InvokeDelimitedTextReviver(Reviver,
              TGocciaNumberLiteralValue.Create(J),
              FieldInfoRows[I][J].Value, Context);
            Row.Elements.Add(ReviverResult);
          end;
          ParsedResult.Elements.Add(Row);
        end;
      end;

      Result := ParsedResult;
    end
    else
      Result := FParser.Parse(Text, Delimiter, Headers, SkipEmptyLines);
  except
    on E: EGocciaCSVParseError do
      ThrowSyntaxError(E.Message);
  end;
  finally
    RemoveTempRootIfNeeded(ContextRoot);
    RemoveTempRootIfNeeded(RowRoot);
    RemoveTempRootIfNeeded(ParsedResultRoot);
  end;
end;

function TGocciaCSVBuiltin.CSVParseChunk(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ChunkResult: TGocciaCSVChunkParseResult;
  Delimiter: Char;
  EndOffset: Integer;
  Headers: Boolean;
  SkipEmptyLines: Boolean;
  StartOffset: Integer;
  Text: string;
  TextLength: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'CSV.parseChunk',
    ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('CSV.parseChunk: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  TextLength := Length(Text);
  ReadOptions(AArgs, 1, Delimiter, Headers, SkipEmptyLines);

  StartOffset := 0;
  EndOffset := TextLength;

  if AArgs.Length >= 3 then
    StartOffset := ToIntegerFromArgs(AArgs, 2);
  if AArgs.Length >= 4 then
    EndOffset := ToIntegerFromArgs(AArgs, 3);

  try
    ChunkResult := FParser.ParseChunk(Text, Delimiter, Headers,
      SkipEmptyLines, StartOffset, EndOffset);
    Result := BuildDelimitedTextChunkResult(ChunkResult.Values,
      ChunkResult.Read, ChunkResult.Done, ChunkResult.ErrorMessage);
  except
    on E: EGocciaCSVParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaCSVBuiltin.CSVStringify(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Data: TGocciaValue;
  Delimiter: Char;
  Headers: Boolean;
  Replacer: TGocciaValue;
  ReplacedArr: TGocciaArrayValue;
  SkipEmptyLines: Boolean;
  ReplacedArrRoot: TGocciaTempRoot;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'CSV.stringify',
    ThrowError);

  Data := AArgs.GetElement(0);
  ReadOptions(AArgs, 1, Delimiter, Headers, SkipEmptyLines);
  Replacer := GetDelimitedTextCallback(AArgs, 2);

  if Assigned(Replacer) and (Data is TGocciaArrayValue) then
  begin
    InitializeTempRoot(ReplacedArrRoot);
    try
      ReplacedArr := ApplyDelimitedTextReplacer(TGocciaArrayValue(Data),
        Replacer);
      AddTempRootIfNeeded(ReplacedArrRoot, ReplacedArr);
      Result := TGocciaStringLiteralValue.Create(
        TGocciaCSVStringifier.Stringify(ReplacedArr, Delimiter, Headers));
    finally
      RemoveTempRootIfNeeded(ReplacedArrRoot);
    end;
  end
  else
    Result := TGocciaStringLiteralValue.Create(
      TGocciaCSVStringifier.Stringify(Data, Delimiter, Headers));
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.

unit Goccia.Builtins.TSV;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.TSV,
  Goccia.Values.Primitives;

type
  TGocciaTSVBuiltin = class(TGocciaBuiltin)
  private
    FParser: TGocciaTSVParser;

    procedure ReadOptions(const AArgs: TGocciaArgumentsCollection;
      const AOptionsIndex: Integer; out AHeaders: Boolean;
      out ASkipEmptyLines: Boolean);
  published
    function TSVParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TSVParseChunk(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TSVStringify(const AArgs: TGocciaArgumentsCollection;
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

constructor TGocciaTSVBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback;
  const ADefineGlobalBinding: Boolean = True);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaTSVParser.Create;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('parse', TSVParse, 1, gmkStaticMethod);
    Members.AddNamedMethod('parseChunk', TSVParseChunk, 1, gmkStaticMethod);
    Members.AddNamedMethod('stringify', TSVStringify, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('TSV'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  if ADefineGlobalBinding then
    AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet, True);
end;

destructor TGocciaTSVBuiltin.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure TGocciaTSVBuiltin.ReadOptions(
  const AArgs: TGocciaArgumentsCollection; const AOptionsIndex: Integer;
  out AHeaders: Boolean; out ASkipEmptyLines: Boolean);
var
  Obj: TGocciaObjectValue;
  Prop: TGocciaValue;
begin
  AHeaders := True;
  ASkipEmptyLines := False;

  if AArgs.Length <= AOptionsIndex then
    Exit;

  if not (AArgs.GetElement(AOptionsIndex) is TGocciaObjectValue) then
    Exit;

  Obj := TGocciaObjectValue(AArgs.GetElement(AOptionsIndex));

  Prop := Obj.GetProperty('headers');
  if Assigned(Prop) and (Prop is TGocciaBooleanLiteralValue) then
    AHeaders := TGocciaBooleanLiteralValue(Prop).Value;

  Prop := Obj.GetProperty('skipEmptyLines');
  if Assigned(Prop) and (Prop is TGocciaBooleanLiteralValue) then
    ASkipEmptyLines := TGocciaBooleanLiteralValue(Prop).Value;
end;

function TGocciaTSVBuiltin.TSVParse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Context: TGocciaObjectValue;
  FieldValue: string;
  FieldInfoRows: TArray<TArray<TGocciaTSVFieldInfo>>;
  Headers: Boolean;
  HeaderRow: TArray<TGocciaTSVFieldInfo>;
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
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'TSV.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('TSV.parse: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  ReadOptions(AArgs, 1, Headers, SkipEmptyLines);
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
      FieldInfoRows := FParser.ParseWithFieldInfo(Text, False,
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
      Result := FParser.Parse(Text, Headers, SkipEmptyLines);
  except
    on E: EGocciaTSVParseError do
      ThrowSyntaxError(E.Message);
  end;
  finally
    RemoveTempRootIfNeeded(ContextRoot);
    RemoveTempRootIfNeeded(RowRoot);
    RemoveTempRootIfNeeded(ParsedResultRoot);
  end;
end;

function TGocciaTSVBuiltin.TSVParseChunk(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  BaseIndex: Integer;
  ChunkResult: TGocciaTSVChunkParseResult;
  EndOffset: Integer;
  Headers: Boolean;
  SkipEmptyLines: Boolean;
  StartOffset: Integer;
  Text: string;
  TextLength: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'TSV.parseChunk',
    ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('TSV.parseChunk: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  TextLength := Length(Text);

  if (AArgs.Length > 1) and
    (AArgs.GetElement(1) is TGocciaObjectValue) then
  begin
    ReadOptions(AArgs, 1, Headers, SkipEmptyLines);
    BaseIndex := 2;
  end
  else
  begin
    Headers := True;
    SkipEmptyLines := False;
    BaseIndex := 1;
  end;

  StartOffset := 0;
  EndOffset := TextLength;

  if AArgs.Length > BaseIndex then
    StartOffset := ToIntegerFromArgs(AArgs, BaseIndex);
  if AArgs.Length > BaseIndex + 1 then
    EndOffset := ToIntegerFromArgs(AArgs, BaseIndex + 1);

  try
    ChunkResult := FParser.ParseChunk(Text, Headers, SkipEmptyLines,
      StartOffset, EndOffset);
    Result := BuildDelimitedTextChunkResult(ChunkResult.Values,
      ChunkResult.Read, ChunkResult.Done, ChunkResult.ErrorMessage);
  except
    on E: EGocciaTSVParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaTSVBuiltin.TSVStringify(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Data: TGocciaValue;
  Headers: Boolean;
  Replacer: TGocciaValue;
  ReplacedArr: TGocciaArrayValue;
  SkipEmptyLines: Boolean;
  ReplacedArrRoot: TGocciaTempRoot;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'TSV.stringify',
    ThrowError);

  Data := AArgs.GetElement(0);
  ReadOptions(AArgs, 1, Headers, SkipEmptyLines);
  Replacer := GetDelimitedTextCallback(AArgs, 2);

  if Assigned(Replacer) and (Data is TGocciaArrayValue) then
  begin
    InitializeTempRoot(ReplacedArrRoot);
    try
      ReplacedArr := ApplyDelimitedTextReplacer(TGocciaArrayValue(Data),
        Replacer);
      AddTempRootIfNeeded(ReplacedArrRoot, ReplacedArr);
      Result := TGocciaStringLiteralValue.Create(
        TGocciaTSVStringifier.Stringify(ReplacedArr, Headers));
    finally
      RemoveTempRootIfNeeded(ReplacedArrRoot);
    end;
  end
  else
    Result := TGocciaStringLiteralValue.Create(
      TGocciaTSVStringifier.Stringify(Data, Headers));
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.

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
    function GetReviver(const AArgs: TGocciaArgumentsCollection;
      const AReviverIndex: Integer): TGocciaValue;
    function GetReplacer(const AArgs: TGocciaArgumentsCollection;
      const AReplacerIndex: Integer): TGocciaValue;
    function BuildChunkResultObject(
      const AChunkResult: TGocciaTSVChunkParseResult): TGocciaValue;
  published
    function TSVParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TSVParseChunk(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TSVStringify(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  Goccia.Constants.ErrorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

constructor TGocciaTSVBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
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
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
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

function TGocciaTSVBuiltin.GetReviver(
  const AArgs: TGocciaArgumentsCollection;
  const AReviverIndex: Integer): TGocciaValue;
begin
  Result := nil;
  if AArgs.Length > AReviverIndex then
  begin
    Result := AArgs.GetElement(AReviverIndex);
    if not Result.IsCallable then
      Result := nil;
  end;
end;

function TGocciaTSVBuiltin.GetReplacer(
  const AArgs: TGocciaArgumentsCollection;
  const AReplacerIndex: Integer): TGocciaValue;
begin
  Result := nil;
  if AArgs.Length > AReplacerIndex then
  begin
    Result := AArgs.GetElement(AReplacerIndex);
    if not Result.IsCallable then
      Result := nil;
  end;
end;

function TGocciaTSVBuiltin.BuildChunkResultObject(
  const AChunkResult: TGocciaTSVChunkParseResult): TGocciaValue;
var
  ErrorValue: TGocciaValue;
  ResultObject: TGocciaObjectValue;
begin
  ResultObject := TGocciaObjectValue.Create;
  if AChunkResult.ErrorMessage = '' then
    ErrorValue := TGocciaNullLiteralValue.NullValue
  else
    ErrorValue := CreateErrorObject(SYNTAX_ERROR_NAME,
      AChunkResult.ErrorMessage, 1);

  ResultObject.AssignProperty(PROP_VALUES, AChunkResult.Values);
  ResultObject.AssignProperty(PROP_READ,
    TGocciaNumberLiteralValue.Create(AChunkResult.Read));
  ResultObject.AssignProperty(PROP_DONE,
    TGocciaBooleanLiteralValue.Create(AChunkResult.Done));
  ResultObject.AssignProperty(PROP_ERROR, ErrorValue);
  Result := ResultObject;
end;

function TGocciaTSVBuiltin.TSVParse(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Headers: Boolean;
  SkipEmptyLines: Boolean;
  Text: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'TSV.parse', ThrowError);

  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError('TSV.parse: first argument must be a string');

  Text := AArgs.GetElement(0).ToStringLiteral.Value;
  ReadOptions(AArgs, 1, Headers, SkipEmptyLines);

  try
    Result := FParser.Parse(Text, Headers, SkipEmptyLines);
  except
    on E: EGocciaTSVParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaTSVBuiltin.TSVParseChunk(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
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
  ReadOptions(AArgs, 1, Headers, SkipEmptyLines);

  StartOffset := 0;
  EndOffset := TextLength;

  if AArgs.Length >= 3 then
    StartOffset := Trunc(AArgs.GetElement(2).ToNumberLiteral.Value);
  if AArgs.Length >= 4 then
    EndOffset := Trunc(AArgs.GetElement(3).ToNumberLiteral.Value);

  try
    ChunkResult := FParser.ParseChunk(Text, Headers, SkipEmptyLines,
      StartOffset, EndOffset);
    Result := BuildChunkResultObject(ChunkResult);
  except
    on E: EGocciaTSVParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaTSVBuiltin.TSVStringify(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Args: TGocciaArgumentsCollection;
  Arr: TGocciaArrayValue;
  Data: TGocciaValue;
  Headers: Boolean;
  I, J: Integer;
  Item: TGocciaValue;
  Key: string;
  Keys: TArray<string>;
  Obj: TGocciaObjectValue;
  Replacer: TGocciaValue;
  ReplacerResult: TGocciaValue;
  ReplacedArr: TGocciaArrayValue;
  ReplacedObj: TGocciaObjectValue;
  ReplacedRow: TGocciaArrayValue;
  Row: TGocciaArrayValue;
  SkipEmptyLines: Boolean;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'TSV.stringify',
    ThrowError);

  Data := AArgs.GetElement(0);
  ReadOptions(AArgs, 1, Headers, SkipEmptyLines);
  Replacer := GetReplacer(AArgs, 2);

  if Assigned(Replacer) and (Data is TGocciaArrayValue) then
  begin
    Arr := TGocciaArrayValue(Data);
    ReplacedArr := TGocciaArrayValue.Create;

    if (Arr.Elements.Count > 0) and
       (Arr.Elements[0] is TGocciaObjectValue) and
       not (Arr.Elements[0] is TGocciaArrayValue) then
    begin
      Keys := TGocciaObjectValue(Arr.Elements[0]).GetOwnPropertyKeys;
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if not (Arr.Elements[I] is TGocciaObjectValue) then
          Continue;
        Obj := TGocciaObjectValue(Arr.Elements[I]);
        ReplacedObj := TGocciaObjectValue.Create;
        for J := 0 to Length(Keys) - 1 do
        begin
          Key := Keys[J];
          Item := Obj.GetProperty(Key);
          if not Assigned(Item) then
            Item := TGocciaUndefinedLiteralValue.UndefinedValue;

          Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
          Args.Add(TGocciaStringLiteralValue.Create(Key));
          Args.Add(Item);
          ReplacerResult := InvokeCallable(Replacer, Args,
            TGocciaUndefinedLiteralValue.UndefinedValue);
          ReplacedObj.AssignProperty(Key, ReplacerResult);
        end;
        ReplacedArr.Elements.Add(ReplacedObj);
      end;
    end
    else
    begin
      for I := 0 to Arr.Elements.Count - 1 do
      begin
        if Arr.Elements[I] is TGocciaArrayValue then
        begin
          Row := TGocciaArrayValue(Arr.Elements[I]);
          ReplacedRow := TGocciaArrayValue.Create;
          for J := 0 to Row.Elements.Count - 1 do
          begin
            Args := TGocciaArgumentsCollection.CreateWithCapacity(2);
            Args.Add(TGocciaNumberLiteralValue.Create(J));
            Args.Add(Row.Elements[J]);
            ReplacerResult := InvokeCallable(Replacer, Args,
              TGocciaUndefinedLiteralValue.UndefinedValue);
            ReplacedRow.Elements.Add(ReplacerResult);
          end;
          ReplacedArr.Elements.Add(ReplacedRow);
        end;
      end;
    end;

    Result := TGocciaStringLiteralValue.Create(
      TGocciaTSVStringifier.Stringify(ReplacedArr, Headers));
  end
  else
    Result := TGocciaStringLiteralValue.Create(
      TGocciaTSVStringifier.Stringify(Data, Headers));
end;

end.

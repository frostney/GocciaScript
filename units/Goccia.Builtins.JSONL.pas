unit Goccia.Builtins.JSONL;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.JSONL,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue;

type
  TGocciaJSONLBuiltin = class(TGocciaBuiltin)
  private
    class var FStaticMembers: array of TGocciaMemberDefinition;
    FParser: TGocciaJSONLParser;

    class function ClampOffset(const AValue, ALimit: Integer): Integer; static;
    class function ReadUint8ArrayBytes(
      const AInput: TGocciaTypedArrayValue): TBytes; static;
    function BuildChunkResultObject(
      const AChunkResult: TGocciaJSONLChunkParseResult): TGocciaValue;
  protected
  published
    function JSONLParse(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function JSONLParseChunk(const AArgs: TGocciaArgumentsCollection;
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
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

constructor TGocciaJSONLBuiltin.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FParser := TGocciaJSONLParser.Create;
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddNamedMethod('parse', JSONLParse, 1, gmkStaticMethod);
    Members.AddNamedMethod('parseChunk', JSONLParseChunk, 1, gmkStaticMethod);
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create('JSONL'),
      [pfConfigurable]);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;

  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

destructor TGocciaJSONLBuiltin.Destroy;
begin
  FParser.Free;
  inherited;
end;

class function TGocciaJSONLBuiltin.ClampOffset(const AValue,
  ALimit: Integer): Integer;
begin
  if AValue < 0 then
    Exit(0);
  if AValue > ALimit then
    Exit(ALimit);
  Result := AValue;
end;

class function TGocciaJSONLBuiltin.ReadUint8ArrayBytes(
  const AInput: TGocciaTypedArrayValue): TBytes;
begin
  if AInput.Kind <> takUint8 then
    raise EGocciaJSONLInvalidInputError.Create(
      'JSONL input must be a string or Uint8Array');

  SetLength(Result, AInput.Length);
  if AInput.Length > 0 then
    Move(AInput.BufferData[AInput.ByteOffset], Result[0], AInput.Length);
end;

function TGocciaJSONLBuiltin.BuildChunkResultObject(
  const AChunkResult: TGocciaJSONLChunkParseResult): TGocciaValue;
var
  ErrorValue: TGocciaValue;
  ResultObject: TGocciaObjectValue;
begin
  ResultObject := TGocciaObjectValue.Create;
  if AChunkResult.ErrorMessage = '' then
    ErrorValue := TGocciaNullLiteralValue.NullValue
  else
    ErrorValue := CreateErrorObject(SYNTAX_ERROR_NAME, AChunkResult.ErrorMessage,
      1);

  ResultObject.AssignProperty(PROP_VALUES, AChunkResult.Values);
  ResultObject.AssignProperty(PROP_READ,
    TGocciaNumberLiteralValue.Create(AChunkResult.Read));
  ResultObject.AssignProperty(PROP_DONE,
    TGocciaBooleanLiteralValue.Create(AChunkResult.Done));
  ResultObject.AssignProperty(PROP_ERROR, ErrorValue);
  Result := ResultObject;
end;

function TGocciaJSONLBuiltin.JSONLParse(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Bytes: TBytes;
  Input: TGocciaValue;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSONL.parse', ThrowError);
  Input := AArgs.GetElement(0);

  try
    if Input is TGocciaStringLiteralValue then
      Result := FParser.Parse(Input.ToStringLiteral.Value)
    else if Input is TGocciaTypedArrayValue then
    begin
      Bytes := ReadUint8ArrayBytes(TGocciaTypedArrayValue(Input));
      Result := FParser.Parse(Bytes);
    end
    else
      ThrowTypeError('JSONL.parse: argument must be a string or Uint8Array');
  except
    on E: EGocciaJSONLInvalidInputError do
      ThrowTypeError(E.Message);
    on E: EGocciaJSONLParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

function TGocciaJSONLBuiltin.JSONLParseChunk(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  ByteLength: Integer;
  Bytes: TBytes;
  ChunkResult: TGocciaJSONLChunkParseResult;
  EndOffset: Integer;
  Input: TGocciaValue;
  StartOffset: Integer;
  TextLength: Integer;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'JSONL.parseChunk',
    ThrowError);
  Input := AArgs.GetElement(0);

  try
    if Input is TGocciaStringLiteralValue then
    begin
      TextLength := Length(Input.ToStringLiteral.Value);
      if AArgs.Length >= 2 then
        StartOffset := ClampOffset(
          Trunc(AArgs.GetElement(1).ToNumberLiteral.Value), TextLength)
      else
        StartOffset := 0;

      if AArgs.Length >= 3 then
        EndOffset := ClampOffset(
          Trunc(AArgs.GetElement(2).ToNumberLiteral.Value), TextLength)
      else
        EndOffset := TextLength;

      ChunkResult := FParser.ParseChunk(Input.ToStringLiteral.Value, StartOffset,
        EndOffset);
      Result := BuildChunkResultObject(ChunkResult);
      Exit;
    end;

    if Input is TGocciaTypedArrayValue then
    begin
      Bytes := ReadUint8ArrayBytes(TGocciaTypedArrayValue(Input));
      ByteLength := Length(Bytes);
      if AArgs.Length >= 2 then
        StartOffset := ClampOffset(
          Trunc(AArgs.GetElement(1).ToNumberLiteral.Value), ByteLength)
      else
        StartOffset := 0;

      if AArgs.Length >= 3 then
        EndOffset := ClampOffset(
          Trunc(AArgs.GetElement(2).ToNumberLiteral.Value), ByteLength)
      else
        EndOffset := ByteLength;

      ChunkResult := FParser.ParseChunk(Bytes, StartOffset, EndOffset);
      Result := BuildChunkResultObject(ChunkResult);
      Exit;
    end;

    ThrowTypeError('JSONL.parseChunk: argument must be a string or Uint8Array');
  except
    on E: EGocciaJSONLInvalidInputError do
      ThrowTypeError(E.Message);
    on E: EGocciaJSONLParseError do
      ThrowSyntaxError(E.Message);
  end;
end;

end.

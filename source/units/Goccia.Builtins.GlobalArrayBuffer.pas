unit Goccia.Builtins.GlobalArrayBuffer;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives;

type
  TGocciaGlobalArrayBuffer = class(TGocciaBuiltin)
  private
    FArrayBufferConstructor: TGocciaNativeFunctionValue;
  published
    function ArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferIsView(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Math,

  Goccia.Constants.NumericLimits,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper,
  Goccia.Values.TypedArrayValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

const
  NO_MAX_BYTE_LENGTH = -1;

constructor TGocciaGlobalArrayBuffer.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  FArrayBufferConstructor := TGocciaNativeFunctionValue.Create(ArrayBufferConstructorFn, 'ArrayBuffer', 1);
  TGocciaArrayBufferValue.ExposePrototype(FArrayBufferConstructor);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(ArrayBufferIsView, 1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
end;

// ES2026 §6.2.4.2 ToIndex(value) — local implementation for constructor path
function ConstructorToIndex(const AValue: TGocciaValue): Integer;
var
  Num: TGocciaNumberLiteralValue;
  IntegerIndex: Double;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
  begin
    Result := 0;
    Exit;
  end;

  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    IntegerIndex := 0
  else if Num.IsInfinite then
  begin
    ThrowRangeError(SErrorInvalidArrayBufferLength, SSuggestArrayLengthRange);
    Exit(0);
  end
  else
    IntegerIndex := Trunc(Num.Value);

  if (IntegerIndex < 0) or (IntegerIndex > MAX_SAFE_INTEGER_F) or
     (IntegerIndex > High(Integer)) then
    ThrowRangeError(SErrorInvalidArrayBufferLength, SSuggestArrayLengthRange);

  Result := Trunc(IntegerIndex);
end;

// ES2026 §25.1.4.1 ArrayBuffer(length [, options])
function TGocciaGlobalArrayBuffer.ArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Len: Integer;
  OptionsArg, MaxByteLengthValue: TGocciaValue;
  RequestedMaxByteLength: Integer;
begin
  if AArgs.Length = 0 then
  begin
    Result := TGocciaArrayBufferValue.Create(0);
    Exit;
  end;

  Len := ConstructorToIndex(AArgs.GetElement(0));

  // ES2026 §25.1.4.1 step 3: GetArrayBufferMaxByteLengthOption(options)
  // ES2026 §25.1.3.7 step 2: If options is not an Object, return empty
  RequestedMaxByteLength := NO_MAX_BYTE_LENGTH;
  if AArgs.Length > 1 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if Assigned(OptionsArg) and not (OptionsArg is TGocciaUndefinedLiteralValue) and
       not OptionsArg.IsPrimitive then
    begin
      MaxByteLengthValue := OptionsArg.GetProperty(PROP_MAX_BYTE_LENGTH);
      if Assigned(MaxByteLengthValue) and not (MaxByteLengthValue is TGocciaUndefinedLiteralValue) then
        RequestedMaxByteLength := ConstructorToIndex(MaxByteLengthValue);
    end;
  end;

  if RequestedMaxByteLength >= 0 then
    Result := TGocciaArrayBufferValue.Create(Len, RequestedMaxByteLength)
  else
    Result := TGocciaArrayBufferValue.Create(Len);
end;

// ES2026 §25.1.5.1 ArrayBuffer.isView(arg)
function TGocciaGlobalArrayBuffer.ArrayBufferIsView(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if (AArgs.Length > 0) and (AArgs.GetElement(0) is TGocciaTypedArrayValue) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

end.

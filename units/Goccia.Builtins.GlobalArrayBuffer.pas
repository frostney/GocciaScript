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
    class var FStaticMembers: array of TGocciaMemberDefinition;
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

  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.TypedArrayValue;

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

// ES2026 §25.1.4.1 ArrayBuffer(length [, options])
function TGocciaGlobalArrayBuffer.ArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Num: TGocciaNumberLiteralValue;
  Len: Integer;
  OptionsArg, MaxByteLengthValue: TGocciaValue;
  RequestedMaxByteLength: Integer;
  MaxNum: TGocciaNumberLiteralValue;
begin
  if AArgs.Length = 0 then
  begin
    Result := TGocciaArrayBufferValue.Create(0);
    Exit;
  end;

  Num := AArgs.GetElement(0).ToNumberLiteral;

  if Num.IsNaN or Num.IsInfinite or (Num.Value < 0) or (Num.Value <> Trunc(Num.Value)) then
    ThrowRangeError('Invalid array buffer length');

  Len := Trunc(Num.Value);

  // ES2026 §25.1.4.1 step 3: GetArrayBufferMaxByteLengthOption(options)
  RequestedMaxByteLength := NO_MAX_BYTE_LENGTH;
  if AArgs.Length > 1 then
  begin
    OptionsArg := AArgs.GetElement(1);
    if Assigned(OptionsArg) and not (OptionsArg is TGocciaUndefinedLiteralValue) then
    begin
      if OptionsArg.IsPrimitive then
        ThrowTypeError('Options argument must be an object');

      MaxByteLengthValue := OptionsArg.GetProperty(PROP_MAX_BYTE_LENGTH);
      if Assigned(MaxByteLengthValue) and not (MaxByteLengthValue is TGocciaUndefinedLiteralValue) then
      begin
        MaxNum := MaxByteLengthValue.ToNumberLiteral;
        if MaxNum.IsNaN or MaxNum.IsInfinite or (MaxNum.Value < 0) or (MaxNum.Value <> Trunc(MaxNum.Value)) then
          ThrowRangeError('Invalid maxByteLength');
        RequestedMaxByteLength := Trunc(MaxNum.Value);
      end;
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

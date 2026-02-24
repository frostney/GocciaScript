unit Goccia.Builtins.GlobalArrayBuffer;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SharedArrayBufferValue;

type
  TGocciaGlobalArrayBuffer = class(TGocciaBuiltin)
  private
    FArrayBufferConstructor: TGocciaNativeFunctionValue;
    FSharedArrayBufferBuiltin: TGocciaObjectValue;

    function ArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ArrayBufferIsView(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    function SharedArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    property SharedArrayBufferBuiltin: TGocciaObjectValue read FSharedArrayBufferBuiltin;
  end;

implementation

uses
  Math,

  Goccia.Values.ErrorHelper,
  Goccia.Values.TypedArrayValue;

constructor TGocciaGlobalArrayBuffer.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FArrayBufferConstructor := TGocciaNativeFunctionValue.Create(ArrayBufferConstructorFn, 'ArrayBuffer', 1);
  TGocciaArrayBufferValue.ExposePrototype(FArrayBufferConstructor);

  FBuiltinObject.RegisterNativeMethod(
    TGocciaNativeFunctionValue.CreateWithoutPrototype(ArrayBufferIsView, 'isView', 1));

  FSharedArrayBufferBuiltin := TGocciaObjectValue.Create;
end;

// ES2026 §25.1.4.1 ArrayBuffer(length)
function TGocciaGlobalArrayBuffer.ArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Num: TGocciaNumberLiteralValue;
  Len: Integer;
begin
  if AArgs.Length = 0 then
    ThrowRangeError('Invalid array buffer length');

  Num := AArgs.GetElement(0).ToNumberLiteral;

  if Num.IsNaN or Num.IsInfinite or (Num.Value < 0) or (Num.Value <> Trunc(Num.Value)) then
    ThrowRangeError('Invalid array buffer length');

  Len := Trunc(Num.Value);
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

// ES2026 §25.2.3.1 SharedArrayBuffer(length)
function TGocciaGlobalArrayBuffer.SharedArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Num: TGocciaNumberLiteralValue;
  Len: Integer;
begin
  if AArgs.Length = 0 then
    ThrowRangeError('Invalid shared array buffer length');

  Num := AArgs.GetElement(0).ToNumberLiteral;

  if Num.IsNaN or Num.IsInfinite or (Num.Value < 0) or (Num.Value <> Trunc(Num.Value)) then
    ThrowRangeError('Invalid shared array buffer length');

  Len := Trunc(Num.Value);
  Result := TGocciaSharedArrayBufferValue.Create(Len);
end;

end.

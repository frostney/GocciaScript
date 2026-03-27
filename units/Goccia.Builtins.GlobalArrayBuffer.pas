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

  Goccia.Values.ErrorHelper,
  Goccia.Values.TypedArrayValue;

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

// ES2026 §25.1.4.1 ArrayBuffer(length) — ToIndex(undefined) returns 0
function TGocciaGlobalArrayBuffer.ArrayBufferConstructorFn(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Num: TGocciaNumberLiteralValue;
  Len: Integer;
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

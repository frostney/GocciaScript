unit Goccia.Builtins.GlobalArray;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ArrayValue, Goccia.Values.ObjectValue, Goccia.Values.NativeFunction, Goccia.Values.Primitives,
  Goccia.Builtins.Base, Goccia.Arguments.Collection, Generics.Collections, Goccia.Scope,
  Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.ObjectPropertyDescriptor, Goccia.Values.ClassHelper;

type
  TGocciaGlobalArray = class(TGocciaBuiltin)
  protected
    function IsArray(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

constructor TGocciaGlobalArray.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(IsArray, 'isArray', 1));

  AScope.DefineBuiltin(AName, FBuiltinObject);
end;

function TGocciaGlobalArray.IsArray(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  // Array.isArray accepts 0 or more arguments; returns false if no argument provided
  if Args.Length < 1 then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.Create(Args.GetElement(0) is TGocciaArrayValue);
end;

end.

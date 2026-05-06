unit Goccia.Builtins.GlobalTextEncoder;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Constants.ConstructorNames,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives,
  Goccia.Values.TextEncoderValue;

type
  TGocciaGlobalTextEncoder = class(TGocciaBuiltin)
  private
    FTextEncoderConstructor: TGocciaNativeFunctionValue;

    function TextEncoderConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TextEncoderConstruct(const AArgs: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue;

constructor TGocciaGlobalTextEncoder.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
  FTextEncoderConstructor := TGocciaNativeFunctionValue.Create(
    TextEncoderConstructorFn, CONSTRUCTOR_TEXT_ENCODER, 0);
  FTextEncoderConstructor.ConstructCallback := TextEncoderConstruct;
  TGocciaTextEncoderValue.ExposePrototype(FTextEncoderConstructor);
end;

function TGocciaGlobalTextEncoder.TextEncoderConstructorFn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaTextEncoderValue.Create;
end;

function TGocciaGlobalTextEncoder.TextEncoderConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Encoder: TGocciaTextEncoderValue;
begin
  Encoder := TGocciaTextEncoderValue.Create;
  Encoder.Prototype := GetProtoFromConstructorWithIntrinsic(ANewTarget,
    TGocciaObjectValue(FTextEncoderConstructor.GetProperty(PROP_PROTOTYPE)));
  Result := Encoder;
end;

end.

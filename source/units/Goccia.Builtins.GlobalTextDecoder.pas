unit Goccia.Builtins.GlobalTextDecoder;

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
  Goccia.Values.TextDecoderValue;

type
  TGocciaGlobalTextDecoder = class(TGocciaBuiltin)
  private
    FTextDecoderConstructor: TGocciaNativeFunctionValue;

    function TextDecoderConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TextDecoderConstruct(const AArgs: TGocciaArgumentsCollection;
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

constructor TGocciaGlobalTextDecoder.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
  FTextDecoderConstructor := TGocciaNativeFunctionValue.Create(
    TextDecoderConstructorFn, CONSTRUCTOR_TEXT_DECODER, 0);
  FTextDecoderConstructor.ConstructCallback := TextDecoderConstruct;
  TGocciaTextDecoderValue.ExposePrototype(FTextDecoderConstructor);
end;

function TGocciaGlobalTextDecoder.TextDecoderConstructorFn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Decoder: TGocciaTextDecoderValue;
begin
  Decoder := TGocciaTextDecoderValue.Create;
  Decoder.InitializeNativeFromArguments(AArgs);
  Result := Decoder;
end;

function TGocciaGlobalTextDecoder.TextDecoderConstruct(
  const AArgs: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Decoder: TGocciaTextDecoderValue;
begin
  Decoder := TGocciaTextDecoderValue.Create;
  Decoder.InitializeNativeFromArguments(AArgs);
  Decoder.Prototype := GetProtoFromConstructorWithIntrinsic(ANewTarget,
    TGocciaObjectValue(FTextDecoderConstructor.GetProperty(PROP_PROTOTYPE)));
  Result := Decoder;
end;

end.

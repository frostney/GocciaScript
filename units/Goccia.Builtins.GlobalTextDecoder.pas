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
  Goccia.Values.Primitives,
  Goccia.Values.TextDecoderValue;

type
  TGocciaGlobalTextDecoder = class(TGocciaBuiltin)
  private
    FTextDecoderConstructor: TGocciaNativeFunctionValue;

    function TextDecoderConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

// WHATWG Encoding §8.2 new TextDecoder([label [, options]])
constructor TGocciaGlobalTextDecoder.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
  FTextDecoderConstructor := TGocciaNativeFunctionValue.Create(
    TextDecoderConstructorFn, CONSTRUCTOR_TEXT_DECODER, 0);
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

end.

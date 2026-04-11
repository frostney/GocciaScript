unit Goccia.Builtins.GlobalTextEncoder;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.NativeFunction,
  Goccia.Values.Primitives,
  Goccia.Values.TextEncoderValue;

type
  TGocciaGlobalTextEncoder = class(TGocciaBuiltin)
  private
    FTextEncoderConstructor: TGocciaNativeFunctionValue;

    function TextEncoderConstructorFn(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope;
      const AThrowError: TGocciaThrowErrorCallback);
  end;

implementation

// WHATWG Encoding §8.3 new TextEncoder()
constructor TGocciaGlobalTextEncoder.Create(const AName: string;
  const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);
  FTextEncoderConstructor := TGocciaNativeFunctionValue.Create(
    TextEncoderConstructorFn, 'TextEncoder', 0);
  TGocciaTextEncoderValue.ExposePrototype(FTextEncoderConstructor);
end;

function TGocciaGlobalTextEncoder.TextEncoderConstructorFn(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  // TextEncoder takes no arguments.
  Result := TGocciaTextEncoderValue.Create;
end;

end.

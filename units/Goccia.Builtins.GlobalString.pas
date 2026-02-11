unit Goccia.Builtins.GlobalString;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.Error, Goccia.Values.ObjectValue, 
  Goccia.Values.Primitives, Goccia.Arguments.Collection, Goccia.Arguments.Validator, Goccia.Values.ClassValue, SysUtils, Generics.Collections;

type
  TGocciaGlobalString = class(TGocciaBuiltin)
  private
    FStringConstructor: TGocciaStringClassValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
    destructor Destroy; override;
  end;

implementation

uses
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ClassHelper;

constructor TGocciaGlobalString.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  // Create String constructor as a class value
  FStringConstructor := TGocciaStringClassValue.Create('String', nil);
  AScope.DefineBuiltin(AName, FStringConstructor);
end;

destructor TGocciaGlobalString.Destroy;
begin
  FStringConstructor.Free;
  inherited;
end;

end.

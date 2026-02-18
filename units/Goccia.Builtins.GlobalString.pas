unit Goccia.Builtins.GlobalString;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Arguments.Validator,
  Goccia.Builtins.Base,
  Goccia.Error,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

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
  Goccia.GarbageCollector,
  Goccia.Values.ClassHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

constructor TGocciaGlobalString.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  // Create String constructor as a class value
  FStringConstructor := TGocciaStringClassValue.Create('String', nil);
  AScope.DefineLexicalBinding(AName, FStringConstructor, dtLet);
end;

destructor TGocciaGlobalString.Destroy;
begin
  if not Assigned(TGocciaGarbageCollector.Instance) then
    FStringConstructor.Free;
  inherited;
end;

end.

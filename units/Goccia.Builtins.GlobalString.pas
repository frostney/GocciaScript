unit Goccia.Builtins.GlobalString;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ClassValue,
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

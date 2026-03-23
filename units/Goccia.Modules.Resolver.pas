unit Goccia.Modules.Resolver;

{$I Goccia.inc}

interface

uses
  SysUtils,

  Modules.Resolver,
  OrderedStringMap;

type
  TGocciaModuleResolver = class(TModuleResolver)
  public
    constructor Create(const ABaseDirectory: string = '');
  end;

  EGocciaModuleNotFound = EModuleNotFound;

implementation

uses
  Goccia.FileExtensions;

constructor TGocciaModuleResolver.Create(const ABaseDirectory: string);
begin
  inherited Create(ABaseDirectory);
  SetExtensions(ScriptExtensions);
end;

end.

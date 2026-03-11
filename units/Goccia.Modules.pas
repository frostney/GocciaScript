unit Goccia.Modules;

{$I Goccia.inc}

interface

uses
  SysUtils,

  OrderedStringMap,

  Goccia.Values.Primitives;

type
  TGocciaModule = class
  private
    FPath: string;
    FExportsTable: TGocciaValueMap;
    FLastModified: TDateTime;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;
    property Path: string read FPath;
    property ExportsTable: TGocciaValueMap read FExportsTable;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

  TLoadModuleCallback = function(const AModulePath, AImportingFilePath: string): TGocciaModule of object;

implementation

constructor TGocciaModule.Create(const APath: string);
begin
  FPath := APath;
  FExportsTable := TGocciaValueMap.Create;
end;

destructor TGocciaModule.Destroy;
begin
  FExportsTable.Free;
  inherited;
end;


end.

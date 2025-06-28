unit Goccia.Modules;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Generics.Collections, SysUtils;

type
  TGocciaModule = class
  private
    FPath: string;
    FExportsTable: TDictionary<string, TGocciaValue>;
    FLastModified: TDateTime;
  public
    constructor Create(const APath: string);
    destructor Destroy; override;
    property Path: string read FPath;
    property ExportsTable: TDictionary<string, TGocciaValue> read FExportsTable;
    property LastModified: TDateTime read FLastModified write FLastModified;
  end;

  TLoadModuleCallback = function(const APath: string): TGocciaModule of object;

implementation

constructor TGocciaModule.Create(const APath: string);
begin
  FPath := APath;
  FExportsTable := TDictionary<string, TGocciaValue>.Create;
end;

destructor TGocciaModule.Destroy;
begin
  FExportsTable.Free;
  inherited;
end;


end.

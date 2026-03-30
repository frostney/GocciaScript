unit Goccia.Bytecode.Module;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk;

type
  TGocciaModuleBinding = record
    ExportName: string;
    LocalSlot: UInt16;
  end;

  TGocciaModuleImport = record
    ModulePath: string;
    Bindings: array of TGocciaModuleBinding;
  end;

  TGocciaModuleExport = record
    Name: string;
    LocalSlot: UInt16;
  end;

  TGocciaBytecodeModule = class
  private
    FFormatVersion: UInt16;
    FRuntimeTag: string;
    FSourcePath: string;
    FTopLevel: TGocciaFunctionTemplate;
    FImports: array of TGocciaModuleImport;
    FImportCount: Integer;
    FExports: array of TGocciaModuleExport;
    FExportCount: Integer;
    FHasDebugInfo: Boolean;
  public
    constructor Create(const ARuntimeTag, ASourcePath: string);
    destructor Destroy; override;

    procedure AddImport(const AModulePath: string;
      const ABindings: array of TGocciaModuleBinding);
    procedure AddExport(const AName: string; const ALocalSlot: UInt16);

    function GetImport(const AIndex: Integer): TGocciaModuleImport;
    function GetExport(const AIndex: Integer): TGocciaModuleExport;

    property FormatVersion: UInt16 read FFormatVersion;
    property RuntimeTag: string read FRuntimeTag;
    property SourcePath: string read FSourcePath;
    property TopLevel: TGocciaFunctionTemplate read FTopLevel write FTopLevel;
    property ImportCount: Integer read FImportCount;
    property ExportCount: Integer read FExportCount;
    property HasDebugInfo: Boolean read FHasDebugInfo write FHasDebugInfo;
  end;

implementation

uses
  Goccia.Bytecode;

constructor TGocciaBytecodeModule.Create(const ARuntimeTag, ASourcePath: string);
begin
  inherited Create;
  FFormatVersion := GOCCIA_FORMAT_VERSION;
  FRuntimeTag := ARuntimeTag;
  FSourcePath := ASourcePath;
  FTopLevel := nil;
  FImportCount := 0;
  FExportCount := 0;
  FHasDebugInfo := True;
end;

destructor TGocciaBytecodeModule.Destroy;
begin
  FTopLevel.Free;
  inherited;
end;

procedure TGocciaBytecodeModule.AddImport(const AModulePath: string;
  const ABindings: array of TGocciaModuleBinding);
var
  I: Integer;
begin
  if FImportCount >= Length(FImports) then
    SetLength(FImports, FImportCount * 2 + 4);
  FImports[FImportCount].ModulePath := AModulePath;
  SetLength(FImports[FImportCount].Bindings, Length(ABindings));
  for I := 0 to High(ABindings) do
    FImports[FImportCount].Bindings[I] := ABindings[I];
  Inc(FImportCount);
end;

procedure TGocciaBytecodeModule.AddExport(const AName: string;
  const ALocalSlot: UInt16);
begin
  if FExportCount >= Length(FExports) then
    SetLength(FExports, FExportCount * 2 + 4);
  FExports[FExportCount].Name := AName;
  FExports[FExportCount].LocalSlot := ALocalSlot;
  Inc(FExportCount);
end;

function TGocciaBytecodeModule.GetImport(
  const AIndex: Integer): TGocciaModuleImport;
begin
  Result := FImports[AIndex];
end;

function TGocciaBytecodeModule.GetExport(
  const AIndex: Integer): TGocciaModuleExport;
begin
  Result := FExports[AIndex];
end;

end.

unit Goccia.Bytecode.Module;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk,
  Goccia.Executor;

type
  TGocciaModuleBinding = record
    ExportName: string;
    LocalSlot: UInt16;
  end;

  { One static module request of the compiled program, in source order: an
    evaluation-phase import or a re-export. ModulePath is the request as
    EncodeImportSpecifierAttribute writes it; Bindings name what the program
    imports or re-exports from it. Line and Column locate the declaration
    and are not serialized, so a module read from a .gbc file reports 0. }
  TGocciaModuleImport = record
    ModulePath: string;
    Bindings: array of TGocciaModuleBinding;
    Line: Integer;
    Column: Integer;
  end;

  TGocciaModuleExport = record
    Name: string;
    LocalSlot: UInt16;
  end;

  TGocciaBytecodeModule = class(TGocciaCompiledModule)
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
      const ABindings: array of TGocciaModuleBinding;
      const ALine: Integer = 0; const AColumn: Integer = 0);
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
  const ABindings: array of TGocciaModuleBinding; const ALine,
  AColumn: Integer);
var
  I: Integer;
begin
  if FImportCount >= Length(FImports) then
    SetLength(FImports, FImportCount * 2 + 4);
  FImports[FImportCount].ModulePath := AModulePath;
  SetLength(FImports[FImportCount].Bindings, Length(ABindings));
  for I := 0 to High(ABindings) do
    FImports[FImportCount].Bindings[I] := ABindings[I];
  FImports[FImportCount].Line := ALine;
  FImports[FImportCount].Column := AColumn;
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

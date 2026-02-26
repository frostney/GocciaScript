unit Souffle.Bytecode.Module;

{$I Souffle.inc}

interface

uses
  Souffle.Bytecode.Chunk;

type
  TSouffleModuleBinding = record
    ExportName: string;
    LocalSlot: UInt16;
  end;

  TSouffleModuleImport = record
    ModulePath: string;
    Bindings: array of TSouffleModuleBinding;
  end;

  TSouffleModuleExport = record
    Name: string;
    LocalSlot: UInt16;
  end;

  TSouffleBytecodeModule = class
  private
    FFormatVersion: UInt16;
    FRuntimeTag: string;
    FSourcePath: string;
    FTopLevel: TSouffleFunctionPrototype;
    FImports: array of TSouffleModuleImport;
    FImportCount: Integer;
    FExports: array of TSouffleModuleExport;
    FExportCount: Integer;
    FHasDebugInfo: Boolean;
  public
    constructor Create(const ARuntimeTag, ASourcePath: string);
    destructor Destroy; override;

    procedure AddImport(const AModulePath: string;
      const ABindings: array of TSouffleModuleBinding);
    procedure AddExport(const AName: string; const ALocalSlot: UInt16);

    function GetImport(const AIndex: Integer): TSouffleModuleImport;
    function GetExport(const AIndex: Integer): TSouffleModuleExport;

    property FormatVersion: UInt16 read FFormatVersion;
    property RuntimeTag: string read FRuntimeTag;
    property SourcePath: string read FSourcePath;
    property TopLevel: TSouffleFunctionPrototype read FTopLevel write FTopLevel;
    property ImportCount: Integer read FImportCount;
    property ExportCount: Integer read FExportCount;
    property HasDebugInfo: Boolean read FHasDebugInfo write FHasDebugInfo;
  end;

implementation

uses
  Souffle.Bytecode;

{ TSouffleBytecodeModule }

constructor TSouffleBytecodeModule.Create(const ARuntimeTag, ASourcePath: string);
begin
  inherited Create;
  FFormatVersion := SOUFFLE_FORMAT_VERSION;
  FRuntimeTag := ARuntimeTag;
  FSourcePath := ASourcePath;
  FTopLevel := nil;
  FImportCount := 0;
  FExportCount := 0;
  FHasDebugInfo := True;
end;

destructor TSouffleBytecodeModule.Destroy;
begin
  FTopLevel.Free;
  inherited;
end;

procedure TSouffleBytecodeModule.AddImport(const AModulePath: string;
  const ABindings: array of TSouffleModuleBinding);
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

procedure TSouffleBytecodeModule.AddExport(const AName: string;
  const ALocalSlot: UInt16);
begin
  if FExportCount >= Length(FExports) then
    SetLength(FExports, FExportCount * 2 + 4);
  FExports[FExportCount].Name := AName;
  FExports[FExportCount].LocalSlot := ALocalSlot;
  Inc(FExportCount);
end;

function TSouffleBytecodeModule.GetImport(
  const AIndex: Integer): TSouffleModuleImport;
begin
  Result := FImports[AIndex];
end;

function TSouffleBytecodeModule.GetExport(
  const AIndex: Integer): TSouffleModuleExport;
begin
  Result := FExports[AIndex];
end;

end.

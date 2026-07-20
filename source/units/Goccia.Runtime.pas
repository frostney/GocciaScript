unit Goccia.Runtime;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.Engine,
  Goccia.Executor,
  Goccia.Executor.Interpreter,
  Goccia.ModuleResolver,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaRuntimeCore = class;
  TGocciaRuntimeExtension = class;
  TGocciaRuntimeExtensionClass = class of TGocciaRuntimeExtension;

  TGocciaRuntimeExtension = class
  private
    FRuntime: TGocciaRuntimeCore;
  protected
    property Runtime: TGocciaRuntimeCore read FRuntime;
  public
    procedure Attach(const ARuntime: TGocciaRuntimeCore); virtual;
    procedure Detach; virtual;
    procedure AddModuleExtensions(const AExtensions: TStrings); virtual;
    function TryLoadModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean; virtual;
    function TryInjectGlobals(const AFormat: string;
      const AContent: string): Boolean; virtual;
    function TryInjectModules(const AFormat: string;
      const AContent: string; const ABaseAddress: string): Boolean; virtual;
    procedure ApplyHostRestrictions(const AAllowedHosts: TStrings); virtual;
    procedure WaitForIdle; virtual;
    procedure DiscardPending; virtual;
  end;

  TGocciaRuntimeCore = class(TGocciaEngineExtension)
  private
    FEngine: TGocciaEngine;
    FExtensions: TObjectList<TGocciaRuntimeExtension>;
    FPrevRuntimeModuleLoader: TGocciaRuntimeModuleLoader;
    FBaseResolverExtensions: TModuleResolverExtensionArray;

    procedure ConfigureFileLoading;
    procedure CaptureResolverExtensions;
    procedure AddResolverExtension(var AExtensions: TModuleResolverExtensionArray;
      var ACount: Integer; const AExtension: string);
    procedure RefreshModuleExtensions;
    function LoadRuntimeModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean;
    function InjectGlobals(const AFormat: string;
      const AContent: string): Boolean;
    function InjectModules(const AFormat: string;
      const AContent: string; const ABaseAddress: string): Boolean;
  public
    constructor Create(const AEngine: TGocciaEngine);
    destructor Destroy; override;

    function Install(const AExtension: TGocciaRuntimeExtension):
      TGocciaRuntimeExtension;
    function FindRuntimeExtension(
      const AClass: TGocciaRuntimeExtensionClass): TGocciaRuntimeExtension;

    procedure RegisterRuntimeGlobalName(const AName: string);
    procedure RegisterGlobalsFromObject(const AValue: TGocciaObjectValue;
      const AKind: string);
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure WaitForIdle; override;
    procedure DiscardPending; override;
    procedure SetAllowedFetchHosts(const AHosts: TStrings); override;
    function InjectGlobalsFromJSON5(
      const AJSON5String: string): Boolean; override;
    function InjectGlobalsFromTOML(
      const ATOMLString: string): Boolean; override;
    function InjectGlobalsFromYAML(const AYamlString: string): Boolean; override;
    function InjectModulesFromJSON5(const AJSON5String: string;
      const ABaseAddress: string): Boolean; override;
    function InjectModulesFromTOML(const ATOMLString: string;
      const ABaseAddress: string): Boolean; override;
    function InjectModulesFromYAML(const AYAMLString: string;
      const ABaseAddress: string): Boolean; override;

    property Engine: TGocciaEngine read FEngine;
  end;

  TGocciaRuntime = class
  private
    FEngine: TGocciaEngine;
    FCore: TGocciaRuntimeCore;
    FOwnsEngine: Boolean;
    FOwnedExecutor: TGocciaExecutor;

    constructor CreateWithEngine(const AEngine: TGocciaEngine;
      const AOwnsEngine: Boolean);
  public
    constructor Create(const AEngine: TGocciaEngine); overload;
    constructor Create(const AEngine: TGocciaEngine;
      const AOwnsEngine: Boolean); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList;
      const AExecutor: TGocciaExecutor); overload;
    destructor Destroy; override;

    class function AttachToEngine(
      const AEngine: TGocciaEngine): TGocciaRuntime; static;

    function Install(const AExtension: TGocciaRuntimeExtension):
      TGocciaRuntimeExtension;
    function FindRuntimeExtension(
      const AClass: TGocciaRuntimeExtensionClass): TGocciaRuntimeExtension;
    function Execute: TGocciaScriptResult;
    procedure WaitForIdle;
    procedure DiscardPending;
    procedure SetAllowedFetchHosts(const AHosts: TStrings);

    class function RunScript(const ASource: string;
      const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload; static;
    class function RunScriptFromFile(
      const AFileName: string): TGocciaScriptResult; overload; static;
    class function RunScriptFromStringList(const ASource: TStringList;
      const AFileName: string): TGocciaScriptResult; overload; static;

    property Engine: TGocciaEngine read FEngine;
    property Core: TGocciaRuntimeCore read FCore;
  end;

function AttachRuntime(const AEngine: TGocciaEngine): TGocciaRuntimeCore;
function GetRuntime(const AEngine: TGocciaEngine): TGocciaRuntimeCore;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.Modules.ContentProvider,
  Goccia.TextFiles,
  Goccia.Values.ArrayValue;

function AttachRuntime(const AEngine: TGocciaEngine): TGocciaRuntimeCore;
begin
  if not Assigned(AEngine) then
    raise Exception.Create('Cannot attach runtime to a nil engine.');

  Result := GetRuntime(AEngine);
  if Assigned(Result) then
    Exit;

  Result := TGocciaRuntimeCore.Create(AEngine);
  try
    AEngine.AddExtension(Result);
  except
    Result.Free;
    raise;
  end;
end;

function GetRuntime(const AEngine: TGocciaEngine): TGocciaRuntimeCore;
begin
  if not Assigned(AEngine) then
    Exit(nil);

  Result := TGocciaRuntimeCore(AEngine.FindExtension(TGocciaRuntimeCore));
end;

{ TGocciaRuntimeExtension }

procedure TGocciaRuntimeExtension.Attach(const ARuntime: TGocciaRuntimeCore);
begin
  if not Assigned(ARuntime) then
    raise Exception.Create('Cannot attach runtime extension to a nil runtime.');
  FRuntime := ARuntime;
end;

procedure TGocciaRuntimeExtension.Detach;
begin
  FRuntime := nil;
end;

procedure TGocciaRuntimeExtension.AddModuleExtensions(
  const AExtensions: TStrings);
begin
end;

function TGocciaRuntimeExtension.TryLoadModule(const AResolvedPath: string;
  out AModule: TGocciaModule): Boolean;
begin
  AModule := nil;
  Result := False;
end;

function TGocciaRuntimeExtension.TryInjectGlobals(const AFormat: string;
  const AContent: string): Boolean;
begin
  Result := False;
end;

function TGocciaRuntimeExtension.TryInjectModules(const AFormat: string;
  const AContent: string; const ABaseAddress: string): Boolean;
begin
  Result := False;
end;

procedure TGocciaRuntimeExtension.ApplyHostRestrictions(
  const AAllowedHosts: TStrings);
begin
end;

procedure TGocciaRuntimeExtension.WaitForIdle;
begin
end;

procedure TGocciaRuntimeExtension.DiscardPending;
begin
end;

{ TGocciaRuntimeCore }

constructor TGocciaRuntimeCore.Create(const AEngine: TGocciaEngine);
begin
  inherited Create;
  if not Assigned(AEngine) then
    raise Exception.Create('TGocciaRuntimeCore requires an engine.');

  FEngine := AEngine;
  FExtensions := TObjectList<TGocciaRuntimeExtension>.Create(True);
  ConfigureFileLoading;
  CaptureResolverExtensions;
  RefreshModuleExtensions;
  FPrevRuntimeModuleLoader := FEngine.ModuleLoader.RuntimeModuleLoader;
  FEngine.ModuleLoader.RuntimeModuleLoader := LoadRuntimeModule;
end;

destructor TGocciaRuntimeCore.Destroy;
var
  I: Integer;
begin
  if Assigned(FEngine) and Assigned(FEngine.ModuleLoader) then
    FEngine.ModuleLoader.RuntimeModuleLoader := FPrevRuntimeModuleLoader;

  if Assigned(FExtensions) then
  begin
    for I := FExtensions.Count - 1 downto 0 do
      FExtensions[I].Detach;
    FExtensions.Free;
  end;

  inherited;
end;

procedure TGocciaRuntimeCore.ConfigureFileLoading;
begin
  if FEngine.ModuleLoader.ContentProvider is
     TGocciaUnavailableModuleContentProvider then
    FEngine.ModuleLoader.SetContentProvider(
      TGocciaFileSystemModuleContentProvider.Create, True);
end;

procedure TGocciaRuntimeCore.CaptureResolverExtensions;
begin
  FBaseResolverExtensions := FEngine.Resolver.GetExtensions;
end;

procedure TGocciaRuntimeCore.AddResolverExtension(
  var AExtensions: TModuleResolverExtensionArray; var ACount: Integer;
  const AExtension: string);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
    if SameText(AExtensions[I], AExtension) then
      Exit;

  if ACount >= Length(AExtensions) then
    SetLength(AExtensions, Length(AExtensions) * 2 + 8);
  AExtensions[ACount] := AExtension;
  Inc(ACount);
end;

procedure TGocciaRuntimeCore.RefreshModuleExtensions;
var
  Count: Integer;
  Extensions: TModuleResolverExtensionArray;
  I: Integer;
  RuntimeExtensions: TStringList;
begin
  Count := 0;
  SetLength(Extensions, Length(FBaseResolverExtensions) +
    Length(EngineModuleImportExtensions) + 16);

  for I := 0 to High(FBaseResolverExtensions) do
    AddResolverExtension(Extensions, Count, FBaseResolverExtensions[I]);
  for I := Low(EngineModuleImportExtensions) to High(EngineModuleImportExtensions) do
    AddResolverExtension(Extensions, Count, EngineModuleImportExtensions[I]);

  RuntimeExtensions := TStringList.Create;
  try
    RuntimeExtensions.CaseSensitive := False;
    RuntimeExtensions.Duplicates := dupIgnore;
    for I := 0 to FExtensions.Count - 1 do
      FExtensions[I].AddModuleExtensions(RuntimeExtensions);
    for I := 0 to RuntimeExtensions.Count - 1 do
      AddResolverExtension(Extensions, Count, RuntimeExtensions[I]);
  finally
    RuntimeExtensions.Free;
  end;

  SetLength(Extensions, Count);
  FEngine.Resolver.SetExtensions(Extensions);
end;

function TGocciaRuntimeCore.Install(
  const AExtension: TGocciaRuntimeExtension): TGocciaRuntimeExtension;
var
  Added: Boolean;
  Existing: TGocciaRuntimeExtension;
begin
  if not Assigned(AExtension) then
    raise Exception.Create('Runtime extension cannot be nil.');

  Existing := FindRuntimeExtension(TGocciaRuntimeExtensionClass(
    AExtension.ClassType));
  if Assigned(Existing) then
  begin
    AExtension.Free;
    Exit(Existing);
  end;

  Added := False;
  try
    AExtension.Attach(Self);
    FExtensions.Add(AExtension);
    Added := True;
    RefreshModuleExtensions;
    FEngine.RefreshGlobalThis;
  except
    if Added then
    begin
      FExtensions.Extract(AExtension);
      try
        RefreshModuleExtensions;
      except
        // Preserve the attachment or refresh failure that triggered cleanup.
      end;
    end;
    try
      AExtension.Detach;
    except
      // Preserve the attachment or refresh failure that triggered cleanup.
    end;
    AExtension.Free;
    raise;
  end;
  Result := AExtension;
end;

function TGocciaRuntimeCore.FindRuntimeExtension(
  const AClass: TGocciaRuntimeExtensionClass): TGocciaRuntimeExtension;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].InheritsFrom(AClass) then
      Exit(FExtensions[I]);
  Result := nil;
end;

procedure TGocciaRuntimeCore.RegisterRuntimeGlobalName(const AName: string);
var
  RuntimeGlobalsValue: TGocciaValue;
begin
  if not Assigned(FEngine.GocciaGlobal) then
    Exit;

  RuntimeGlobalsValue := FEngine.GocciaGlobal.GetProperty(PROP_RUNTIME_GLOBALS);
  if RuntimeGlobalsValue is TGocciaArrayValue then
    TGocciaArrayValue(RuntimeGlobalsValue).Elements.Add(
      TGocciaStringLiteralValue.Create(AName));
end;

procedure TGocciaRuntimeCore.RegisterGlobalsFromObject(
  const AValue: TGocciaObjectValue; const AKind: string);
var
  Key: string;
begin
  if not Assigned(AValue) then
    FEngine.ThrowError('Globals ' + AKind + ' must be a top-level object.', 0, 0);

  for Key in AValue.GetOwnPropertyKeys do
    FEngine.RegisterGlobal(Key, AValue.GetProperty(Key));

  FEngine.RefreshGlobalThis;
end;

function TGocciaRuntimeCore.SpeciesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

function TGocciaRuntimeCore.LoadRuntimeModule(const AResolvedPath: string;
  out AModule: TGocciaModule): Boolean;
var
  I: Integer;
begin
  AModule := nil;
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].TryLoadModule(AResolvedPath, AModule) then
      Exit(True);

  if Assigned(FPrevRuntimeModuleLoader) then
    Exit(FPrevRuntimeModuleLoader(AResolvedPath, AModule));

  Result := False;
end;

function TGocciaRuntimeCore.InjectGlobals(const AFormat: string;
  const AContent: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].TryInjectGlobals(AFormat, AContent) then
      Exit(True);
  Result := False;
end;

function TGocciaRuntimeCore.InjectModules(const AFormat: string;
  const AContent: string; const ABaseAddress: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    if FExtensions[I].TryInjectModules(AFormat, AContent,
       ABaseAddress) then
      Exit(True);
  Result := False;
end;

procedure TGocciaRuntimeCore.WaitForIdle;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    FExtensions[I].WaitForIdle;
end;

procedure TGocciaRuntimeCore.DiscardPending;
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    FExtensions[I].DiscardPending;
end;

procedure TGocciaRuntimeCore.SetAllowedFetchHosts(const AHosts: TStrings);
var
  I: Integer;
begin
  for I := 0 to FExtensions.Count - 1 do
    FExtensions[I].ApplyHostRestrictions(AHosts);
end;

function TGocciaRuntimeCore.InjectGlobalsFromJSON5(
  const AJSON5String: string): Boolean;
begin
  Result := InjectGlobals('json5', AJSON5String);
end;

function TGocciaRuntimeCore.InjectGlobalsFromTOML(
  const ATOMLString: string): Boolean;
begin
  Result := InjectGlobals('toml', ATOMLString);
end;

function TGocciaRuntimeCore.InjectGlobalsFromYAML(
  const AYamlString: string): Boolean;
begin
  Result := InjectGlobals('yaml', AYamlString);
end;

function TGocciaRuntimeCore.InjectModulesFromJSON5(
  const AJSON5String: string; const ABaseAddress: string): Boolean;
begin
  Result := InjectModules('json5', AJSON5String, ABaseAddress);
end;

function TGocciaRuntimeCore.InjectModulesFromTOML(
  const ATOMLString: string; const ABaseAddress: string): Boolean;
begin
  Result := InjectModules('toml', ATOMLString, ABaseAddress);
end;

function TGocciaRuntimeCore.InjectModulesFromYAML(
  const AYAMLString: string; const ABaseAddress: string): Boolean;
begin
  Result := InjectModules('yaml', AYAMLString, ABaseAddress);
end;

{ TGocciaRuntime }

constructor TGocciaRuntime.CreateWithEngine(const AEngine: TGocciaEngine;
  const AOwnsEngine: Boolean);
begin
  inherited Create;
  if not Assigned(AEngine) then
    raise Exception.Create('TGocciaRuntime requires an engine.');

  FEngine := AEngine;
  FOwnsEngine := AOwnsEngine;
  try
    FCore := AttachRuntime(FEngine);
  except
    if FOwnsEngine then
      FEngine.Free;
    FEngine := nil;
    raise;
  end;
end;

constructor TGocciaRuntime.Create(const AEngine: TGocciaEngine);
begin
  CreateWithEngine(AEngine, False);
end;

constructor TGocciaRuntime.Create(const AEngine: TGocciaEngine;
  const AOwnsEngine: Boolean);
begin
  CreateWithEngine(AEngine, AOwnsEngine);
end;

constructor TGocciaRuntime.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  FOwnedExecutor := TGocciaInterpreterExecutor.Create;
  try
    CreateWithEngine(
      TGocciaEngine.Create(AFileName, ASourceLines, FOwnedExecutor), True);
  except
    FreeAndNil(FOwnedExecutor);
    raise;
  end;
end;

constructor TGocciaRuntime.Create(const AFileName: string;
  const ASourceLines: TStringList; const AExecutor: TGocciaExecutor);
begin
  CreateWithEngine(TGocciaEngine.Create(AFileName, ASourceLines, AExecutor),
    True);
end;

destructor TGocciaRuntime.Destroy;
begin
  if FOwnsEngine then
    FEngine.Free;
  FEngine := nil;
  FCore := nil;
  FOwnedExecutor.Free;
  FOwnedExecutor := nil;
  inherited;
end;

class function TGocciaRuntime.AttachToEngine(
  const AEngine: TGocciaEngine): TGocciaRuntime;
begin
  Result := TGocciaRuntime.Create(AEngine);
end;

function TGocciaRuntime.Install(
  const AExtension: TGocciaRuntimeExtension): TGocciaRuntimeExtension;
begin
  Result := FCore.Install(AExtension);
end;

function TGocciaRuntime.FindRuntimeExtension(
  const AClass: TGocciaRuntimeExtensionClass): TGocciaRuntimeExtension;
begin
  Result := FCore.FindRuntimeExtension(AClass);
end;

function TGocciaRuntime.Execute: TGocciaScriptResult;
begin
  Result := FEngine.Execute;
end;

procedure TGocciaRuntime.WaitForIdle;
begin
  if Assigned(FCore) then
    FCore.WaitForIdle;
end;

procedure TGocciaRuntime.DiscardPending;
begin
  if Assigned(FCore) then
    FCore.DiscardPending;
end;

procedure TGocciaRuntime.SetAllowedFetchHosts(const AHosts: TStrings);
begin
  if Assigned(FCore) then
    FCore.SetAllowedFetchHosts(AHosts);
end;

class function TGocciaRuntime.RunScript(const ASource: string;
  const AFileName: string): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := CreateTextLines(ASource);
  try
    Result := RunScriptFromStringList(SourceList, AFileName);
  finally
    SourceList.Free;
  end;
end;

class function TGocciaRuntime.RunScriptFromFile(
  const AFileName: string): TGocciaScriptResult;
var
  Source: TStringList;
begin
  Source := CreateFileTextLines(ReadUTF8FileText(AFileName));
  try
    Result := RunScriptFromStringList(Source, AFileName);
  finally
    Source.Free;
  end;
end;

class function TGocciaRuntime.RunScriptFromStringList(
  const ASource: TStringList; const AFileName: string): TGocciaScriptResult;
var
  Runtime: TGocciaRuntime;
begin
  Runtime := TGocciaRuntime.Create(AFileName, ASource);
  try
    Result := Runtime.Execute;
  finally
    Runtime.Free;
  end;
end;

end.

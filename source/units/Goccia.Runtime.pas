unit Goccia.Runtime;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Builtins.Benchmark,
  Goccia.Builtins.Console,
  Goccia.Builtins.CSV,
  Goccia.Builtins.GlobalFetch,
  Goccia.Builtins.GlobalFFI,
  Goccia.Builtins.GlobalURL,
  Goccia.Builtins.JSON5,
  Goccia.Builtins.JSONL,
  Goccia.Builtins.Performance,
  Goccia.Builtins.TestingLibrary,
  Goccia.Builtins.TOML,
  Goccia.Builtins.TSV,
  Goccia.Builtins.YAML,
  Goccia.Engine,
  Goccia.Executor,
  Goccia.Modules,
  Goccia.Modules.Loader,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaRuntimeGlobal = (
    rgConsole,
    rgHostGlobals,
    rgCSV,
    rgJSON5,
    rgJSONL,
    rgTOML,
    rgTSV,
    rgYAML,
    rgTextAssets,
    rgPerformance,
    rgTextEncoder,
    rgTextDecoder,
    rgURL,
    rgFetch,
    rgSemver,
    rgTestAssertions,
    rgBenchmark,
    rgFFI
  );

  TGocciaRuntimeGlobals = set of TGocciaRuntimeGlobal;
  TGocciaRuntimeImportExtensionArray = array of string;

  TGocciaRuntimeExtension = class(TGocciaEngineExtension)
  private
    FEngine: TGocciaEngine;
    FGlobals: TGocciaRuntimeGlobals;
    FPrevRuntimeModuleLoader: TGocciaRuntimeModuleLoader;

    FBuiltinConsole: TGocciaConsole;
    FBuiltinCSV: TGocciaCSVBuiltin;
    FBuiltinJSON5: TGocciaJSON5Builtin;
    FBuiltinJSONL: TGocciaJSONLBuiltin;
    FBuiltinTOML: TGocciaTOMLBuiltin;
    FBuiltinTSV: TGocciaTSVBuiltin;
    FBuiltinYAML: TGocciaYAMLBuiltin;
    FBuiltinPerformance: TGocciaPerformance;
    FBuiltinURL: TGocciaGlobalURL;
    FBuiltinURLSearchParams: TGocciaGlobalURLSearchParams;
    FBuiltinFetch: TGocciaGlobalFetch;
    FBuiltinFFI: TGocciaGlobalFFI;
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FBuiltinBenchmark: TGocciaBenchmark;

    procedure ConfigureModuleExtensions;
    procedure ConfigureFileLoading;
    procedure RegisterBuiltIns;
    procedure RegisterRuntimeBuiltinName(const AName: string);
    procedure RegisterRuntimeConstructors;
    procedure RegisterGlobalsFromObject(const AValue: TGocciaObjectValue;
      const AKind: string);
    function LoadRuntimeModule(const AResolvedPath: string;
      out AModule: TGocciaModule): Boolean;
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AEngine: TGocciaEngine;
      const AGlobals: TGocciaRuntimeGlobals);
    destructor Destroy; override;

    procedure WaitForIdle; override;
    procedure DiscardPending; override;
    procedure SetAllowedFetchHosts(const AHosts: TStrings); override;
    function InjectGlobalsFromJSON5(
      const AJSON5String: UTF8String): Boolean; override;
    function InjectGlobalsFromTOML(
      const ATOMLString: UTF8String): Boolean; override;
    function InjectGlobalsFromYAML(const AYamlString: string): Boolean; override;

    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
    property BuiltinFetch: TGocciaGlobalFetch read FBuiltinFetch;
    property Globals: TGocciaRuntimeGlobals read FGlobals;
  end;

  TGocciaRuntime = class
  private
    FEngine: TGocciaEngine;
    FExtension: TGocciaRuntimeExtension;
    FOwnsEngine: Boolean;

    constructor CreateWithEngine(const AEngine: TGocciaEngine;
      const AOwnsEngine: Boolean; const AGlobals: TGocciaRuntimeGlobals);
    function GetBuiltinBenchmark: TGocciaBenchmark;
    function GetBuiltinConsole: TGocciaConsole;
    function GetBuiltinFetch: TGocciaGlobalFetch;
    function GetGlobals: TGocciaRuntimeGlobals;
  public
    constructor Create(const AEngine: TGocciaEngine); overload;
    constructor Create(const AEngine: TGocciaEngine;
      const ARuntimeGlobals: TGocciaRuntimeGlobals); overload;
    constructor Create(const AEngine: TGocciaEngine;
      const AOwnsEngine: Boolean); overload;
    constructor Create(const AEngine: TGocciaEngine;
      const ARuntimeGlobals: TGocciaRuntimeGlobals;
      const AOwnsEngine: Boolean); overload;
    constructor Create(const AFileName: string;
      const ASourceLines: TStringList); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList;
      const ARuntimeGlobals: TGocciaRuntimeGlobals); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList;
      const AExecutor: TGocciaExecutor); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList;
      const AExecutor: TGocciaExecutor;
      const ARuntimeGlobals: TGocciaRuntimeGlobals); overload;
    destructor Destroy; override;

    class function AttachToEngine(
      const AEngine: TGocciaEngine): TGocciaRuntime; overload; static;
    class function AttachToEngine(const AEngine: TGocciaEngine;
      const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaRuntime; overload; static;

    function Execute: TGocciaScriptResult;
    procedure WaitForIdle;
    procedure DiscardPending;
    procedure SetAllowedFetchHosts(const AHosts: TStrings);

    class function RunScript(const ASource: string;
      const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload; static;
    class function RunScript(const ASource: string; const AFileName: string;
      const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaScriptResult; overload; static;
    class function RunScriptFromFile(
      const AFileName: string): TGocciaScriptResult; overload; static;
    class function RunScriptFromFile(const AFileName: string;
      const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaScriptResult; overload; static;
    class function RunScriptFromStringList(const ASource: TStringList;
      const AFileName: string): TGocciaScriptResult; overload; static;
    class function RunScriptFromStringList(const ASource: TStringList;
      const AFileName: string;
      const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaScriptResult; overload; static;

    property Engine: TGocciaEngine read FEngine;
    property Extension: TGocciaRuntimeExtension read FExtension;
    property BuiltinConsole: TGocciaConsole read GetBuiltinConsole;
    property BuiltinBenchmark: TGocciaBenchmark read GetBuiltinBenchmark;
    property BuiltinFetch: TGocciaGlobalFetch read GetBuiltinFetch;
    property Globals: TGocciaRuntimeGlobals read GetGlobals;
  end;

const
  DefaultRuntimeGlobals: TGocciaRuntimeGlobals = [
    rgConsole,
    rgHostGlobals,
    rgCSV,
    rgJSON5,
    rgJSONL,
    rgTOML,
    rgTSV,
    rgYAML,
    rgTextAssets,
    rgPerformance,
    rgTextEncoder,
    rgTextDecoder,
    rgURL,
    rgFetch,
    rgSemver
  ];

function AttachRuntimeExtension(
  const AEngine: TGocciaEngine): TGocciaRuntimeExtension; overload;
function AttachRuntimeExtension(const AEngine: TGocciaEngine;
  const AGlobals: TGocciaRuntimeGlobals): TGocciaRuntimeExtension; overload;
function GetRuntimeExtension(const AEngine: TGocciaEngine): TGocciaRuntimeExtension;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.Builtins.Semver,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.CSV,
  Goccia.Error,
  Goccia.FetchManager,
  Goccia.FileExtensions,
  Goccia.GarbageCollector,
  Goccia.JSON5,
  Goccia.JSONL,
  Goccia.ModuleResolver,
  Goccia.Modules.ContentProvider,
  Goccia.ObjectModel.Engine,
  Goccia.Scope,
  Goccia.TextFiles,
  Goccia.TOML,
  Goccia.TSV,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.HeadersValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ResponseValue,
  Goccia.Values.TextDecoderValue,
  Goccia.Values.TextEncoderValue,
  Goccia.Values.URLSearchParamsValue,
  Goccia.Values.URLValue,
  Goccia.YAML;

const
  TEXT_ASSET_KIND = 'text';

procedure AddRuntimeExtension(var AExtensions: TGocciaRuntimeImportExtensionArray;
  var ACount: Integer; const AExtension: string);
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

function BuiltinObjectOrNil(const ABuiltin: TGocciaBuiltin): TGocciaObjectValue;
begin
  if Assigned(ABuiltin) then
    Result := ABuiltin.BuiltinObject
  else
    Result := nil;
end;

procedure ExposeTextEncoderPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaTextEncoderValue.ExposePrototype(AConstructor);
end;

procedure ExposeTextDecoderPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaTextDecoderValue.ExposePrototype(AConstructor);
end;

procedure ExposeURLPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLValue.ExposePrototype(AConstructor);
end;

procedure ExposeURLSearchParamsPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaURLSearchParamsValue.ExposePrototype(AConstructor);
end;

procedure ExposeHeadersPrototype(const AConstructor: TGocciaValue);
begin
  TGocciaHeadersValue.ExposePrototype(AConstructor);
end;

procedure ExposeResponsePrototype(const AConstructor: TGocciaValue);
begin
  TGocciaResponseValue.ExposePrototype(AConstructor);
end;

function AttachRuntimeExtension(
  const AEngine: TGocciaEngine): TGocciaRuntimeExtension;
begin
  Result := AttachRuntimeExtension(AEngine, DefaultRuntimeGlobals);
end;

function AttachRuntimeExtension(const AEngine: TGocciaEngine;
  const AGlobals: TGocciaRuntimeGlobals): TGocciaRuntimeExtension;
begin
  if not Assigned(AEngine) then
    raise Exception.Create('Cannot attach runtime to a nil engine.');

  Result := GetRuntimeExtension(AEngine);
  if Assigned(Result) then
  begin
    if Result.Globals <> AGlobals then
      raise Exception.Create(
        'Runtime is already attached with a different global configuration.');
    Exit;
  end;

  Result := TGocciaRuntimeExtension.Create(AEngine, AGlobals);
  try
    AEngine.AddExtension(Result);
  except
    Result.Free;
    raise;
  end;
end;

function GetRuntimeExtension(const AEngine: TGocciaEngine): TGocciaRuntimeExtension;
begin
  Result := TGocciaRuntimeExtension(
    AEngine.FindExtension(TGocciaRuntimeExtension));
end;

{ TGocciaRuntime }

constructor TGocciaRuntime.CreateWithEngine(const AEngine: TGocciaEngine;
  const AOwnsEngine: Boolean; const AGlobals: TGocciaRuntimeGlobals);
begin
  inherited Create;
  if not Assigned(AEngine) then
    raise Exception.Create('TGocciaRuntime requires an engine.');

  FEngine := AEngine;
  FOwnsEngine := AOwnsEngine;
  try
    FExtension := AttachRuntimeExtension(FEngine, AGlobals);
  except
    if FOwnsEngine then
      FEngine.Free;
    FEngine := nil;
    raise;
  end;
end;

constructor TGocciaRuntime.Create(const AEngine: TGocciaEngine);
begin
  CreateWithEngine(AEngine, False, DefaultRuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AEngine: TGocciaEngine;
  const ARuntimeGlobals: TGocciaRuntimeGlobals);
begin
  CreateWithEngine(AEngine, False, ARuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AEngine: TGocciaEngine;
  const AOwnsEngine: Boolean);
begin
  CreateWithEngine(AEngine, AOwnsEngine, DefaultRuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AEngine: TGocciaEngine;
  const ARuntimeGlobals: TGocciaRuntimeGlobals;
  const AOwnsEngine: Boolean);
begin
  CreateWithEngine(AEngine, AOwnsEngine, ARuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AFileName: string;
  const ASourceLines: TStringList);
begin
  CreateWithEngine(TGocciaEngine.Create(AFileName, ASourceLines), True,
    DefaultRuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AFileName: string;
  const ASourceLines: TStringList;
  const ARuntimeGlobals: TGocciaRuntimeGlobals);
begin
  CreateWithEngine(TGocciaEngine.Create(AFileName, ASourceLines), True,
    ARuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AFileName: string;
  const ASourceLines: TStringList; const AExecutor: TGocciaExecutor);
begin
  CreateWithEngine(TGocciaEngine.Create(AFileName, ASourceLines, AExecutor),
    True, DefaultRuntimeGlobals);
end;

constructor TGocciaRuntime.Create(const AFileName: string;
  const ASourceLines: TStringList; const AExecutor: TGocciaExecutor;
  const ARuntimeGlobals: TGocciaRuntimeGlobals);
begin
  CreateWithEngine(TGocciaEngine.Create(AFileName, ASourceLines, AExecutor),
    True, ARuntimeGlobals);
end;

destructor TGocciaRuntime.Destroy;
begin
  if FOwnsEngine then
    FEngine.Free;
  FEngine := nil;
  FExtension := nil;
  inherited;
end;

class function TGocciaRuntime.AttachToEngine(
  const AEngine: TGocciaEngine): TGocciaRuntime;
begin
  Result := AttachToEngine(AEngine, DefaultRuntimeGlobals);
end;

class function TGocciaRuntime.AttachToEngine(const AEngine: TGocciaEngine;
  const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaRuntime;
begin
  Result := TGocciaRuntime.Create(AEngine, ARuntimeGlobals);
end;

function TGocciaRuntime.Execute: TGocciaScriptResult;
begin
  Result := FEngine.Execute;
end;

procedure TGocciaRuntime.WaitForIdle;
begin
  if Assigned(FExtension) then
    FExtension.WaitForIdle;
end;

procedure TGocciaRuntime.DiscardPending;
begin
  if Assigned(FExtension) then
    FExtension.DiscardPending;
end;

procedure TGocciaRuntime.SetAllowedFetchHosts(const AHosts: TStrings);
begin
  FEngine.SetAllowedFetchHosts(AHosts);
end;

class function TGocciaRuntime.RunScript(const ASource: string;
  const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScript(ASource, AFileName, DefaultRuntimeGlobals);
end;

class function TGocciaRuntime.RunScript(const ASource: string;
  const AFileName: string;
  const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := CreateUTF8StringList(ASource);
  try
    Result := RunScriptFromStringList(SourceList, AFileName, ARuntimeGlobals);
  finally
    SourceList.Free;
  end;
end;

class function TGocciaRuntime.RunScriptFromFile(
  const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromFile(AFileName, DefaultRuntimeGlobals);
end;

class function TGocciaRuntime.RunScriptFromFile(const AFileName: string;
  const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaScriptResult;
var
  Source: TStringList;
begin
  Source := CreateUTF8FileTextLines(ReadUTF8FileText(AFileName));
  try
    Result := RunScriptFromStringList(Source, AFileName, ARuntimeGlobals);
  finally
    Source.Free;
  end;
end;

class function TGocciaRuntime.RunScriptFromStringList(
  const ASource: TStringList; const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromStringList(ASource, AFileName,
    DefaultRuntimeGlobals);
end;

class function TGocciaRuntime.RunScriptFromStringList(
  const ASource: TStringList; const AFileName: string;
  const ARuntimeGlobals: TGocciaRuntimeGlobals): TGocciaScriptResult;
var
  Runtime: TGocciaRuntime;
begin
  Runtime := TGocciaRuntime.Create(AFileName, ASource, ARuntimeGlobals);
  try
    Result := Runtime.Execute;
  finally
    Runtime.Free;
  end;
end;

function TGocciaRuntime.GetBuiltinBenchmark: TGocciaBenchmark;
begin
  if Assigned(FExtension) then
    Result := FExtension.BuiltinBenchmark
  else
    Result := nil;
end;

function TGocciaRuntime.GetBuiltinConsole: TGocciaConsole;
begin
  if Assigned(FExtension) then
    Result := FExtension.BuiltinConsole
  else
    Result := nil;
end;

function TGocciaRuntime.GetBuiltinFetch: TGocciaGlobalFetch;
begin
  if Assigned(FExtension) then
    Result := FExtension.BuiltinFetch
  else
    Result := nil;
end;

function TGocciaRuntime.GetGlobals: TGocciaRuntimeGlobals;
begin
  if Assigned(FExtension) then
    Result := FExtension.Globals
  else
    Result := [];
end;

{ TGocciaRuntimeExtension }

constructor TGocciaRuntimeExtension.Create(const AEngine: TGocciaEngine;
  const AGlobals: TGocciaRuntimeGlobals);
begin
  inherited Create;
  FEngine := AEngine;
  FGlobals := AGlobals;

  if rgFetch in FGlobals then
    TGocciaFetchManager.Initialize;

  RegisterBuiltIns;
  RegisterRuntimeConstructors;
  ConfigureFileLoading;
  ConfigureModuleExtensions;
  FPrevRuntimeModuleLoader := FEngine.ModuleLoader.RuntimeModuleLoader;
  FEngine.ModuleLoader.RuntimeModuleLoader := LoadRuntimeModule;
  FEngine.RefreshGlobalThis;
end;

destructor TGocciaRuntimeExtension.Destroy;
begin
  if Assigned(FEngine) then
    FEngine.ModuleLoader.RuntimeModuleLoader := FPrevRuntimeModuleLoader;

  FBuiltinConsole.Free;
  FBuiltinCSV.Free;
  FBuiltinJSON5.Free;
  FBuiltinJSONL.Free;
  FBuiltinTOML.Free;
  FBuiltinTSV.Free;
  FBuiltinYAML.Free;
  FBuiltinPerformance.Free;
  FBuiltinURL.Free;
  FBuiltinURLSearchParams.Free;
  FBuiltinFetch.Free;
  FBuiltinFFI.Free;
  FBuiltinTestAssertions.Free;
  FBuiltinBenchmark.Free;

  if rgFetch in FGlobals then
    TGocciaFetchManager.Shutdown;

  inherited;
end;

procedure TGocciaRuntimeExtension.ConfigureModuleExtensions;
var
  Count: Integer;
  ExistingExtensions: TModuleResolverExtensionArray;
  Extensions: TGocciaRuntimeImportExtensionArray;
  I: Integer;
begin
  Count := 0;
  ExistingExtensions := FEngine.Resolver.GetExtensions;
  SetLength(Extensions, Length(ExistingExtensions) +
    Length(EngineModuleImportExtensions) + 16);

  for I := 0 to High(ExistingExtensions) do
    AddRuntimeExtension(Extensions, Count, ExistingExtensions[I]);
  AddRuntimeExtension(Extensions, Count, EXT_JS);
  AddRuntimeExtension(Extensions, Count, EXT_JSX);
  AddRuntimeExtension(Extensions, Count, EXT_TS);
  AddRuntimeExtension(Extensions, Count, EXT_TSX);
  AddRuntimeExtension(Extensions, Count, EXT_MJS);
  AddRuntimeExtension(Extensions, Count, EXT_JSON);

  if rgJSON5 in FGlobals then
  begin
    AddRuntimeExtension(Extensions, Count, EXT_JSON5);
    AddRuntimeExtension(Extensions, Count, EXT_JSONC);
  end;
  if rgJSONL in FGlobals then
    AddRuntimeExtension(Extensions, Count, EXT_JSONL);
  if rgTOML in FGlobals then
    AddRuntimeExtension(Extensions, Count, EXT_TOML);
  if rgYAML in FGlobals then
  begin
    AddRuntimeExtension(Extensions, Count, EXT_YAML);
    AddRuntimeExtension(Extensions, Count, EXT_YML);
  end;
  if rgCSV in FGlobals then
    AddRuntimeExtension(Extensions, Count, EXT_CSV);
  if rgTSV in FGlobals then
    AddRuntimeExtension(Extensions, Count, EXT_TSV);
  if rgTextAssets in FGlobals then
  begin
    AddRuntimeExtension(Extensions, Count, EXT_TXT);
    AddRuntimeExtension(Extensions, Count, EXT_MD);
  end;

  SetLength(Extensions, Count);
  FEngine.Resolver.SetExtensions(Extensions);
end;

procedure TGocciaRuntimeExtension.ConfigureFileLoading;
begin
  if FEngine.ModuleLoader.ContentProvider is
     TGocciaUnavailableModuleContentProvider then
    FEngine.ModuleLoader.SetContentProvider(
      TGocciaFileSystemModuleContentProvider.Create, True);
end;

procedure TGocciaRuntimeExtension.RegisterBuiltIns;
var
  Scope: TGocciaScope;
begin
  Scope := FEngine.Interpreter.GlobalScope;

  if rgConsole in FGlobals then
    FBuiltinConsole := TGocciaConsole.Create('console', Scope, FEngine.ThrowError);
  if rgHostGlobals in FGlobals then
    FEngine.BuiltinGlobals.RegisterRuntimeGlobals;
  if rgCSV in FGlobals then
    FBuiltinCSV := TGocciaCSVBuiltin.Create('CSV', Scope, FEngine.ThrowError);
  if rgJSON5 in FGlobals then
    FBuiltinJSON5 := TGocciaJSON5Builtin.Create('JSON5', Scope, FEngine.ThrowError);
  if rgJSONL in FGlobals then
    FBuiltinJSONL := TGocciaJSONLBuiltin.Create('JSONL', Scope, FEngine.ThrowError);
  if rgTOML in FGlobals then
    FBuiltinTOML := TGocciaTOMLBuiltin.Create('TOML', Scope, FEngine.ThrowError);
  if rgTSV in FGlobals then
    FBuiltinTSV := TGocciaTSVBuiltin.Create('TSV', Scope, FEngine.ThrowError);
  if rgYAML in FGlobals then
    FBuiltinYAML := TGocciaYAMLBuiltin.Create('YAML', Scope, FEngine.ThrowError);
  if rgPerformance in FGlobals then
    FBuiltinPerformance := TGocciaPerformance.Create('performance', Scope, FEngine.ThrowError);
  if rgURL in FGlobals then
  begin
    FBuiltinURL := TGocciaGlobalURL.Create(CONSTRUCTOR_URL, Scope,
      FEngine.ThrowError);
    FBuiltinURLSearchParams := TGocciaGlobalURLSearchParams.Create(
      CONSTRUCTOR_URL_SEARCH_PARAMS, Scope, FEngine.ThrowError);
  end;
  if rgFetch in FGlobals then
    FBuiltinFetch := TGocciaGlobalFetch.Create('Fetch', Scope, FEngine.ThrowError);

  if rgTestAssertions in FGlobals then
  begin
    FBuiltinTestAssertions := TGocciaTestAssertions.Create(
      'TestAssertions', Scope, FEngine.ThrowError);
    RegisterRuntimeBuiltinName('TestAssertions');
  end;
  if rgBenchmark in FGlobals then
  begin
    FBuiltinBenchmark := TGocciaBenchmark.Create(
      'Benchmark', Scope, FEngine.ThrowError);
    RegisterRuntimeBuiltinName('Benchmark');
  end;
  if rgFFI in FGlobals then
  begin
    FBuiltinFFI := TGocciaGlobalFFI.Create(CONSTRUCTOR_FFI, Scope,
      FEngine.ThrowError);
    RegisterRuntimeBuiltinName('FFI');
  end;

  if (rgSemver in FGlobals) and Assigned(FEngine.GocciaGlobal) then
    FEngine.GocciaGlobal.AssignProperty(
      SEMVER_NAMESPACE_PROPERTY, CreateSemverNamespace);
end;

procedure TGocciaRuntimeExtension.RegisterRuntimeBuiltinName(
  const AName: string);
var
  BuiltInsValue: TGocciaValue;
begin
  if not Assigned(FEngine.GocciaGlobal) then
    Exit;

  BuiltInsValue := FEngine.GocciaGlobal.GetProperty('builtIns');
  if BuiltInsValue is TGocciaArrayValue then
    TGocciaArrayValue(BuiltInsValue).Elements.Add(
      TGocciaStringLiteralValue.Create(AName));
end;

procedure TGocciaRuntimeExtension.RegisterRuntimeConstructors;
var
  RuntimeConstructor: TGocciaClassValue;
  ObjectPrototype: TGocciaObjectValue;
  PerformanceConstructor: TGocciaNativeFunctionValue;
  TypeDef: TGocciaTypeDefinition;
begin
  if not Assigned(FEngine.ObjectConstructor) then
    Exit;
  ObjectPrototype := FEngine.ObjectConstructor.Prototype;

  if rgTextEncoder in FGlobals then
  begin
    TypeDef.ConstructorName := CONSTRUCTOR_TEXT_ENCODER;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaTextEncoderClassValue;
    TypeDef.ExposePrototype := @ExposeTextEncoderPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectPrototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FEngine.Interpreter.GlobalScope, TypeDef,
      SpeciesGetter, RuntimeConstructor);
  end;

  if rgTextDecoder in FGlobals then
  begin
    TypeDef.ConstructorName := CONSTRUCTOR_TEXT_DECODER;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaTextDecoderClassValue;
    TypeDef.ExposePrototype := @ExposeTextDecoderPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectPrototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FEngine.Interpreter.GlobalScope, TypeDef,
      SpeciesGetter, RuntimeConstructor);
  end;

  if rgURL in FGlobals then
  begin
    TypeDef.ConstructorName := CONSTRUCTOR_URL;
    TypeDef.Kind := gtdkCollectionLikeNativeType;
    TypeDef.ClassValueClass := TGocciaURLClassValue;
    TypeDef.ExposePrototype := @ExposeURLPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinURL);
    TypeDef.PrototypeParent := ObjectPrototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FEngine.Interpreter.GlobalScope, TypeDef,
      SpeciesGetter, RuntimeConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_URL_SEARCH_PARAMS;
    TypeDef.Kind := gtdkCollectionLikeNativeType;
    TypeDef.ClassValueClass := TGocciaURLSearchParamsClassValue;
    TypeDef.ExposePrototype := @ExposeURLSearchParamsPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := BuiltinObjectOrNil(FBuiltinURLSearchParams);
    TypeDef.PrototypeParent := ObjectPrototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FEngine.Interpreter.GlobalScope, TypeDef,
      SpeciesGetter, RuntimeConstructor);
  end;

  if rgFetch in FGlobals then
  begin
    TypeDef.ConstructorName := CONSTRUCTOR_HEADERS;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaHeadersClassValue;
    TypeDef.ExposePrototype := @ExposeHeadersPrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectPrototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FEngine.Interpreter.GlobalScope, TypeDef,
      SpeciesGetter, RuntimeConstructor);

    TypeDef.ConstructorName := CONSTRUCTOR_RESPONSE;
    TypeDef.Kind := gtdkNativeInstanceType;
    TypeDef.ClassValueClass := TGocciaResponseClassValue;
    TypeDef.ExposePrototype := @ExposeResponsePrototype;
    TypeDef.PrototypeProvider := nil;
    TypeDef.StaticSource := nil;
    TypeDef.PrototypeParent := ObjectPrototype;
    TypeDef.AddSpeciesGetter := False;
    RegisterTypeDefinition(FEngine.Interpreter.GlobalScope, TypeDef,
      SpeciesGetter, RuntimeConstructor);
  end;

  if rgPerformance in FGlobals then
  begin
    PerformanceConstructor := TGocciaPerformance.CreateInterfaceObject;
    FEngine.Interpreter.GlobalScope.DefineLexicalBinding(
      CONSTRUCTOR_PERFORMANCE, PerformanceConstructor, dtConst);
  end;
end;

procedure TGocciaRuntimeExtension.RegisterGlobalsFromObject(
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

function TGocciaRuntimeExtension.LoadRuntimeModule(const AResolvedPath: string;
  out AModule: TGocciaModule): Boolean;
var
  Content: TGocciaModuleContent;
  CSVParser: TGocciaCSVParser;
  CSVRecords: TGocciaArrayValue;
  Documents: TGocciaArrayValue;
  DocumentIndex: Integer;
  Extension: string;
  JSON5Parser: TGocciaJSON5Parser;
  JSONLParser: TGocciaJSONLParser;
  JSONLRecords: TGocciaArrayValue;
  Key: string;
  LoadSucceeded: Boolean;
  Metadata: TGocciaObjectValue;
  NormalizedText: UTF8String;
  Obj: TGocciaObjectValue;
  ParsedDocument: TGocciaValue;
  ParsedValue: TGocciaValue;
  TOMLParser: TGocciaTOMLParser;
  TSVParser: TGocciaTSVParser;
  TSVRecords: TGocciaArrayValue;
  YAMLParser: TGocciaYAMLParser;
begin
  AModule := nil;
  Extension := LowerCase(ExtractFileExt(AResolvedPath));
  Result := ((rgCSV in FGlobals) and IsCSVExtension(Extension)) or
    ((rgTSV in FGlobals) and IsTSVExtension(Extension)) or
    ((rgJSON5 in FGlobals) and IsJSON5Extension(Extension)) or
    ((rgJSONL in FGlobals) and IsJSONLExtension(Extension)) or
    ((rgTOML in FGlobals) and IsTOMLExtension(Extension)) or
    ((rgYAML in FGlobals) and IsYAMLExtension(Extension)) or
    ((rgTextAssets in FGlobals) and IsTextAssetExtension(Extension));
  if not Result then
  begin
    if Assigned(FPrevRuntimeModuleLoader) then
      Result := FPrevRuntimeModuleLoader(AResolvedPath, AModule);
    Exit;
  end;

  Content := FEngine.ModuleLoader.ContentProvider.LoadContent(AResolvedPath);
  CSVRecords := nil;
  Documents := nil;
  JSONLRecords := nil;
  ParsedDocument := nil;
  ParsedValue := nil;
  TSVRecords := nil;
  try
    if IsTextAssetExtension(Extension) then
    begin
      NormalizedText := NormalizeUTF8NewlinesToLF(Content.Text);
      Metadata := TGocciaObjectValue.Create(
        TGocciaObjectValue.SharedObjectPrototype, 5);
      Metadata.SetProperty(PROP_KIND,
        TGocciaStringLiteralValue.Create(TEXT_ASSET_KIND));
      Metadata.SetProperty(PROP_PATH,
        TGocciaStringLiteralValue.Create(AResolvedPath));
      Metadata.SetProperty(PROP_FILE_NAME,
        TGocciaStringLiteralValue.Create(ExtractFileName(AResolvedPath)));
      Metadata.SetProperty(PROP_EXTENSION,
        TGocciaStringLiteralValue.Create(Extension));
      Metadata.SetProperty(PROP_BYTE_LENGTH,
        TGocciaNumberLiteralValue.Create(Length(Content.Text)));
      Metadata.Freeze;

      AModule := TGocciaModule.Create(AResolvedPath);
      AModule.LastModified := Content.LastModified;
      LoadSucceeded := False;
      try
        AModule.ExportsTable.AddOrSetValue(PROP_METADATA, Metadata);
        AModule.ExportsTable.AddOrSetValue(PROP_CONTENT,
          TGocciaStringLiteralValue.FromUTF8(NormalizedText));
        LoadSucceeded := True;
      finally
        if not LoadSucceeded then
        begin
          AModule.Free;
          AModule := nil;
        end;
      end;
      Exit;
    end;

    if IsCSVExtension(Extension) then
    begin
      CSVParser := TGocciaCSVParser.Create;
      try
        try
          CSVRecords := CSVParser.Parse(Content.Text);
        except
          on E: EGocciaCSVParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse CSV module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        CSVParser.Free;
      end;
    end
    else if IsTSVExtension(Extension) then
    begin
      TSVParser := TGocciaTSVParser.Create;
      try
        try
          TSVRecords := TSVParser.Parse(Content.Text);
        except
          on E: EGocciaTSVParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse TSV module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        TSVParser.Free;
      end;
    end
    else if IsJSON5Extension(Extension) then
    begin
      JSON5Parser := TGocciaJSON5Parser.Create;
      try
        try
          ParsedValue := JSON5Parser.Parse(Content.Text);
        except
          on E: EGocciaJSON5ParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse JSON5 module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        JSON5Parser.Free;
      end;
    end
    else if IsJSONLExtension(Extension) then
    begin
      JSONLParser := TGocciaJSONLParser.Create;
      try
        try
          JSONLRecords := JSONLParser.Parse(Content.Text);
        except
          on E: EGocciaJSONLParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse JSONL module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        JSONLParser.Free;
      end;
    end
    else if IsTOMLExtension(Extension) then
    begin
      TOMLParser := TGocciaTOMLParser.Create;
      try
        try
          ParsedValue := TOMLParser.Parse(Content.Text);
        except
          on E: EGocciaTOMLParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse TOML module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        TOMLParser.Free;
      end;
    end
    else if IsYAMLExtension(Extension) then
    begin
      YAMLParser := TGocciaYAMLParser.Create;
      try
        try
          Documents := YAMLParser.ParseDocuments(Content.Text);
        except
          on E: EGocciaYAMLParseError do
            raise TGocciaRuntimeError.Create(
              Format('Failed to parse YAML module "%s": %s',
                [AResolvedPath, E.Message]),
              0, 0, AResolvedPath, nil);
        end;
      finally
        YAMLParser.Free;
      end;

      if Documents.Elements.Count = 0 then
        raise TGocciaRuntimeError.Create(
          Format('YAML module "%s" must contain at least one top-level document.',
            [AResolvedPath]),
          0, 0, AResolvedPath, nil);
      if Documents.Elements.Count = 1 then
        ParsedDocument := Documents.Elements[0];
    end;

    AModule := TGocciaModule.Create(AResolvedPath);
    AModule.LastModified := Content.LastModified;
    LoadSucceeded := False;
    try
      if IsJSON5Extension(Extension) or IsTOMLExtension(Extension) then
        ParsedDocument := ParsedValue;

      if Assigned(CSVRecords) then
      begin
        for DocumentIndex := 0 to CSVRecords.Elements.Count - 1 do
          AModule.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            CSVRecords.Elements[DocumentIndex]);
      end
      else if Assigned(TSVRecords) then
      begin
        for DocumentIndex := 0 to TSVRecords.Elements.Count - 1 do
          AModule.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            TSVRecords.Elements[DocumentIndex]);
      end
      else if Assigned(JSONLRecords) then
      begin
        for DocumentIndex := 0 to JSONLRecords.Elements.Count - 1 do
          AModule.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            JSONLRecords.Elements[DocumentIndex]);
      end
      else if Assigned(Documents) and (Documents.Elements.Count > 1) then
      begin
        for DocumentIndex := 0 to Documents.Elements.Count - 1 do
          AModule.ExportsTable.AddOrSetValue(IntToStr(DocumentIndex),
            Documents.Elements[DocumentIndex]);
      end
      else if ParsedDocument is TGocciaObjectValue then
      begin
        Obj := TGocciaObjectValue(ParsedDocument);
        for Key in Obj.GetOwnPropertyKeys do
          AModule.ExportsTable.AddOrSetValue(Key, Obj.GetProperty(Key));
      end;

      LoadSucceeded := True;
    finally
      if not LoadSucceeded then
      begin
        AModule.Free;
        AModule := nil;
      end;
    end;
  finally
    CSVRecords.Free;
    TSVRecords.Free;
    JSONLRecords.Free;
    Documents.Free;
    Content.Free;
  end;
end;

function TGocciaRuntimeExtension.SpeciesGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

procedure TGocciaRuntimeExtension.WaitForIdle;
begin
  if rgFetch in FGlobals then
    WaitForFetchIdle;
end;

procedure TGocciaRuntimeExtension.DiscardPending;
begin
  if rgFetch in FGlobals then
    DiscardFetchCompletions;
end;

procedure TGocciaRuntimeExtension.SetAllowedFetchHosts(const AHosts: TStrings);
var
  EmptyHosts: TStringList;
begin
  if not Assigned(FBuiltinFetch) then
    Exit;

  if Assigned(AHosts) then
    FBuiltinFetch.SetAllowedHosts(AHosts)
  else
  begin
    EmptyHosts := TStringList.Create;
    try
      FBuiltinFetch.SetAllowedHosts(EmptyHosts);
    finally
      EmptyHosts.Free;
    end;
  end;
end;

function TGocciaRuntimeExtension.InjectGlobalsFromJSON5(
  const AJSON5String: UTF8String): Boolean;
var
  Parser: TGocciaJSON5Parser;
  ParsedValue: TGocciaValue;
begin
  Result := rgJSON5 in FGlobals;
  if not Result then
    Exit;

  Parser := TGocciaJSON5Parser.Create;
  try
    ParsedValue := Parser.Parse(AJSON5String);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    FEngine.ThrowError('Globals JSON5 must be a top-level object.', 0, 0);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    RegisterGlobalsFromObject(TGocciaObjectValue(ParsedValue), 'JSON5');
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

function TGocciaRuntimeExtension.InjectGlobalsFromTOML(
  const ATOMLString: UTF8String): Boolean;
var
  Parser: TGocciaTOMLParser;
  ParsedValue: TGocciaValue;
begin
  Result := rgTOML in FGlobals;
  if not Result then
    Exit;

  Parser := TGocciaTOMLParser.Create;
  try
    ParsedValue := Parser.Parse(ATOMLString);
  finally
    Parser.Free;
  end;

  if not (ParsedValue is TGocciaObjectValue) then
    FEngine.ThrowError('Globals TOML must be a top-level object.', 0, 0);

  TGarbageCollector.Instance.AddTempRoot(ParsedValue);
  try
    RegisterGlobalsFromObject(TGocciaObjectValue(ParsedValue), 'TOML');
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ParsedValue);
  end;
end;

function TGocciaRuntimeExtension.InjectGlobalsFromYAML(
  const AYamlString: string): Boolean;
var
  Documents: TGocciaArrayValue;
  ParsedDocument: TGocciaValue;
  Parser: TGocciaYAMLParser;
begin
  Result := rgYAML in FGlobals;
  if not Result then
    Exit;

  Parser := TGocciaYAMLParser.Create;
  try
    Documents := Parser.ParseDocuments(AYamlString);
  finally
    Parser.Free;
  end;

  try
    if Documents.Elements.Count <> 1 then
      FEngine.ThrowError(
        'Globals YAML must contain exactly one top-level document.', 0, 0);

    ParsedDocument := Documents.Elements[0];
    if not (ParsedDocument is TGocciaObjectValue) then
      FEngine.ThrowError('Globals YAML must be a top-level object.', 0, 0);

    TGarbageCollector.Instance.AddTempRoot(ParsedDocument);
    try
      RegisterGlobalsFromObject(TGocciaObjectValue(ParsedDocument), 'YAML');
    finally
      TGarbageCollector.Instance.RemoveTempRoot(ParsedDocument);
    end;
  finally
    Documents.Free;
  end;
end;

end.

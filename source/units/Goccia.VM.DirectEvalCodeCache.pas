unit Goccia.VM.DirectEvalCodeCache;

{$I Goccia.inc}

{ The per-call-site code cache of direct eval (ADR 0134 Phase 4), like
  JavaScriptCore's DirectEvalCodeCache and SpiderMonkey's EvalCache.

  A direct eval compiles its source as a function nested at its call site,
  against the site's eval site record (ADR 0134 Phase 3), and the engine keeps
  the compiled code for its own lifetime. Each eval site of a function
  template therefore gets a cache of the code it compiled, keyed on the source
  text. The site fixes strictness and the environment the code compiles
  against; the entry also records everything else that decides how the source
  parses, which early errors it raises and how it compiles
  (TGocciaDirectEvalCodeContext), so a hit is code the miss would have
  compiled again.

  Only the compiled template is shared. Each evaluation still instantiates its
  declarations (ES2026 §19.2.1.3 EvalDeclarationInstantiation) and creates a
  fresh closure of the calling frame, with fresh bindings and functions.

  A site keeps at most DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES entries, as
  JavaScriptCore's DirectEvalCodeCache does (maxCacheEntries = 64). Past
  that, the site caches nothing more: its cached sources keep being served,
  and any other source is compiled at each evaluation, its code kept for the
  engine's lifetime as ADR 0134 decision 1 accepts. A site that builds a new
  source at each evaluation, `eval("x + " + i)`, so stops adding to the cache
  what it already adds as retained code. There is no limit on the length of
  a cached source: the engine keeps uncached code alive, so skipping a long
  source would keep a new copy of it at each evaluation.

  The cache holds no reference the GC traces: the templates belong to modules
  the engine retains, and the name lists and keys are Pascal strings. A cache
  belongs to the function template whose site it serves, and function
  templates never outlive their engine. }

interface

uses
  OrderedStringMap,
  UnicodeStringList,

  Goccia.Bytecode.Chunk,
  Goccia.SourcePipeline;

const
  { The most entries one direct eval site's code cache holds. }
  DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES = 64;

type
  { Everything besides the source text and the eval site record that decides
    what a direct eval compiles to:
    - the source name its positions and errors carry;
    - the language profile the source parses with;
    - the early errors of ES2026 §19.2.1.1 PerformEval step 11, which depend
      on the running function (inFunction, inMethod, inDerivedConstructor):
      whether new.target, super.x and super() are allowed. A class whose
      heritage evaluates to null or a constructor shares its constructor
      template, so super() can be allowed at one evaluation of a site and not
      at another. }
  TGocciaDirectEvalCodeContext = record
    SourceName: string;
    ParseOptions: TGocciaSourcePipelineOptions;
    AllowNewTarget: Boolean;
    AllowSuperProperty: Boolean;
    AllowSuperCall: Boolean;
  end;

  { Direct eval code compiled at one site, and the declaration names its
    EvalDeclarationInstantiation needs at every evaluation. A strict eval has
    no names: its declarations are locals of its template. The entry owns the
    name lists, not the template. }
  TGocciaDirectEvalCode = class
  private
    FContext: TGocciaDirectEvalCodeContext;
    FTemplate: TGocciaFunctionTemplate;
    FStrictEval: Boolean;
    FStandardVarNames: TUnicodeStringList;
    FVarNames: TUnicodeStringList;
    FFunctionNames: TUnicodeStringList;
    FDeclaredVarNames: TUnicodeStringList;
    // The next entry of the same source text, compiled in another context.
    FNext: TGocciaDirectEvalCode;
  public
    constructor Create(const AContext: TGocciaDirectEvalCodeContext;
      const ATemplate: TGocciaFunctionTemplate; const AStrictEval: Boolean;
      const AStandardVarNames, AVarNames, AFunctionNames,
      ADeclaredVarNames: TUnicodeStringList);
    destructor Destroy; override;

    property Template: TGocciaFunctionTemplate read FTemplate;
    property StrictEval: Boolean read FStrictEval;
    property StandardVarNames: TUnicodeStringList read FStandardVarNames;
    property VarNames: TUnicodeStringList read FVarNames;
    property FunctionNames: TUnicodeStringList read FFunctionNames;
    property DeclaredVarNames: TUnicodeStringList read FDeclaredVarNames;
  end;

  { The code cache of one direct eval site. }
  TGocciaDirectEvalCodeCache = class
  private
    FEntries: TOrderedStringMap<TGocciaDirectEvalCode>;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Find(const ASourceText: string;
      const AContext: TGocciaDirectEvalCodeContext): TGocciaDirectEvalCode;
    { Caches ACode, which must not be cached yet, and takes ownership of it,
      unless the cache is full. False when full: the caller keeps ACode. }
    function TryAdd(const ASourceText: string;
      const ACode: TGocciaDirectEvalCode): Boolean;

    { The entries cached, each context of a source counted. }
    property Count: Integer read FCount;
  end;

{ Whether code compiled for direct eval can serve a later evaluation of the
  same source at its site. Not when it contains a tagged template: ES2026
  §13.2.8.4 GetTemplateObject keys the template object on its Parse Node, and
  each eval parses its source into new Parse Nodes, so each evaluation needs
  a template object of its own, where the VM caches one per template
  instruction. }
function IsDirectEvalCodeCacheable(
  const ATemplate: TGocciaFunctionTemplate): Boolean;

implementation

function SameDirectEvalCodeContext(
  const A, B: TGocciaDirectEvalCodeContext): Boolean;
begin
  Result := (A.SourceName = B.SourceName) and
    (A.AllowNewTarget = B.AllowNewTarget) and
    (A.AllowSuperProperty = B.AllowSuperProperty) and
    (A.AllowSuperCall = B.AllowSuperCall) and
    (A.ParseOptions.Preprocessors = B.ParseOptions.Preprocessors) and
    (A.ParseOptions.Compatibility = B.ParseOptions.Compatibility) and
    (A.ParseOptions.LabelStatementsEnabled =
      B.ParseOptions.LabelStatementsEnabled) and
    (A.ParseOptions.ForInLoopsEnabled = B.ParseOptions.ForInLoopsEnabled) and
    (A.ParseOptions.ExperimentalJSModuleSourceEnabled =
      B.ParseOptions.ExperimentalJSModuleSourceEnabled) and
    (A.ParseOptions.WarningUnsupportedFeatures =
      B.ParseOptions.WarningUnsupportedFeatures);
end;

function IsDirectEvalCodeCacheable(
  const ATemplate: TGocciaFunctionTemplate): Boolean;
var
  I: Integer;
begin
  for I := 0 to ATemplate.ConstantCount - 1 do
    if ATemplate.GetConstantUnchecked(I)^.Kind = bckTemplateObject then
      Exit(False);
  for I := 0 to ATemplate.FunctionCount - 1 do
    if not IsDirectEvalCodeCacheable(ATemplate.GetFunction(I)) then
      Exit(False);
  Result := True;
end;

{ TGocciaDirectEvalCode }

constructor TGocciaDirectEvalCode.Create(
  const AContext: TGocciaDirectEvalCodeContext;
  const ATemplate: TGocciaFunctionTemplate; const AStrictEval: Boolean;
  const AStandardVarNames, AVarNames, AFunctionNames,
  ADeclaredVarNames: TUnicodeStringList);
begin
  inherited Create;
  FContext := AContext;
  FTemplate := ATemplate;
  FStrictEval := AStrictEval;
  FStandardVarNames := AStandardVarNames;
  FVarNames := AVarNames;
  FFunctionNames := AFunctionNames;
  FDeclaredVarNames := ADeclaredVarNames;
  FNext := nil;
end;

destructor TGocciaDirectEvalCode.Destroy;
begin
  FNext.Free;
  FDeclaredVarNames.Free;
  FFunctionNames.Free;
  FVarNames.Free;
  FStandardVarNames.Free;
  inherited;
end;

{ TGocciaDirectEvalCodeCache }

constructor TGocciaDirectEvalCodeCache.Create;
begin
  inherited Create;
  FEntries := TOrderedStringMap<TGocciaDirectEvalCode>.Create;
  FCount := 0;
end;

destructor TGocciaDirectEvalCodeCache.Destroy;
var
  Pair: TOrderedStringMap<TGocciaDirectEvalCode>.TKeyValuePair;
begin
  for Pair in FEntries do
    Pair.Value.Free;
  FEntries.Free;
  inherited;
end;

function TGocciaDirectEvalCodeCache.Find(const ASourceText: string;
  const AContext: TGocciaDirectEvalCodeContext): TGocciaDirectEvalCode;
begin
  if not FEntries.TryGetValue(ASourceText, Result) then
    Exit(nil);
  while Assigned(Result) and
        not SameDirectEvalCodeContext(Result.FContext, AContext) do
    Result := Result.FNext;
end;

function TGocciaDirectEvalCodeCache.TryAdd(const ASourceText: string;
  const ACode: TGocciaDirectEvalCode): Boolean;
var
  Head: TGocciaDirectEvalCode;
begin
  if FCount >= DIRECT_EVAL_CODE_CACHE_MAX_ENTRIES then
    Exit(False);
  if FEntries.TryGetValue(ASourceText, Head) then
    ACode.FNext := Head;
  FEntries.AddOrSetValue(ASourceText, ACode);
  Inc(FCount);
  Result := True;
end;

end.

unit Goccia.SourcePipeline;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  UnicodeStringList,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Constants,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.OriginMap,
  Goccia.SourceMap,
  Goccia.Token;

type
  TGocciaPreprocessor = (ppJSX);
  TGocciaPreprocessors = set of TGocciaPreprocessor;

  TGocciaCompatibility = (cfASI, cfVar, cfFunction, cfTraditionalFor,
    cfWhileLoops, cfLooseEquality, cfNonStrictMode, cfArgumentsObject,
    cfLabel, cfForIn, cfExperimentalJSModuleSource);
  TGocciaCompatibilityFlags = set of TGocciaCompatibility;

  { The goal symbol an entry's source is parsed and run with (ES2026 §16.1
    Script, §16.2 Module). stDetect leaves it to the source (ADR 0140): a
    top-level import or export declaration makes it a Module, and anything
    else a Script. A parse settles it; see
    TGocciaSourcePipelineResult.SourceType. }
  TGocciaSourceType = (stScript, stModule, stDetect);

  TGocciaSourcePipelineOptions = record
    Preprocessors: TGocciaPreprocessors;
    Compatibility: TGocciaCompatibilityFlags;
    LabelStatementsEnabled: Boolean;
    ForInLoopsEnabled: Boolean;
    ExperimentalJSModuleSourceEnabled: Boolean;
    WarningUnsupportedFeatures: Boolean;
    SourceType: TGocciaSourceType;
    InheritedStrictMode: Boolean;
    { Opt-in comment trivia. The parser has no use for comments; a tool that
      reads the parse result may. Off by default so an ordinary run does not
      pay for the array. }
    CollectComments: Boolean;
    { Opt-in copy of the committed token stream, lexed with the goals the
      parser chose. The provider package crawl reads literal import
      specifiers from it, and `goccia:ast` reports it. With the JSX
      preprocessor on, it also asks the transformer for the JSX tokens the
      lexer never sees. Off by default. }
    CollectTokens: Boolean;
  end;

  { One token of TGocciaSourcePipelineResult.Tokens. Offsets index the text
    the lexer read, which is the preprocessed text when a preprocessor ran. }
  TGocciaSourceToken = record
    TokenType: TGocciaTokenType;
    Lexeme: string;
    StartOffset: Integer;  // 0-based, inclusive
    EndOffset: Integer;    // 0-based, exclusive
    ContainsEscape: Boolean;
  end;

  TGocciaSourceTokenArray = array of TGocciaSourceToken;

  TGocciaFunctionBodyParseResult = record
    IsValid: Boolean;
    HasUseStrictDirective: Boolean;
  end;

  TGocciaSourcePipelineWarning = record
    Message: string;
    Suggestion: string;
    Line: Integer;
    Column: Integer;
    GeneratedLine: Integer;
    GeneratedColumn: Integer;
  end;

  TGocciaSourcePipelineResult = class
  private
    FProgramNode: TGocciaProgram;
    FSourceMap: TGocciaSourceMap;
    FOriginMap: TGocciaOriginMap;
    FGeneratedSourceLines: TStringList;
    FLexTimeNanoseconds: Int64;
    FParseTimeNanoseconds: Int64;
    FComments: TGocciaCommentSpanArray;
    FTokens: TGocciaSourceTokenArray;
    FJSXTokens: TGocciaJSXTokenArray;
    FSourceType: TGocciaSourceType;
    FWarnings: array of TGocciaSourcePipelineWarning;
    FWarningCount: Integer;

    procedure AddWarning(const AMessage, ASuggestion: string;
      const ALine, AColumn, AGeneratedLine, AGeneratedColumn: Integer);
    function GetWarning(const AIndex: Integer): TGocciaSourcePipelineWarning;
  public
    destructor Destroy; override;

    function TakeProgramNode: TGocciaProgram;
    function TakeSourceMap: TGocciaSourceMap;
    function TakeGeneratedSourceLines: TStringList;

    property ProgramNode: TGocciaProgram read FProgramNode;
    property SourceMap: TGocciaSourceMap read FSourceMap;
    { Which offsets of the generated source are which offsets of the original.
      Assigned exactly when a preprocessor rewrote the text; nil means the two
      are the same string. }
    property OriginMap: TGocciaOriginMap read FOriginMap;
    property GeneratedSourceLines: TStringList read FGeneratedSourceLines;
    { Empty unless the run asked for comments. Ordered by start offset. }
    property Comments: TGocciaCommentSpanArray read FComments;
    { Empty unless the run asked for tokens. In source order, EOF last. }
    property Tokens: TGocciaSourceTokenArray read FTokens;
    { Empty unless the run asked for tokens and the JSX preprocessor rewrote
      the text. In source order, and measured in the original text. }
    property JSXTokens: TGocciaJSXTokenArray read FJSXTokens;
    property LexTimeNanoseconds: Int64 read FLexTimeNanoseconds;
    property ParseTimeNanoseconds: Int64 read FParseTimeNanoseconds;
    { The goal the source was parsed with: the options' SourceType, or for
      stDetect the goal the parse settled on. Never stDetect. }
    property SourceType: TGocciaSourceType read FSourceType;
    property WarningCount: Integer read FWarningCount;
    property Warnings[const AIndex: Integer]: TGocciaSourcePipelineWarning
      read GetWarning;
  end;

  TGocciaSourcePipelineModuleResult = class
  private
    FProgramNode: TGocciaProgram;
    FWarnings: array of TGocciaSourcePipelineWarning;
    FWarningCount: Integer;

    procedure AddWarning(const AWarning: TGocciaSourcePipelineWarning);
    function GetWarning(const AIndex: Integer): TGocciaSourcePipelineWarning;
  public
    destructor Destroy; override;

    function TakeProgramNode: TGocciaProgram;

    property ProgramNode: TGocciaProgram read FProgramNode;
    property WarningCount: Integer read FWarningCount;
    property Warnings[const AIndex: Integer]: TGocciaSourcePipelineWarning
      read GetWarning;
  end;

  TGocciaSourcePipelineOptionsScope = class
  private
    FOptions: TGocciaSourcePipelineOptions;
    FPrevious: TGocciaSourcePipelineOptionsScope;
    FNext: TGocciaSourcePipelineOptionsScope;

    constructor Create(const AOptions: TGocciaSourcePipelineOptions);
  public
    destructor Destroy; override;
  end;

  TGocciaSourcePipeline = class
  public
    class function DefaultOptions: TGocciaSourcePipelineOptions; static;
    class function CurrentOptionsOrDefault: TGocciaSourcePipelineOptions; static;
    class function ActivateOptions(
      const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineOptionsScope; static;
    class function Parse(const ASource: TStringList; const AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions;
      const ADeclaredPrivateNames: TStrings = nil): TGocciaSourcePipelineResult; static;
    class function ParseModuleSource(const ASource: string;
      const AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineModuleResult; static;
    class function ParseFunctionParameters(const AParametersSource,
      AFileName: string; const AOptions: TGocciaSourcePipelineOptions;
      const AKind: TGocciaDynamicFunctionKind = dfkNormal): Boolean; static;
    class function ParseFunctionBodyWrapper(const ABodySource,
      AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions;
      const AKind: TGocciaDynamicFunctionKind = dfkNormal): TGocciaFunctionBodyParseResult; static;
    class function ParseDynamicFunctionWrapper(const AParametersSource,
      ABodySource, AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions;
      const AKind: TGocciaDynamicFunctionKind = dfkNormal): TGocciaProgram; static;
  end;

implementation

uses
  SysUtils,

  OrderedStringMap,
  TextSemantics,
  TimingUtils,

  Goccia.AST.BindingPatterns,
  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.Parser;

threadvar
  // Non-owning "active scope" pointer, save/restored around each pipeline run
  // (nil at rest), so it is not a thread-exit leak (object-reference threadvar
  // audit, #892).
  GActiveSourcePipelineOptionsScope: TGocciaSourcePipelineOptionsScope;

function SourceTextToLines(const ASource: string): TStringList;
begin
  Result := CreateFileTextLines(NormalizeNewlinesToLF(ASource));
end;

function CloneStringList(const ASource: TStrings): TStringList;
begin
  Result := TStringList.Create;
  if Assigned(ASource) then
    Result.Assign(ASource);
end;

function ApplyPreprocessors(const ASource, AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  out ASourceMap: TGocciaSourceMap;
  out AOriginMap: TGocciaOriginMap;
  out AJSXTokens: TGocciaJSXTokenArray): string;
var
  JSXResult: TGocciaJSXTransformResult;
begin
  Result := ASource;
  ASourceMap := nil;
  AOriginMap := nil;
  AJSXTokens := nil;

  // '.ts' sources never contain JSX, so '<' stays type syntax there: running
  // the transformer would rewrite generic annotations such as
  // `const g: <T>(x: T) => T` into a bogus JSX element before the parser sees
  // them. Every other extension keeps its current behaviour, including JSX in
  // '.js' (transformed, with the non-JSX-extension warning).
  if (ppJSX in AOptions.Preprocessors) and
     not IsJSXExcludedExtension(ExtractFileExt(AFileName)) then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(Result, AFileName,
      DEFAULT_JSX_FACTORY_NAME, DEFAULT_JSX_FRAGMENT_NAME,
      AOptions.CollectTokens);
    Result := JSXResult.Source;
    ASourceMap := JSXResult.SourceMap;
    AOriginMap := JSXResult.OriginMap;
    AJSXTokens := JSXResult.Tokens;
  end;
end;

procedure BuildParserOptionsForSourcePipeline(
  const AOptions: TGocciaSourcePipelineOptions;
  out AParserOptions: TGocciaParserOptions);
begin
  FillChar(AParserOptions, SizeOf(AParserOptions), 0);
  AParserOptions.AutomaticSemicolonInsertion := cfASI in AOptions.Compatibility;
  AParserOptions.VarDeclarationsEnabled := cfVar in AOptions.Compatibility;
  AParserOptions.FunctionDeclarationsEnabled := cfFunction in AOptions.Compatibility;
  AParserOptions.TraditionalForLoopsEnabled := cfTraditionalFor in AOptions.Compatibility;
  AParserOptions.WhileLoopsEnabled := cfWhileLoops in AOptions.Compatibility;
  AParserOptions.LooseEqualityEnabled := cfLooseEquality in AOptions.Compatibility;
  AParserOptions.NonStrictModeEnabled := (cfNonStrictMode in AOptions.Compatibility) and
    (AOptions.SourceType <> stModule);
  AParserOptions.InheritedStrictMode := AOptions.InheritedStrictMode;
  AParserOptions.ModuleSource := AOptions.SourceType = stModule;
  AParserOptions.ImportMetaAllowed := AOptions.SourceType = stModule;
  AParserOptions.LabelStatementsEnabled := AOptions.LabelStatementsEnabled or
    (cfLabel in AOptions.Compatibility);
  AParserOptions.ForInLoopsEnabled := AOptions.ForInLoopsEnabled or
    (cfForIn in AOptions.Compatibility);
  AParserOptions.WarningUnsupportedFeatures := AOptions.WarningUnsupportedFeatures;
end;

procedure ConfigureParser(const AParser: TGocciaParser;
  const AOptions: TGocciaSourcePipelineOptions);
var
  ParserOptions: TGocciaParserOptions;
begin
  BuildParserOptionsForSourcePipeline(AOptions, ParserOptions);
  AParser.ApplyOptions(ParserOptions);
end;

function HasSingleArrowFunctionProgram(const AProgramNode: TGocciaProgram;
  out AArrowFunction: TGocciaArrowFunctionExpression): Boolean;
var
  ExpressionStatement: TGocciaExpressionStatement;
begin
  AArrowFunction := nil;
  Result := Assigned(AProgramNode) and (AProgramNode.Body.Count = 1) and
    (AProgramNode.Body[0] is TGocciaExpressionStatement);
  if not Result then
    Exit;

  ExpressionStatement := TGocciaExpressionStatement(AProgramNode.Body[0]);
  Result := ExpressionStatement.Expression is TGocciaArrowFunctionExpression;
  if Result then
    AArrowFunction := TGocciaArrowFunctionExpression(
      ExpressionStatement.Expression);
end;

function DynamicFunctionWrapperSource(const AParametersSource,
  ABodySource: string; const AKind: TGocciaDynamicFunctionKind): string;
var
  Prefix: string;
begin
  case AKind of
    dfkGenerator:
      Prefix := 'function*';
    dfkAsync:
      Prefix := 'async function';
    dfkAsyncGenerator:
      Prefix := 'async function*';
  else
    Prefix := 'function';
  end;
  Result := '(' + Prefix + ' anonymous(' + AParametersSource + #10 + ') {' +
    #10 + ABodySource + #10 + '})';
end;

type
  { The names a module's top level declares, each once, in the order the
    module first declares it, so an early error names the same binding the
    source order would. A hash map rather than a list: each check is one
    lookup, so the module's early errors cost time linear in its
    declarations instead of quadratic. }
  TModuleDeclaredNames = TOrderedStringMap<Boolean>;

  { Where an import declaration binds a name, for the early error that
    reports a later conflict at that declaration. }
  TModuleImportBindingSite = record
    Line: Integer;
    Column: Integer;
  end;

  TModuleImportBindingSites = TOrderedStringMap<TModuleImportBindingSite>;

procedure AddNameOnce(const ANames: TModuleDeclaredNames;
  const AName: string);
begin
  if AName <> '' then
    ANames.Add(AName, True);
end;

procedure CollectTopLevelModuleDeclarationNames(const AStmt: TGocciaStatement;
  const AVarNames, ALexicalNames: TModuleDeclaredNames);
var
  I, J: Integer;
  Names: TUnicodeStringList;
  VarDecl: TGocciaVariableDeclaration;
begin
  if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
    begin
      Names := TUnicodeStringList.Create;
      try
        CollectVariableInfoBindingNames(VarDecl.Variables[I], Names, True);
        for J := 0 to Names.Count - 1 do
          if VarDecl.IsVar then
            AddNameOnce(AVarNames, Names[J])
          else
            AddNameOnce(ALexicalNames, Names[J]);
      finally
        Names.Free;
      end;
    end;
    Exit;
  end;

  if AStmt is TGocciaExportVariableDeclaration then
  begin
    CollectTopLevelModuleDeclarationNames(
      TGocciaExportVariableDeclaration(AStmt).Declaration, AVarNames,
      ALexicalNames);
    Exit;
  end;

  if AStmt is TGocciaFunctionDeclaration then
    AddNameOnce(ALexicalNames, TGocciaFunctionDeclaration(AStmt).Name)
  else if AStmt is TGocciaExportFunctionDeclaration then
    AddNameOnce(ALexicalNames,
      TGocciaExportFunctionDeclaration(AStmt).Declaration.Name)
  else if AStmt is TGocciaClassDeclaration then
    AddNameOnce(ALexicalNames,
      TGocciaClassDeclaration(AStmt).ClassDefinition.Name)
  else if AStmt is TGocciaExportClassDeclaration then
    AddNameOnce(ALexicalNames,
      TGocciaExportClassDeclaration(AStmt).Declaration.ClassDefinition.Name)
  else if AStmt is TGocciaEnumDeclaration then
    AddNameOnce(ALexicalNames, TGocciaEnumDeclaration(AStmt).Name)
  else if AStmt is TGocciaExportEnumDeclaration then
    AddNameOnce(ALexicalNames,
      TGocciaExportEnumDeclaration(AStmt).Declaration.Name)
  else if AStmt is TGocciaExportDefaultDeclaration then
    if TGocciaExportDefaultDeclaration(AStmt).LocalName <>
       GOCCIA_DEFAULT_EXPORT_BINDING then
      AddNameOnce(ALexicalNames,
        TGocciaExportDefaultDeclaration(AStmt).LocalName);
end;

procedure RaiseModuleRedeclaration(const AName, AFileName: string;
  const ASourceLines: TStringList; const ALine: Integer = 0;
  const AColumn: Integer = 0);
begin
  raise TGocciaSyntaxError.Create(
    Format('Identifier "%s" has already been declared', [AName]),
    ALine, AColumn, AFileName, ASourceLines);
end;

{ ES2026 §16.2.1.1 Static Semantics: Early Errors, Module: the
  LexicallyDeclaredNames of a module, which include the BoundNames of its
  import declarations (§8.2.4), have no duplicate, and none of them is also
  a VarDeclaredName. }
procedure ValidateModuleEarlyErrors(const AProgram: TGocciaProgram;
  const AFileName: string; const ASourceLines: TStringList);
var
  I: Integer;
  ImportDecl: TGocciaImportDeclaration;
  ImportedNames: TModuleImportBindingSites;
  ImportedPair: TModuleImportBindingSites.TKeyValuePair;
  ImportPair: TStringStringMap.TKeyValuePair;
  LexicalNames: TModuleDeclaredNames;
  Stmt: TGocciaStatement;
  VarPair: TModuleDeclaredNames.TKeyValuePair;
  VarNames: TModuleDeclaredNames;

  procedure AddImportedName(const AName: string);
  var
    Site: TModuleImportBindingSite;
  begin
    if AName = '' then
      Exit;
    if ImportedNames.ContainsKey(AName) then
      RaiseModuleRedeclaration(AName, AFileName, ASourceLines,
        ImportDecl.Line, ImportDecl.Column);
    Site.Line := ImportDecl.Line;
    Site.Column := ImportDecl.Column;
    ImportedNames.Add(AName, Site);
  end;

begin
  if not Assigned(AProgram) then
    Exit;

  VarNames := TModuleDeclaredNames.Create;
  LexicalNames := TModuleDeclaredNames.Create;
  ImportedNames := TModuleImportBindingSites.Create;
  try
    for I := 0 to AProgram.Body.Count - 1 do
    begin
      Stmt := AProgram.Body[I];
      if Stmt is TGocciaImportDeclaration then
      begin
        ImportDecl := TGocciaImportDeclaration(Stmt);
        if ImportDecl.DuplicateBinding <> '' then
          RaiseModuleRedeclaration(ImportDecl.DuplicateBinding, AFileName,
            ASourceLines, ImportDecl.Line, ImportDecl.Column);
        AddImportedName(ImportDecl.NamespaceName);
        for ImportPair in ImportDecl.Imports do
          AddImportedName(ImportPair.Key);
      end
      else
        CollectTopLevelModuleDeclarationNames(Stmt, VarNames, LexicalNames);
    end;

    for VarPair in VarNames do
      if LexicalNames.ContainsKey(VarPair.Key) then
        RaiseModuleRedeclaration(VarPair.Key, AFileName, ASourceLines);
    for ImportedPair in ImportedNames do
      if LexicalNames.ContainsKey(ImportedPair.Key) or
         VarNames.ContainsKey(ImportedPair.Key) then
        RaiseModuleRedeclaration(ImportedPair.Key, AFileName, ASourceLines,
          ImportedPair.Value.Line, ImportedPair.Value.Column);
  finally
    ImportedNames.Free;
    LexicalNames.Free;
    VarNames.Free;
  end;
end;

{ TGocciaSourcePipelineOptionsScope }

constructor TGocciaSourcePipelineOptionsScope.Create(
  const AOptions: TGocciaSourcePipelineOptions);
begin
  inherited Create;
  FOptions := AOptions;
  FPrevious := GActiveSourcePipelineOptionsScope;
  if Assigned(FPrevious) then
    FPrevious.FNext := Self;
  GActiveSourcePipelineOptionsScope := Self;
end;

destructor TGocciaSourcePipelineOptionsScope.Destroy;
begin
  if GActiveSourcePipelineOptionsScope = Self then
    GActiveSourcePipelineOptionsScope := FPrevious;
  if Assigned(FPrevious) then
    FPrevious.FNext := FNext;
  if Assigned(FNext) then
    FNext.FPrevious := FPrevious;
  inherited Destroy;
end;

{ TGocciaSourcePipelineResult }

destructor TGocciaSourcePipelineResult.Destroy;
begin
  FProgramNode.Free;
  FSourceMap.Free;
  FOriginMap.Free;
  FGeneratedSourceLines.Free;
  inherited;
end;

procedure TGocciaSourcePipelineResult.AddWarning(const AMessage,
  ASuggestion: string; const ALine, AColumn, AGeneratedLine,
  AGeneratedColumn: Integer);
begin
  Inc(FWarningCount);
  SetLength(FWarnings, FWarningCount);
  FWarnings[FWarningCount - 1].Message := AMessage;
  FWarnings[FWarningCount - 1].Suggestion := ASuggestion;
  FWarnings[FWarningCount - 1].Line := ALine;
  FWarnings[FWarningCount - 1].Column := AColumn;
  FWarnings[FWarningCount - 1].GeneratedLine := AGeneratedLine;
  FWarnings[FWarningCount - 1].GeneratedColumn := AGeneratedColumn;
end;

function TGocciaSourcePipelineResult.GetWarning(
  const AIndex: Integer): TGocciaSourcePipelineWarning;
begin
  if (AIndex < 0) or (AIndex >= FWarningCount) then
    raise ERangeError.CreateFmt('Source pipeline warning index out of range: %d',
      [AIndex]);
  Result := FWarnings[AIndex];
end;

function TGocciaSourcePipelineResult.TakeProgramNode: TGocciaProgram;
begin
  Result := FProgramNode;
  FProgramNode := nil;
end;

function TGocciaSourcePipelineResult.TakeSourceMap: TGocciaSourceMap;
begin
  Result := FSourceMap;
  FSourceMap := nil;
end;

function TGocciaSourcePipelineResult.TakeGeneratedSourceLines: TStringList;
begin
  Result := FGeneratedSourceLines;
  FGeneratedSourceLines := nil;
end;

{ TGocciaSourcePipelineModuleResult }

destructor TGocciaSourcePipelineModuleResult.Destroy;
begin
  FProgramNode.Free;
  inherited;
end;

procedure TGocciaSourcePipelineModuleResult.AddWarning(
  const AWarning: TGocciaSourcePipelineWarning);
begin
  Inc(FWarningCount);
  SetLength(FWarnings, FWarningCount);
  FWarnings[FWarningCount - 1] := AWarning;
end;

function TGocciaSourcePipelineModuleResult.GetWarning(
  const AIndex: Integer): TGocciaSourcePipelineWarning;
begin
  if (AIndex < 0) or (AIndex >= FWarningCount) then
    raise ERangeError.CreateFmt('Source pipeline module warning index out of range: %d',
      [AIndex]);
  Result := FWarnings[AIndex];
end;

function TGocciaSourcePipelineModuleResult.TakeProgramNode: TGocciaProgram;
begin
  Result := FProgramNode;
  FProgramNode := nil;
end;

{ TGocciaSourcePipeline }

class function TGocciaSourcePipeline.DefaultOptions: TGocciaSourcePipelineOptions;
begin
  Result.Preprocessors := [];
  Result.Compatibility := [];
  Result.LabelStatementsEnabled := False;
  Result.ForInLoopsEnabled := False;
  Result.ExperimentalJSModuleSourceEnabled := False;
  Result.WarningUnsupportedFeatures := False;
  Result.SourceType := stScript;
  Result.InheritedStrictMode := False;
  Result.CollectComments := False;
  Result.CollectTokens := False;
end;

class function TGocciaSourcePipeline.CurrentOptionsOrDefault: TGocciaSourcePipelineOptions;
begin
  if Assigned(GActiveSourcePipelineOptionsScope) then
    Result := GActiveSourcePipelineOptionsScope.FOptions
  else
    Result := DefaultOptions;
end;

class function TGocciaSourcePipeline.ActivateOptions(
  const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineOptionsScope;
begin
  Result := TGocciaSourcePipelineOptionsScope.Create(AOptions);
end;

{ One parse of ASource with the goal AOptions names. For stDetect the parser
  settles the goal (ADR 0140), and the result records it. AParseAsModule
  reports a detected Module whose parse is not usable as one:
  --compat-non-strict-mode parsed it as sloppy Script code, and a Module is
  strict throughout. }
function ParseSourceWithGoal(const ASource: TStringList;
  const AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  const ADeclaredPrivateNames: TStrings;
  out AParseAsModule: Boolean): TGocciaSourcePipelineResult;
var
  SourceText, OriginalSourceText: string;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ParserOptions: TGocciaParserOptions;
  ParserWarning: TGocciaParserWarning;
  PreprocessorSourceMap: TGocciaSourceMap;
  PreprocessorOriginMap: TGocciaOriginMap;
  PreprocessorJSXTokens: TGocciaJSXTokenArray;
  StartTime, ParseStart, ParseEnd, ParsePhaseTime: Int64;
  OrigLine, OrigCol, I: Integer;
begin
  AParseAsModule := False;
  Result := TGocciaSourcePipelineResult.Create;
  try
    StartTime := GetNanoseconds;
    if Assigned(ASource) then
      SourceText := StringListToSourceText(ASource)
    else
      SourceText := '';
    OriginalSourceText := SourceText;

    try
      SourceText := ApplyPreprocessors(SourceText, AFileName, AOptions,
        PreprocessorSourceMap, PreprocessorOriginMap, PreprocessorJSXTokens);
    except
      on E: TGocciaError do
      begin
        // A preprocessor error is already positioned in the original source —
        // no source map exists yet — so only the source context is missing for
        // the caret display the parser errors below get for free.
        E.TranslatePosition(E.Line, E.Column, ASource);
        raise;
      end;
    end;
    Result.FSourceMap := PreprocessorSourceMap;
    Result.FOriginMap := PreprocessorOriginMap;
    Result.FJSXTokens := PreprocessorJSXTokens;
    if Assigned(Result.FSourceMap) then
    begin
      Result.FSourceMap.SetSourceContent(0, OriginalSourceText);
      if not IsJSXNativeExtension(ExtractFileExt(AFileName)) then
        Result.AddWarning(
          'JSX syntax found in source with a non-JSX extension',
          'Use a .jsx or .tsx extension for JSX source',
          1, 1, 1, 1);
    end;

    try
      Lexer := TGocciaLexer.Create(SourceText, AFileName);
      Lexer.CollectComments := AOptions.CollectComments;
      try
        Parser := TGocciaParser.CreateFromLexer(Lexer, AFileName,
          Lexer.SourceLines);
        BuildParserOptionsForSourcePipeline(AOptions, ParserOptions);
        ParserOptions.DetectModuleGoal := AOptions.SourceType = stDetect;
        Parser.ApplyOptions(ParserOptions);
        try
          ParseStart := GetNanoseconds;
          if Assigned(ADeclaredPrivateNames) then
            Result.FProgramNode :=
              Parser.ParseWithPrivateNames(ADeclaredPrivateNames)
          else
            Result.FProgramNode := Parser.Parse;
          Result.FSourceType := AOptions.SourceType;
          if AOptions.SourceType = stDetect then
          begin
            if Parser.ModuleSyntaxDetected then
              Result.FSourceType := stModule
            else
              Result.FSourceType := stScript;
            AParseAsModule := (Result.FSourceType = stModule) and
              ParserOptions.NonStrictModeEnabled;
          end;
          if (Result.FSourceType = stModule) and not AParseAsModule then
            ValidateModuleEarlyErrors(Result.FProgramNode, AFileName,
              Lexer.SourceLines);
          ParseEnd := GetNanoseconds;
          Result.FLexTimeNanoseconds :=
            (ParseStart - StartTime) + Lexer.ScanTimeNanoseconds;
          ParsePhaseTime := ParseEnd - ParseStart;
          if ParsePhaseTime > Lexer.ScanTimeNanoseconds then
            Result.FParseTimeNanoseconds :=
              ParsePhaseTime - Lexer.ScanTimeNanoseconds
          else
            Result.FParseTimeNanoseconds := 0;
          Result.FGeneratedSourceLines := CloneStringList(Lexer.SourceLines);
          if AOptions.CollectComments then
            Result.FComments := Lexer.TakeComments;
          if AOptions.CollectTokens then
          begin
            SetLength(Result.FTokens, Lexer.Tokens.Count);
            for I := 0 to Lexer.Tokens.Count - 1 do
            begin
              Result.FTokens[I].TokenType := Lexer.Tokens[I].TokenType;
              Result.FTokens[I].Lexeme := Lexer.Tokens[I].Lexeme;
              Result.FTokens[I].StartOffset :=
                Lexer.Tokens[I].Span.StartOffset;
              Result.FTokens[I].EndOffset := Lexer.Tokens[I].Span.EndOffset;
              Result.FTokens[I].ContainsEscape :=
                Lexer.Tokens[I].ContainsEscape;
            end;
          end;

          for I := 0 to Parser.WarningCount - 1 do
          begin
            ParserWarning := Parser.GetWarning(I);
            OrigLine := ParserWarning.Line;
            OrigCol := ParserWarning.Column;
            if Assigned(Result.FSourceMap) then
              Result.FSourceMap.Translate(ParserWarning.Line,
                ParserWarning.Column, OrigLine, OrigCol);
            Result.AddWarning(ParserWarning.Message, ParserWarning.Suggestion,
              OrigLine, OrigCol, ParserWarning.Line, ParserWarning.Column);
          end;
        finally
          Parser.Free;
        end;
      finally
        Lexer.Free;
      end;
    except
      on E: TGocciaError do
      begin
        if Assigned(Result.FSourceMap) and
           Result.FSourceMap.Translate(E.Line, E.Column, OrigLine, OrigCol) then
          E.TranslatePosition(OrigLine, OrigCol, ASource);
        raise;
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

class function TGocciaSourcePipeline.Parse(const ASource: TStringList;
  const AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  const ADeclaredPrivateNames: TStrings): TGocciaSourcePipelineResult;
var
  ModuleOptions: TGocciaSourcePipelineOptions;
  ParseAsModule: Boolean;
  DetectLexTime, DetectParseTime: Int64;
begin
  Result := ParseSourceWithGoal(ASource, AFileName, AOptions,
    ADeclaredPrivateNames, ParseAsModule);
  if not ParseAsModule then
    Exit;
  { Only a detected Module under --compat-non-strict-mode parses twice: the
    first parse was sloppy, and sloppy code is not a strict Module's. The
    reported lex and parse times cover both passes. }
  DetectLexTime := Result.FLexTimeNanoseconds;
  DetectParseTime := Result.FParseTimeNanoseconds;
  Result.Free;
  ModuleOptions := AOptions;
  ModuleOptions.SourceType := stModule;
  Result := ParseSourceWithGoal(ASource, AFileName, ModuleOptions,
    ADeclaredPrivateNames, ParseAsModule);
  Inc(Result.FLexTimeNanoseconds, DetectLexTime);
  Inc(Result.FParseTimeNanoseconds, DetectParseTime);
end;

class function TGocciaSourcePipeline.ParseModuleSource(const ASource: string;
  const AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineModuleResult;
var
  ModuleOptions: TGocciaSourcePipelineOptions;
  PipelineResult: TGocciaSourcePipelineResult;
  SourceLines: TStringList;
  I: Integer;
begin
  ModuleOptions := AOptions;
  ModuleOptions.SourceType := stModule;
  SourceLines := SourceTextToLines(ASource);
  try
    PipelineResult := Parse(SourceLines, AFileName, ModuleOptions);
    try
      Result := TGocciaSourcePipelineModuleResult.Create;
      try
        Result.FProgramNode := PipelineResult.TakeProgramNode;
        for I := 0 to PipelineResult.WarningCount - 1 do
          Result.AddWarning(PipelineResult.Warnings[I]);
      except
        Result.Free;
        raise;
      end;
    finally
      PipelineResult.Free;
    end;
  finally
    SourceLines.Free;
  end;
end;

class function TGocciaSourcePipeline.ParseFunctionParameters(
  const AParametersSource, AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  const AKind: TGocciaDynamicFunctionKind): Boolean;
var
  PipelineResult: TGocciaSourcePipelineResult;
  Source: TStringList;
begin
  Source := CreateECMAScriptSourceLines(
    DynamicFunctionWrapperSource(AParametersSource, '', AKind));
  try
    PipelineResult := Parse(Source, AFileName, AOptions);
    try
      Result := Assigned(PipelineResult.ProgramNode) and
        (PipelineResult.ProgramNode.Body.Count = 1) and
        (PipelineResult.ProgramNode.Body[0] is TGocciaExpressionStatement) and
        (TGocciaExpressionStatement(
          PipelineResult.ProgramNode.Body[0]).Expression is
          TGocciaFunctionExpression);
    finally
      PipelineResult.Free;
    end;
  finally
    Source.Free;
  end;
end;

class function TGocciaSourcePipeline.ParseFunctionBodyWrapper(
  const ABodySource, AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  const AKind: TGocciaDynamicFunctionKind): TGocciaFunctionBodyParseResult;
var
  ExpressionStatement: TGocciaExpressionStatement;
  FunctionExpression: TGocciaFunctionExpression;
  PipelineResult: TGocciaSourcePipelineResult;
  Source: TStringList;
begin
  Result.IsValid := False;
  Result.HasUseStrictDirective := False;
  Source := CreateECMAScriptSourceLines(
    DynamicFunctionWrapperSource('', ABodySource, AKind));
  try
    PipelineResult := Parse(Source, AFileName, AOptions);
    try
      FunctionExpression := nil;
      if Assigned(PipelineResult.ProgramNode) and
         (PipelineResult.ProgramNode.Body.Count = 1) and
         (PipelineResult.ProgramNode.Body[0] is TGocciaExpressionStatement) then
      begin
        ExpressionStatement := TGocciaExpressionStatement(
          PipelineResult.ProgramNode.Body[0]);
        if ExpressionStatement.Expression is TGocciaFunctionExpression then
          FunctionExpression := TGocciaFunctionExpression(
            ExpressionStatement.Expression);
      end;
      Result.IsValid := Assigned(FunctionExpression);
      if Result.IsValid then
        Result.HasUseStrictDirective := HasUseStrictDirective(
          FunctionExpression.Body);
    finally
      PipelineResult.Free;
    end;
  finally
    Source.Free;
  end;
end;

class function TGocciaSourcePipeline.ParseDynamicFunctionWrapper(
  const AParametersSource, ABodySource, AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  const AKind: TGocciaDynamicFunctionKind): TGocciaProgram;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  Source: string;
  OriginalSourceLines: TStringList;
  SourceMap: TGocciaSourceMap;
  OriginMap: TGocciaOriginMap;
  JSXTokens: TGocciaJSXTokenArray;
  OrigLine, OrigCol: Integer;
begin
  Source := DynamicFunctionWrapperSource(AParametersSource, ABodySource, AKind);

  OriginalSourceLines := CreateECMAScriptSourceLines(Source);
  try
    try
      Source := ApplyPreprocessors(Source, AFileName, AOptions, SourceMap,
        OriginMap, JSXTokens);
    except
      on E: TGocciaError do
      begin
        // A preprocessor error is positioned in the wrapper source, which is
        // exactly what OriginalSourceLines holds — attach it so the caret
        // display matches the coordinates the error already carries.
        E.TranslatePosition(E.Line, E.Column, OriginalSourceLines);
        raise;
      end;
    end;
    try
      Lexer := TGocciaLexer.Create(Source, AFileName);
      try
        Parser := TGocciaParser.CreateFromLexer(Lexer, AFileName,
          Lexer.SourceLines);
        ConfigureParser(Parser, AOptions);
        try
          try
            Result := Parser.ParseUnchecked;
          except
            on E: TGocciaError do
            begin
              if Assigned(SourceMap) and
                 SourceMap.Translate(E.Line, E.Column, OrigLine, OrigCol) then
                E.TranslatePosition(OrigLine, OrigCol, OriginalSourceLines);
              raise;
            end;
          end;
        finally
          Parser.Free;
        end;
      finally
        Lexer.Free;
      end;
    finally
      SourceMap.Free;
      OriginMap.Free;
    end;
  finally
    OriginalSourceLines.Free;
  end;
end;

end.

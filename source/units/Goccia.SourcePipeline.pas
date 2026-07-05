unit Goccia.SourcePipeline;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Constants,
  Goccia.SourceMap;

type
  TGocciaPreprocessor = (ppJSX);
  TGocciaPreprocessors = set of TGocciaPreprocessor;

  TGocciaCompatibility = (cfASI, cfVar, cfFunction, cfTraditionalFor,
    cfWhileLoops, cfLooseEquality, cfNonStrictMode, cfArgumentsObject,
    cfLabel, cfForIn, cfExperimentalJSModuleSource);
  TGocciaCompatibilityFlags = set of TGocciaCompatibility;

  TGocciaSourceType = (stScript, stModule);

  TGocciaSourcePipelineOptions = record
    Preprocessors: TGocciaPreprocessors;
    Compatibility: TGocciaCompatibilityFlags;
    LabelStatementsEnabled: Boolean;
    ForInLoopsEnabled: Boolean;
    ExperimentalJSModuleSourceEnabled: Boolean;
    WarningUnsupportedFeatures: Boolean;
    SourceType: TGocciaSourceType;
    InheritedStrictMode: Boolean;
  end;

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
    FGeneratedSourceLines: TStringList;
    FLexTimeNanoseconds: Int64;
    FParseTimeNanoseconds: Int64;
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
    property GeneratedSourceLines: TStringList read FGeneratedSourceLines;
    property LexTimeNanoseconds: Int64 read FLexTimeNanoseconds;
    property ParseTimeNanoseconds: Int64 read FParseTimeNanoseconds;
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
    class function ParseModuleSource(const ASource: UTF8String;
      const AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineModuleResult; static;
    class function ParseExpression(const AExpressionText,
      AFileName: string; const AOptions: TGocciaSourcePipelineOptions;
      const ADeclaredPrivateNames: TStrings = nil): TGocciaExpression; static;
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

  TextSemantics,
  TimingUtils,

  Goccia.AST.Statements,
  Goccia.Error,
  Goccia.FileExtensions,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.Parser,
  Goccia.Token;

threadvar
  // Non-owning "active scope" pointer, save/restored around each pipeline run
  // (nil at rest), so it is not a thread-exit leak (object-reference threadvar
  // audit, #892).
  GActiveSourcePipelineOptionsScope: TGocciaSourcePipelineOptionsScope;

function SourceTextToLines(const ASource: UTF8String): TStringList;
begin
  Result := CreateUTF8FileTextLines(NormalizeUTF8NewlinesToLF(ASource));
end;

function CloneStringList(const ASource: TStrings): TStringList;
begin
  Result := TStringList.Create;
  if Assigned(ASource) then
    Result.Assign(ASource);
end;

function ApplyPreprocessors(const ASource: string;
  const AOptions: TGocciaSourcePipelineOptions;
  out ASourceMap: TGocciaSourceMap): string;
var
  JSXResult: TGocciaJSXTransformResult;
begin
  Result := ASource;
  ASourceMap := nil;

  if ppJSX in AOptions.Preprocessors then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(Result);
    Result := JSXResult.Source;
    ASourceMap := JSXResult.SourceMap;
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
  Result := '(' + Prefix + ' anonymous(' + AParametersSource + ') {' + #10 +
    ABodySource + #10 + '})';
end;

procedure AddNameOnce(const ANames: TStrings; const AName: string);
begin
  if (AName <> '') and (ANames.IndexOf(AName) < 0) then
    ANames.Add(AName);
end;

procedure CollectTopLevelModuleDeclarationNames(const AStmt: TGocciaStatement;
  const AVarNames, ALexicalNames: TStrings);
var
  I: Integer;
  VarDecl: TGocciaVariableDeclaration;
begin
  if AStmt is TGocciaVariableDeclaration then
  begin
    VarDecl := TGocciaVariableDeclaration(AStmt);
    for I := 0 to High(VarDecl.Variables) do
      if VarDecl.IsVar then
        AddNameOnce(AVarNames, VarDecl.Variables[I].Name)
      else
        AddNameOnce(ALexicalNames, VarDecl.Variables[I].Name);
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

procedure ValidateModuleEarlyErrors(const AProgram: TGocciaProgram;
  const AFileName: string; const ASourceLines: TStringList);
var
  I: Integer;
  LexicalNames: TStringList;
  Stmt: TGocciaStatement;
  VarName: string;
  VarNames: TStringList;
begin
  if not Assigned(AProgram) then
    Exit;

  VarNames := TStringList.Create;
  LexicalNames := TStringList.Create;
  try
    VarNames.CaseSensitive := True;
    LexicalNames.CaseSensitive := True;
    for I := 0 to AProgram.Body.Count - 1 do
    begin
      Stmt := AProgram.Body[I];
      CollectTopLevelModuleDeclarationNames(Stmt, VarNames, LexicalNames);
    end;

    for VarName in VarNames do
      if LexicalNames.IndexOf(VarName) >= 0 then
        raise TGocciaSyntaxError.Create(
          Format('Identifier "%s" has already been declared', [VarName]),
          0, 0, AFileName, ASourceLines);
  finally
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

class function TGocciaSourcePipeline.Parse(const ASource: TStringList;
  const AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions;
  const ADeclaredPrivateNames: TStrings): TGocciaSourcePipelineResult;
var
  SourceText, OriginalSourceText: string;
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ParserWarning: TGocciaParserWarning;
  PreprocessorSourceMap: TGocciaSourceMap;
  StartTime, ParseStart, ParseEnd, ParsePhaseTime: Int64;
  OrigLine, OrigCol, I: Integer;
begin
  Result := TGocciaSourcePipelineResult.Create;
  try
    StartTime := GetNanoseconds;
    if Assigned(ASource) then
      SourceText := StringListToLFText(ASource)
    else
      SourceText := '';
    OriginalSourceText := SourceText;

    SourceText := ApplyPreprocessors(SourceText, AOptions, PreprocessorSourceMap);
    Result.FSourceMap := PreprocessorSourceMap;
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
      try
        Parser := TGocciaParser.CreateFromLexer(Lexer, AFileName,
          Lexer.SourceLines);
        ConfigureParser(Parser, AOptions);
        try
          ParseStart := GetNanoseconds;
          if Assigned(ADeclaredPrivateNames) then
            Result.FProgramNode :=
              Parser.ParseWithPrivateNames(ADeclaredPrivateNames)
          else
            Result.FProgramNode := Parser.Parse;
          if AOptions.SourceType = stModule then
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

          for I := 0 to Parser.WarningCount - 1 do
          begin
            ParserWarning := Parser.GetWarning(I);
            if (AOptions.SourceType = stModule) and
               (ParserWarning.Message =
                 '''with'' statements require --compat-non-strict-mode') then
              raise TGocciaSyntaxError.Create(
                '''with'' statements are not allowed in strict mode',
                ParserWarning.Line, ParserWarning.Column, AFileName,
                Lexer.SourceLines);

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

class function TGocciaSourcePipeline.ParseModuleSource(const ASource: UTF8String;
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

class function TGocciaSourcePipeline.ParseExpression(const AExpressionText,
  AFileName: string; const AOptions: TGocciaSourcePipelineOptions;
  const ADeclaredPrivateNames: TStrings): TGocciaExpression;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  SourceLines: TStringList;
begin
  Result := nil;
  if AExpressionText = '' then
    Exit;

  SourceLines := CreateECMAScriptSourceLines(AExpressionText);
  try
    Lexer := TGocciaLexer.Create(AExpressionText, AFileName);
    try
      Parser := TGocciaParser.CreateFromLexer(Lexer, AFileName, SourceLines);
      ConfigureParser(Parser, AOptions);
      try
        if Assigned(ADeclaredPrivateNames) then
          Result := Parser.ParseExpressionWithPrivateNames(ADeclaredPrivateNames)
        else
          Result := Parser.Expression;
        if not Parser.AtEnd then
        begin
          Result.Free;
          Result := nil;
        end;
      finally
        Parser.Free;
      end;
    finally
      Lexer.Free;
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
  Source := TStringList.Create;
  try
    Source.Text := DynamicFunctionWrapperSource(AParametersSource, '', AKind);
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
  Source := TStringList.Create;
  try
    Source.Text := DynamicFunctionWrapperSource('', ABodySource, AKind);
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
  OrigLine, OrigCol: Integer;
begin
  Source := DynamicFunctionWrapperSource(AParametersSource, ABodySource, AKind);

  OriginalSourceLines := CreateECMAScriptSourceLines(Source);
  try
    Source := ApplyPreprocessors(Source, AOptions, SourceMap);
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
    end;
  finally
    OriginalSourceLines.Free;
  end;
end;

end.

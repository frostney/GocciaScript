unit Goccia.SourcePipeline;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.SourceMap;

type
  TGocciaPreprocessor = (ppJSX);
  TGocciaPreprocessors = set of TGocciaPreprocessor;

  TGocciaCompatibility = (cfASI, cfVar, cfFunction, cfTraditionalFor,
    cfWhileLoops, cfLooseEquality, cfNonStrictMode);
  TGocciaCompatibilityFlags = set of TGocciaCompatibility;

  TGocciaSourceType = (stScript, stModule);

  TGocciaSourcePipelineOptions = record
    Preprocessors: TGocciaPreprocessors;
    Compatibility: TGocciaCompatibilityFlags;
    SourceType: TGocciaSourceType;
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

  TGocciaSourcePipeline = class
  public
    class function Parse(const ASource: TStringList; const AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineResult; static;
    class function ParseModuleSource(const ASource: UTF8String;
      const AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineModuleResult; static;
    class function ParseExpression(const AExpressionText,
      AFileName: string; const AOptions: TGocciaSourcePipelineOptions;
      const ADeclaredPrivateNames: TStrings = nil): TGocciaExpression; static;
    class function ParseFunctionParameters(const AParametersSource,
      AFileName: string; const AOptions: TGocciaSourcePipelineOptions): Boolean; static;
    class function ParseFunctionBodyWrapper(const ABodySource,
      AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions): TGocciaFunctionBodyParseResult; static;
    class function ParseDynamicFunctionWrapper(const AParametersSource,
      ABodySource, AFileName: string;
      const AOptions: TGocciaSourcePipelineOptions): TGocciaProgram; static;
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

function ParserOptionsForSourcePipeline(
  const AOptions: TGocciaSourcePipelineOptions): TGocciaParserOptions;
begin
  Result.AutomaticSemicolonInsertion := cfASI in AOptions.Compatibility;
  Result.VarDeclarationsEnabled := cfVar in AOptions.Compatibility;
  Result.FunctionDeclarationsEnabled := cfFunction in AOptions.Compatibility;
  Result.TraditionalForLoopsEnabled := cfTraditionalFor in AOptions.Compatibility;
  Result.WhileLoopsEnabled := cfWhileLoops in AOptions.Compatibility;
  Result.LooseEqualityEnabled := cfLooseEquality in AOptions.Compatibility;
  Result.NonStrictModeEnabled := (cfNonStrictMode in AOptions.Compatibility) or
    (AOptions.SourceType = stModule);
end;

procedure ConfigureParser(const AParser: TGocciaParser;
  const AOptions: TGocciaSourcePipelineOptions);
begin
  AParser.ApplyOptions(ParserOptionsForSourcePipeline(AOptions));
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

class function TGocciaSourcePipeline.Parse(const ASource: TStringList;
  const AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions): TGocciaSourcePipelineResult;
var
  SourceText, OriginalSourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  Lexer: TGocciaLexer;
  Tokens: TObjectList<TGocciaToken>;
  Parser: TGocciaParser;
  ParserWarning: TGocciaParserWarning;
  StartTime, LexEnd, ParseEnd: Int64;
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

    if ppJSX in AOptions.Preprocessors then
    begin
      JSXResult := TGocciaJSXTransformer.Transform(SourceText);
      SourceText := JSXResult.Source;
      Result.FSourceMap := JSXResult.SourceMap;
      if Assigned(Result.FSourceMap) then
      begin
        Result.FSourceMap.SetSourceContent(0, OriginalSourceText);
        if not IsJSXNativeExtension(ExtractFileExt(AFileName)) then
          Result.AddWarning(
            'JSX syntax found in source with a non-JSX extension',
            'Use a .jsx or .tsx extension for JSX source',
            1, 1, 1, 1);
      end;
    end;

    try
      Lexer := TGocciaLexer.Create(SourceText, AFileName);
      try
        Tokens := Lexer.ScanTokens;
        LexEnd := GetNanoseconds;
        Result.FLexTimeNanoseconds := LexEnd - StartTime;
        Result.FGeneratedSourceLines := CloneStringList(Lexer.SourceLines);

        Parser := TGocciaParser.Create(Tokens, AFileName, Lexer.SourceLines);
        ConfigureParser(Parser, AOptions);
        try
          Result.FProgramNode := Parser.Parse;
          ParseEnd := GetNanoseconds;
          Result.FParseTimeNanoseconds := ParseEnd - LexEnd;

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
  Tokens: TObjectList<TGocciaToken>;
begin
  Result := nil;
  if AExpressionText = '' then
    Exit;

  SourceLines := CreateECMAScriptSourceLines(AExpressionText);
  try
    Lexer := TGocciaLexer.Create(AExpressionText, AFileName);
    try
      Tokens := Lexer.ScanTokens;
      if not Assigned(Tokens) then
        Exit;
      Parser := TGocciaParser.Create(Tokens, AFileName, SourceLines);
      ConfigureParser(Parser, AOptions);
      try
        if Assigned(ADeclaredPrivateNames) then
          Result := Parser.ParseExpressionWithPrivateNames(ADeclaredPrivateNames)
        else
          Result := Parser.Expression;
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
  const AOptions: TGocciaSourcePipelineOptions): Boolean;
var
  ArrowFunction: TGocciaArrowFunctionExpression;
  PipelineResult: TGocciaSourcePipelineResult;
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.Text := '(' + AParametersSource + ') => {}';
    PipelineResult := Parse(Source, AFileName, AOptions);
    try
      Result := HasSingleArrowFunctionProgram(PipelineResult.ProgramNode,
        ArrowFunction);
    finally
      PipelineResult.Free;
    end;
  finally
    Source.Free;
  end;
end;

class function TGocciaSourcePipeline.ParseFunctionBodyWrapper(
  const ABodySource, AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions): TGocciaFunctionBodyParseResult;
var
  ArrowFunction: TGocciaArrowFunctionExpression;
  PipelineResult: TGocciaSourcePipelineResult;
  Source: TStringList;
begin
  Result.IsValid := False;
  Result.HasUseStrictDirective := False;
  Source := TStringList.Create;
  try
    Source.Text := '() => {' + #10 + ABodySource + #10 + '}';
    PipelineResult := Parse(Source, AFileName, AOptions);
    try
      Result.IsValid := HasSingleArrowFunctionProgram(PipelineResult.ProgramNode,
        ArrowFunction);
      if Result.IsValid then
        Result.HasUseStrictDirective := HasUseStrictDirective(
          ArrowFunction.Body);
    finally
      PipelineResult.Free;
    end;
  finally
    Source.Free;
  end;
end;

class function TGocciaSourcePipeline.ParseDynamicFunctionWrapper(
  const AParametersSource, ABodySource, AFileName: string;
  const AOptions: TGocciaSourcePipelineOptions): TGocciaProgram;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  Source: string;
begin
  Source := '({ anonymous(' + AParametersSource + ') {' + #10 + ABodySource +
    #10 + '} }).anonymous';

  Lexer := TGocciaLexer.Create(Source, AFileName);
  try
    Parser := TGocciaParser.Create(Lexer.ScanTokens, AFileName,
      Lexer.SourceLines);
    ConfigureParser(Parser, AOptions);
    try
      Result := Parser.ParseUnchecked;
    finally
      Parser.Free;
    end;
  finally
    Lexer.Free;
  end;
end;

end.

unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.AST.Node,
  Goccia.Builtins.Benchmark,
  Goccia.Builtins.Console,
  Goccia.Environment.Bootstrap,
  Goccia.Environment.Types,
  Goccia.Interpreter,
  Goccia.JSX.SourceMap,
  Goccia.Modules,
  Goccia.Modules.Resolver,
  Goccia.Parser,
  Goccia.Values.Primitives;

type
  TGocciaGlobalBuiltin = Goccia.Environment.Types.TGocciaGlobalBuiltin;
  TGocciaGlobalBuiltins = Goccia.Environment.Types.TGocciaGlobalBuiltins;

  TGocciaScriptResult = record
    Result: TGocciaValue;
    LexTimeNanoseconds: Int64;
    ParseTimeNanoseconds: Int64;
    ExecuteTimeNanoseconds: Int64;
    TotalTimeNanoseconds: Int64;
    FileName: string;
  end;

type
  TGocciaEngine = class
  public
    const DefaultGlobals: TGocciaGlobalBuiltins = [
      ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber,
      ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggPerformance,
      ggTemporal, ggJSX, ggArrayBuffer
    ];
  private
    FInterpreter: TGocciaInterpreter;
    FBootstrap: TGocciaEnvironmentBootstrap;
    FResolver: TGocciaModuleResolver;
    FOwnsResolver: Boolean;
    FFileName: string;
    FSourceLines: TStringList;
    FGlobals: TGocciaGlobalBuiltins;
    FPreviousExceptionMask: TFPUExceptionMask;
    FSuppressWarnings: Boolean;

    procedure PinSingletons;
    procedure PrintParserWarnings(const AParser: TGocciaParser; const ASourceMap: TGocciaSourceMap = nil);
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
    function GetBuiltinBenchmark: TGocciaBenchmark;
    function GetBuiltinConsole: TGocciaConsole;
  public
    constructor Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins); overload;
    constructor Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins; const AResolver: TGocciaModuleResolver); overload;
    destructor Destroy; override;

    function Execute: TGocciaScriptResult;
    function ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;

    procedure AddAlias(const APattern, AReplacement: string);
    procedure RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);

    class function RunScript(const ASource: string; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScript(const ASource: string; const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const AFileName: string): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult; overload;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property Resolver: TGocciaModuleResolver read FResolver;
    property BuiltinConsole: TGocciaConsole read GetBuiltinConsole;
    property BuiltinBenchmark: TGocciaBenchmark read GetBuiltinBenchmark;
    property SuppressWarnings: Boolean read FSuppressWarnings write FSuppressWarnings;
  end;


implementation

uses
  Generics.Collections,
  Math,
  SysUtils,

  GarbageCollector.Generic,
  TimingUtils,

  Goccia.CallStack,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Scope,
  Goccia.Token;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins);
begin
  Create(AFileName, ASourceLines, AGlobals, nil);
end;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins; const AResolver: TGocciaModuleResolver);
begin
  FPreviousExceptionMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobals := AGlobals;

  if Assigned(AResolver) then
  begin
    FResolver := AResolver;
    FOwnsResolver := False;
  end
  else
  begin
    FResolver := TGocciaModuleResolver.Create(ExtractFilePath(ExpandFileName(AFileName)));
    FOwnsResolver := True;
  end;

  TGarbageCollector.Initialize;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;

  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines);
  FInterpreter.JSXEnabled := ggJSX in FGlobals;
  FInterpreter.Resolver := FResolver;

  TGarbageCollector.Instance.AddRootObject(FInterpreter.GlobalScope);

  PinSingletons;
  FBootstrap := TGocciaEnvironmentBootstrap.Create(
    FGlobals, FInterpreter.GlobalScope, ThrowError);
  FBootstrap.MaterializeInterpreterEnvironment(False);
end;

destructor TGocciaEngine.Destroy;
begin
  try
    if Assigned(TGarbageCollector.Instance) and Assigned(FInterpreter) then
      TGarbageCollector.Instance.RemoveRootObject(FInterpreter.GlobalScope);

    FBootstrap.Free;
    FInterpreter.Free;
    if FOwnsResolver then
      FResolver.Free;
  finally
    SetExceptionMask(FPreviousExceptionMask);
  end;
  inherited;
end;

procedure PinIfAssigned(const AValue: TGocciaValue); inline;
begin
  if Assigned(AValue) then
    TGarbageCollector.Instance.PinObject(AValue);
end;

procedure TGocciaEngine.PinSingletons;
var
  I: Integer;
begin
  PinIfAssigned(TGocciaUndefinedLiteralValue.UndefinedValue);
  PinIfAssigned(TGocciaBooleanLiteralValue.TrueValue);
  PinIfAssigned(TGocciaBooleanLiteralValue.FalseValue);
  PinIfAssigned(TGocciaNumberLiteralValue.NaNValue);
  PinIfAssigned(TGocciaNumberLiteralValue.ZeroValue);
  PinIfAssigned(TGocciaNumberLiteralValue.OneValue);
  PinIfAssigned(TGocciaNumberLiteralValue.NegativeZeroValue);
  PinIfAssigned(TGocciaNumberLiteralValue.InfinityValue);
  PinIfAssigned(TGocciaNumberLiteralValue.NegativeInfinityValue);
  for I := 0 to 255 do
    PinIfAssigned(TGocciaNumberLiteralValue.SmallInt(I));
end;

function TGocciaEngine.GetBuiltinBenchmark: TGocciaBenchmark;
begin
  if Assigned(FBootstrap) then
    Result := FBootstrap.BuiltinBenchmark
  else
    Result := nil;
end;

function TGocciaEngine.GetBuiltinConsole: TGocciaConsole;
begin
  if Assigned(FBootstrap) then
    Result := FBootstrap.BuiltinConsole
  else
    Result := nil;
end;

procedure TGocciaEngine.AddAlias(const APattern, AReplacement: string);
begin
  FResolver.AddAlias(APattern, AReplacement);
end;

procedure TGocciaEngine.RegisterGlobalModule(const AName: string; const AModule: TGocciaModule);
begin
  FInterpreter.GlobalModules.AddOrSetValue(AName, AModule);
end;

function TGocciaEngine.Execute: TGocciaScriptResult;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
  StartTime, LexEnd, ParseEnd, ExecEnd: Int64;
  SourceText: string;
  JSXResult: TGocciaJSXTransformResult;
  SourceMap: TGocciaSourceMap;
  OrigLine, OrigCol: Integer;
begin
  Result.FileName := FFileName;
  StartTime := GetNanoseconds;

  SourceText := FSourceLines.Text;
  SourceMap := nil;

  if ggJSX in FGlobals then
  begin
    JSXResult := TGocciaJSXTransformer.Transform(SourceText);
    SourceText := JSXResult.Source;
    SourceMap := JSXResult.SourceMap;
    if Assigned(SourceMap) then
      WarnIfJSXExtensionMismatch(FFileName);
  end;

  try
    try
      Lexer := TGocciaLexer.Create(SourceText, FFileName);
      try
        Tokens := Lexer.ScanTokens;
        LexEnd := GetNanoseconds;
        Result.LexTimeNanoseconds := LexEnd - StartTime;

        Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
        try
          ProgramNode := Parser.Parse;
          PrintParserWarnings(Parser, SourceMap);
          ParseEnd := GetNanoseconds;
          Result.ParseTimeNanoseconds := ParseEnd - LexEnd;

          try
            Result.Result := FInterpreter.Execute(ProgramNode);
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.DrainQueue;
            ExecEnd := GetNanoseconds;
            Result.ExecuteTimeNanoseconds := ExecEnd - ParseEnd;
            Result.TotalTimeNanoseconds := ExecEnd - StartTime;
          finally
            if Assigned(TGocciaMicrotaskQueue.Instance) then
              TGocciaMicrotaskQueue.Instance.ClearQueue;
            ProgramNode.Free;
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
        if Assigned(SourceMap) and
           SourceMap.Translate(E.Line, E.Column, OrigLine, OrigCol) then
          E.TranslatePosition(OrigLine, OrigCol, FSourceLines);
        raise;
      end;
    end;
  finally
    SourceMap.Free;
  end;
end;

function TGocciaEngine.ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;
begin
  try
    Result := FInterpreter.Execute(AProgram);
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.DrainQueue;
  finally
    if Assigned(TGocciaMicrotaskQueue.Instance) then
      TGocciaMicrotaskQueue.Instance.ClearQueue;
  end;
end;

class function TGocciaEngine.RunScript(const ASource: string; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := TStringList.Create;
  try
    SourceList.Text := ASource;
    Result := RunScriptFromStringList(SourceList, AFileName, AGlobals);
  finally
    SourceList.Free;
  end;
end;

class function TGocciaEngine.RunScript(const ASource: string; const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScript(ASource, AFileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptFromFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(AFileName);
    Result := RunScriptFromStringList(Source, AFileName, AGlobals);
  finally
    Source.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromFile(const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromFile(AFileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptFromStringList(const ASource: TStringList; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  Engine: TGocciaEngine;
begin
  Engine := TGocciaEngine.Create(AFileName, ASource, AGlobals);
  try
    Result := Engine.Execute;
  finally
    Engine.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromStringList(ASource, AFileName, TGocciaEngine.DefaultGlobals);
end;

procedure TGocciaEngine.PrintParserWarnings(const AParser: TGocciaParser; const ASourceMap: TGocciaSourceMap);
var
  Warning: TGocciaParserWarning;
  I, OrigLine, OrigCol: Integer;
begin
  if FSuppressWarnings then
    Exit;
  for I := 0 to AParser.WarningCount - 1 do
  begin
    Warning := AParser.GetWarning(I);
    WriteLn(Format('Warning: %s', [Warning.Message]));
    if Warning.Suggestion <> '' then
      WriteLn(Format('  Suggestion: %s', [Warning.Suggestion]));
    if Assigned(ASourceMap) and ASourceMap.Translate(Warning.Line, Warning.Column, OrigLine, OrigCol) then
      WriteLn(Format('  --> %s:%d:%d', [FFileName, OrigLine, OrigCol]))
    else
      WriteLn(Format('  --> %s:%d:%d', [FFileName, Warning.Line, Warning.Column]));
  end;
end;

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.

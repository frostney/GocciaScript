unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,

  Goccia.AST.Node,
  Goccia.Builtins.Benchmark,
  Goccia.Builtins.Console,
  Goccia.Builtins.GlobalArray,
  Goccia.Builtins.GlobalMap,
  Goccia.Builtins.GlobalNumber,
  Goccia.Builtins.GlobalObject,
  Goccia.Builtins.GlobalPromise,
  Goccia.Builtins.Globals,
  Goccia.Builtins.GlobalSet,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Builtins.JSON,
  Goccia.Builtins.Math,
  Goccia.Builtins.Temporal,
  Goccia.Builtins.TestAssertions,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.Interpreter,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Modules,
  Goccia.Parser,
  Goccia.Scope,
  Goccia.Token,
  Goccia.Values.ClassValue,
  Goccia.Values.Error,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalBuiltin = (
    ggConsole,
    ggMath,
    ggGlobalObject,
    ggGlobalArray,
    ggGlobalNumber,
    ggPromise,
    ggJSON,
    ggSymbol,
    ggSet,
    ggMap,
    ggTestAssertions,
    ggBenchmark,
    ggTemporal
  );

  TGocciaGlobalBuiltins = set of TGocciaGlobalBuiltin;

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
    const DefaultGlobals: TGocciaGlobalBuiltins = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggTemporal];
  private
    FInterpreter: TGocciaInterpreter;
    FFileName: string;
    FSourceLines: TStringList;
    FGlobals: TGocciaGlobalBuiltins;

    // Built-in objects
    FBuiltinConsole: TGocciaConsole;
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;
    FBuiltinGlobalArray: TGocciaGlobalArray;
    FBuiltinGlobalNumber: TGocciaGlobalNumber;
    FBuiltinGlobals: TGocciaGlobals;
    FBuiltinJSON: TGocciaJSONBuiltin;
    FBuiltinSymbol: TGocciaGlobalSymbol;
    FBuiltinSet: TGocciaGlobalSet;
    FBuiltinMap: TGocciaGlobalMap;
    FBuiltinPromise: TGocciaGlobalPromise;
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FBuiltinBenchmark: TGocciaBenchmark;
    FBuiltinTemporal: TGocciaTemporalBuiltin;

    procedure PinSingletons;
    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors;
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
  public
    constructor Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins);
    destructor Destroy; override;

    function Execute: TGocciaScriptResult;
    function ExecuteProgram(const AProgram: TGocciaProgram): TGocciaValue;

    class function RunScript(const ASource: string; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScript(const ASource: string; const AFileName: string = 'inline.goccia'): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const AFileName: string): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string; const AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const ASource: TStringList; const AFileName: string): TGocciaScriptResult; overload;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinMath: TGocciaMath read FBuiltinMath;
    property BuiltinGlobalObject: TGocciaGlobalObject read FBuiltinGlobalObject;
    property BuiltinGlobalArray: TGocciaGlobalArray read FBuiltinGlobalArray;
    property BuiltinGlobalNumber: TGocciaGlobalNumber read FBuiltinGlobalNumber;
    property BuiltinGlobals: TGocciaGlobals read FBuiltinGlobals;
    property BuiltinJSON: TGocciaJSONBuiltin read FBuiltinJSON;
    property BuiltinSymbol: TGocciaGlobalSymbol read FBuiltinSymbol;
    property BuiltinSet: TGocciaGlobalSet read FBuiltinSet;
    property BuiltinMap: TGocciaGlobalMap read FBuiltinMap;
    property BuiltinPromise: TGocciaGlobalPromise read FBuiltinPromise;
    property BuiltinTestAssertions: TGocciaTestAssertions read FBuiltinTestAssertions;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
    property BuiltinTemporal: TGocciaTemporalBuiltin read FBuiltinTemporal;
  end;


implementation

uses
  TimingUtils;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins);
var
  I: Integer;
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobals := AGlobals;

  // Initialize singletons (only creates if not yet initialized)
  TGocciaGarbageCollector.Initialize;
  TGocciaMicrotaskQueue.Initialize;

  // Note: We don't collect between engines because class-level shared objects
  // (like TGocciaFunctionBase.FSharedPrototype and its children) are only
  // reachable via class vars, not via GC roots. The GC handles collection
  // during execution (between function calls, in benchmarks, etc.)

  // Create interpreter without globals
  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines);

  // Register the global scope as a GC root
  TGocciaGarbageCollector.Instance.AddRoot(FInterpreter.GlobalScope);

  // Pin singleton values so the GC never collects them
  PinSingletons;

  // Register built-ins based on the globals set
  RegisterBuiltIns;
end;

destructor TGocciaEngine.Destroy;
begin
  // Remove global scope from GC roots before cleanup
  if Assigned(TGocciaGarbageCollector.Instance) and Assigned(FInterpreter) then
    TGocciaGarbageCollector.Instance.RemoveRoot(FInterpreter.GlobalScope);

  // Free builtin wrapper objects (these are NOT GC-managed, but their
  // FBuiltinObject fields ARE - the wrapper destructors skip freeing them)
  FBuiltinConsole.Free;
  FBuiltinMath.Free;
  FBuiltinGlobalObject.Free;
  FBuiltinGlobalArray.Free;
  FBuiltinGlobalNumber.Free;
  FBuiltinGlobals.Free;
  FBuiltinJSON.Free;
  FBuiltinSymbol.Free;
  FBuiltinSet.Free;
  FBuiltinMap.Free;
  FBuiltinPromise.Free;
  FBuiltinTestAssertions.Free;
  FBuiltinBenchmark.Free;
  FBuiltinTemporal.Free;

  // Free interpreter after builtins
  FInterpreter.Free;
  inherited;
end;

procedure PinIfAssigned(const AValue: TGocciaValue); inline;
begin
  if Assigned(AValue) then
    TGocciaGarbageCollector.Instance.PinValue(AValue);
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

procedure TGocciaEngine.RegisterBuiltIns;
var
  Scope: TGocciaScope;
begin
  Scope := FInterpreter.GlobalScope;

  // Flag-gated built-ins: each creates a wrapper that registers its
  // native methods/properties into the global scope.
  if ggConsole in FGlobals then
    FBuiltinConsole := TGocciaConsole.Create('console', Scope, ThrowError);
  if ggMath in FGlobals then
    FBuiltinMath := TGocciaMath.Create('Math', Scope, ThrowError);
  if ggGlobalObject in FGlobals then
    FBuiltinGlobalObject := TGocciaGlobalObject.Create('Object', Scope, ThrowError);
  if ggGlobalArray in FGlobals then
    FBuiltinGlobalArray := TGocciaGlobalArray.Create('Array', Scope, ThrowError);
  if ggGlobalNumber in FGlobals then
    FBuiltinGlobalNumber := TGocciaGlobalNumber.Create('Number', Scope, ThrowError);
  if ggJSON in FGlobals then
    FBuiltinJSON := TGocciaJSONBuiltin.Create('JSON', Scope, ThrowError);
  if ggSymbol in FGlobals then
    FBuiltinSymbol := TGocciaGlobalSymbol.Create('Symbol', Scope, ThrowError);
  if ggSet in FGlobals then
    FBuiltinSet := TGocciaGlobalSet.Create('Set', Scope, ThrowError);
  if ggMap in FGlobals then
    FBuiltinMap := TGocciaGlobalMap.Create('Map', Scope, ThrowError);
  if ggPromise in FGlobals then
    FBuiltinPromise := TGocciaGlobalPromise.Create('Promise', Scope, ThrowError);
  if ggTestAssertions in FGlobals then
    FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', Scope, ThrowError);
  if ggBenchmark in FGlobals then
    FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark', Scope, ThrowError);
  if ggTemporal in FGlobals then
    FBuiltinTemporal := TGocciaTemporalBuiltin.Create('Temporal', Scope, ThrowError);

  // Always-registered built-ins
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, ThrowError);
  RegisterBuiltinConstructors;
end;

procedure TGocciaEngine.RegisterBuiltinConstructors;
var
  ArrayConstructor, ObjectConstructor, StringConstructor, NumberConstructor: TGocciaClassValue;
  BooleanConstructor, FunctionConstructor: TGocciaClassValue;
  Key: string;
begin
  // Create Object constructor first (must be first since it's the root of the prototype chain)
  ObjectConstructor := TGocciaClassValue.Create('Object', nil);
  if Assigned(FBuiltinGlobalObject) then
  begin
    for Key in FBuiltinGlobalObject.BuiltinObject.GetAllPropertyNames do
      ObjectConstructor.SetProperty(Key, FBuiltinGlobalObject.BuiltinObject.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineLexicalBinding('Object', ObjectConstructor, dtConst);

  // Create Array constructor and copy static methods
  ArrayConstructor := TGocciaClassValue.Create('Array', nil);
  ArrayConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalArray) then
  begin
    for Key in FBuiltinGlobalArray.BuiltinObject.GetAllPropertyNames do
      ArrayConstructor.SetProperty(Key, FBuiltinGlobalArray.BuiltinObject.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineLexicalBinding('Array', ArrayConstructor, dtConst);

  // Create other constructors - use specialized subclasses for primitives
  StringConstructor := TGocciaStringClassValue.Create('String', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('String', StringConstructor, dtConst);

  NumberConstructor := TGocciaNumberClassValue.Create('Number', nil);
  if Assigned(FBuiltinGlobalNumber) then
  begin
    for Key in FBuiltinGlobalNumber.BuiltinObject.GetAllPropertyNames do
      NumberConstructor.SetProperty(Key, FBuiltinGlobalNumber.BuiltinObject.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineLexicalBinding('Number', NumberConstructor, dtConst);

  BooleanConstructor := TGocciaBooleanClassValue.Create('Boolean', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('Boolean', BooleanConstructor, dtConst);

  // Create Function constructor
  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst);
end;

function TGocciaEngine.Execute: TGocciaScriptResult;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
  StartTime, LexEnd, ParseEnd, ExecEnd: Int64;
begin
  Result.FileName := FFileName;
  StartTime := GetNanoseconds;

  Lexer := TGocciaLexer.Create(FSourceLines.Text, FFileName);
  try
    Tokens := Lexer.ScanTokens;
    LexEnd := GetNanoseconds;
    Result.LexTimeNanoseconds := LexEnd - StartTime;

    Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
    try
      ProgramNode := Parser.Parse;
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

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.

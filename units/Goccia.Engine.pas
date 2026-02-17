unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  Goccia.Interpreter, Goccia.Parser, Goccia.Lexer, Goccia.Token,
  Goccia.AST.Node, Goccia.Error, Goccia.Values.Primitives,
  Goccia.Values.ObjectValue, Goccia.Builtins.Console, Goccia.Builtins.Math,
  Goccia.Builtins.GlobalObject, Goccia.Builtins.GlobalArray,
  Goccia.Builtins.GlobalNumber, Goccia.Builtins.Globals, Goccia.Builtins.JSON,
  Goccia.Builtins.GlobalSymbol, Goccia.Builtins.GlobalSet, Goccia.Builtins.GlobalMap,
  Goccia.Builtins.TestAssertions, Goccia.Builtins.Benchmark,
  Goccia.Builtins.GlobalPromise, Goccia.Scope, Goccia.Modules,
  Goccia.Values.ClassValue, Goccia.Values.Error, Goccia.GarbageCollector,
  Goccia.MicrotaskQueue;

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
    ggBenchmark
  );

  TGocciaGlobalBuiltins = set of TGocciaGlobalBuiltin;

  TGocciaScriptResult = record
    Result: TGocciaValue;
    LexTimeMicroseconds: Int64;
    ParseTimeMicroseconds: Int64;
    ExecuteTimeMicroseconds: Int64;
    TotalTimeMicroseconds: Int64;
    FileName: string;
  end;

type
  TGocciaEngine = class
  public
    const DefaultGlobals: TGocciaGlobalBuiltins = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap];
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
    FBuiltinJSON: TGocciaJSON;
    FBuiltinSymbol: TGocciaGlobalSymbol;
    FBuiltinSet: TGocciaGlobalSet;
    FBuiltinMap: TGocciaGlobalMap;
    FBuiltinPromise: TGocciaGlobalPromise;
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FBuiltinBenchmark: TGocciaBenchmark;

    procedure PinSingletons;
    procedure RegisterBuiltIns;
    procedure RegisterBuiltinConstructors;
    procedure ThrowError(const Message: string; Line, Column: Integer);
  public
    constructor Create(const AFileName: string; ASourceLines: TStringList; AGlobals: TGocciaGlobalBuiltins);
    destructor Destroy; override;

    function Execute: TGocciaScriptResult;
    function ExecuteProgram(AProgram: TGocciaProgram): TGocciaValue;

    class function RunScript(const Source: string; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScript(const Source: string; const FileName: string = 'inline.goccia'): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromFile(const FileName: string): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult; overload;
    class function RunScriptFromStringList(const Source: TStringList; const FileName: string): TGocciaScriptResult; overload;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinMath: TGocciaMath read FBuiltinMath;
    property BuiltinGlobalObject: TGocciaGlobalObject read FBuiltinGlobalObject;
    property BuiltinGlobalArray: TGocciaGlobalArray read FBuiltinGlobalArray;
    property BuiltinGlobalNumber: TGocciaGlobalNumber read FBuiltinGlobalNumber;
    property BuiltinGlobals: TGocciaGlobals read FBuiltinGlobals;
    property BuiltinJSON: TGocciaJSON read FBuiltinJSON;
    property BuiltinSymbol: TGocciaGlobalSymbol read FBuiltinSymbol;
    property BuiltinSet: TGocciaGlobalSet read FBuiltinSet;
    property BuiltinMap: TGocciaGlobalMap read FBuiltinMap;
    property BuiltinPromise: TGocciaGlobalPromise read FBuiltinPromise;
    property BuiltinTestAssertions: TGocciaTestAssertions read FBuiltinTestAssertions;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
  end;


implementation

uses
  TimingUtils;

constructor TGocciaEngine.Create(const AFileName: string; ASourceLines: TStringList; AGlobals: TGocciaGlobalBuiltins);
var
  I: Integer;
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobals := AGlobals;

  // Initialize singletons (only creates if not yet initialized)
  TGocciaGC.Initialize;
  TGocciaMicrotaskQueue.Initialize;

  // Note: We don't collect between engines because class-level shared objects
  // (like TGocciaFunctionBase.FSharedPrototype and its children) are only
  // reachable via class vars, not via GC roots. The GC handles collection
  // during execution (between function calls, in benchmarks, etc.)

  // Create interpreter without globals
  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines);

  // Register the global scope as a GC root
  TGocciaGC.Instance.AddRoot(FInterpreter.GlobalScope);

  // Pin singleton values so the GC never collects them
  PinSingletons;

  // Register built-ins based on the globals set
  RegisterBuiltIns;
end;

destructor TGocciaEngine.Destroy;
begin
  // Remove global scope from GC roots before cleanup
  if Assigned(TGocciaGC.Instance) and Assigned(FInterpreter) then
    TGocciaGC.Instance.RemoveRoot(FInterpreter.GlobalScope);

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

  // Free interpreter after builtins
  FInterpreter.Free;
  inherited;
end;

procedure PinIfAssigned(Value: TGocciaValue); inline;
begin
  if Assigned(Value) then
    TGocciaGC.Instance.PinValue(Value);
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
    FBuiltinJSON := TGocciaJSON.Create('JSON', Scope, ThrowError);
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
  StartTime := GetMicroseconds;

  Lexer := TGocciaLexer.Create(FSourceLines.Text, FFileName);
  try
    Tokens := Lexer.ScanTokens;
    LexEnd := GetMicroseconds;
    Result.LexTimeMicroseconds := LexEnd - StartTime;

    Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
    try
      ProgramNode := Parser.Parse;
      ParseEnd := GetMicroseconds;
      Result.ParseTimeMicroseconds := ParseEnd - LexEnd;

      try
        Result.Result := FInterpreter.Execute(ProgramNode);
        if Assigned(TGocciaMicrotaskQueue.Instance) then
          TGocciaMicrotaskQueue.Instance.DrainQueue;
        ExecEnd := GetMicroseconds;
        Result.ExecuteTimeMicroseconds := ExecEnd - ParseEnd;
        Result.TotalTimeMicroseconds := ExecEnd - StartTime;
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

function TGocciaEngine.ExecuteProgram(AProgram: TGocciaProgram): TGocciaValue;
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

class function TGocciaEngine.RunScript(const Source: string; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  SourceList: TStringList;
begin
  SourceList := TStringList.Create;
  try
    SourceList.Text := Source;
    Result := RunScriptFromStringList(SourceList, FileName, AGlobals);
  finally
    SourceList.Free;
  end;
end;

class function TGocciaEngine.RunScript(const Source: string; const FileName: string): TGocciaScriptResult;
begin
  Result := RunScript(Source, FileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptFromFile(const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  Source: TStringList;
begin
  Source := TStringList.Create;
  try
    Source.LoadFromFile(FileName);
    Result := RunScriptFromStringList(Source, FileName, AGlobals);
  finally
    Source.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromFile(const FileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromFile(FileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptFromStringList(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaScriptResult;
var
  Engine: TGocciaEngine;
begin
  Engine := TGocciaEngine.Create(FileName, Source, AGlobals);
  try
    Result := Engine.Execute;
  finally
    Engine.Free;
  end;
end;

class function TGocciaEngine.RunScriptFromStringList(const Source: TStringList; const FileName: string): TGocciaScriptResult;
begin
  Result := RunScriptFromStringList(Source, FileName, TGocciaEngine.DefaultGlobals);
end;

procedure TGocciaEngine.ThrowError(const Message: string; Line, Column: Integer);
begin
  raise TGocciaRuntimeError.Create(Message, Line, Column, FFileName, FSourceLines);
end;

end.

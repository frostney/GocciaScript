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
  Goccia.Builtins.TestAssertions, Goccia.Builtins.Benchmark, Goccia.Scope, Goccia.Modules,
  Goccia.Values.Interfaces, Goccia.Values.ClassValue, Goccia.Values.Error, Goccia.GC;

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

  TGocciaTimingResult = record
    Result: TGocciaValue;
    LexTimeMs: Int64;
    ParseTimeMs: Int64;
    ExecuteTimeMs: Int64;
    TotalTimeMs: Int64;
  end;

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
    FBuiltinTestAssertions: TGocciaTestAssertions;
    FBuiltinBenchmark: TGocciaBenchmark;

    procedure RegisterBuiltIns;
    procedure RegisterConsole;
    procedure RegisterMath;
    procedure RegisterJSON;
    procedure RegisterTestAssertions;
    procedure RegisterPromise;
    procedure RegisterGlobalArray;
    procedure RegisterObjectMethods;
    procedure RegisterGlobalNumber;
    procedure RegisterGlobals;
    procedure RegisterSymbol;
    procedure RegisterSet;
    procedure RegisterMap;
    procedure RegisterBenchmark;
    procedure RegisterBuiltinConstructors;
    procedure ThrowError(const Message: string; Line, Column: Integer);
  public
    constructor Create(const AFileName: string; ASourceLines: TStringList; AGlobals: TGocciaGlobalBuiltins);
    destructor Destroy; override;

    function Execute: TGocciaValue;
    function ExecuteWithTiming: TGocciaTimingResult;
    function ExecuteProgram(AProgram: TGocciaProgram): TGocciaValue;

    class function RunScript(const Source: string; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaValue; overload;
    class function RunScript(const Source: string; const FileName: string = 'inline.goccia'): TGocciaValue; overload;
    class function RunScriptFromFile(const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaValue; overload;
    class function RunScriptFromFile(const FileName: string): TGocciaValue; overload;
    class function RunScriptFromStringList(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaValue; overload;
    class function RunScriptFromStringList(const Source: TStringList; const FileName: string): TGocciaValue; overload;
    class function RunScriptWithTiming(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaTimingResult;

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
    property BuiltinTestAssertions: TGocciaTestAssertions read FBuiltinTestAssertions;
    property BuiltinBenchmark: TGocciaBenchmark read FBuiltinBenchmark;
  end;


implementation

constructor TGocciaEngine.Create(const AFileName: string; ASourceLines: TStringList; AGlobals: TGocciaGlobalBuiltins);
var
  I: Integer;
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobals := AGlobals;

  // Initialize the garbage collector (singleton - only creates if not yet initialized)
  TGocciaGC.Initialize;

  // Note: We don't collect between engines because class-level shared objects
  // (like TGocciaFunctionBase.FSharedPrototype and its children) are only
  // reachable via class vars, not via GC roots. The GC handles collection
  // during execution (between function calls, in benchmarks, etc.)

  // Create interpreter without globals
  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines);

  // Register the global scope as a GC root
  TGocciaGC.Instance.AddRoot(FInterpreter.GlobalScope);

  // Pin singleton values so the GC never collects them
  if Assigned(TGocciaUndefinedLiteralValue.UndefinedValue) then
    TGocciaGC.Instance.PinValue(TGocciaUndefinedLiteralValue.UndefinedValue);
  if Assigned(TGocciaBooleanLiteralValue.TrueValue) then
    TGocciaGC.Instance.PinValue(TGocciaBooleanLiteralValue.TrueValue);
  if Assigned(TGocciaBooleanLiteralValue.FalseValue) then
    TGocciaGC.Instance.PinValue(TGocciaBooleanLiteralValue.FalseValue);
  if Assigned(TGocciaNumberLiteralValue.NaNValue) then
    TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.NaNValue);
  if Assigned(TGocciaNumberLiteralValue.ZeroValue) then
    TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.ZeroValue);
  if Assigned(TGocciaNumberLiteralValue.OneValue) then
    TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.OneValue);
  if Assigned(TGocciaNumberLiteralValue.NegativeZeroValue) then
    TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.NegativeZeroValue);
  if Assigned(TGocciaNumberLiteralValue.InfinityValue) then
    TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.InfinityValue);
  if Assigned(TGocciaNumberLiteralValue.NegativeInfinityValue) then
    TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.NegativeInfinityValue);
  // Pin SmallInt cache values
  for I := 0 to 255 do
  begin
    if Assigned(TGocciaNumberLiteralValue.SmallInt(I)) then
      TGocciaGC.Instance.PinValue(TGocciaNumberLiteralValue.SmallInt(I));
  end;

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
  if Assigned(FBuiltinConsole) then
    FBuiltinConsole.Free;
  if Assigned(FBuiltinMath) then
    FBuiltinMath.Free;
  if Assigned(FBuiltinGlobalObject) then
    FBuiltinGlobalObject.Free;
  if Assigned(FBuiltinGlobalArray) then
    FBuiltinGlobalArray.Free;
  if Assigned(FBuiltinGlobalNumber) then
    FBuiltinGlobalNumber.Free;
  if Assigned(FBuiltinGlobals) then
    FBuiltinGlobals.Free;
  if Assigned(FBuiltinJSON) then
    FBuiltinJSON.Free;
  if Assigned(FBuiltinSymbol) then
    FBuiltinSymbol.Free;
  if Assigned(FBuiltinSet) then
    FBuiltinSet.Free;
  if Assigned(FBuiltinMap) then
    FBuiltinMap.Free;
  if Assigned(FBuiltinTestAssertions) then
    FBuiltinTestAssertions.Free;
  if Assigned(FBuiltinBenchmark) then
    FBuiltinBenchmark.Free;

  // Free interpreter after builtins
  FInterpreter.Free;
  inherited;
end;

procedure TGocciaEngine.RegisterBuiltIns;
begin
  if ggConsole in FGlobals then RegisterConsole;
  if ggMath in FGlobals then RegisterMath;
  if ggGlobalObject in FGlobals then RegisterObjectMethods;
  if ggGlobalArray in FGlobals then RegisterGlobalArray;
  if ggGlobalNumber in FGlobals then RegisterGlobalNumber;
  if ggPromise in FGlobals then RegisterPromise;
  if ggJSON in FGlobals then RegisterJSON;
  if ggSymbol in FGlobals then RegisterSymbol;
  if ggSet in FGlobals then RegisterSet;
  if ggMap in FGlobals then RegisterMap;
  if ggTestAssertions in FGlobals then RegisterTestAssertions;
  if ggBenchmark in FGlobals then RegisterBenchmark;

  RegisterGlobals;
  RegisterBuiltinConstructors;
end;

procedure TGocciaEngine.RegisterConsole;
begin
  FBuiltinConsole := TGocciaConsole.Create('console', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterMath;
begin
  FBuiltinMath := TGocciaMath.Create('Math', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterJSON;
begin
  FBuiltinJSON := TGocciaJSON.Create('JSON', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterTestAssertions;
begin
  FBuiltinTestAssertions := TGocciaTestAssertions.Create('TestAssertions', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterBenchmark;
begin
  FBuiltinBenchmark := TGocciaBenchmark.Create('Benchmark', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterPromise;
begin
  // TODO: Implement Promise support
end;

procedure TGocciaEngine.RegisterGlobalArray;
begin
  FBuiltinGlobalArray := TGocciaGlobalArray.Create('Array', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterObjectMethods;
begin
  FBuiltinGlobalObject := TGocciaGlobalObject.Create('Object', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterGlobalNumber;
begin
  FBuiltinGlobalNumber := TGocciaGlobalNumber.Create('Number', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterGlobals;
begin
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterSymbol;
begin
  FBuiltinSymbol := TGocciaGlobalSymbol.Create('Symbol', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterSet;
begin
  FBuiltinSet := TGocciaGlobalSet.Create('Set', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterMap;
begin
  FBuiltinMap := TGocciaGlobalMap.Create('Map', FInterpreter.GlobalScope, ThrowError);
end;

procedure TGocciaEngine.RegisterBuiltinConstructors;
var
  ArrayConstructor, ObjectConstructor, StringConstructor, NumberConstructor: TGocciaClassValue;
  BooleanConstructor, FunctionConstructor: TGocciaClassValue;
  ExistingArray, ExistingObject, ExistingNumber: TGocciaValue;
  ArrayObj, ObjectObj, NumberObj: TGocciaObjectValue;
  Key: string;
begin
  // Get existing built-in objects that have static methods (only if they exist)
  ExistingArray := nil;
  ExistingObject := nil;
  ExistingNumber := nil;

  if FInterpreter.GlobalScope.Contains('Array') then
    ExistingArray := FInterpreter.GlobalScope.GetValue('Array');
  if FInterpreter.GlobalScope.Contains('Object') then
    ExistingObject := FInterpreter.GlobalScope.GetValue('Object');
  if FInterpreter.GlobalScope.Contains('Number') then
    ExistingNumber := FInterpreter.GlobalScope.GetValue('Number');

  // Create Object constructor first (must be first since it's the root of the prototype chain)
  ObjectConstructor := TGocciaClassValue.Create('Object', nil);
  if (ExistingObject is TGocciaObjectValue) then
  begin
    ObjectObj := TGocciaObjectValue(ExistingObject);
    // Copy all static methods from existing Object using property descriptors
    for Key in ObjectObj.GetAllPropertyNames do
      ObjectConstructor.SetProperty(Key, ObjectObj.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineLexicalBinding('Object', ObjectConstructor, dtUnknown);

  // Create Array constructor and copy static methods
  ArrayConstructor := TGocciaClassValue.Create('Array', nil);
  // Connect Array.prototype.prototype to Object.prototype
  ArrayConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if (ExistingArray is TGocciaObjectValue) then
  begin
    ArrayObj := TGocciaObjectValue(ExistingArray);
    // Copy all static methods from existing Array object using property descriptors
    for Key in ArrayObj.GetAllPropertyNames do
      ArrayConstructor.SetProperty(Key, ArrayObj.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineLexicalBinding('Array', ArrayConstructor, dtUnknown);

  // Create other constructors - use specialized subclasses for primitives
  StringConstructor := TGocciaStringClassValue.Create('String', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('String', StringConstructor, dtUnknown);

  NumberConstructor := TGocciaNumberClassValue.Create('Number', nil);
  if (ExistingNumber is TGocciaObjectValue) then
  begin
    NumberObj := TGocciaObjectValue(ExistingNumber);
    // Copy all static methods from existing Number object using property descriptors
    for Key in NumberObj.GetAllPropertyNames do
      NumberConstructor.SetProperty(Key, NumberObj.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineLexicalBinding('Number', NumberConstructor, dtUnknown);

  BooleanConstructor := TGocciaBooleanClassValue.Create('Boolean', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('Boolean', BooleanConstructor, dtUnknown);

  // Create Function constructor
  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtUnknown);

  // RangeError, Error, TypeError, ReferenceError are registered as native functions in RegisterGlobals

  // Symbol is now registered as a proper built-in via RegisterSymbol
end;

function TGocciaEngine.Execute: TGocciaValue;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
begin
  Lexer := TGocciaLexer.Create(FSourceLines.Text, FFileName);
  try
    Tokens := Lexer.ScanTokens;
    Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
    try
      ProgramNode := Parser.Parse;
      try
        Result := FInterpreter.Execute(ProgramNode);
      finally
        ProgramNode.Free;
      end;
    finally
      Parser.Free;
      // Don't free Tokens - the lexer owns them and will free them
    end;
  finally
    Lexer.Free;
  end;
end;

function TGocciaEngine.ExecuteWithTiming: TGocciaTimingResult;
var
  Lexer: TGocciaLexer;
  Parser: TGocciaParser;
  ProgramNode: TGocciaProgram;
  Tokens: TObjectList<TGocciaToken>;
  StartTime, LexEnd, ParseEnd, ExecEnd: Int64;
begin
  StartTime := GetTickCount64;

  Lexer := TGocciaLexer.Create(FSourceLines.Text, FFileName);
  try
    Tokens := Lexer.ScanTokens;
    LexEnd := GetTickCount64;
    Result.LexTimeMs := LexEnd - StartTime;

    Parser := TGocciaParser.Create(Tokens, FFileName, Lexer.SourceLines);
    try
      ProgramNode := Parser.Parse;
      ParseEnd := GetTickCount64;
      Result.ParseTimeMs := ParseEnd - LexEnd;

      try
        Result.Result := FInterpreter.Execute(ProgramNode);
        ExecEnd := GetTickCount64;
        Result.ExecuteTimeMs := ExecEnd - ParseEnd;
        Result.TotalTimeMs := ExecEnd - StartTime;
      finally
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
  Result := FInterpreter.Execute(AProgram);
end;

class function TGocciaEngine.RunScript(const Source: string; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaValue;
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

class function TGocciaEngine.RunScript(const Source: string; const FileName: string): TGocciaValue;
begin
  Result := RunScript(Source, FileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptFromFile(const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaValue;
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

class function TGocciaEngine.RunScriptFromFile(const FileName: string): TGocciaValue;
begin
  Result := RunScriptFromFile(FileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptFromStringList(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaValue;
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

class function TGocciaEngine.RunScriptFromStringList(const Source: TStringList; const FileName: string): TGocciaValue;
begin
  Result := RunScriptFromStringList(Source, FileName, TGocciaEngine.DefaultGlobals);
end;

class function TGocciaEngine.RunScriptWithTiming(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins): TGocciaTimingResult;
var
  Engine: TGocciaEngine;
begin
  Engine := TGocciaEngine.Create(FileName, Source, AGlobals);
  try
    Result := Engine.ExecuteWithTiming;
  finally
    Engine.Free;
  end;
end;

procedure TGocciaEngine.ThrowError(const Message: string; Line, Column: Integer);
begin
  raise TGocciaRuntimeError.Create(Message, Line, Column, FFileName, FSourceLines);
end;

end.

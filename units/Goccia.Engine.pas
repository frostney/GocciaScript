unit Goccia.Engine;

{$I Goccia.inc}

interface

uses
  Classes,

  Goccia.Arguments.Collection,
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
  Goccia.Builtins.GlobalString,
  Goccia.Builtins.GlobalSymbol,
  Goccia.Builtins.JSON,
  Goccia.Builtins.Math,
  Goccia.Builtins.Temporal,
  Goccia.Builtins.TestAssertions,
  Goccia.Interpreter,
  Goccia.JSX.SourceMap,
  Goccia.Modules,
  Goccia.Modules.Resolver,
  Goccia.Parser,
  Goccia.Values.IteratorValue,
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
    ggTemporal,
    ggJSX
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
    const DefaultGlobals: TGocciaGlobalBuiltins = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON, ggSymbol, ggSet, ggMap, ggTemporal, ggJSX];
  private
    FInterpreter: TGocciaInterpreter;
    FResolver: TGocciaModuleResolver;
    FOwnsResolver: Boolean;
    FFileName: string;
    FSourceLines: TStringList;
    FGlobals: TGocciaGlobalBuiltins;

    // Built-in objects
    FBuiltinConsole: TGocciaConsole;
    FBuiltinMath: TGocciaMath;
    FBuiltinGlobalObject: TGocciaGlobalObject;
    FBuiltinGlobalArray: TGocciaGlobalArray;
    FBuiltinGlobalNumber: TGocciaGlobalNumber;
    FBuiltinGlobalString: TGocciaGlobalString;
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
    procedure RegisterGlobalThis;
    procedure RegisterGocciaScriptGlobal;
    function SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    procedure PrintParserWarnings(const AParser: TGocciaParser; const ASourceMap: TGocciaSourceMap = nil);
    procedure ThrowError(const AMessage: string; const ALine, AColumn: Integer);
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
  Generics.Collections,
  SysUtils,
  TypInfo,

  TimingUtils,

  Goccia.CallStack,
  Goccia.Error,
  Goccia.GarbageCollector,
  Goccia.JSX.Transformer,
  Goccia.Lexer,
  Goccia.MicrotaskQueue,
  Goccia.Scope,
  Goccia.Token,
  Goccia.Values.ArrayValue,
  Goccia.Values.BooleanObjectValue,
  Goccia.Values.ClassValue,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.NumberObjectValue,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.SetValue,
  Goccia.Values.StringObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Version;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins);
begin
  Create(AFileName, ASourceLines, AGlobals, nil);
end;

constructor TGocciaEngine.Create(const AFileName: string; const ASourceLines: TStringList; const AGlobals: TGocciaGlobalBuiltins; const AResolver: TGocciaModuleResolver);
begin
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

  TGocciaGarbageCollector.Initialize;
  TGocciaCallStack.Initialize;
  TGocciaMicrotaskQueue.Initialize;

  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines);
  FInterpreter.JSXEnabled := ggJSX in FGlobals;
  FInterpreter.Resolver := FResolver;

  TGocciaGarbageCollector.Instance.AddRoot(FInterpreter.GlobalScope);

  PinSingletons;
  RegisterBuiltIns;
end;

destructor TGocciaEngine.Destroy;
begin
  if Assigned(TGocciaGarbageCollector.Instance) and Assigned(FInterpreter) then
    TGocciaGarbageCollector.Instance.RemoveRoot(FInterpreter.GlobalScope);

  FBuiltinConsole.Free;
  FBuiltinMath.Free;
  FBuiltinGlobalObject.Free;
  FBuiltinGlobalArray.Free;
  FBuiltinGlobalNumber.Free;
  FBuiltinGlobalString.Free;
  FBuiltinGlobals.Free;
  FBuiltinJSON.Free;
  FBuiltinSymbol.Free;
  FBuiltinSet.Free;
  FBuiltinMap.Free;
  FBuiltinPromise.Free;
  FBuiltinTestAssertions.Free;
  FBuiltinBenchmark.Free;
  FBuiltinTemporal.Free;

  FInterpreter.Free;
  if FOwnsResolver then
    FResolver.Free;
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
  FBuiltinGlobalString := TGocciaGlobalString.Create('String', Scope, ThrowError);
  FBuiltinGlobals := TGocciaGlobals.Create('Globals', Scope, ThrowError);
  Scope.DefineLexicalBinding('Iterator', TGocciaIteratorValue.CreateGlobalObject, dtConst);
  RegisterBuiltinConstructors;
end;

procedure TGocciaEngine.RegisterBuiltinConstructors;
var
  ObjectConstructor, FunctionConstructor: TGocciaClassValue;
  ArrayConstructor: TGocciaArrayClassValue;
  MapConstructor: TGocciaMapClassValue;
  SetConstructor: TGocciaSetClassValue;
  StringConstructor: TGocciaStringClassValue;
  NumberConstructor: TGocciaNumberClassValue;
  BooleanConstructor: TGocciaBooleanClassValue;
  Key: string;
begin
  ObjectConstructor := TGocciaClassValue.Create('Object', nil);
  if Assigned(FBuiltinGlobalObject) then
    for Key in FBuiltinGlobalObject.BuiltinObject.GetAllPropertyNames do
      ObjectConstructor.SetProperty(Key, FBuiltinGlobalObject.BuiltinObject.GetProperty(Key));
  FInterpreter.GlobalScope.DefineLexicalBinding('Object', ObjectConstructor, dtConst);

  ArrayConstructor := TGocciaArrayClassValue.Create('Array', nil);
  TGocciaArrayValue.ExposePrototype(ArrayConstructor);
  ArrayConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalArray) then
    for Key in FBuiltinGlobalArray.BuiltinObject.GetAllPropertyNames do
      ArrayConstructor.SetProperty(Key, FBuiltinGlobalArray.BuiltinObject.GetProperty(Key));
  ArrayConstructor.DefineSymbolProperty(
    TGocciaSymbolValue.WellKnownSpecies,
    TGocciaPropertyDescriptorAccessor.Create(
      TGocciaNativeFunctionValue.CreateWithoutPrototype(SpeciesGetter, 'get [Symbol.species]', 0),
      nil, [pfConfigurable]));
  FInterpreter.GlobalScope.DefineLexicalBinding('Array', ArrayConstructor, dtConst);

  if ggMap in FGlobals then
  begin
    MapConstructor := TGocciaMapClassValue.Create('Map', nil);
    TGocciaMapValue.ExposePrototype(MapConstructor);
    MapConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    if Assigned(FBuiltinMap) then
      for Key in FBuiltinMap.BuiltinObject.GetAllPropertyNames do
        MapConstructor.SetProperty(Key, FBuiltinMap.BuiltinObject.GetProperty(Key));
    MapConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));
    FInterpreter.GlobalScope.DefineLexicalBinding('Map', MapConstructor, dtConst);
  end;

  if ggSet in FGlobals then
  begin
    SetConstructor := TGocciaSetClassValue.Create('Set', nil);
    TGocciaSetValue.ExposePrototype(SetConstructor);
    SetConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
    SetConstructor.DefineSymbolProperty(
      TGocciaSymbolValue.WellKnownSpecies,
      TGocciaPropertyDescriptorAccessor.Create(
        TGocciaNativeFunctionValue.CreateWithoutPrototype(SpeciesGetter, 'get [Symbol.species]', 0),
        nil, [pfConfigurable]));
    FInterpreter.GlobalScope.DefineLexicalBinding('Set', SetConstructor, dtConst);
  end;

  StringConstructor := TGocciaStringClassValue.Create('String', nil);
  StringConstructor.ReplacePrototype(TGocciaStringObjectValue.GetSharedPrototype);
  StringConstructor.Prototype.AssignProperty('constructor', StringConstructor);
  StringConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalString) then
    for Key in FBuiltinGlobalString.BuiltinObject.GetAllPropertyNames do
      StringConstructor.SetProperty(Key, FBuiltinGlobalString.BuiltinObject.GetProperty(Key));
  FInterpreter.GlobalScope.DefineLexicalBinding('String', StringConstructor, dtConst);

  NumberConstructor := TGocciaNumberClassValue.Create('Number', nil);
  NumberConstructor.ReplacePrototype(TGocciaNumberObjectValue.GetSharedPrototype);
  NumberConstructor.Prototype.AssignProperty('constructor', NumberConstructor);
  NumberConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  if Assigned(FBuiltinGlobalNumber) then
    for Key in FBuiltinGlobalNumber.BuiltinObject.GetAllPropertyNames do
      NumberConstructor.SetProperty(Key, FBuiltinGlobalNumber.BuiltinObject.GetProperty(Key));
  FInterpreter.GlobalScope.DefineLexicalBinding('Number', NumberConstructor, dtConst);

  BooleanConstructor := TGocciaBooleanClassValue.Create('Boolean', nil);
  BooleanConstructor.ReplacePrototype(TGocciaBooleanObjectValue.GetSharedPrototype);
  BooleanConstructor.Prototype.AssignProperty('constructor', BooleanConstructor);
  BooleanConstructor.Prototype.Prototype := ObjectConstructor.Prototype;
  FInterpreter.GlobalScope.DefineLexicalBinding('Boolean', BooleanConstructor, dtConst);

  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  FInterpreter.GlobalScope.DefineLexicalBinding('Function', FunctionConstructor, dtConst);

  RegisterGocciaScriptGlobal;
  RegisterGlobalThis;
end;

procedure TGocciaEngine.RegisterGlobalThis;
var
  GlobalThisObj: TGocciaObjectValue;
  Scope: TGocciaScope;
  Name: string;
begin
  Scope := FInterpreter.GlobalScope;
  GlobalThisObj := TGocciaObjectValue.Create;

  for Name in Scope.GetOwnBindingNames do
    GlobalThisObj.AssignProperty(Name, Scope.GetValue(Name));

  GlobalThisObj.AssignProperty('globalThis', GlobalThisObj);
  Scope.DefineLexicalBinding('globalThis', GlobalThisObj, dtConst);
end;

procedure TGocciaEngine.RegisterGocciaScriptGlobal;
const
  PREFIX_LENGTH = 2; // Strip 'gg' prefix from enum names
var
  GocciaObj: TGocciaObjectValue;
  BuiltInsArray: TGocciaArrayValue;
  Flag: TGocciaGlobalBuiltin;
  Name: string;
begin
  BuiltInsArray := TGocciaArrayValue.Create;
  for Flag in FGlobals do
  begin
    Name := GetEnumName(TypeInfo(TGocciaGlobalBuiltin), Ord(Flag));
    BuiltInsArray.Elements.Add(TGocciaStringLiteralValue.Create(Copy(Name, PREFIX_LENGTH + 1, Length(Name) - PREFIX_LENGTH)));
  end;

  GocciaObj := TGocciaObjectValue.Create;
  GocciaObj.AssignProperty('version', TGocciaStringLiteralValue.Create(GetVersion));
  GocciaObj.AssignProperty('commit', TGocciaStringLiteralValue.Create(GetCommit));
  GocciaObj.AssignProperty('builtIns', BuiltInsArray);

  FInterpreter.GlobalScope.DefineLexicalBinding('GocciaScript', GocciaObj, dtConst);
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

function TGocciaEngine.SpeciesGetter(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := AThisValue;
end;

procedure TGocciaEngine.ThrowError(const AMessage: string; const ALine, AColumn: Integer);
begin
  raise TGocciaRuntimeError.Create(AMessage, ALine, AColumn, FFileName, FSourceLines);
end;

end.

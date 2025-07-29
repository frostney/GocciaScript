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
  Goccia.Builtins.TestAssertions, Goccia.Scope, Goccia.Modules,
  Goccia.Values.Interfaces, Goccia.Values.ClassValue, Goccia.Values.Error;

type
  TGocciaGlobalBuiltin = (
    ggConsole,
    ggMath,
    ggGlobalObject,
    ggGlobalArray,
    ggGlobalNumber,
    ggPromise,
    ggJSON,
    ggTestAssertions
  );

  TGocciaGlobalBuiltins = set of TGocciaGlobalBuiltin;

  TGocciaEngine = class
  const DefaultGlobals: TGocciaGlobalBuiltins = [ggConsole, ggMath, ggGlobalObject, ggGlobalArray, ggGlobalNumber, ggPromise, ggJSON];
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
    FBuiltinTestAssertions: TGocciaTestAssertions;

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
    procedure RegisterBuiltinConstructors;
    procedure ThrowError(const Message: string; Line, Column: Integer);
  public
    constructor Create(const AFileName: string; ASourceLines: TStringList; AGlobals: TGocciaGlobalBuiltins);
    destructor Destroy; override;

    function Execute: TGocciaValue;
    function ExecuteProgram(AProgram: TGocciaProgram): TGocciaValue;

    class function RunScript(const Source: string; const FileName: string = 'inline.goccia'; AGlobals: TGocciaGlobalBuiltins = DefaultGlobals): TGocciaValue;
    class function RunScriptFromFile(const FileName: string; AGlobals: TGocciaGlobalBuiltins = DefaultGlobals): TGocciaValue;
    class function RunScriptFromStringList(const Source: TStringList; const FileName: string; AGlobals: TGocciaGlobalBuiltins = DefaultGlobals): TGocciaValue;

    property Interpreter: TGocciaInterpreter read FInterpreter;
    property BuiltinConsole: TGocciaConsole read FBuiltinConsole;
    property BuiltinMath: TGocciaMath read FBuiltinMath;
    property BuiltinGlobalObject: TGocciaGlobalObject read FBuiltinGlobalObject;
    property BuiltinGlobalArray: TGocciaGlobalArray read FBuiltinGlobalArray;
    property BuiltinGlobalNumber: TGocciaGlobalNumber read FBuiltinGlobalNumber;
    property BuiltinGlobals: TGocciaGlobals read FBuiltinGlobals;
    property BuiltinJSON: TGocciaJSON read FBuiltinJSON;
    property BuiltinTestAssertions: TGocciaTestAssertions read FBuiltinTestAssertions;
  end;


implementation

constructor TGocciaEngine.Create(const AFileName: string; ASourceLines: TStringList; AGlobals: TGocciaGlobalBuiltins);
begin
  FFileName := AFileName;
  FSourceLines := ASourceLines;
  FGlobals := AGlobals;

  // Create interpreter without globals
  FInterpreter := TGocciaInterpreter.Create(AFileName, ASourceLines);

  // Register built-ins based on the globals set
  RegisterBuiltIns;
end;

destructor TGocciaEngine.Destroy;
begin
  FInterpreter.Free;
  FBuiltinConsole.Free;
  FBuiltinMath.Free;
  FBuiltinGlobalObject.Free;
  FBuiltinGlobalArray.Free;
  FBuiltinGlobalNumber.Free;
  FBuiltinGlobals.Free;
  FBuiltinJSON.Free;
  FBuiltinTestAssertions.Free;
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
  if ggTestAssertions in FGlobals then RegisterTestAssertions;

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
  FBuiltinTestAssertions := TGocciaTestAssertions.Create(FInterpreter, 'TestAssertions', FInterpreter.GlobalScope, ThrowError);
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

procedure TGocciaEngine.RegisterBuiltinConstructors;
var
  ArrayConstructor, ObjectConstructor, StringConstructor, NumberConstructor: TGocciaClassValue;
  BooleanConstructor, FunctionConstructor, RangeErrorConstructor: TGocciaClassValue;
  ExistingArray, ExistingObject, ExistingNumber: TGocciaValue;
  ArrayObj, ObjectObj, NumberObj: TGocciaObjectValue;
  Key: string;
begin
  // Get existing built-in objects that have static methods
  ExistingArray := FInterpreter.GlobalScope.GetValue('Array');
  ExistingObject := FInterpreter.GlobalScope.GetValue('Object');
  ExistingNumber := FInterpreter.GlobalScope.GetValue('Number');

  // Create Array constructor and copy static methods
  ArrayConstructor := TGocciaClassValue.Create('Array', nil);
  if (ExistingArray is TGocciaObjectValue) then
  begin
    ArrayObj := TGocciaObjectValue(ExistingArray);
    // Copy all static methods from existing Array object using property descriptors
    for Key in ArrayObj.GetAllPropertyNames do
      ArrayConstructor.SetProperty(Key, ArrayObj.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineBuiltin('Array', ArrayConstructor);

  // Create Object constructor and copy static methods
  ObjectConstructor := TGocciaClassValue.Create('Object', nil);
  if (ExistingObject is TGocciaObjectValue) then
  begin
    ObjectObj := TGocciaObjectValue(ExistingObject);
    // Copy all static methods from existing Object using property descriptors
    for Key in ObjectObj.GetAllPropertyNames do
      ObjectConstructor.SetProperty(Key, ObjectObj.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineBuiltin('Object', ObjectConstructor);

  // Create other constructors
  StringConstructor := TGocciaClassValue.Create('String', nil);
  FInterpreter.GlobalScope.DefineBuiltin('String', StringConstructor);

  NumberConstructor := TGocciaClassValue.Create('Number', nil);
  if (ExistingNumber is TGocciaObjectValue) then
  begin
    NumberObj := TGocciaObjectValue(ExistingNumber);
    // Copy all static methods from existing Number object using property descriptors
    for Key in NumberObj.GetAllPropertyNames do
      NumberConstructor.SetProperty(Key, NumberObj.GetProperty(Key));
  end;
  FInterpreter.GlobalScope.DefineBuiltin('Number', NumberConstructor);

  BooleanConstructor := TGocciaClassValue.Create('Boolean', nil);
  FInterpreter.GlobalScope.DefineBuiltin('Boolean', BooleanConstructor);

  // Create Function constructor
  FunctionConstructor := TGocciaClassValue.Create('Function', nil);
  FInterpreter.GlobalScope.DefineBuiltin('Function', FunctionConstructor);

  // Create Error constructors
  RangeErrorConstructor := TGocciaClassValue.Create('RangeError', nil);
  FInterpreter.GlobalScope.DefineBuiltin('RangeError', RangeErrorConstructor);

  // Register Symbol constructor and well-known symbols
  FInterpreter.GlobalScope.Define('Symbol', TGocciaObjectValue.Create);
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
    try
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
      end;
    finally
      Tokens.Free;
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

procedure TGocciaEngine.ThrowError(const Message: string; Line, Column: Integer);
begin
  raise TGocciaRuntimeError.Create(Message, Line, Column, FFileName, FSourceLines);
end;

end.

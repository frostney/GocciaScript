unit G2.Eval;
{ Parsing and compiling at run time: the script entry and the Function constructor. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils,

  G2.Bytecode,
  G2.Compiler,
  G2.Core,
  G2.VM;

{ Goal: see CompileProgram (0 isolated, 1 a Script, 2 global eval code) }
{ 16.2.1.7.1 ParseModule + compile (goal 3): the template, and the module info (the caller owns it); Err /
  ErrName on failure }
function ParseAndCompileModule(const ASource, AFileName: UnicodeString; AId: Integer; var T: PFuncTemplate;
  var AInfo: TModuleInfo; var AErr, AErrName: UnicodeString): Boolean;
function ParseAndCompile(const ASource, AFileName: UnicodeString; AAsExpression: Boolean; var T: PFuncTemplate;
  var AErr: UnicodeString; AGoal: Integer = 0): Boolean;
procedure InitEval;

threadvar
  { why the last ParseAndCompile failed: 1 the parser rejected the source (an early error; ParseErrorName is
    the ECMAScript error it stands for), 2 G2's compiler does not support a construct }
  CompileFailKind: Integer;
  ParseErrorName: UnicodeString;

implementation

uses
  Classes,

  Goccia.AST.Node,
  Goccia.Error,
  Goccia.Lexer,
  Goccia.Parser;

function ParseAndCompileModule(const ASource, AFileName: UnicodeString; AId: Integer; var T: PFuncTemplate;
  var AInfo: TModuleInfo; var AErr, AErrName: UnicodeString): Boolean;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Opts: TGocciaParserOptions;
begin
  Result := False;
  AInfo := nil;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(ASource, AFileName);
      P := TGocciaParser.CreateFromLexer(L, AFileName, nil);
      Opts := P.Options;
      Opts.AutomaticSemicolonInsertion := True;
      Opts.VarDeclarationsEnabled := True;
      Opts.FunctionDeclarationsEnabled := True;
      Opts.TraditionalForLoopsEnabled := True;
      Opts.WhileLoopsEnabled := True;
      Opts.LooseEqualityEnabled := True;
      Opts.LabelStatementsEnabled := True;
      Opts.ForInLoopsEnabled := True;
      Opts.ModuleSource := True;
      Opts.ImportMetaAllowed := True;
      P.ApplyOptions(Opts);
      Prog := P.Parse;
      L.Tokens.Clear;
      CompilingModuleId := AId;
      T := CompileProgram(Prog, AFileName, False, 3);
      AInfo := LastModuleInfo;
      LastModuleInfo := nil;
      Result := True;
    except
      on E: Exception do
      begin
        AErr := UnicodeString(E.Message);
        if E is TGocciaReferenceError then AErrName := 'ReferenceError' else AErrName := 'SyntaxError';
      end;
    end;
  finally
    CompilingModuleId := 0;
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

function ParseAndCompile(const ASource, AFileName: UnicodeString; AAsExpression: Boolean; var T: PFuncTemplate;
  var AErr: UnicodeString; AGoal: Integer): Boolean;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Opts: TGocciaParserOptions;
begin
  Result := False;
  CompileFailKind := 1;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(ASource, AFileName);
      P := TGocciaParser.CreateFromLexer(L, AFileName, nil);   { source lines: built only if an error needs them }
      Opts := P.Options;
      Opts.AutomaticSemicolonInsertion := True;
      Opts.VarDeclarationsEnabled := True;
      Opts.FunctionDeclarationsEnabled := True;
      Opts.TraditionalForLoopsEnabled := True;
      Opts.WhileLoopsEnabled := True;
      Opts.LooseEqualityEnabled := True;
      Opts.NonStrictModeEnabled := True;
      Opts.LabelStatementsEnabled := True;
      Opts.ForInLoopsEnabled := True;
      P.ApplyOptions(Opts);
      CompileFailKind := 1;
      { goal 4 (a direct eval in function code): the private names of its caller's classes are visible; the
        compiler checks each against the caller's bindings (AllPrivateIdentifiersValid) }
      if AGoal = 4 then Prog := P.ParseUnchecked else Prog := P.Parse;
      L.Tokens.Clear;
      CompileFailKind := 2;
      T := CompileProgram(Prog, AFileName, AAsExpression, AGoal);
      CompileFailKind := 0;
      Result := True;
    except
      on E: Exception do
      begin
        AErr := UnicodeString(E.Message);
        if E is EEarlySyntaxError then CompileFailKind := 1;
        if E is TGocciaReferenceError then ParseErrorName := 'ReferenceError' else ParseErrorName := 'SyntaxError';
      end;
    end;
  finally
    { a template keeps nothing of these: the AST, then the parser, then the lexer, which owns the source
      coordinates the AST's spans point at. Before, every eval and Function call leaked all three. }
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

{ 20.2.1.1.1 CreateDynamicFunction: Function, GeneratorFunction, AsyncFunction and AsyncGeneratorFunction
  (NativeData 0-3) }
function Function_Ctor(AArgs: PValue; AArgc: Integer; var ARes: TValue): Boolean;
const
  Heads: array[0..3] of UnicodeString = ('(function anonymous(', '(function* anonymous(', '(async function anonymous(',
    '(async function* anonymous(');
var I: Integer; Params, Body, Src, Err: UnicodeString; S: TValue; T: PFuncTemplate; Clo: TValue; Kind: Integer;
begin
  Kind := NativeData;
  Params := '';
  Body := '';
  for I := 1 to AArgc do
  begin
    if not ToStringV(AArgs[I], S) then Exit(False);
    if I = AArgc then Body := StrToUnicode(StrOf(S))
    else
    begin
      if Params <> '' then Params := Params + ',';
      Params := Params + StrToUnicode(StrOf(S));
    end;
  end;
  Src := Heads[Kind] + Params + #10') {'#10 + Body + #10'})';
  if not ParseAndCompile(Src, '<Function>', True, T, Err) then Exit(ThrowError('SyntaxError', Err));
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, VUndefined, nil, 0, ARes);
end;

function AsyncIter_Self(AArgs: PValue; AArgc: Integer; var ARes: TValue): Boolean;
begin
  ARes := AArgs[0];
  Result := True;
end;

procedure InitEval;
var F: TValue;
  function Proto(AParent: TValue; var ASlot: TValue): TValue;
  begin
    AddRoot(@ASlot);
    ASlot := MkPtr(NewPlainObject(AParent, 8));
    Result := ASlot;
  end;
  procedure Ctor(const AName: UnicodeString; AKind: Integer; AProtoV: TValue);
  var C: TValue;
  begin
    PushRoot(AProtoV);
    C := NewNativeFunctionData(Function_Ctor, AName, 1, AKind);
    PushRoot(C);
    SetProto(ObjOf(C), RootAt(TempTop - 3));   { [[Prototype]]: %Function% }
    DefineOwn(RootAt(TempTop - 1), AtomPrototype, RootAt(TempTop - 2), 0);
    DefineOwn(RootAt(TempTop - 2), AtomConstructor, RootAt(TempTop - 1), pfConfigurable);
    PopRoots(2);
  end;
  procedure Tag(O: TValue; const T: UnicodeString);
  begin
    DefineOwn(O, SymKey(SymToStringTagW), MkPtr(AtomOf(T)), pfConfigurable);
  end;
begin
  F := NewNativeFunction(Function_Ctor, 'Function', 1);
  PushRoot(F);
  DefineOwn(RootAt(TempTop - 1), AtomPrototype, FunctionProto, 0);
  DefineOwn(FunctionProto, AtomConstructor, RootAt(TempTop - 1), pfHidden);
  DefineOwn(GlobalObj, AtomOf('Function'), RootAt(TempTop - 1), pfHidden);
  { 27.3 - 27.7: the function and iterator intrinsics of generators and async code }
  Proto(FunctionProto, GenFunctionProto);
  Proto(FunctionProto, AsyncFunctionProto);
  Proto(FunctionProto, AsyncGenFunctionProto);
  Proto(ObjectProto, AsyncIteratorProto);
  Proto(AsyncIteratorProto, AsyncGeneratorProto);
  Proto(AsyncIteratorProto, AsyncFromSyncIterProto);
  Ctor('GeneratorFunction', 1, GenFunctionProto);
  Ctor('AsyncFunction', 2, AsyncFunctionProto);
  Ctor('AsyncGeneratorFunction', 3, AsyncGenFunctionProto);
  DefineOwn(GenFunctionProto, AtomPrototype, GeneratorProto, pfConfigurable);
  DefineOwn(GeneratorProto, AtomConstructor, GenFunctionProto, pfConfigurable);
  DefineOwn(AsyncGenFunctionProto, AtomPrototype, AsyncGeneratorProto, pfConfigurable);
  DefineOwn(AsyncGeneratorProto, AtomConstructor, AsyncGenFunctionProto, pfConfigurable);
  Tag(GenFunctionProto, 'GeneratorFunction');
  Tag(AsyncFunctionProto, 'AsyncFunction');
  Tag(AsyncGenFunctionProto, 'AsyncGeneratorFunction');
  Tag(AsyncGeneratorProto, 'AsyncGenerator');
  F := NewNativeFunction(AsyncIter_Self, '[Symbol.asyncIterator]', 0);
  DefineOwn(AsyncIteratorProto, SymKey(SymAsyncIterator), F, pfHidden);
  GenFunctionShape := FnShapeFor(GenFunctionProto);
  AsyncFunctionShape := FnShapeFor(AsyncFunctionProto);
  AsyncGenFunctionShape := FnShapeFor(AsyncGenFunctionProto);
  PopRoots(1);
end;

end.

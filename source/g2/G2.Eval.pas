unit G2.Eval;
{ Parsing and compiling at run time: the script entry and the Function constructor. }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  SysUtils, G2.Core, G2.Bytecode, G2.VM, G2.Compiler;

{ Goal: see CompileProgram (0 isolated, 1 a Script, 2 global eval code) }
{ 16.2.1.7.1 ParseModule + compile (goal 3): the template, and the module info (the caller owns it); Err /
  ErrName on failure }
function ParseAndCompileModule(const Source, FileName: UnicodeString; Id: Integer; var T: PFuncTemplate;
  var Info: TModuleInfo; var Err, ErrName: UnicodeString): Boolean;
function ParseAndCompile(const Source, FileName: UnicodeString; AsExpression: Boolean; var T: PFuncTemplate;
  var Err: UnicodeString; Goal: Integer = 0): Boolean;
procedure InitEval;

threadvar
  { why the last ParseAndCompile failed: 1 the parser rejected the source (an early error; ParseErrorName is
    the ECMAScript error it stands for), 2 G2's compiler does not support a construct }
  CompileFailKind: Integer;
  ParseErrorName: UnicodeString;

implementation

uses
  Classes, Linux, UnixType, Goccia.Lexer, Goccia.Parser, Goccia.AST.Node, Goccia.AST.Expressions, Goccia.AST.Statements, Goccia.Error, Goccia.SourceSpan, G2.Scan,
  G2.PreParse, G2.Lazy;

threadvar
  LazyMode: Integer;   { G2_LAZY: 0 not read yet, 1 off (G2_LAZY=0), 2 on (the default) }

function LazyOn: Boolean;
begin
  if LazyMode = 0 then
    if GetEnvironmentVariable('G2_LAZY') = '0' then LazyMode := 1 else LazyMode := 2;
  Result := LazyMode = 2;
end;

procedure ScriptOptions(P: TGocciaParser);
var Opts: TGocciaParserOptions;
begin
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
end;

{ LastModuleInfo, taken: through a noinline routine (an inline threadvar load here got FPC 3.3.1's absolute
  relocation, tlsscan.py) }
function TakeLastModuleInfo: TModuleInfo; noinline;
begin
  Result := LastModuleInfo;
  LastModuleInfo := nil;
end;

procedure ModuleOptions(P: TGocciaParser);
var Opts: TGocciaParserOptions;
begin
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
end;

{ a module with lazy functions: as LazyParseAndCompile, goal 3 }
function LazyParseAndCompileModule(const Source, FileName: UnicodeString; Id: Integer; var T: PFuncTemplate;
  var Info: TModuleInfo; var Err, ErrName: UnicodeString; out Ok: Boolean): Boolean;
var PP: TPreParser; LS: TLazySource; L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram;
begin
  Result := False;
  Ok := False;
  LS := nil;
  PP := TPreParser.Create(PWideChar(Source), Length(Source), pgModule, True);
  try
    try
      PP.ParseProgram;
      LS := BuildLazySource(Source, FileName, PP);
    except
      on EScanError do Exit;
    end;
  finally
    PP.Free;
  end;
  if LS = nil then Exit;
  Result := True;
  LS.IsModule := True;
  LS.ModuleId := Id;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(LS.Text, FileName);
      LS.Coords := L.SourceCoordinates;
      LS.CoordsRef := LS.Coords;
      L.StandardIdentifiers := True;
      P := TGocciaParser.CreateFromLexer(L, FileName, nil);
      ModuleOptions(P);
      P.SetFunctionBodySkipper(@LazyBodySkipper, LS);
      P.SetEarlyErrorsChecked(True);
      Prog := P.Parse;
      L.Tokens.Clear;
      CompilingModuleId := Id;
      CurLazySource := LS;
      try
        T := CompileProgram(Prog, FileName, False, 3);
      finally
        CurLazySource := nil;
      end;
      Info := TakeLastModuleInfo;
      Ok := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is TGocciaReferenceError then ErrName := 'ReferenceError' else ErrName := 'SyntaxError';
      end;
    end;
  finally
    CompilingModuleId := 0;
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

{ a Script with lazy functions (G2.Lazy): pre-parsed, then parsed with the eligible bodies skipped and compiled
  with stubs for them. False when the source goes the eager way (a syntax error, whose message the eager parse
  gives, or nothing to defer); otherwise Ok and Err are the result }
function LazyParseAndCompile(const Source, FileName: UnicodeString; var T: PFuncTemplate; var Err: UnicodeString;
  out Ok: Boolean): Boolean;
var PP: TPreParser; LS: TLazySource; L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram;
begin
  Result := False;
  Ok := False;
  LS := nil;
  PP := TPreParser.Create(PWideChar(Source), Length(Source), pgScript, False);
  try
    try
      PP.ParseProgram;
      LS := BuildLazySource(Source, FileName, PP);
    except
      on EScanError do Exit;
    end;
  finally
    PP.Free;
  end;
  if LS = nil then Exit;
  Result := True;
  CompileFailKind := 1;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(LS.Text, FileName);
      LS.Coords := L.SourceCoordinates;
      LS.CoordsRef := LS.Coords;
      L.HTMLComments := True;
      L.StandardIdentifiers := True;
      P := TGocciaParser.CreateFromLexer(L, FileName, nil);
      ScriptOptions(P);
      P.SetFunctionBodySkipper(@LazyBodySkipper, LS);
      P.SetEarlyErrorsChecked(True);
      Prog := P.Parse;
      L.Tokens.Clear;
      CompileFailKind := 2;
      CurLazySource := LS;
      try
        T := CompileProgram(Prog, FileName, False, 1);
      finally
        CurLazySource := nil;
      end;
      CompileFailKind := 0;
      Ok := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is EEarlySyntaxError then CompileFailKind := 1;
        if E is TGocciaReferenceError then ParseErrorName := 'ReferenceError' else ParseErrorName := 'SyntaxError';
      end;
    end;
  finally
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

type
  { the function node whose body block starts at Target (a lazily compiled method, in its container's parse) }
  TBodyFinder = class(TGocciaASTChildVisitor)
  public
    Target: Integer;
    Found: Boolean;
    Params: TGocciaParameterArray;
    Body: TGocciaASTNode;
    procedure VisitChild(const ANode: TGocciaASTNode); override;
    procedure EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
      const AFlags: TGocciaASTPartFlags); override;
    procedure LeavePart; override;
  end;

procedure TBodyFinder.VisitChild(const ANode: TGocciaASTNode);
var B: TGocciaASTNode;
begin
  if (ANode = nil) or Found then Exit;
  B := nil;
  if ANode is TGocciaFunctionExpression then B := TGocciaFunctionExpression(ANode).Body
  else if ANode is TGocciaArrowFunctionExpression then B := TGocciaArrowFunctionExpression(ANode).Body
  else if ANode is TGocciaGetterExpression then B := TGocciaGetterExpression(ANode).Body
  else if ANode is TGocciaSetterExpression then B := TGocciaSetterExpression(ANode).Body
  else if ANode is TGocciaClassMethod then B := TGocciaClassMethod(ANode).Body;
  if (B <> nil) and (B is TGocciaBlockStatement) and (B.Span.StartOffset = Target) then
  begin
    Found := True;
    Body := B;
    if ANode is TGocciaFunctionExpression then Params := TGocciaFunctionExpression(ANode).Parameters
    else if ANode is TGocciaArrowFunctionExpression then Params := TGocciaArrowFunctionExpression(ANode).Parameters
    else if ANode is TGocciaSetterExpression then Params := TGocciaSetterExpression(ANode).Parameters
    else if ANode is TGocciaClassMethod then Params := TGocciaClassMethod(ANode).Parameters
    else Params := nil;
    Exit;
  end;
  ANode.ForEachChild(Self);
end;

procedure TBodyFinder.EnterPart(const AKind: TGocciaASTPartKind; const AStartOffset, AEndOffset: Integer;
  const AFlags: TGocciaASTPartFlags);
begin
end;

procedure TBodyFinder.LeavePart;
begin
end;

{ LazyCompileHook: a lazy template's function, parsed from its own source range and compiled, moved into the
  template (its closures keep the stub's upvalues, name and source text) }
var
  { G2_LAZY_DEBUG set: 1, not: 0, not read yet: -1 }
  LazyDebug: Integer = -1;

function LazyCompile(T: PFuncTemplate): Boolean;
var St: PLazyStub; LS, PrevSrc: TLazySource; R: ^TLazyRec; L: TGocciaLexer; P: TGocciaParser; E: TGocciaExpression;
  NT: PFuncTemplate; SaveUp: array of TUpvalDesc; SaveName: PStr; SaveRange: TGocciaLazySourceText;
  Finder: TBodyFinder; IsMethod: Boolean; PrevModule: Integer;
  Arena, PrevArena: TGocciaASTArena; Msg: UnicodeString; TS0, TS1, TSa, TSb: TTimeSpec; RStart, RStop: Integer;
begin
  clock_gettime(CLOCK_MONOTONIC, @TS0);
  St := PLazyStub(T^.Lazy);
  LS := St^.Src;
  R := @LS.Recs[St^.Id];
  if LazyDebug < 0 then
    if GetEnvironmentVariable('G2_LAZY_DEBUG') <> '' then LazyDebug := 1 else LazyDebug := 0;
  if LazyDebug = 1 then
    WriteLn(ErrOutput, 'lazy compile ', St^.CompileName, ' (', LS.FileName, ' ', R^.Start, '..', R^.Stop, ', module ',
      LS.IsModule, ', method ', R^.CStart >= 0, ')');
  L := nil; P := nil; Finder := nil;
  Msg := '';
  IsMethod := R^.CStart >= 0;
  Arena := TGocciaASTArena.Create;
  try
    try
      { a method is parsed with its object literal or class, the other bodies there skipped }
      if IsMethod then begin RStart := R^.CStart; RStop := R^.CStop; end
      else begin RStart := R^.Start; RStop := R^.Stop; end;
      if (LS.Lexer <> nil) and not LS.LexerBusy then
      begin
        L := TGocciaLexer(LS.Lexer);
        L.Restart(RStart, RStop);
      end
      else
      begin
        L := TGocciaLexer.Create(LS.Text, LS.FileName, LS.Coords);
        L.HTMLComments := not LS.IsModule;
        L.StandardIdentifiers := True;
        L.SetRange(RStart, RStop);
        if LS.Lexer = nil then LS.Lexer := L;
      end;
      if L = LS.Lexer then LS.LexerBusy := True;
      P := TGocciaParser.CreateFromLexer(L, LS.FileName, nil);
      if LS.IsModule then ModuleOptions(P) else ScriptOptions(P);
      if IsMethod then
      begin
        { the container's own context }
        P.SetStrictMode(ffCtxStrict in R^.Flags);
        P.SetFunctionContext(ffCtxAwait in R^.Flags, ffCtxYield in R^.Flags);
      end
      else P.SetStrictMode(St^.IsStrict);
      P.SetFunctionBodySkipper(@LazyBodySkipper, LS);
      P.SetEarlyErrorsChecked(True);
      LS.Compiling := St^.Id;
      LS.SkipOutside := IsMethod;
      LS.TargetStart := R^.Start;
      LS.TargetStop := R^.Stop;
      PrevArena := ActivateASTArena(Arena);
      clock_gettime(CLOCK_MONOTONIC, @TSa);
      try
        E := P.ParseExpressionUnchecked;
      finally
        RestoreASTArena(PrevArena);
        LS.Compiling := -1;
        LS.SkipOutside := False;
      end;
      clock_gettime(CLOCK_MONOTONIC, @TSb);
      Inc(LazySetupNs, (Int64(TSa.tv_sec) - TS0.tv_sec) * 1000000000 + (TSa.tv_nsec - TS0.tv_nsec));
      Inc(LazyParseNs, (Int64(TSb.tv_sec) - TSa.tv_sec) * 1000000000 + (TSb.tv_nsec - TSa.tv_nsec));
      Inc(LazyTokens, L.Tokens.Count);
      Inc(LazyChars, R^.Stop - R^.Start);
      Finder := TBodyFinder.Create;
      Finder.Target := R^.Body;
      Finder.VisitChild(E);
      if not Finder.Found or not P.AtEnd then
        raise Exception.Create('lazy compile: the function is not in its source range (' + E.ClassName + ' found ' +
          BoolToStr(Finder.Found, True) + ' at end ' + BoolToStr(P.AtEnd, True) + ')');
      PrevSrc := CurLazySource;
      PrevModule := CompilingModuleId;
      CurLazySource := LS;
      if LS.IsModule then CompilingModuleId := LS.ModuleId else CompilingModuleId := 0;
      try
        NT := CompileLazyFunction(Finder.Params, Finder.Body, St, LS.FileName);
      finally
        CurLazySource := PrevSrc;
        CompilingModuleId := PrevModule;
      end;
      SaveUp := T^.Upvals;
      SaveName := T^.Name;
      SaveRange := T^.SourceRange;
      T^ := NT^;
      T^.Upvals := SaveUp;
      T^.Name := SaveName;
      T^.SourceRange := SaveRange;
      T^.Lazy := nil;
      FinishTemplate(T);
      { NT stays registered, emptied: T owns its arrays and IC memory now }
      Finalize(NT^);
      FillChar(NT^, SizeOf(TFuncTemplate), 0);
      Dispose(St);
    except
      on Ex: Exception do Msg := UnicodeString(Ex.Message);
    end;
  finally
    Finder.Free;
    P.Free;
    if L = LS.Lexer then
    begin
      LS.LexerBusy := False;
      L.Tokens.Reset;
    end
    else L.Free;
    Arena.Free;
  end;
  clock_gettime(CLOCK_MONOTONIC, @TS1);
  Inc(LazyCompileCount);
  Inc(LazyCompileNs, (Int64(TS1.tv_sec) - TS0.tv_sec) * 1000000000 + (TS1.tv_nsec - TS0.tv_nsec));
  if Msg <> '' then Exit(ThrowError('SyntaxError', Msg));
  Result := True;
end;

function ParseAndCompileModule(const Source, FileName: UnicodeString; Id: Integer; var T: PFuncTemplate;
  var Info: TModuleInfo; var Err, ErrName: UnicodeString): Boolean;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Opts: TGocciaParserOptions;
begin
  Info := nil;
  if LazyOn and LazyParseAndCompileModule(Source, FileName, Id, T, Info, Err, ErrName, Result) then Exit;
  Result := False;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(Source, FileName);
      L.StandardIdentifiers := True;
      P := TGocciaParser.CreateFromLexer(L, FileName, nil);
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
      CompilingModuleId := Id;
      T := CompileProgram(Prog, FileName, False, 3);
      Info := LastModuleInfo;
      LastModuleInfo := nil;
      Result := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
        if E is TGocciaReferenceError then ErrName := 'ReferenceError' else ErrName := 'SyntaxError';
      end;
    end;
  finally
    CompilingModuleId := 0;
    Prog.Free;
    P.Free;
    L.Free;
  end;
end;

function ParseAndCompile(const Source, FileName: UnicodeString; AsExpression: Boolean; var T: PFuncTemplate;
  var Err: UnicodeString; Goal: Integer): Boolean;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Opts: TGocciaParserOptions;
begin
  if (Goal = 1) and not AsExpression and LazyOn and LazyParseAndCompile(Source, FileName, T, Err, Result) then Exit;
  Result := False;
  CompileFailKind := 1;
  L := nil; P := nil; Prog := nil;
  try
    try
      L := TGocciaLexer.Create(Source, FileName);
      L.HTMLComments := True;   { B.1.1: Script, eval and Function code }
      L.StandardIdentifiers := True;
      P := TGocciaParser.CreateFromLexer(L, FileName, nil);   { source lines: built only if an error needs them }
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
      if Goal = 4 then Prog := P.ParseUnchecked else Prog := P.Parse;
      L.Tokens.Clear;
      CompileFailKind := 2;
      T := CompileProgram(Prog, FileName, AsExpression, Goal);
      CompileFailKind := 0;
      Result := True;
    except
      on E: Exception do
      begin
        Err := UnicodeString(E.Message);
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

{ whether parameter text could open something (a comment, string, template, regular expression, bracket or
  HTML-like comment) that the body text closes: only then do the parameters need a parse of their own }
function ParamsMayEscape(const P: UnicodeString): Boolean;
var I: Integer;
begin
  for I := 1 to Length(P) do
    case P[I] of
      '/', '`', '''', '"', '(', ')', '[', ']', '{', '}', '<', '-', '\': Exit(True);
    end;
  Result := False;
end;

{ 20.2.1.1.1 CreateDynamicFunction: Function, GeneratorFunction, AsyncFunction and AsyncGeneratorFunction
  (NativeData 0-3) }
function Function_Ctor(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
const
  Heads: array[0..3] of UnicodeString = ('(function anonymous(', '(function* anonymous(', '(async function anonymous(',
    '(async function* anonymous(');
var I: Integer; Params, Body, Src, Err: UnicodeString; S: TValue; T: PFuncTemplate; Clo: TValue; Kind: Integer;
  Ok: Boolean;
begin
  Kind := NativeData;
  Params := '';
  Body := '';
  for I := 1 to Argc do
  begin
    if not ToStringV(Args[I], S) then Exit(False);
    if I = Argc then Body := StrToUnicode(StrOf(S))
    else
    begin
      if Params <> '' then Params := Params + ',';
      Params := Params + StrToUnicode(StrOf(S));
    end;
  end;
  { 20.2.1.1.1 steps 15-18: the parameters parse as FormalParameters on their own (a comment, template or
    function they open does not end in the body) }
  if ParamsMayEscape(Params) then
  begin
    DynFuncAnonymous := True;
    try
      Ok := ParseAndCompile(Heads[Kind] + Params + #10') {'#10'})', '<Function>', True, T, Err);
    finally
      DynFuncAnonymous := False;
    end;
    if not Ok then Exit(ThrowError('SyntaxError', Err));
  end;
  Src := Heads[Kind] + Params + #10') {'#10 + Body + #10'})';
  DynFuncAnonymous := True;
  try
    Ok := ParseAndCompile(Src, '<Function>', True, T, Err);
  finally
    DynFuncAnonymous := False;
  end;
  if not Ok then Exit(ThrowError('SyntaxError', Err));
  if not MakeScriptClosure(T, Clo) then Exit(False);
  Result := CallValue(Clo, VUndefined, nil, 0, Res);
  { and the body as a FunctionBody: the function the source makes ends at the brace added after it }
  if Result and IsObject(Res) and (ObjOf(Res)^.OKind = okFunction) and
     (UTF8Decode(PFuncTemplate(ObjOf(Res)^.Native)^.SourceRange.Get) <> Copy(Src, 2, Length(Src) - 2)) then
    Result := ThrowError('SyntaxError', 'Function body is not a FunctionBody');
end;

function AsyncIter_Self(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := Args[0];
  Result := True;
end;

procedure InitEval;
var F: TValue;
  function Proto(Parent: TValue; var Slot: TValue): TValue;
  begin
    AddRoot(@Slot);
    Slot := MkPtr(NewPlainObject(Parent, 8));
    Result := Slot;
  end;
  procedure Ctor(const Name: UnicodeString; Kind: Integer; ProtoV: TValue);
  var C: TValue;
  begin
    PushRoot(ProtoV);
    C := NewNativeFunctionData(Function_Ctor, Name, 1, Kind);
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
  LazyCompileHook := @LazyCompile;
  PreParseRegExpCheck := RegExpValidateHook;
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

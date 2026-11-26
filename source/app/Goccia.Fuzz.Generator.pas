unit Goccia.Fuzz.Generator;

{ Turns a fuzzer's input into a GocciaScript program that parses.

  Each byte of the input decides one choice of the grammar: which statement
  comes next, which expression fills a slot, which binding it names. A
  fuzzer that mutates the bytes mutates the program at the level of its
  syntax, and every input, whatever its bytes, is a program the parser
  accepts, so no run is spent on a parse error. When the input runs out,
  every further choice is the first one, and the first one is always the
  smallest: a statement list ends, an expression is a literal. A byte set to
  zero therefore removes or simplifies what it decided, which is what a
  reducer needs.

  The program is deterministic, so two runs of it print the same lines: the
  harness runs it in the interpreter and with the JIT and compares them. It
  reads no clock, no random numbers and no host state, and every loop runs a
  constant number of times. Only recursion through a value the generator
  cannot see (a method an object was given later, an accessor) can run until
  the frame limit, and the harness does not compare a run that reached it.
  Functions are called hot, enough times for the baseline JIT and the
  optimizing tier, with arguments whose types can change late in the loop,
  so compiled code meets a deoptimization; each call's result goes into a
  digest the program prints.

  The bindings in scope carry the type their value was made with, and the
  expressions that use them are chosen for it: a number gets arithmetic, an
  array its methods. The type is a hint, not a guarantee (an assignment can
  change it, an element read can be anything), so a wrong guess makes a
  TypeError the program catches, never an invalid program.

  The generator estimates the work each statement does (how often the loops
  around it run it, and what the functions it calls cost) and stops adding
  loops, calls and statements past a budget, so the interpreter run stays
  within the harness's instruction limit. }

{$I Goccia.inc}

interface

uses
  SysUtils;

{ The program AData decides. }
function GenerateFuzzProgram(const AData: TBytes): string;

implementation

uses
  StringBuffer;

const
  MAX_EXPR_DEPTH = 4;
  MAX_BLOCK_DEPTH = 4;
  MAX_BLOCK_STATEMENTS = 6;
  MAX_TOP_ITEMS = 32;
  MAX_PARAMS = 3;
  { the product of the iteration counts of the loops around a statement }
  MAX_LOOP_WORK = 2048;
  { the estimated statements one call of a function runs, and the program's
    whole run; a hot loop iteration costs HOT_ITERATION_WORK on top of the
    call, for the digest }
  MAX_FUNCTION_WORK = 4000;
  MAX_PROGRAM_WORK = 200000;
  HOT_ITERATION_WORK = 24;
  { what a call through a value the generator cannot see is assumed to
    cost: a method, an accessor }
  DYNAMIC_CALL_WORK = 64;

  { The helpers every program starts with. __s renders a value the same
    way in every run, without the console's formatting; __e an error;
    __mix folds a value into a digest; __l prints a line. }
  FUZZ_PRELUDE =
    'let __u;' + #10 +
    'const __e = (e) => { try { return (e !== null && typeof e === "object" && ' +
    'typeof e.constructor === "function") ? e.constructor.name + ":" + String(e.message) : ' +
    '__s(e, 0); } catch (x) { return "?"; } };' + #10 +
    'const __s = (v, d) => {' + #10 +
    '  const t = typeof v;' + #10 +
    '  if (t === "number") return Object.is(v, -0) ? "-0" : String(v);' + #10 +
    '  if (t === "string") return JSON.stringify(v);' + #10 +
    '  if (t === "bigint") return String(v) + "n";' + #10 +
    '  if (t === "symbol") return "Symbol(" + String(v.description) + ")";' + #10 +
    '  if (t === "function") return "fn/" + v.length;' + #10 +
    '  if (t !== "object") return String(v);' + #10 +
    '  if (v === null) return "null";' + #10 +
    '  if (d > 2) return "{..}";' + #10 +
    '  try {' + #10 +
    '    if (Array.isArray(v)) return "[" + v.slice(0, 8).map((x) => __s(x, d + 1)).join(",") + ' +
    '(v.length > 8 ? ",.." + v.length : "") + "]";' + #10 +
    '    if (v instanceof Map) return "Map" + v.size;' + #10 +
    '    if (v instanceof Set) return "Set" + v.size;' + #10 +
    '    if (ArrayBuffer.isView(v)) return "TA" + v.length + "[" + ' +
    'Array.prototype.slice.call(v, 0, 8).join(",") + "]";' + #10 +
    '    if (v instanceof Error) return __e(v);' + #10 +
    '    const ks = Object.keys(v).slice(0, 8);' + #10 +
    '    return "{" + ks.map((k) => k + ":" + __s(v[k], d + 1)).join(",") + "}";' + #10 +
    '  } catch (x) { return "!" + __e(x); }' + #10 +
    '};' + #10 +
    'const __mix = (h, v) => { const t = __s(v, 1); let r = h; ' +
    'for (let k = 0; k < t.length; k++) { r = (Math.imul(r, 31) + t.charCodeAt(k)) | 0; } return r; };' + #10 +
    'const __l = (...a) => { let t = ""; for (let k = 0; k < a.length; k++) { ' +
    't = t + (k > 0 ? " " : "") + __s(a[k], 0); } console.log(t); };' + #10;

  NUM_LITERALS: array[0..38] of string = ('0', '1', '2', '3', '7', '10', '0.5',
    '1.5', '2.5', '-1', '-0', '-2.5', '255', '256', '1024', '65535', '65536',
    '1073741823', '1073741824', '-1073741825', '2147483647', '2147483648',
    '-2147483648', '-2147483649', '4294967295', '4294967296',
    '9007199254740991', '9007199254740992', '-9007199254740993', '1e21',
    '1e-7', '0.1', '0.2', 'NaN', 'Infinity', '-Infinity', 'Number.MAX_VALUE',
    'Number.MIN_VALUE', 'Number.EPSILON');
  INT_LITERALS: array[0..15] of string = ('0', '1', '2', '3', '4', '5', '7',
    '8', '-1', '15', '16', '31', '32', '63', '100', '-2');
  STR_LITERALS: array[0..21] of string = ('""', '"a"', '"abc"', '"x"', '"0"',
    '"10"', '"-1.5"', '" a b "', '"été"', '"😀"',
    '"\ud800"', '"length"', '"__proto__"', '"constructor"', '"toString"',
    '"1e3"', '"true"', '"null"', '"a,b,c"', '"0x1f"', '"  12  "', '"aaaa"');
  BIG_LITERALS: array[0..7] of string = ('0n', '1n', '2n', '-1n', '(2n ** 64n)',
    '(-(2n ** 63n))', '123456789012345678901234567890n', '255n');
  KEY_NAMES: array[0..8] of string = ('a', 'b', 'c', 'x', 'y', 'g', 'm',
    'length', 'constructor');
  KEY_LITERALS: array[0..5] of string = ('"a"', '"b"', '0', '1', '"0"',
    '"x"');
  REGEX_LITERALS: array[0..7] of string = ('/a/', '/a+/g', '/[aeiou]/g',
    '/(\d+)/', '/^\s+|\s+$/g', '/,/', '/(a)(b)?/', '/./gu');
  MATH_UNARY: array[0..16] of string = ('abs', 'floor', 'ceil', 'round',
    'trunc', 'sign', 'sqrt', 'cbrt', 'sin', 'cos', 'tan', 'exp', 'log',
    'log2', 'clz32', 'fround', 'expm1');
  MATH_BINARY: array[0..5] of string = ('max', 'min', 'pow', 'atan2', 'imul',
    'hypot');
  ERROR_TYPES: array[0..3] of string = ('Error', 'TypeError', 'RangeError',
    'SyntaxError');
  HOT_COUNTS: array[0..7] of Integer = (1, 3, 25, 60, 300, 1100, 1500, 2500);
  LOOP_COUNTS: array[0..7] of Integer = (0, 1, 2, 3, 8, 17, 40, 130);

type
  TFuzzType = (ftAny, ftNum, ftInt, ftStr, ftBool, ftArr, ftObj, ftBig, ftFun,
    ftClass);

  TFuzzBinding = record
    Name: string;
    Kind: TFuzzType;
    Mutable: Boolean;
    { a function's or class constructor's parameters }
    Params: array of TFuzzType;
    { a generator function: a call is spread into an array }
    Generator: Boolean;
    { an async function: a call is a promise }
    Async: Boolean;
    { the estimated statements one call runs }
    Work: Integer;
  end;

  TFuzzGenerator = class
  private
    FData: TBytes;
    FPos: Integer;
    FOut: TStringBuffer;
    FIndent: Integer;
    FScope: array of TFuzzBinding;
    FScopeCount: Integer;
    FNextId: Integer;
    FExprDepth: Integer;
    FBlockDepth: Integer;
    { how often the statement being generated runs per call of its function
      (or per program run at the top level) }
    FLoopWork: Integer;
    { the estimated statements the current function runs per call }
    FWork: Integer;
    FWorkLimit: Integer;
    { break and continue targets: the labels of the enclosing loops (those of
      the current function from FLabelBase on), and whether a switch
      encloses the statement }
    FLabels: array of string;
    FLabelCount: Integer;
    FLabelBase: Integer;
    FLoopDepth: Integer;
    FSwitchDepth: Integer;
    FFunctionDepth: Integer;
    FInAsync: Boolean;
    FInGenerator: Boolean;
    { inside a class body: `this`, `super` and the class's private names }
    FInMethod: Boolean;
    FInDerived: Boolean;
    FTag: Integer;

    function Choose(ACount: Integer): Integer;
    function Maybe(AOneIn: Integer): Boolean;
    function Pick(const AItems: array of string): string;
    function NewName(const APrefix: string): string;
    function NextTag: string;

    procedure Line(const AText: string);
    procedure Indent;
    procedure Dedent;

    function AddBinding(const AName: string; AKind: TFuzzType;
      AMutable: Boolean): Integer;
    function FindBinding(AKind: TFuzzType; AMutableOnly, AAllowAny: Boolean;
      out AIndex: Integer): Boolean;
    function HasWorkFor(AWork: Integer): Boolean;
    procedure AddWork(AWork: Integer);

    function ValueKind: TFuzzType;
    function Recv(const AExpr: string): string;
    function Leaf(AKind: TFuzzType): string;
    function Literal(AKind: TFuzzType): string;
    function Expr(AKind: TFuzzType): string;
    function NumExpr: string;
    function IntExpr: string;
    function StrExpr: string;
    function BoolExpr: string;
    function ArrExpr: string;
    function ObjExpr: string;
    function BigExpr: string;
    function FunExpr: string;
    function AnyExpr: string;
    function Lambda(AParams: Integer; AResult: TFuzzType): string;
    function CallExpr(out AText: string): Boolean;
    function ArgsFor(const AParams: array of TFuzzType): string;
    function PropertyRead: string;
    function Target(out AText: string; out AIndex: Integer): Boolean;

    procedure Block(AMaxStatements: Integer);
    procedure Statement;
    procedure DeclareStatement;
    procedure AssignStatement;
    procedure MutateStatement;
    procedure IfStatement;
    procedure LoopBody(AIterations: Integer; const ALabel: string);
    procedure ForStatement;
    procedure ForOfStatement;
    procedure ForInStatement;
    procedure WhileStatement;
    procedure SwitchStatement;
    procedure TryStatement;
    procedure JumpStatement;
    procedure DestructureStatement;
    procedure LogStatement;
    procedure ObjectModelStatement;

    procedure FunctionDefinition(ATopLevel: Boolean);
    procedure ClassDefinition;
    procedure HotCall;
    procedure AsyncBlock;
    procedure TopLevelItem;
  public
    constructor Create(const AData: TBytes);
    function Generate: string;
  end;

{ ---- choices ---- }

constructor TFuzzGenerator.Create(const AData: TBytes);
begin
  inherited Create;
  FData := AData;
  FOut := TStringBuffer.Create(4096);
  FLoopWork := 1;
  FWorkLimit := MAX_PROGRAM_WORK;
end;

function TFuzzGenerator.Choose(ACount: Integer): Integer;
begin
  if (ACount <= 1) or (FPos >= Length(FData)) then
    Exit(0);
  Result := FData[FPos];
  Inc(FPos);
  if ACount > 256 then
  begin
    if FPos < Length(FData) then
    begin
      Result := Result or (Integer(FData[FPos]) shl 8);
      Inc(FPos);
    end;
  end;
  Result := Result mod ACount;
end;

{ True one time in AOneIn; never once the input has run out }
function TFuzzGenerator.Maybe(AOneIn: Integer): Boolean;
begin
  Result := Choose(AOneIn) = AOneIn - 1;
end;

function TFuzzGenerator.Pick(const AItems: array of string): string;
begin
  Result := AItems[Choose(Length(AItems))];
end;

function TFuzzGenerator.NewName(const APrefix: string): string;
begin
  Result := APrefix + IntToStr(FNextId);
  Inc(FNextId);
end;

function TFuzzGenerator.NextTag: string;
begin
  Result := '"t' + IntToStr(FTag) + '"';
  Inc(FTag);
end;

procedure TFuzzGenerator.Line(const AText: string);
begin
  FOut.Append(StringOfChar(' ', FIndent * 2) + AText + #10);
end;

procedure TFuzzGenerator.Indent;
begin
  Inc(FIndent);
end;

procedure TFuzzGenerator.Dedent;
begin
  Dec(FIndent);
end;

{ ---- scope ---- }

function TFuzzGenerator.AddBinding(const AName: string; AKind: TFuzzType;
  AMutable: Boolean): Integer;
begin
  if FScopeCount >= Length(FScope) then
    SetLength(FScope, FScopeCount * 2 + 16);
  Result := FScopeCount;
  FScope[Result].Name := AName;
  FScope[Result].Kind := AKind;
  FScope[Result].Mutable := AMutable;
  SetLength(FScope[Result].Params, 0);
  FScope[Result].Generator := False;
  FScope[Result].Async := False;
  FScope[Result].Work := 1;
  Inc(FScopeCount);
end;

{ A binding of AKind (a number for ftNum also an integer; with AAllowAny
  sometimes a binding of no known type) }
function TFuzzGenerator.FindBinding(AKind: TFuzzType;
  AMutableOnly, AAllowAny: Boolean; out AIndex: Integer): Boolean;
var
  Candidates: array of Integer;
  Count, I: Integer;
  Kind: TFuzzType;
  Any: Boolean;
begin
  AIndex := -1;
  Any := AAllowAny and Maybe(4);
  SetLength(Candidates, FScopeCount);
  Count := 0;
  for I := 0 to FScopeCount - 1 do
  begin
    if AMutableOnly and not FScope[I].Mutable then
      Continue;
    Kind := FScope[I].Kind;
    if (Kind = AKind) or ((AKind = ftNum) and (Kind = ftInt)) or
      ((AKind = ftAny) and (Kind <> ftClass)) or (Any and (Kind = ftAny)) then
    begin
      Candidates[Count] := I;
      Inc(Count);
    end;
  end;
  if Count = 0 then
    Exit(False);
  AIndex := Candidates[Count - 1 - Choose(Count)];
  Result := True;
end;

function TFuzzGenerator.HasWorkFor(AWork: Integer): Boolean;
begin
  Result := FWork + Int64(AWork) * FLoopWork <= FWorkLimit;
end;

procedure TFuzzGenerator.AddWork(AWork: Integer);
begin
  Inc(FWork, AWork * FLoopWork);
end;

{ ---- expressions ---- }

function TFuzzGenerator.ValueKind: TFuzzType;
begin
  case Choose(8) of
    0: Result := ftInt;
    1: Result := ftNum;
    2: Result := ftStr;
    3: Result := ftBool;
    4: Result := ftArr;
    5: Result := ftObj;
    6: Result := ftBig;
  else
    Result := ftFun;
  end;
end;

{ AExpr as the receiver of a member access: an identifier as it is,
  anything else in parentheses }
function TFuzzGenerator.Recv(const AExpr: string): string;
var
  I: Integer;
begin
  for I := 1 to Length(AExpr) do
    if not CharInSet(AExpr[I], ['a'..'z', 'A'..'Z', '0'..'9', '_']) or
      ((I = 1) and CharInSet(AExpr[I], ['0'..'9'])) then
      Exit('(' + AExpr + ')');
  Result := AExpr;
end;

function TFuzzGenerator.Literal(AKind: TFuzzType): string;
begin
  case AKind of
    ftInt:
      Result := '(' + Pick(INT_LITERALS) + ')';
    ftNum:
      Result := '(' + Pick(NUM_LITERALS) + ')';
    ftStr:
      Result := Pick(STR_LITERALS);
    ftBool:
      if Choose(2) = 0 then
        Result := 'false'
      else
        Result := 'true';
    ftArr:
      case Choose(4) of
        0: Result := '[]';
        1: Result := '[1, 2, 3]';
        2: Result := '[1, , "b", null]';
      else
        Result := '[0.5, -0, NaN, "3"]';
      end;
    ftObj:
      case Choose(4) of
        0: Result := '{}';
        1: Result := '{ a: 1, b: "two" }';
        2: Result := 'Object.create(null)';
      else
        Result := '{ x: 1.5, y: [2], a: { b: 3 } }';
      end;
    ftBig:
      Result := '(' + Pick(BIG_LITERALS) + ')';
    ftFun:
      case Choose(3) of
        0: Result := '((q) => q)';
        1: Result := 'Math.max';
      else
        Result := '(() => undefined)';
      end;
  else
    case Choose(5) of
      0: Result := 'undefined';
      1: Result := 'null';
      2: Result := 'Symbol("s")';
      3: Result := 'Symbol.iterator';
    else
      Result := Literal(ValueKind);
    end;
  end;
end;

function TFuzzGenerator.Leaf(AKind: TFuzzType): string;
var
  Index: Integer;
begin
  if AKind = ftAny then
    AKind := ValueKind;
  if (Choose(3) > 0) and FindBinding(AKind, False, True, Index) then
    Exit(FScope[Index].Name);
  if FInMethod and (AKind = ftObj) and Maybe(3) then
    Exit('this');
  Result := Literal(AKind);
end;

function TFuzzGenerator.Expr(AKind: TFuzzType): string;
begin
  Inc(FExprDepth);
  try
    if FExprDepth > MAX_EXPR_DEPTH then
      Exit(Leaf(AKind));
    case AKind of
      ftNum: Result := NumExpr;
      ftInt: Result := IntExpr;
      ftStr: Result := StrExpr;
      ftBool: Result := BoolExpr;
      ftArr: Result := ArrExpr;
      ftObj: Result := ObjExpr;
      ftBig: Result := BigExpr;
      ftFun: Result := FunExpr;
    else
      Result := AnyExpr;
    end;
  finally
    Dec(FExprDepth);
  end;
end;

{ A call of a function binding the work budget allows: false when none }
function TFuzzGenerator.CallExpr(out AText: string): Boolean;
var
  Index: Integer;
begin
  AText := '';
  if not FindBinding(ftFun, False, False, Index) or
    not HasWorkFor(FScope[Index].Work) then
    Exit(False);
  { a binding that can be reassigned could come to hold the function being
    generated: only the top level calls one, so calls stay acyclic }
  if FScope[Index].Mutable and (FFunctionDepth > 0) then
    Exit(False);
  if (Length(FScope[Index].Params) = 0) and (FScope[Index].Work <= 1) and
    not FScope[Index].Generator and not FScope[Index].Async then
  begin
    { a function value of unknown shape: call it with any arguments }
    AText := FScope[Index].Name + '(' + Expr(ftAny) + ')';
    AddWork(DYNAMIC_CALL_WORK);
    Exit(True);
  end;
  AText := FScope[Index].Name + '(' + ArgsFor(FScope[Index].Params) + ')';
  if FScope[Index].Generator then
    AText := '[...' + AText + ']';
  AddWork(FScope[Index].Work);
  Result := True;
end;

function TFuzzGenerator.ArgsFor(const AParams: array of TFuzzType): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AParams) do
  begin
    if I > 0 then
      Result := Result + ', ';
    if Maybe(8) then
      Result := Result + Expr(ftAny)
    else
      Result := Result + Expr(AParams[I]);
  end;
end;

{ An arrow function with an expression body; its parameters are bindings of
  no known type while the body is generated }
function TFuzzGenerator.Lambda(AParams: Integer; AResult: TFuzzType): string;
var
  Saved, I: Integer;
  Names, Name: string;
begin
  Saved := FScopeCount;
  Names := '';
  for I := 0 to AParams - 1 do
  begin
    Name := NewName('q');
    if I > 0 then
      Names := Names + ', ';
    Names := Names + Name;
    AddBinding(Name, ftAny, False);
  end;
  { the body in parentheses, so an object literal is not read as a block }
  Result := '((' + Names + ') => (' + Expr(AResult) + '))';
  FScopeCount := Saved;
end;

function TFuzzGenerator.PropertyRead: string;
var
  Receiver: string;
begin
  Receiver := Recv(Expr(ftAny));
  case Choose(6) of
    0: Result := Receiver + '.' + Pick(KEY_NAMES);
    1: Result := Receiver + '?.' + Pick(KEY_NAMES);
    2: Result := Receiver + '[' + Pick(KEY_LITERALS) + ']';
    3: Result := Receiver + '[' + Expr(ftStr) + ']';
    4:
      begin
        Result := Receiver + '?.m?.(' + Expr(ftAny) + ')';
        AddWork(DYNAMIC_CALL_WORK);
      end;
  else
    if FInMethod then
      Result := Receiver + '.#p'
    else
      Result := Receiver + '[' + Expr(ftInt) + ']';
  end;
end;

function TFuzzGenerator.NumExpr: string;
var
  Text: string;
begin
  case Choose(18) of
    0: Result := Leaf(ftNum);
    1: Result := '(' + Expr(ftNum) + ' ' + Pick(['+', '-', '*', '/', '%', '**']) + ' ' +
        Expr(ftNum) + ')';
    2: Result := '(' + Expr(ftAny) + ' ' + Pick(['+', '-', '*']) + ' ' + Expr(ftAny) + ')';
    3: Result := '(' + Pick(['-', '+', '~']) + Expr(ftNum) + ')';
    4: Result := 'Math.' + Pick(MATH_UNARY) + '(' + Expr(ftNum) + ')';
    5: Result := 'Math.' + Pick(MATH_BINARY) + '(' + Expr(ftNum) + ', ' + Expr(ftNum) + ')';
    6: Result := Recv(Expr(ftArr)) + '.length';
    7: Result := Pick(['Number(', 'parseFloat(', 'Math.round(']) + Expr(ftAny) + ')';
    8: Result := '(' + Expr(ftBool) + ' ? ' + Expr(ftNum) + ' : ' + Expr(ftNum) + ')';
    9:
      if CallExpr(Text) then
        Result := Text
      else
        Result := Leaf(ftNum);
    10: Result := Recv(Expr(ftStr)) + '.' + Pick(['indexOf(', 'lastIndexOf(', 'charCodeAt(',
        'codePointAt(', 'localeCompare(']) + Expr(ftStr) + ')';
    11: Result := Recv(Expr(ftArr)) + '.reduce(' + Lambda(2, ftNum) + ', ' + Expr(ftNum) + ')';
    12: Result := Recv(Expr(ftArr)) + '[' + Expr(ftInt) + ']';
    13: Result := 'Math.' + Pick(['max', 'min']) + '(...' + Expr(ftArr) + ')';
    14: Result := 'Number(' + Expr(ftBig) + ')';
    15: Result := PropertyRead;
    16: Result := Recv(Expr(ftArr)) + '.' + Pick(['findIndex(', 'findLastIndex(']) +
        Lambda(1, ftBool) + ')';
  else
    Result := 'parseInt(' + Expr(ftStr) + ', ' + Pick(['10', '16', '2', '36', 'undefined']) + ')';
  end;
end;

function TFuzzGenerator.IntExpr: string;
var
  Index: Integer;
begin
  case Choose(10) of
    0: Result := Leaf(ftInt);
    1: Result := '(' + Expr(ftNum) + ' | 0)';
    2: Result := '(' + Expr(ftInt) + ' ' + Pick(['&', '|', '^', '<<', '>>', '>>>']) + ' ' +
        Expr(ftInt) + ')';
    3: Result := '(' + Expr(ftInt) + ' ' + Pick(['+', '-', '*']) + ' ' + Expr(ftInt) + ')';
    4: Result := '(' + Expr(ftInt) + ' % ' + Pick(['2', '3', '7', '-4', '0']) + ')';
    5: Result := Recv(Expr(ftStr)) + '.length';
    6: Result := 'Math.' + Pick(['floor(', 'trunc(', 'clz32(']) + Expr(ftNum) + ')';
    7:
      if FindBinding(ftInt, True, False, Index) then
        Result := '(' + FScope[Index].Name + Pick(['++', '--', ' += 1', ' -= 3', ' <<= 1']) + ')'
      else
        Result := Leaf(ftInt);
    8: Result := Recv(Expr(ftArr)) + '.length';
  else
    Result := '(' + Expr(ftBool) + ' ? ' + Expr(ftInt) + ' : ' + Expr(ftInt) + ')';
  end;
end;

function TFuzzGenerator.StrExpr: string;
begin
  case Choose(16) of
    0: Result := Leaf(ftStr);
    1: Result := '(' + Expr(ftStr) + ' + ' + Expr(ftAny) + ')';
    2: Result := '`' + Pick(['', 'a', 'x=']) + '${' + Expr(ftAny) + '}' + Pick(['', '-', ' b ']) +
        '${' + Expr(ftAny) + '}`';
    3: Result := Pick(['String(', 'JSON.stringify(', '__s(']) + Expr(ftAny) + ')';
    4: Result := Recv(Expr(ftStr)) + '.' + Pick(['slice(', 'substring(', 'at(', 'charAt(']) +
        Expr(ftInt) + ')';
    5: Result := Recv(Expr(ftStr)) + '.' + Pick(['toUpperCase()', 'toLowerCase()', 'trim()',
        'trimStart()', 'normalize("NFD")', 'toString()']);
    6: Result := Recv(Expr(ftStr)) + '.' + Pick(['padStart(', 'padEnd(']) +
        Pick(['3', '8', '0']) + ', ' + Expr(ftStr) + ')';
    7: Result := Recv(Expr(ftStr)) + '.repeat(' + Pick(['0', '1', '3']) + ')';
    8: Result := Recv(Expr(ftStr)) + '.' + Pick(['replace(', 'replaceAll(']) +
        Pick([Expr(ftStr), '/a/g', '/[aeiou]/g']) + ', ' + Expr(ftStr) + ')';
    9: Result := Recv(Expr(ftStr)) + '.split(' + Expr(ftStr) + ').join(' + Expr(ftStr) + ')';
    10: Result := '(typeof ' + Expr(ftAny) + ')';
    11: Result := Recv(Expr(ftArr)) + '.join(' + Expr(ftStr) + ')';
    12: Result := Recv(Expr(ftNum)) + '.' + Pick(['toFixed(', 'toPrecision(', 'toString(',
        'toExponential(']) + Pick(['1', '2', '5', '16', '20']) + ')';
    13: Result := 'String.fromCharCode(' + Expr(ftInt) + ')';
    14: Result := Recv(Expr(ftStr)) + '.replace(' + Pick(REGEX_LITERALS) + ', ' +
        Pick(['"$1"', '"<$&>"', '""', Lambda(1, ftStr)]) + ')';
  else
    Result := '(' + Expr(ftBool) + ' ? ' + Expr(ftStr) + ' : ' + Expr(ftStr) + ')';
  end;
end;

function TFuzzGenerator.BoolExpr: string;
var
  Index: Integer;
begin
  case Choose(14) of
    0: Result := Leaf(ftBool);
    1: Result := '(' + Expr(ftNum) + ' ' + Pick(['===', '!==', '<', '<=', '>', '>=']) + ' ' +
        Expr(ftNum) + ')';
    2: Result := '(' + Expr(ftAny) + ' ' + Pick(['===', '!==', '==', '!=', '<', '>=']) + ' ' +
        Expr(ftAny) + ')';
    3: Result := '(!' + Expr(ftAny) + ')';
    4: Result := '(' + Expr(ftBool) + ' ' + Pick(['&&', '||', '??']) + ' ' + Expr(ftBool) + ')';
    5: Result := Pick(['Array.isArray(', 'Number.isInteger(', 'Number.isNaN(', 'isFinite(',
        'Number.isSafeInteger(', 'Object.isFrozen(', 'Boolean(']) + Expr(ftAny) + ')';
    6: Result := 'Object.is(' + Expr(ftAny) + ', ' + Expr(ftAny) + ')';
    7: Result := '(' + Pick(KEY_LITERALS) + ' in ' + Expr(ftObj) + ')';
    8:
      if FindBinding(ftClass, False, False, Index) and Maybe(2) then
        Result := '(' + Expr(ftAny) + ' instanceof ' + FScope[Index].Name + ')'
      else
        Result := '(' + Expr(ftAny) + ' instanceof ' + Pick(['Array', 'Object', 'Error',
          'Function']) + ')';
    9: Result := Recv(Expr(ftStr)) + '.' + Pick(['includes(', 'startsWith(', 'endsWith(']) +
        Expr(ftStr) + ')';
    10: Result := Recv(Expr(ftArr)) + '.' + Pick(['some(', 'every(']) + Lambda(1, ftBool) + ')';
    11: Result := Recv(Expr(ftArr)) + '.includes(' + Expr(ftAny) + ')';
    12: Result := Pick(REGEX_LITERALS) + '.test(' + Expr(ftStr) + ')';
  else
    if FInMethod then
      Result := '(#p in ' + Expr(ftObj) + ')'
    else
      Result := '(' + Expr(ftBig) + ' ' + Pick(['<', '===', '==', '>']) + ' ' +
        Expr(ftAny) + ')';
  end;
end;

function TFuzzGenerator.ArrExpr: string;
var
  I, Count: Integer;
begin
  case Choose(16) of
    0: Result := Leaf(ftArr);
    1:
      begin
        Count := Choose(5);
        Result := '[';
        for I := 0 to Count - 1 do
        begin
          if I > 0 then
            Result := Result + ', ';
          if not Maybe(6) then
            Result := Result + Expr(ftAny);
        end;
        Result := Result + ']';
      end;
    2: Result := 'Array.from({ length: ' + Pick(['0', '3', '9', '40']) + ' }, ' +
        Lambda(2, ftAny) + ')';
    3: Result := Recv(Expr(ftArr)) + '.' + Pick(['map(', 'filter(', 'flatMap(']) +
        Lambda(2, ftAny) + ')';
    4: Result := Recv(Expr(ftArr)) + '.' + Pick(['slice(', 'concat(', 'with(0, ']) +
        Expr(ftAny) + ')';
    5: Result := '[...' + Expr(ftArr) + ', ' + Expr(ftAny) + ']';
    6: Result := Recv(Expr(ftArr)) + '.' + Pick(['flat()', 'toReversed()', 'toSorted()',
        'reverse()', 'sort()', 'keys().toArray?.()', 'entries().next().value']);
    7: Result := '[...' + Expr(ftStr) + ']';
    8: Result := 'Object.' + Pick(['keys(', 'values(', 'entries(', 'getOwnPropertyNames(']) +
        Expr(ftAny) + ')';
    9: Result := Recv(Expr(ftStr)) + '.split(' + Pick([Expr(ftStr), '""', '/,/', '/(a)/']) + ')';
    10: Result := 'new Array(' + Pick(['0', '2', '5']) + ').fill(' + Expr(ftAny) + ')';
    11: Result := Recv(Expr(ftArr)) + '.sort(' + Lambda(2, ftNum) + ')';
    12: Result := 'new ' + Pick(['Int32Array', 'Float64Array', 'Uint8Array', 'Uint8ClampedArray',
        'Int8Array', 'Float32Array', 'Uint32Array']) + '(' +
        Pick([Expr(ftArr), '4', '[1.5, -1, 300, NaN]']) + ')';
    13: Result := 'Array.of(' + Expr(ftAny) + ', ' + Expr(ftAny) + ')';
    14: Result := Recv(Expr(ftStr)) + '.match(' + Pick(REGEX_LITERALS) + ')';
  else
    Result := 'Array.from(' + Expr(ftAny) + ')';
  end;
end;

function TFuzzGenerator.ObjExpr: string;
var
  Index: Integer;
begin
  case Choose(14) of
    0: Result := Leaf(ftObj);
    1: Result := '{ ' + Pick(KEY_NAMES) + ': ' + Expr(ftAny) + ', ' + Pick(KEY_NAMES) + ': ' +
        Expr(ftAny) + ' }';
    2: Result := '{ ...' + Expr(ftAny) + ', ' + Pick(KEY_NAMES) + ': ' + Expr(ftAny) + ' }';
    3:
      if FindBinding(ftClass, False, False, Index) and HasWorkFor(DYNAMIC_CALL_WORK) then
      begin
        Result := 'new ' + FScope[Index].Name + '(' + ArgsFor(FScope[Index].Params) + ')';
        AddWork(DYNAMIC_CALL_WORK);
      end
      else
        Result := '{ a: ' + Expr(ftAny) + ' }';
    4: Result := 'Object.assign({}, ' + Expr(ftAny) + ')';
    5: Result := 'Object.' + Pick(['freeze(', 'seal(', 'preventExtensions(']) + Expr(ftObj) + ')';
    6: Result := 'new Map([[' + Expr(ftAny) + ', ' + Expr(ftAny) + ']])';
    7: Result := 'new Set(' + Expr(ftArr) + ')';
    8: Result := 'Object.create(' + Expr(ftObj) + ')';
    9: Result := '{ [' + Expr(ftStr) + ']: ' + Expr(ftAny) + ', [Symbol.toPrimitive]: ' +
        '(() => ' + Literal(ftNum) + ') }';
    10: Result := '{ get g() { return ' + Literal(ftAny) + '; }, set g(v) { __u = v; }, ' +
        'm(v) { return ' + Literal(ftNum) + '; } }';
    11: Result := 'JSON.parse(' + Pick(['"{\"a\":[1,2]}"', '"[1,{}]"', 'JSON.stringify(' +
        Expr(ftAny) + ') ?? "0"']) + ')';
    12: Result := 'new ' + Pick(ERROR_TYPES) + '(' + Expr(ftStr) + ')';
  else
    Result := 'new Proxy(' + Expr(ftObj) + ', {})';
  end;
end;

function TFuzzGenerator.BigExpr: string;
begin
  case Choose(6) of
    0: Result := Leaf(ftBig);
    1: Result := '(' + Expr(ftBig) + ' ' + Pick(['+', '-', '*', '/', '%', '&', '|', '^']) + ' ' +
        Expr(ftBig) + ')';
    2: Result := '(' + Expr(ftBig) + ' ' + Pick(['**', '<<', '>>']) + ' ' +
        Pick(['0n', '1n', '3n', '64n']) + ')';
    3: Result := 'BigInt(' + Expr(ftInt) + ')';
    4: Result := 'BigInt.' + Pick(['asIntN(', 'asUintN(']) + Pick(['8', '32', '64']) + ', ' +
        Expr(ftBig) + ')';
  else
    Result := '(-' + Expr(ftBig) + ')';
  end;
end;

function TFuzzGenerator.FunExpr: string;
var
  Index: Integer;
begin
  case Choose(4) of
    0: Result := Leaf(ftFun);
    1: Result := Lambda(Choose(3), ftAny);
    2:
      if FindBinding(ftFun, False, False, Index) then
        Result := FScope[Index].Name + '.bind(null, ' + Expr(ftAny) + ')'
      else
        Result := Lambda(1, ftAny);
  else
    Result := Pick(['Math.abs', 'String', 'Number', 'Array.isArray', 'Object.keys']);
  end;
end;

function TFuzzGenerator.AnyExpr: string;
var
  Text: string;
begin
  case Choose(6) of
    0: Result := Leaf(ftAny);
    1:
      if CallExpr(Text) then
        Result := Text
      else
        Result := Expr(ValueKind);
    2: Result := PropertyRead;
    3: Result := '(' + Expr(ftAny) + ' ' + Pick(['??', '||', '&&', ',']) + ' ' + Expr(ftAny) + ')';
  else
    Result := Expr(ValueKind);
  end;
end;

{ ---- statements ---- }

procedure TFuzzGenerator.Block(AMaxStatements: Integer);
var
  Saved, Count, I: Integer;
begin
  Saved := FScopeCount;
  Inc(FBlockDepth);
  Indent;
  try
    if FBlockDepth > MAX_BLOCK_DEPTH then
      Exit;
    Count := Choose(AMaxStatements + 1);
    for I := 1 to Count do
    begin
      if not HasWorkFor(1) then
        Break;
      AddWork(1);
      Statement;
    end;
  finally
    Dedent;
    Dec(FBlockDepth);
    FScopeCount := Saved;
  end;
end;

procedure TFuzzGenerator.Statement;
begin
  case Choose(20) of
    0: LogStatement;
    1, 2: DeclareStatement;
    3, 4: AssignStatement;
    5: MutateStatement;
    6: IfStatement;
    7: ForStatement;
    8: ForOfStatement;
    9: ForInStatement;
    10: WhileStatement;
    11: SwitchStatement;
    12: TryStatement;
    13: JumpStatement;
    14: DestructureStatement;
    15: ObjectModelStatement;
    16:
      if FFunctionDepth > 0 then
        Line('if (' + Expr(ftBool) + ') return ' + Expr(ftAny) + ';')
      else
        LogStatement;
    17:
      if FInGenerator then
        Line('__u = yield ' + Expr(ftAny) + ';')
      else if FInAsync then
        Line('__u = await ' + Expr(ftAny) + ';')
      else
        AssignStatement;
    18:
      if FFunctionDepth > 0 then
        FunctionDefinition(False)
      else
        DeclareStatement;
  else
    Line('if (' + Expr(ftBool) + ') throw ' + Pick(['new ' + Pick(ERROR_TYPES) + '(' +
      Expr(ftStr) + ')', Expr(ftAny)]) + ';');
  end;
end;

procedure TFuzzGenerator.LogStatement;
begin
  Line('__l(' + NextTag + ', ' + Expr(ftAny) + ', ' + Expr(ftAny) + ');');
end;

procedure TFuzzGenerator.DeclareStatement;
var
  Kind: TFuzzType;
  Name, Init: string;
  Mutable: Boolean;
begin
  Kind := ValueKind;
  Mutable := Choose(3) > 0;
  Name := NewName('v');
  Init := Expr(Kind);
  if Mutable then
    Line('let ' + Name + ' = ' + Init + ';')
  else
    Line('const ' + Name + ' = ' + Init + ';');
  AddBinding(Name, Kind, Mutable);
end;

{ An assignable place: a mutable binding (AIndex its scope index) or a
  property or element }
function TFuzzGenerator.Target(out AText: string; out AIndex: Integer): Boolean;
begin
  AIndex := -1;
  case Choose(3) of
    0, 1:
      if FindBinding(ftAny, True, False, AIndex) then
      begin
        AText := FScope[AIndex].Name;
        Exit(True);
      end;
  end;
  case Choose(4) of
    0: AText := Recv(Expr(ftObj)) + '.' + Pick(KEY_NAMES);
    1: AText := Recv(Expr(ftArr)) + '[' + Pick(['0', '1', Expr(ftInt), '100', '-1',
        '"x"']) + ']';
    2: AText := Recv(Expr(ftObj)) + '[' + Expr(ftStr) + ']';
  else
    if FInMethod then
      AText := 'this.' + Pick(['a', 'b', '#p'])
    else
      AText := Recv(Expr(ftAny)) + '.' + Pick(KEY_NAMES);
  end;
  Result := True;
end;

procedure TFuzzGenerator.AssignStatement;
var
  Text: string;
  Index: Integer;
  Kind: TFuzzType;
begin
  Target(Text, Index);
  if Index >= 0 then
    Kind := FScope[Index].Kind
  else
    Kind := ftAny;
  case Choose(4) of
    0, 1:
      if Maybe(6) and (Index >= 0) then
      begin
        { the binding's type changes }
        Line(Text + ' = ' + Expr(ftAny) + ';');
        FScope[Index].Kind := ftAny;
      end
      else
        Line(Text + ' = ' + Expr(Kind) + ';');
    2: Line(Text + ' ' + Pick(['+=', '-=', '*=', '|=', '>>=', '**=', '??=', '||=', '&&=']) + ' ' +
        Expr(Kind) + ';');
  else
    Line(Text + Pick(['++', '--']) + ';');
  end;
end;

procedure TFuzzGenerator.MutateStatement;
var
  Receiver: string;
begin
  Receiver := Recv(Expr(ftArr));
  case Choose(10) of
    0: Line('__u = ' + Receiver + '.push(' + Expr(ftAny) + ');');
    1: Line('__u = ' + Receiver + '.pop();');
    2: Line('__u = ' + Receiver + '.shift();');
    3: Line('__u = ' + Receiver + '.unshift(' + Expr(ftAny) + ');');
    4: Line('__u = ' + Receiver + '.splice(' + Expr(ftInt) + ', ' + Pick(['0', '1', '2']) + ', ' +
        Expr(ftAny) + ');');
    5: Line(Receiver + '.length = ' + Pick(['0', '1', '3', '10']) + ';');
    6: Line('__u = ' + Receiver + '.fill(' + Expr(ftAny) + ');');
    7: Line('__u = ' + Receiver + '.copyWithin(0, ' + Expr(ftInt) + ');');
    8: Line('__u = delete ' + Receiver + '[' + Expr(ftInt) + '];');
  else
    Line('__u = ' + Receiver + '.forEach(' + Lambda(2, ftAny) + ');');
  end;
end;

procedure TFuzzGenerator.ObjectModelStatement;
var
  Receiver: string;
begin
  Receiver := Expr(ftObj);
  case Choose(7) of
    0: Line('__u = delete ' + Recv(Receiver) + '.' + Pick(KEY_NAMES) + ';');
    1: Line('Object.defineProperty(' + Receiver + ', ' + Pick(KEY_LITERALS) + ', { value: ' +
        Expr(ftAny) + ', writable: ' + Literal(ftBool) + ', configurable: ' + Literal(ftBool) +
        ' });');
    2: Line('Object.setPrototypeOf(' + Receiver + ', ' + Pick(['null', Expr(ftObj),
        'Array.prototype']) + ');');
    3: Line('Object.' + Pick(['freeze(', 'seal(', 'preventExtensions(']) + Receiver + ');');
    4: Line('Object.defineProperty(' + Receiver + ', ' + Pick(KEY_LITERALS) +
        ', { get: () => (' + Literal(ftAny) + '), configurable: true });');
    5: Line('__u = Object.assign(' + Receiver + ', ' + Expr(ftAny) + ');');
  else
    Line('__u = Reflect.' + Pick(['ownKeys(', 'getPrototypeOf(', 'isExtensible(']) +
      Receiver + ');');
  end;
end;

procedure TFuzzGenerator.IfStatement;
begin
  Line('if (' + Expr(ftBool) + ') {');
  Block(MAX_BLOCK_STATEMENTS);
  if Maybe(2) then
  begin
    Line('} else {');
    Block(MAX_BLOCK_STATEMENTS);
  end;
  Line('}');
end;

{ A loop's body, run AIterations times per run of the loop; ALabel the
  loop's label ('' none) }
procedure TFuzzGenerator.LoopBody(AIterations: Integer; const ALabel: string);
var
  SavedWork, SavedLabels: Integer;
begin
  SavedWork := FLoopWork;
  SavedLabels := FLabelCount;
  FLoopWork := FLoopWork * AIterations;
  if FLoopWork < 1 then
    FLoopWork := 1;
  if FLabelCount >= Length(FLabels) then
    SetLength(FLabels, FLabelCount + 8);
  FLabels[FLabelCount] := ALabel;
  Inc(FLabelCount);
  Inc(FLoopDepth);
  try
    Block(MAX_BLOCK_STATEMENTS);
  finally
    Dec(FLoopDepth);
    FLabelCount := SavedLabels;
    FLoopWork := SavedWork;
  end;
end;

procedure TFuzzGenerator.ForStatement;
var
  Count: Integer;
  Counter, LabelName, Prefix: string;
  Saved: Integer;
begin
  Count := LOOP_COUNTS[Choose(Length(LOOP_COUNTS))];
  while (Count > 1) and (FLoopWork * Count > MAX_LOOP_WORK) do
    Count := Count div 2;
  Counter := NewName('i');
  LabelName := '';
  Prefix := '';
  if Maybe(3) then
  begin
    LabelName := NewName('L');
    Prefix := LabelName + ': ';
  end;
  Line(Prefix + 'for (let ' + Counter + ' = 0; ' + Counter + ' < ' + IntToStr(Count) + '; ' +
    Counter + '++) {');
  Saved := FScopeCount;
  AddBinding(Counter, ftInt, False);
  LoopBody(Count, LabelName);
  FScopeCount := Saved;
  Line('}');
end;

procedure TFuzzGenerator.ForOfStatement;
var
  Item, Source: string;
  Saved: Integer;
begin
  Item := NewName('o');
  case Choose(4) of
    0: Source := Recv(Expr(ftArr)) + '.slice(0, 16)';
    1: Source := Recv(Expr(ftStr)) + '.slice(0, 16)';
    2: Source := '[...' + Recv(Expr(ftObj)) + '?.entries?.() ?? []].slice(0, 16)';
  else
    Source := 'Object.entries(' + Expr(ftAny) + ').slice(0, 16)';
  end;
  Line('for (const ' + Item + ' of ' + Source + ') {');
  Saved := FScopeCount;
  AddBinding(Item, ftAny, False);
  LoopBody(16, '');
  FScopeCount := Saved;
  Line('}');
end;

procedure TFuzzGenerator.ForInStatement;
var
  Key: string;
  Saved: Integer;
begin
  Key := NewName('k');
  Line('for (const ' + Key + ' in ' + Expr(ftAny) + ') {');
  Saved := FScopeCount;
  AddBinding(Key, ftStr, False);
  { an object of ours has few keys, an array of ours few elements }
  LoopBody(16, '');
  FScopeCount := Saved;
  Line('}');
end;

procedure TFuzzGenerator.WhileStatement;
var
  Count: Integer;
  Guard: string;
begin
  Count := LOOP_COUNTS[Choose(Length(LOOP_COUNTS))];
  while (Count > 1) and (FLoopWork * Count > MAX_LOOP_WORK) do
    Count := Count div 2;
  Guard := NewName('w');
  Line('let ' + Guard + ' = 0;');
  if Maybe(2) then
  begin
    Line('do {');
    LoopBody(Count, '');
    Line('} while (' + Guard + '++ < ' + IntToStr(Count) + ' && ' + Expr(ftBool) + ');');
  end
  else
  begin
    Line('while (' + Guard + '++ < ' + IntToStr(Count) + ' && ' + Expr(ftBool) + ') {');
    LoopBody(Count, '');
    Line('}');
  end;
end;

procedure TFuzzGenerator.SwitchStatement;
var
  I, Count: Integer;
begin
  Line('switch (' + Pick([Expr(ftInt), Expr(ftAny)]) + ') {');
  Indent;
  Inc(FSwitchDepth);
  Count := 1 + Choose(4);
  for I := 0 to Count - 1 do
  begin
    Line('case ' + Pick([IntToStr(I), Literal(ftAny)]) + ': {');
    Block(MAX_BLOCK_STATEMENTS div 2);
    if Choose(3) > 0 then
      Line('  break;');
    Line('}');
  end;
  if Maybe(2) then
  begin
    Line('default: {');
    Block(MAX_BLOCK_STATEMENTS div 2);
    Line('}');
  end;
  Dec(FSwitchDepth);
  Dedent;
  Line('}');
end;

procedure TFuzzGenerator.TryStatement;
var
  Caught: string;
  Saved: Integer;
begin
  Line('try {');
  Block(MAX_BLOCK_STATEMENTS);
  Caught := NewName('e');
  Line('} catch (' + Caught + ') {');
  Saved := FScopeCount;
  AddBinding(Caught, ftAny, True);
  Indent;
  Line('__l(' + NextTag + ', __e(' + Caught + '));');
  Dedent;
  Block(MAX_BLOCK_STATEMENTS div 2);
  FScopeCount := Saved;
  if Maybe(3) then
  begin
    Line('} finally {');
    Block(MAX_BLOCK_STATEMENTS div 2);
  end;
  Line('}');
end;

procedure TFuzzGenerator.JumpStatement;
var
  I: Integer;
  Labelled: string;
begin
  if FLoopDepth = 0 then
  begin
    if FSwitchDepth > 0 then
      Line('if (' + Expr(ftBool) + ') break;')
    else
      LogStatement;
    Exit;
  end;
  Labelled := '';
  I := Choose(FLabelCount - FLabelBase + 1);
  if (I > 0) and (FLabels[FLabelBase + I - 1] <> '') then
    Labelled := ' ' + FLabels[FLabelBase + I - 1];
  Line('if (' + Expr(ftBool) + ') ' + Pick(['break', 'continue']) + Labelled + ';');
end;

procedure TFuzzGenerator.DestructureStatement;
var
  A, B, C: string;
begin
  A := NewName('d');
  B := NewName('d');
  C := NewName('d');
  if Choose(2) = 0 then
    Line('let [' + A + ', ' + B + ' = ' + Expr(ftAny) + ', ...' + C + '] = ' +
      Expr(ftArr) + ';')
  else
    Line('let { a: ' + A + ', ' + Pick(KEY_NAMES) + ': ' + B + ' = ' + Expr(ftAny) + ', ...' +
      C + ' } = ' + Expr(ftObj) + ';');
  AddBinding(A, ftAny, True);
  AddBinding(B, ftAny, True);
  AddBinding(C, ftAny, True);
end;

{ ---- definitions ---- }

{ A function: an arrow, a declaration, a generator or an async arrow, with
  typed parameters and a statement body. Its binding is added after the
  body, so it cannot call itself; calls only go to what was defined before
  them. }
procedure TFuzzGenerator.FunctionDefinition(ATopLevel: Boolean);
var
  Name, Params: string;
  Kinds: array of TFuzzType;
  Names: array of string;
  Form, Count, I, Saved, Index, SavedWork, SavedLimit, SavedLoopWork,
    SavedLabelBase, SavedLoops, SavedSwitches: Integer;
  SavedAsync, SavedGenerator: Boolean;
begin
  Form := Choose(5);
  Count := Choose(MAX_PARAMS + 1);
  SetLength(Kinds, Count);
  SetLength(Names, Count);
  Params := '';
  Saved := FScopeCount;
  for I := 0 to Count - 1 do
  begin
    Kinds[I] := ValueKind;
    Names[I] := NewName('p');
    if I > 0 then
      Params := Params + ', ';
    Params := Params + Names[I];
  end;
  Name := NewName('f');
  case Form of
    0, 1: Line('const ' + Name + ' = (' + Params + ') => {');
    2: Line('function ' + Name + '(' + Params + ') {');
    3: Line('function* ' + Name + '(' + Params + ') {');
  else
    Line('const ' + Name + ' = async (' + Params + ') => {');
  end;
  for I := 0 to Count - 1 do
    AddBinding(Names[I], Kinds[I], True);
  SavedWork := FWork;
  SavedLimit := FWorkLimit;
  SavedLoopWork := FLoopWork;
  SavedLabelBase := FLabelBase;
  SavedLoops := FLoopDepth;
  SavedSwitches := FSwitchDepth;
  SavedAsync := FInAsync;
  SavedGenerator := FInGenerator;
  FWork := 1;
  FWorkLimit := MAX_FUNCTION_WORK;
  FLoopWork := 1;
  { the enclosing function's loops are no targets in this one }
  FLabelBase := FLabelCount;
  FLoopDepth := 0;
  FSwitchDepth := 0;
  FInAsync := Form = 4;
  FInGenerator := Form = 3;
  Inc(FFunctionDepth);
  try
    Block(MAX_BLOCK_STATEMENTS);
    Indent;
    Line('return ' + Expr(ftAny) + ';');
    Dedent;
  finally
    Dec(FFunctionDepth);
    I := FWork;
    FWork := SavedWork;
    FWorkLimit := SavedLimit;
    FLoopWork := SavedLoopWork;
    FLabelBase := SavedLabelBase;
    FLoopDepth := SavedLoops;
    FSwitchDepth := SavedSwitches;
    FInAsync := SavedAsync;
    FInGenerator := SavedGenerator;
    FScopeCount := Saved;
  end;
  if Form = 2 then
    Line('}')
  else
    Line('};');
  Index := AddBinding(Name, ftFun, False);
  FScope[Index].Params := Kinds;
  FScope[Index].Generator := Form = 3;
  FScope[Index].Async := Form = 4;
  FScope[Index].Work := I;
  if not ATopLevel then
    AddWork(1);
end;

{ A class, perhaps derived from an earlier one, with public and private
  fields, an accessor pair, methods and a static member. It is assigned in
  a try, since a field or `extends` can throw. }
procedure TFuzzGenerator.ClassDefinition;
var
  Name, Base, Params, P: string;
  Kinds: array of TFuzzType;
  I, Count, Saved, Index, SavedWork, SavedLimit, SavedLoopWork: Integer;
  SavedMethod, SavedDerived: Boolean;

  procedure MethodBody(AReturn: TFuzzType);
  begin
    Block(MAX_BLOCK_STATEMENTS div 2);
    Indent;
    Line('return ' + Expr(AReturn) + ';');
    Dedent;
  end;

begin
  Name := NewName('C');
  Base := '';
  if FindBinding(ftClass, False, False, Index) and Maybe(2) then
    Base := ' extends ' + FScope[Index].Name
  else if Maybe(6) then
    Base := ' extends ' + Pick(['Array', 'Error', 'Map', 'Object']);
  Line('let ' + Name + ';');
  Line('try {');
  Indent;
  Line(Name + ' = class' + Base + ' {');
  Indent;
  SavedMethod := FInMethod;
  SavedDerived := FInDerived;
  SavedWork := FWork;
  SavedLimit := FWorkLimit;
  SavedLoopWork := FLoopWork;
  FInMethod := True;
  FInDerived := Base <> '';
  FWork := 1;
  FWorkLimit := DYNAMIC_CALL_WORK;
  FLoopWork := 1;
  Inc(FFunctionDepth);
  try
    Line('a = ' + Expr(ftAny) + ';');
    Line('#p = ' + Expr(ftAny) + ';');
    if Maybe(2) then
      Line('static s = ' + Literal(ftAny) + ';');
    Count := Choose(MAX_PARAMS + 1);
    SetLength(Kinds, Count);
    Params := '';
    Saved := FScopeCount;
    for I := 0 to Count - 1 do
    begin
      Kinds[I] := ValueKind;
      P := NewName('p');
      if I > 0 then
        Params := Params + ', ';
      Params := Params + P;
      AddBinding(P, Kinds[I], True);
    end;
    Line('constructor(' + Params + ') {');
    Indent;
    if FInDerived then
      Line('super(' + ArgsFor(Kinds) + ');');
    Line('this.b = ' + Expr(ftAny) + ';');
    Dedent;
    Block(MAX_BLOCK_STATEMENTS div 2);
    Line('}');
    FScopeCount := Saved;
    Line('get g() { return this.#p; }');
    Line('set g(v) { this.#p = v; }');
    P := NewName('p');
    Line('m(' + P + ') {');
    AddBinding(P, ftAny, True);
    MethodBody(ftAny);
    FScopeCount := Saved;
    Line('}');
    if Maybe(2) then
    begin
      Line('#q() {');
      MethodBody(ftAny);
      Line('}');
    end;
    if Maybe(2) then
      Line('static sm(v) { return ' + Expr(ftAny) + '; }');
    if Maybe(3) then
    begin
      Line('valueOf() { return ' + Literal(ftNum) + '; }');
      Line('toString() { return ' + Literal(ftStr) + '; }');
    end;
  finally
    Dec(FFunctionDepth);
    FInMethod := SavedMethod;
    FInDerived := SavedDerived;
    FWork := SavedWork;
    FWorkLimit := SavedLimit;
    FLoopWork := SavedLoopWork;
  end;
  Dedent;
  Line('};');
  Dedent;
  Line('} catch (e) {');
  Line('  __l(' + NextTag + ', __e(e));');
  Line('}');
  Index := AddBinding(Name, ftClass, False);
  FScope[Index].Params := Kinds;
end;

{ A function called hot: H times, its arguments changing with the loop
  counter and perhaps changing type late in the loop, each result folded
  into a digest that is printed after the loop. }
procedure TFuzzGenerator.HotCall;
var
  Index, Hot, Switch, I, Saved: Integer;
  Counter, Digest, Caught, Args, Call: string;
  Params: array of TFuzzType;
begin
  if not FindBinding(ftFun, False, False, Index) or (Length(FScope[Index].Params) = 0) and
    (FScope[Index].Work <= 1) then
  begin
    LogStatement;
    Exit;
  end;
  Hot := HOT_COUNTS[Choose(Length(HOT_COUNTS))];
  while (Hot > 1) and not HasWorkFor(Hot * (FScope[Index].Work + HOT_ITERATION_WORK)) do
    Hot := Hot div 2;
  if not HasWorkFor(Hot * (FScope[Index].Work + HOT_ITERATION_WORK)) then
  begin
    LogStatement;
    Exit;
  end;
  AddWork(Hot * (FScope[Index].Work + HOT_ITERATION_WORK));
  Params := Copy(FScope[Index].Params);
  Counter := NewName('i');
  Digest := NewName('h');
  Caught := NewName('e');
  Switch := Hot - 1 - Choose(Hot div 4 + 1);
  Saved := FScopeCount;
  AddBinding(Counter, ftInt, False);
  Args := '';
  for I := 0 to High(Params) do
  begin
    if I > 0 then
      Args := Args + ', ';
    case Choose(4) of
      0: Args := Args + Expr(Params[I]);
      1: Args := Args + '(' + Counter + ' < ' + IntToStr(Switch) + ' ? ' + Expr(Params[I]) + ' : ' +
          Expr(ftAny) + ')';
      2: Args := Args + '[' + Expr(Params[I]) + ', ' + Expr(Params[I]) + ', ' + Expr(ftAny) + '][' +
          Counter + ' % 3]';
    else
      Args := Args + '(' + Counter + ' ' + Pick(['+', '*', '-']) + ' ' + Expr(ftNum) + ')';
    end;
  end;
  FScopeCount := Saved;
  Call := FScope[Index].Name + '(' + Args + ')';
  if FScope[Index].Generator then
    Call := '[...' + Call + ']';
  Line('let ' + Digest + ' = 0;');
  Line('for (let ' + Counter + ' = 0; ' + Counter + ' < ' + IntToStr(Hot) + '; ' + Counter +
    '++) {');
  Line('  try { ' + Digest + ' = __mix(' + Digest + ', ' + Call + '); } catch (' + Caught +
    ') { ' + Digest + ' = __mix(' + Digest + ', __e(' + Caught + ')); }');
  Line('}');
  Line('__l(' + NextTag + ', ' + Digest + ');');
end;

{ Statements in an async arrow that is called at once; its result and any
  rejection are printed when the promise settles, after the script. }
procedure TFuzzGenerator.AsyncBlock;
var
  SavedAsync: Boolean;
  Tag: string;
begin
  Tag := NextTag;
  Line('__u = (async () => {');
  SavedAsync := FInAsync;
  FInAsync := True;
  Inc(FFunctionDepth);
  Block(MAX_BLOCK_STATEMENTS);
  Indent;
  Line('return await ' + Expr(ftAny) + ';');
  Dedent;
  Dec(FFunctionDepth);
  FInAsync := SavedAsync;
  Line('})().then((v) => __l(' + Tag + ', v), (e) => __l(' + Tag + ', "rejected", __e(e)));');
end;

procedure TFuzzGenerator.TopLevelItem;
var
  Name: string;
  Kind: TFuzzType;
begin
  case Choose(9) of
    0:
      begin
        { a binding the rest of the program can use, made in a try }
        Kind := ValueKind;
        Name := NewName('v');
        Line('let ' + Name + ';');
        Line('try { ' + Name + ' = ' + Expr(Kind) + '; } catch (e) { __l(' + NextTag +
          ', __e(e)); }');
        AddBinding(Name, Kind, True);
      end;
    1, 2: FunctionDefinition(True);
    3: ClassDefinition;
    4, 5:
      begin
        Line('try {');
        Indent;
        HotCall;
        Dedent;
        Line('} catch (e) { __l(' + NextTag + ', __e(e)); }');
      end;
    6: AsyncBlock;
  else
    Line('try {');
    Block(MAX_BLOCK_STATEMENTS);
    Line('} catch (e) { __l(' + NextTag + ', __e(e)); }');
  end;
end;

function TFuzzGenerator.Generate: string;
var
  I, Count: Integer;
  Names: string;
begin
  FOut.Append(FUZZ_PRELUDE);
  Count := Choose(MAX_TOP_ITEMS + 1);
  for I := 1 to Count do
  begin
    if not HasWorkFor(1) then
      Break;
    AddWork(1);
    TopLevelItem;
  end;
  { every top-level binding's final value }
  Names := '';
  for I := 0 to FScopeCount - 1 do
    if FScope[I].Kind <> ftClass then
      Names := Names + ', ' + FScope[I].Name;
  Line('__l("end"' + Names + ');');
  Result := FOut.ToString;
end;

function GenerateFuzzProgram(const AData: TBytes): string;
var
  Generator: TFuzzGenerator;
begin
  Generator := TFuzzGenerator.Create(AData);
  try
    Result := Generator.Generate;
  finally
    Generator.Free;
  end;
end;

end.

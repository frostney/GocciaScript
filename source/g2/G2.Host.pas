unit G2.Host;
{ The embedding layer: what a host application uses to run scripts and expose Pascal code to them.

  - TG2Value is a handle. It keeps its value alive and follows it when the collector moves it, so host
    code never roots anything by hand.
  - Host functions are closures; host classes are registered from their RTTI. Calls go through Goccia's
    FFI call plans (Goccia.FFI.ABI / Goccia.FFI.Call), compiled once per method at registration.
  - Errors are Pascal exceptions in both directions: raise EG2TypeError (or any exception) in host code to
    throw in JS; a JS exception reaching host code is raised as EG2Exception.
  - Objects created by JS, and objects host methods return, belong to the engine and are freed after the
    collector finds them dead, unless the method is marked [JSBorrowed].

  Limits of this spike: one engine per process (G2 keeps its state in unit globals); x86-64 System V only
  (the small-record parameter rule below); destructors of engine-owned objects must not call the engine. }
{$mode delphiunicode}{$H+}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}
{$modeswitch advancedrecords}
{$modeswitch prefixedattributes}

interface

uses
  SysUtils, Classes, TypInfo, Rtti, Generics.Collections,
  Goccia.FFI.Types, Goccia.FFI.ABI, Goccia.FFI.Call,
  G2.Core, G2.VM;

type
  TRawValue = G2.Core.TValue;
  PRawValue = G2.Core.PValue;

  { ---- attributes for registered classes ---- }
  JSNameAttribute = class(TCustomAttribute)
  public
    Name: UnicodeString;
    constructor Create(const AName: UnicodeString);
  end;
  JSHiddenAttribute = class(TCustomAttribute)      { not exposed to JS }
  public
    constructor Create;
  end;
  JSBorrowedAttribute = class(TCustomAttribute)    { a returned object stays owned by the host }
  public
    constructor Create;
  end;

  { ---- values ---- }
  TG2Value = record
  private
    FSlot: Integer;     { slot + 1; 0 = none, so zeroed memory (container gaps, FillChar) owns nothing }
    procedure SetRaw(V: TRawValue);
  public
    class operator Initialize(var H: TG2Value);
    class operator Finalize(var H: TG2Value);
    class operator Copy(constref Src: TG2Value; var Dst: TG2Value);
    class operator AddRef(var H: TG2Value);
    class function FromRaw(V: TRawValue): TG2Value; static;
    function Raw: TRawValue; inline;
    function IsUndefined: Boolean;
    function IsNull: Boolean;
    function IsNumber: Boolean;
    function IsString: Boolean;
    function IsBoolean: Boolean;
    function IsObject: Boolean;
    function IsFunction: Boolean;
    function AsNumber: Double;
    function AsInteger: Int64;
    function AsString: UnicodeString;
    function AsBoolean: Boolean;
    function AsObject: TObject;          { the Pascal object behind a host object, or nil }
    function Get(const Name: UnicodeString): TG2Value;
    procedure Put(const Name: UnicodeString; const V: TG2Value);
    function Call(const Args: array of TG2Value): TG2Value;
    function Invoke(const Method: UnicodeString; const Args: array of TG2Value): TG2Value;
  end;

  { what a host function or a raw host method receives }
  TG2Call = record
  private
    FArgs: PRawValue;     { [0] = this, [1..Argc] = arguments; lives in the VM stack, which the GC updates }
    FArgc: Integer;
    FInstance: TObject;
  public
    function Count: Integer; inline;
    function Arg(I: Integer): TG2Value;          { 0-based; undefined past the end }
    function This: TG2Value;
    function ArgNumber(I: Integer): Double;
    function ArgInt(I: Integer): Int64;
    function ArgString(I: Integer): UnicodeString;
    function ArgBool(I: Integer): Boolean;
    function ArgObject(I: Integer): TObject;
    property Instance: TObject read FInstance;   { the Pascal object for a raw method call }
  end;

  TG2Function = reference to function(const Call: TG2Call): TG2Value;

  { ---- errors ---- }
  EG2Error = class(Exception)
  public
    Kind: UnicodeString;    { the JS error constructor: TypeError, RangeError, ... }
    constructor Create(const AKind, AMessage: UnicodeString);
  end;
  EG2TypeError = class(EG2Error)
  public
    constructor Create(const AMessage: UnicodeString);
  end;
  EG2RangeError = class(EG2Error)
  public
    constructor Create(const AMessage: UnicodeString);
  end;
  { a JS exception that reached host code; raising it again from a host function rethrows the same value }
  EG2Exception = class(Exception)
  public
    Value: TG2Value;
  end;
  EG2SyntaxError = class(Exception);
  EG2BindError = class(Exception);   { a class cannot be registered as declared }

  TG2Engine = class
  public
    constructor Create(NurseryBytes: SizeInt = 8 * 1024 * 1024);
    destructor Destroy; override;
    function Eval(const Source: UnicodeString; const FileName: string = 'script.js'): TG2Value;
    function Global: TG2Value;
    procedure SetGlobal(const Name: UnicodeString; const V: TG2Value);
    procedure DefineFunction(const Name: UnicodeString; Length: Integer; const Fn: TG2Function);
    function RegisterClass(AClass: TClass; const AName: UnicodeString = ''): TG2Value;   { the JS constructor }
    function Wrap(Obj: TObject; Owned: Boolean): TG2Value;   { the class (or an ancestor) must be registered }
    procedure Forget(Obj: TObject);   { the host freed Obj: its wrapper throws from now on }
    procedure CollectGarbage;
    function LiveHostObjects: Integer;
  end;

function G2Undefined: TG2Value;
function G2Null: TG2Value;
function G2Number(D: Double): TG2Value;
function G2Str(const S: UnicodeString): TG2Value;
function G2Bool(B: Boolean): TG2Value;
function G2Object: TG2Value;
function G2Func(const Name: UnicodeString; Length: Integer; const Fn: TG2Function): TG2Value;
function G2HandleCount: Integer;   { live handles, for leak checks }

implementation

uses
  Math, Goccia.Lexer, Goccia.Parser, Goccia.AST.Node, G2.Bytecode, G2.Compiler,
  G2.Builtins, G2.Builtins2, G2.RegExpV, G2.Eval, G2.Builtins3, G2.Promise, G2.Builtins4, G2.ObjModel, G2.ArrayProto, G2.IterHelpers, G2.TypedArrays, G2.Disposal, G2.Modules, G2.Builtins5, G2.Temporal;

const
  okHost = 30;
  MAX_STR_ARGS = 4;
  ARG_BUF_BYTES = 256;

type
  TRttiMethodAccess = class(TRttiMethod);

{ ================= handles ================= }

var
  HSlots: array of TRawValue;
  HFree: array of Integer;
  HFreeTop, HHigh: Integer;

function HAlloc(V: TRawValue): Integer;
begin
  if HFreeTop > 0 then
  begin
    Dec(HFreeTop);
    Result := HFree[HFreeTop];
  end
  else
  begin
    if HHigh >= Length(HSlots) then SetLength(HSlots, HHigh * 2 + 256);
    Result := HHigh;
    Inc(HHigh);
  end;
  HSlots[Result] := V;
end;

procedure HRelease(I: Integer);
begin
  HSlots[I] := VUndefined;
  if HFreeTop >= Length(HFree) then SetLength(HFree, HFreeTop * 2 + 256);
  HFree[HFreeTop] := I;
  Inc(HFreeTop);
end;

function G2HandleCount: Integer;
begin
  Result := HHigh - HFreeTop;
end;

procedure HostScanRoots;
var I: Integer;
begin
  for I := 0 to HHigh - 1 do GCVisit(HSlots[I]);
end;

class operator TG2Value.Initialize(var H: TG2Value);
begin
  H.FSlot := 0;
end;

class operator TG2Value.Finalize(var H: TG2Value);
begin
  if H.FSlot > 0 then HRelease(H.FSlot - 1);
  H.FSlot := 0;
end;

class operator TG2Value.Copy(constref Src: TG2Value; var Dst: TG2Value);
var V: TRawValue;
begin
  if Src.FSlot = 0 then
  begin
    if Dst.FSlot > 0 then HRelease(Dst.FSlot - 1);
    Dst.FSlot := 0;
    Exit;
  end;
  V := HSlots[Src.FSlot - 1];
  if Dst.FSlot = 0 then Dst.FSlot := HAlloc(V) + 1 else HSlots[Dst.FSlot - 1] := V;
end;

class operator TG2Value.AddRef(var H: TG2Value);
begin
  { a bitwise copy (by-value parameter, array copy) gets its own slot }
  if H.FSlot > 0 then H.FSlot := HAlloc(HSlots[H.FSlot - 1]) + 1;
end;

procedure TG2Value.SetRaw(V: TRawValue);
begin
  if FSlot = 0 then FSlot := HAlloc(V) + 1 else HSlots[FSlot - 1] := V;
end;

class function TG2Value.FromRaw(V: TRawValue): TG2Value;
begin
  Result.SetRaw(V);
end;

function TG2Value.Raw: TRawValue;
begin
  if FSlot = 0 then Result := VUndefined else Result := HSlots[FSlot - 1];
end;

{ ================= errors ================= }

constructor EG2Error.Create(const AKind, AMessage: UnicodeString);
begin
  inherited Create(UTF8Encode(AMessage));
  Kind := AKind;
end;

constructor EG2TypeError.Create(const AMessage: UnicodeString);
begin
  inherited Create('TypeError', AMessage);
end;

constructor EG2RangeError.Create(const AMessage: UnicodeString);
begin
  inherited Create('RangeError', AMessage);
end;

{ the pending JS exception, as a Pascal exception }
procedure RaisePending;
var E: EG2Exception; V: TG2Value; S: TRawValue; Msg: UnicodeString;
begin
  V.SetRaw(Pending);
  Msg := 'uncaught exception';
  if ToStringV(V.Raw, S) and G2.Core.IsString(S) then Msg := StrToUnicode(StrOf(S));
  Pending := VUndefined;   { the host holds it now; HostFail puts it back if it is rethrown }
  E := EG2Exception.Create(UTF8Encode(Msg));
  E.Value := V;
  raise E;
end;

{ a Pascal exception leaving host code becomes the pending JS exception }
var HostTrace: Boolean;

function HostFail(E: Exception; SavedTemp: Integer): Boolean;
begin
  TempTop := SavedTemp;
  if E = nil then
  begin
    ThrowError('Error', 'host code raised an object that is not an Exception');
    Exit(False);
  end;
  if HostTrace and not (E is EG2Exception) then
  begin
    WriteLn(ErrOutput, 'host exception ', E.ClassName, ': ', E.Message);
    DumpExceptionBackTrace(ErrOutput);
  end;
  if E is EG2Exception then Pending := EG2Exception(E).Value.Raw
  else if E is EG2Error then ThrowError(EG2Error(E).Kind, UnicodeString(E.Message))
  else ThrowError('Error', UnicodeString(E.Message));
  Result := False;
end;

{ ================= conversions ================= }

function NumOrThrow(V: TRawValue): Double;
begin
  if G2.Core.IsNumber(V) then Exit(NumOf(V));
  if not ToNumberV(V, Result) then RaisePending;
end;

function ToInt32JS(D: Double): Int32;
var M: Double;
begin
  if IsNan(D) or IsInfinite(D) then Exit(0);
  M := Int(D);
  M := M - DFloor(M / 4294967296.0) * 4294967296.0;
  if M >= 2147483648.0 then M := M - 4294967296.0;
  Result := Int32(Trunc(M));
end;

function ToInt64JS(D: Double): Int64;
begin
  if IsNan(D) then Exit(0);
  if D >= 9.2233720368547758e18 then Exit(High(Int64));
  if D <= -9.2233720368547758e18 then Exit(Low(Int64));
  Result := Trunc(D);
end;

function StrOrThrow(V: TRawValue): UnicodeString;
var S: TRawValue;
begin
  if G2.Core.IsString(V) then Exit(StrToUnicode(StrOf(V)));
  if not ToStringV(V, S) then RaisePending;
  Result := StrToUnicode(StrOf(S));
end;

function HostObjectOf(V: TRawValue): TObject; inline;
begin
  Result := nil;
  if G2.Core.IsObject(V) and (ObjOf(V)^.OKind = okHost) then Result := TObject(ObjOf(V)^.Native);
end;

function TG2Value.IsUndefined: Boolean; begin Result := Raw = VUndefined; end;
function TG2Value.IsNull: Boolean; begin Result := Raw = VNull; end;
function TG2Value.IsNumber: Boolean; begin Result := G2.Core.IsNumber(Raw); end;
function TG2Value.IsString: Boolean; begin Result := G2.Core.IsString(Raw); end;
function TG2Value.IsBoolean: Boolean; begin Result := (Raw = VTrue) or (Raw = VFalse); end;
function TG2Value.IsObject: Boolean; begin Result := G2.Core.IsObject(Raw); end;
function TG2Value.IsFunction: Boolean; begin Result := IsCallable(Raw); end;
function TG2Value.AsNumber: Double; begin Result := NumOrThrow(Raw); end;
function TG2Value.AsInteger: Int64; begin Result := ToInt64JS(NumOrThrow(Raw)); end;
function TG2Value.AsString: UnicodeString; begin Result := StrOrThrow(Raw); end;
function TG2Value.AsBoolean: Boolean; begin Result := ToBool(Raw); end;
function TG2Value.AsObject: TObject; begin Result := HostObjectOf(Raw); end;

function TG2Value.Get(const Name: UnicodeString): TG2Value;
var Key: PStr; R: TRawValue;
begin
  Key := AtomOf(Name);   { atoms are permanent: they never move }
  if not GetV(Raw, Key, R) then RaisePending;
  Result.SetRaw(R);
end;

procedure TG2Value.Put(const Name: UnicodeString; const V: TG2Value);
var Key: PStr;
begin
  Key := AtomOf(Name);
  if not SetV(Raw, Key, V.Raw) then RaisePending;
end;

function CallRaw(F, This: TRawValue; const Args: array of TG2Value): TG2Value;
var Base, I: Integer; R: TRawValue; Ok: Boolean;
begin
  Base := TempTop;
  if Base + System.Length(Args) + 2 > System.Length(TempRoots) then
    raise EG2RangeError.Create('too many arguments for a call from host code');
  PushRoot(F);
  PushRoot(This);
  for I := 0 to High(Args) do PushRoot(Args[I].Raw);
  Ok := CallValue(RootAt(Base), RootAt(Base + 1), @TempRoots[Base + 2], Length(Args), R);
  TempTop := Base;
  if not Ok then RaisePending;
  Result.SetRaw(R);
end;

function TG2Value.Call(const Args: array of TG2Value): TG2Value;
begin
  Result := CallRaw(Raw, VUndefined, Args);
end;

function TG2Value.Invoke(const Method: UnicodeString; const Args: array of TG2Value): TG2Value;
var F: TG2Value;
begin
  F := Get(Method);
  Result := CallRaw(F.Raw, Raw, Args);
end;

function G2Undefined: TG2Value; begin Result.SetRaw(VUndefined); end;
function G2Null: TG2Value; begin Result.SetRaw(VNull); end;
function G2Number(D: Double): TG2Value; begin Result.SetRaw(MkNumber(D)); end;
function G2Str(const S: UnicodeString): TG2Value; begin Result.SetRaw(StrFromUnicode(S)); end;
function G2Bool(B: Boolean): TG2Value; begin Result.SetRaw(MkBool(B)); end;
function G2Object: TG2Value; begin Result.SetRaw(MkPtr(NewPlainObject(ObjectProto, 4))); end;

{ ---- TG2Call ---- }

function TG2Call.Count: Integer; begin Result := FArgc; end;

function TG2Call.Arg(I: Integer): TG2Value;
begin
  if (I >= 0) and (I < FArgc) then Result.SetRaw(FArgs[I + 1]) else Result.SetRaw(VUndefined);
end;

function TG2Call.This: TG2Value; begin Result.SetRaw(FArgs[0]); end;

function RawArg(const C: TG2Call; I: Integer): TRawValue; inline;
begin
  if (I >= 0) and (I < C.FArgc) then Result := C.FArgs[I + 1] else Result := VUndefined;
end;

function TG2Call.ArgNumber(I: Integer): Double; begin Result := NumOrThrow(RawArg(Self, I)); end;
function TG2Call.ArgInt(I: Integer): Int64; begin Result := ToInt64JS(NumOrThrow(RawArg(Self, I))); end;
function TG2Call.ArgString(I: Integer): UnicodeString; begin Result := StrOrThrow(RawArg(Self, I)); end;
function TG2Call.ArgBool(I: Integer): Boolean; begin Result := ToBool(RawArg(Self, I)); end;
function TG2Call.ArgObject(I: Integer): TObject; begin Result := HostObjectOf(RawArg(Self, I)); end;

{ ================= host objects and closures: weak tracking ================= }

type
  PWeak = ^TWeak;
  TWeak = record
    Cell: Pointer;        { the JS object; updated when the collector moves it }
    Obj: TObject;         { a host object's Pascal object, or nil }
    Owned: Boolean;
    FnIndex: Integer;     { a host closure's slot in HostFns, or -1 }
  end;

var
  YoungWeak, OldWeak: array of PWeak;
  YoungCount, OldCount: Integer;
  ObjMap: TDictionary<Pointer, PWeak>;   { Pascal object -> its wrapper }
  PendingFree: array of TObject;
  PendingFreeCount: Integer;
  PendingFn: array of Integer;
  PendingFnCount: Integer;
  HostFns: array of TG2Function;
  HostFnFree: array of Integer;
  HostFnCount, HostFnFreeTop: Integer;
  LiveObjects: Integer;

procedure AddWeak(W: PWeak);
begin
  if (PByte(W^.Cell) >= NurseryStart) and (PByte(W^.Cell) < NurseryLimit) then
  begin
    if YoungCount >= Length(YoungWeak) then SetLength(YoungWeak, YoungCount * 2 + 64);
    YoungWeak[YoungCount] := W;
    Inc(YoungCount);
  end
  else
  begin
    if OldCount >= Length(OldWeak) then SetLength(OldWeak, OldCount * 2 + 64);
    OldWeak[OldCount] := W;
    Inc(OldCount);
  end;
end;

{ in the collector: no allocation, no JS; freeing waits for FlushFinalizers }
procedure KillWeak(W: PWeak);
begin
  if W^.Obj <> nil then
  begin
    ObjMap.Remove(W^.Obj);
    if W^.Owned then
    begin
      if PendingFreeCount >= Length(PendingFree) then SetLength(PendingFree, PendingFreeCount * 2 + 64);
      PendingFree[PendingFreeCount] := W^.Obj;
      Inc(PendingFreeCount);
    end;
  end;
  if W^.FnIndex >= 0 then
  begin
    if PendingFnCount >= Length(PendingFn) then SetLength(PendingFn, PendingFnCount * 2 + 64);
    PendingFn[PendingFnCount] := W^.FnIndex;
    Inc(PendingFnCount);
  end;
  Dispose(W);
end;

procedure HostWeakHook;
var I, N: Integer; W: PWeak;
begin
  if GCInMinor then
  begin
    { young wrappers either moved to old space or died; old ones are not collected in a minor GC }
    for I := 0 to YoungCount - 1 do
    begin
      W := YoungWeak[I];
      if GCSurvives(W^.Cell) then
      begin
        if OldCount >= Length(OldWeak) then SetLength(OldWeak, OldCount * 2 + 64);
        OldWeak[OldCount] := W;
        Inc(OldCount);
      end
      else
        KillWeak(W);
    end;
    YoungCount := 0;
  end
  else
  begin
    N := 0;
    for I := 0 to OldCount - 1 do
    begin
      W := OldWeak[I];
      if GCSurvives(W^.Cell) then
      begin
        OldWeak[N] := W;
        Inc(N);
      end
      else
        KillWeak(W);
    end;
    OldCount := N;
  end;
end;

procedure FlushFinalizers;
var I: Integer; O: TObject;
begin
  while PendingFreeCount > 0 do
  begin
    Dec(PendingFreeCount);
    O := PendingFree[PendingFreeCount];
    Dec(LiveObjects);
    try
      O.Free;
    except
      { a destructor's exception has nowhere to go: the object is gone either way }
    end;
  end;
  for I := 0 to PendingFnCount - 1 do
  begin
    HostFns[PendingFn[I]] := nil;   { releases the closure and what it captured }
    if HostFnFreeTop >= Length(HostFnFree) then SetLength(HostFnFree, HostFnFreeTop * 2 + 64);
    HostFnFree[HostFnFreeTop] := PendingFn[I];
    Inc(HostFnFreeTop);
  end;
  PendingFnCount := 0;
end;

{ ================= host functions ================= }

function HostFnTrampoline(Args: PRawValue; Argc: Integer; var Res: TRawValue): Boolean;
var Idx: UInt32; Saved: Integer; C: TG2Call; R: TG2Value; Fn: TG2Function;
begin
  Idx := NativeData;   { read first: any nested native call overwrites it }
  Saved := TempTop;
  if PendingFreeCount + PendingFnCount > 0 then FlushFinalizers;
  C.FArgs := Args;
  C.FArgc := Argc;
  C.FInstance := nil;
  try
    Fn := HostFns[Idx];   { holds the closure while it runs }
    R := Fn(C);
    Res := R.Raw;
    Result := True;
  except
    on E: Exception do Result := HostFail(E, Saved);
    else Result := HostFail(nil, Saved);
  end;
end;

function G2Func(const Name: UnicodeString; Length: Integer; const Fn: TG2Function): TG2Value;
var Idx: Integer; W: PWeak;
begin
  if HostFnFreeTop > 0 then
  begin
    Dec(HostFnFreeTop);
    Idx := HostFnFree[HostFnFreeTop];
  end
  else
  begin
    if HostFnCount >= System.Length(HostFns) then SetLength(HostFns, HostFnCount * 2 + 64);
    Idx := HostFnCount;
    Inc(HostFnCount);
  end;
  HostFns[Idx] := Fn;
  Result.SetRaw(NewNativeFunctionData(HostFnTrampoline, Name, Length, Idx));
  New(W);
  W^.Cell := PtrOf(Result.Raw);
  W^.Obj := nil;
  W^.Owned := False;
  W^.FnIndex := Idx;
  AddWeak(W);
end;

{ ================= class binding ================= }

type
  TArgKind = (akSelf, akVmt, akDouble, akSingle, akInt32, akInt64, akBool, akUStr, akAStr, akObject,
    akValue, akValueRef, akCall, akResUStr, akResAStr, akResValue);
  TRetKind = (rkVoid, rkDouble, rkSingle, rkInt32, rkInt64, rkBool, rkObject, rkUStr, rkAStr, rkValue);

  TBinding = class;

  TPlan = class
    JSName: UnicodeString;
    Desc: string;                { Class.Method, for messages }
    Binding: TBinding;
    Owner: TClass;               { receivers must inherit from this }
    Code: CodePointer;
    VirtualIndex: Integer;       { >= 0: look the code up in the receiver's VMT }
    Sig: TGocciaFFICompiledSignature;
    Kinds: array of TArgKind;
    ArgClasses: array of TClass; { for akObject }
    JSArgc: Integer;
    Ret: TRetKind;
    RetClass: TClass;
    Borrowed, IsCtor, IsClassMethod, IsStatic: Boolean;
    NeedsTemps: Boolean;         { passes or returns strings or handles, or takes a TG2Call }
    Next: TPlan;                 { overloads }
    destructor Destroy; override;
  end;

  TPropPlan = class
    JSName: UnicodeString;
    Desc: string;
    Owner: TClass;
    Info: PPropInfo;
    Kind: TRetKind;
    ObjClass: TClass;
    KeepKey: PStr;     { hidden own property that keeps an assigned object's wrapper alive }
  end;

  TBinding = class
    Cls: TClass;
    JSName: UnicodeString;
    Index: Integer;
    Proto, Ctor: TG2Value;
    Ctors: TPlan;
    HasHiddenCtor: Boolean;   { declares constructors, none of them exposed }
  end;

var
  Plans: TObjectList<TPlan>;
  PropPlans: TObjectList<TPropPlan>;
  Bindings: TObjectList<TBinding>;
  Ctx: TRttiContext;

destructor TPlan.Destroy;
begin
  Sig.Free;
  inherited;
end;

function FindBinding(C: TClass): TBinding;
var B: TBinding;
begin
  while C <> nil do
  begin
    for B in Bindings do
      if B.Cls = C then Exit(B);
    C := C.ClassParent;
  end;
  Result := nil;
end;

function BindingExact(C: TClass): TBinding;
var B: TBinding;
begin
  for B in Bindings do
    if B.Cls = C then Exit(B);
  Result := nil;
end;

function ClassLabel(C: TClass): UnicodeString;
var B: TBinding;
begin
  for B in Bindings do
    if B.Cls = C then Exit(B.JSName);
  Result := UnicodeString(C.ClassName);
end;

{ Pascal name -> JS name: Length -> length, IsFar -> isFar, URLPath -> urlPath, X -> x }
function JSMemberName(const S: string): UnicodeString;
var I, N: Integer;
begin
  Result := UnicodeString(S);
  N := 0;
  while (N < System.Length(Result)) and CharInSet(Result[N + 1], ['A'..'Z']) do Inc(N);
  if N = 0 then Exit;
  if (N > 1) and (N < System.Length(Result)) then Dec(N);   { keep the capital that starts the next word }
  for I := 1 to N do Result[I] := WideChar(Ord(Result[I]) + 32);
end;

function JSClassName(C: TClass): UnicodeString;
var S: UnicodeString;
begin
  S := UnicodeString(C.ClassName);
  if (System.Length(S) > 1) and (S[1] = 'T') and CharInSet(S[2], ['A'..'Z']) then Delete(S, 1, 1);
  Result := S;
end;

function AttrName(const Attrs: TCustomAttributeArray; const Default: UnicodeString): UnicodeString;
var A: TCustomAttribute;
begin
  for A in Attrs do
    if A is JSNameAttribute then Exit(JSNameAttribute(A).Name);
  Result := Default;
end;

function HasAttr(const Attrs: TCustomAttributeArray; C: TClass): Boolean;
var A: TCustomAttribute;
begin
  for A in Attrs do
    if A is C then Exit(True);
  Result := False;
end;

function FloatKind(T: TRttiType; out K: TArgKind): Boolean;
begin
  Result := True;
  case TRttiFloatType(T).FloatType of
    ftDouble: K := akDouble;
    ftSingle: K := akSingle;
  else Result := False;
  end;
end;

{ the FFI argument kind of one parameter; False when this layer cannot pass it }
function ParamKind(P: TRttiParameter; out K: TArgKind; out C: TClass; out FT: TGocciaFFIType): Boolean;
var T: TRttiType;
begin
  Result := True;
  C := nil;
  FT := fftPointer;
  if pfSelf in P.Flags then begin K := akSelf; Exit; end;
  if pfVmt in P.Flags then begin K := akVmt; Exit; end;
  T := P.ParamType;
  if pfResult in P.Flags then
  begin
    if T = nil then Exit(False);
    case T.TypeKind of
      tkUString: K := akResUStr;
      tkAString: K := akResAStr;
      tkRecord: if T.Handle = TypeInfo(TG2Value) then K := akResValue else Exit(False);
    else Exit(False);
    end;
    Exit;
  end;
  if (T = nil) or ((P.Flags * [pfVar, pfOut, pfArray, pfHigh]) <> []) then Exit(False);
  if pfConstRef in P.Flags then
  begin
    { constref passes an address: supported for handles and the call record only }
    if T.Handle = TypeInfo(TG2Value) then K := akValueRef
    else if T.Handle = TypeInfo(TG2Call) then K := akCall
    else Exit(False);
    Exit;
  end;
  case T.TypeKind of
    tkFloat:
      begin
        if not FloatKind(T, K) then Exit(False);
        if K = akDouble then FT := fftF64 else FT := fftF32;
      end;
    tkInteger:
      if TRttiOrdinalType(T).OrdType = otSLong then begin K := akInt32; FT := fftI32; end else Exit(False);
    tkInt64: begin K := akInt64; FT := fftI64; end;
    tkBool:
      if T.Handle = TypeInfo(Boolean) then begin K := akBool; FT := fftBool; end else Exit(False);
    tkUString: K := akUStr;
    tkAString: K := akAStr;
    tkClass: begin K := akObject; C := TRttiInstanceType(T).MetaclassType; end;
    tkRecord:
      if (T.Handle = TypeInfo(TG2Value)) and (pfConst in P.Flags) then
      begin
        { a 4-byte record passed const arrives by value in a register (x86-64 System V): its slot }
        K := akValue;
        FT := fftI32;
      end
      else if (T.Handle = TypeInfo(TG2Call)) and (pfConst in P.Flags) then
        K := akCall   { larger than a register: passed by reference }
      else Exit(False);
  else Exit(False);
  end;
end;

function RetKindOf(T: TRttiType; out K: TRetKind; out C: TClass; out FT: TGocciaFFIType): Boolean;
var AK: TArgKind;
begin
  Result := True;
  C := nil;
  FT := fftVoid;
  K := rkVoid;
  if T = nil then Exit;
  case T.TypeKind of
    tkFloat:
      begin
        if not FloatKind(T, AK) then Exit(False);
        if AK = akDouble then begin K := rkDouble; FT := fftF64; end else begin K := rkSingle; FT := fftF32; end;
      end;
    tkInteger:
      if TRttiOrdinalType(T).OrdType = otSLong then begin K := rkInt32; FT := fftI32; end else Exit(False);
    tkInt64: begin K := rkInt64; FT := fftI64; end;
    tkBool:
      if T.Handle = TypeInfo(Boolean) then begin K := rkBool; FT := fftBool; end else Exit(False);
    tkClass: begin K := rkObject; C := TRttiInstanceType(T).MetaclassType; FT := fftPointer; end;
  else Exit(False);
  end;
end;

function BuildPlan(M: TRttiMethod; B: TBinding; const JSName: UnicodeString): TPlan;
var Ps: TRttiParameterArray; Descs: array of TGocciaFFITypeDescriptor; RetDesc: TGocciaFFITypeDescriptor;
  I, StrCount, ValCount: Integer; K: TArgKind; C: TClass; FT, RFT: TGocciaFFIType; HiddenResult: Boolean;
begin
  Result := TPlan.Create;
  try
    Result.JSName := JSName;
    Result.Desc := B.Cls.ClassName + '.' + M.Name;
    Result.Binding := B;
    Result.Owner := TRttiInstanceType(M.Parent).MetaclassType;
    Result.Code := M.CodeAddress;
    Result.IsCtor := M.IsConstructor;
    Result.IsClassMethod := M.IsClassMethod and not M.IsConstructor;
    Result.IsStatic := M.IsStatic;
    Result.Borrowed := HasAttr(M.GetAttributes, JSBorrowedAttribute);
    if M.IsConstructor or M.IsStatic then Result.VirtualIndex := -1 else Result.VirtualIndex := M.VirtualIndex;
    if not (M.CallingConvention in [ccReg, ccCdecl]) then
      raise EG2BindError.CreateFmt('%s: calling convention not supported', [Result.Desc]);
    Ps := TRttiMethodAccess(M).GetParameters(True);
    SetLength(Result.Kinds, System.Length(Ps));
    SetLength(Result.ArgClasses, System.Length(Ps));
    SetLength(Descs, System.Length(Ps));
    HiddenResult := False;
    StrCount := 0;
    ValCount := 0;
    for I := 0 to High(Ps) do
    begin
      if not ParamKind(Ps[I], K, C, FT) then
        raise EG2BindError.CreateFmt('%s: parameter %s cannot be passed from JS; mark the method [JSHidden]',
          [Result.Desc, Ps[I].Name]);
      Result.Kinds[I] := K;
      Result.ArgClasses[I] := C;
      if K in [akUStr, akAStr] then Inc(StrCount);
      if K in [akValue, akValueRef] then Inc(ValCount);
      if K in [akResUStr, akResAStr, akResValue] then HiddenResult := True;
      if not (TypInfo.pfHidden in Ps[I].Flags) then Inc(Result.JSArgc);
      Descs[I] := TGocciaFFITypeDescriptor.CreateScalar(FT);
    end;
    for I := 0 to High(Result.Kinds) do
      if Result.Kinds[I] in [akUStr, akAStr, akValue, akValueRef, akCall, akResUStr, akResAStr, akResValue] then
        Result.NeedsTemps := True;
    if (StrCount > MAX_STR_ARGS) or (ValCount > MAX_STR_ARGS) then
      raise EG2BindError.CreateFmt('%s: too many string or value parameters', [Result.Desc]);
    if HiddenResult then
    begin
      for I := 0 to High(Ps) do
        case Result.Kinds[I] of
          akResUStr: Result.Ret := rkUStr;
          akResAStr: Result.Ret := rkAStr;
          akResValue: Result.Ret := rkValue;
        end;
      RFT := fftVoid;
    end
    else if M.IsConstructor then
    begin
      Result.Ret := rkObject;
      Result.RetClass := B.Cls;
      RFT := fftPointer;
    end
    else if not RetKindOf(M.ReturnType, Result.Ret, Result.RetClass, RFT) then
      raise EG2BindError.CreateFmt('%s: the result cannot be passed to JS; mark the method [JSHidden]', [Result.Desc]);
    RetDesc := TGocciaFFITypeDescriptor.CreateScalar(RFT);
    Result.Sig := TGocciaFFICompiledSignature.Create(CurrentFFIABI, Descs, RetDesc);
    for I := 0 to High(Descs) do Descs[I].ReleaseReference;
    RetDesc.ReleaseReference;
    if Result.Sig.ArgumentDataSize > ARG_BUF_BYTES then
      raise EG2BindError.CreateFmt('%s: too many parameters', [Result.Desc]);
  except
    Result.Free;
    raise;
  end;
end;

function PickOverload(P: TPlan; Argc: Integer): TPlan;
var Above, Best: TPlan;
begin
  Above := nil;
  Best := P;
  while P <> nil do
  begin
    if P.JSArgc = Argc then Exit(P);
    if (P.JSArgc > Argc) and ((Above = nil) or (P.JSArgc < Above.JSArgc)) then Above := P;
    if P.JSArgc > Best.JSArgc then Best := P;
    P := P.Next;
  end;
  if Above <> nil then Result := Above else Result := Best;
end;

procedure RaiseUnregistered(Obj: TObject; Owned: Boolean);
var N: UnicodeString;
begin
  N := UnicodeString(Obj.ClassName);
  if Owned then
    try Obj.Free; except end;   { it was handed to the engine, which cannot represent it }
  raise EG2TypeError.Create('class ' + N + ' is not registered');
end;

{ Keeper: for a borrowed object, the JS object that owns it (a VM stack slot the GC updates, or nil).
  The wrapper keeps it alive, so the owner cannot be freed while JS can still reach the borrowed object. }
function WrapObject(Obj: TObject; Owned: Boolean; Keeper: PRawValue = nil): TRawValue;
var W: PWeak; B: TBinding; O: PObj;
begin
  if Obj = nil then Exit(VNull);
  if ObjMap.TryGetValue(Obj, W) then Exit(MkPtr(W^.Cell));
  B := FindBinding(Obj.ClassType);
  if B = nil then RaiseUnregistered(Obj, Owned);
  O := NewObject(RootShapeFor(B.Proto.Raw), 2, okHost);
  O^.Native := Obj;
  O^.Pad := B.Index;
  if (Keeper <> nil) and not Owned and G2.Core.IsObject(Keeper^) then
  begin
    O^.Aux1 := Keeper^;   { read after the allocation: Keeper points at a slot the GC updated }
    WriteBarrier(@O^.Hdr, O^.Aux1);
  end;
  New(W);
  W^.Cell := O;
  W^.Obj := Obj;
  W^.Owned := Owned;
  W^.FnIndex := -1;
  ObjMap.Add(Obj, W);
  AddWeak(W);
  if Owned then Inc(LiveObjects);
  Result := MkPtr(O);
end;

{ temporaries a call needs only when it passes or returns strings or handles: kept out of the plain path,
  where FPC's initialization and finalization of managed locals cost more than the call itself }
type
  PCallTemps = ^TCallTemps;
  TCallTemps = record
    US: array[0..MAX_STR_ARGS - 1] of UnicodeString;
    AS_: array[0..MAX_STR_ARGS - 1] of AnsiString;
    VH: array[0..MAX_STR_ARGS - 1] of TG2Value;
    ResU: UnicodeString;
    ResA: AnsiString;
    ResV: TG2Value;
    NS, NA, NV: Integer;
  end;

procedure RaiseArgType(P: TPlan; JSIndex, I: Integer);
begin
  raise EG2TypeError.Create(UnicodeString(P.Desc) + ': argument ' + UnicodeString(IntToStr(JSIndex)) +
    ' must be a ' + ClassLabel(P.ArgClasses[I]));
end;

procedure PutUStr(T: PCallTemps; V: TRawValue; D: PByte);
begin
  T^.US[T^.NS] := StrOrThrow(V);
  PPointer(D)^ := Pointer(T^.US[T^.NS]);
  Inc(T^.NS);
end;

procedure PutAStr(T: PCallTemps; V: TRawValue; D: PByte);
begin
  T^.AS_[T^.NA] := UTF8Encode(StrOrThrow(V));
  PPointer(D)^ := Pointer(T^.AS_[T^.NA]);
  Inc(T^.NA);
end;

procedure PutValue(T: PCallTemps; V: TRawValue; D: PByte; ByRef: Boolean);
begin
  T^.VH[T^.NV].SetRaw(V);
  if ByRef then PPointer(D)^ := @T^.VH[T^.NV]
  else PInt32(D)^ := T^.VH[T^.NV].FSlot;   { the record's bits, passed by value }
  Inc(T^.NV);
end;

function TempResult(P: TPlan; T: PCallTemps): TRawValue;
begin
  case P.Ret of
    rkUStr: Result := StrFromUnicode(T^.ResU);
    rkAStr: Result := StrFromUnicode(UTF8Decode(T^.ResA));
  else Result := T^.ResV.Raw;
  end;
end;

{ converts the JS arguments, makes the FFI call, converts the result; T is nil for plans without temporaries }
function InvokeWith(P: TPlan; Args: PRawValue; Argc: Integer; SelfPtr: Pointer; T: PCallTemps; CallRec: Pointer): TRawValue;
var Buf: array[0..ARG_BUF_BYTES div 8 - 1] of Int64; RetBuf: array[0..1] of Int64;
  I, J: Integer; D: PByte; V: TRawValue; O: TObject; Code: CodePointer;
begin
  J := 1;
  for I := 0 to High(P.Kinds) do
  begin
    D := PByte(@Buf[0]) + P.Sig.ArgumentDataOffset(I);
    case P.Kinds[I] of
      akSelf: begin PPointer(D)^ := SelfPtr; Continue; end;
      akVmt: begin PPointer(D)^ := Pointer(1); Continue; end;   { 1: allocate (psub.pas: "if vmt=1 then newinstance") }
      akResUStr: begin PPointer(D)^ := @T^.ResU; Continue; end;
      akResAStr: begin PPointer(D)^ := @T^.ResA; Continue; end;
      akResValue: begin PPointer(D)^ := @T^.ResV; Continue; end;
      akCall: begin PPointer(D)^ := CallRec; Inc(J); Continue; end;   { stands for all JS arguments }
    end;
    if J <= Argc then V := Args[J] else V := VUndefined;
    Inc(J);
    case P.Kinds[I] of
      akDouble: PDouble(D)^ := NumOrThrow(V);
      akSingle: PSingle(D)^ := NumOrThrow(V);
      akInt32: PInt32(D)^ := ToInt32JS(NumOrThrow(V));
      akInt64: PInt64(D)^ := ToInt64JS(NumOrThrow(V));
      akBool: PByte(D)^ := Ord(ToBool(V));
      akUStr: PutUStr(T, V, D);
      akAStr: PutAStr(T, V, D);
      akValue: PutValue(T, V, D, False);
      akValueRef: PutValue(T, V, D, True);
      akObject:
        begin
          if IsNullish(V) then O := nil
          else
          begin
            O := HostObjectOf(V);
            if (O = nil) or not O.InheritsFrom(P.ArgClasses[I]) then RaiseArgType(P, J - 1, I);
          end;
          PPointer(D)^ := O;
        end;
    end;
  end;
  Code := P.Code;
  { a virtual method dispatches through the receiver's VMT (the RTTI index counts from vmtMethodStart) }
  if P.VirtualIndex >= 0 then
    if P.IsClassMethod then Code := PCodePointer(PByte(SelfPtr) + vmtMethodStart + P.VirtualIndex * SizeOf(CodePointer))^
    else Code := PCodePointer(PByte(PPointer(SelfPtr)^) + vmtMethodStart + P.VirtualIndex * SizeOf(CodePointer))^;
  RetBuf[0] := 0;
  FFIInvokeCompiled(Code, P.Sig, @Buf[0], @RetBuf[0]);
  case P.Ret of
    rkVoid: Result := VUndefined;
    rkDouble: Result := MkNumber(PDouble(@RetBuf[0])^);
    rkSingle: Result := MkNumber(PSingle(@RetBuf[0])^);
    rkInt32: Result := MkNumber(PInt32(@RetBuf[0])^);
    rkInt64: Result := MkNumber(PInt64(@RetBuf[0])^);
    rkBool: Result := MkBool(PByte(@RetBuf[0])^ <> 0);
    rkObject:
      if P.Borrowed and not (P.IsCtor or P.IsClassMethod or P.IsStatic) then
        Result := WrapObject(TObject(PPointer(@RetBuf[0])^), False, @Args[0])
      else
        Result := WrapObject(TObject(PPointer(@RetBuf[0])^), not P.Borrowed or P.IsCtor);
  else Result := TempResult(P, T);
  end;
end;

function InvokeWithTemps(P: TPlan; Args: PRawValue; Argc: Integer; SelfPtr: Pointer): TRawValue;
var T: TCallTemps; CallRec: TG2Call;
begin
  T.NS := 0; T.NA := 0; T.NV := 0;
  CallRec.FArgs := Args;
  CallRec.FArgc := Argc;
  if P.IsCtor or P.IsClassMethod then CallRec.FInstance := nil else CallRec.FInstance := TObject(SelfPtr);
  Result := InvokeWith(P, Args, Argc, SelfPtr, @T, @CallRec);
end;

function InvokePlan(P: TPlan; Args: PRawValue; Argc: Integer; SelfPtr: Pointer): TRawValue;
begin
  if P.NeedsTemps then Result := InvokeWithTemps(P, Args, Argc, SelfPtr)
  else Result := InvokeWith(P, Args, Argc, SelfPtr, nil, nil);
end;

function CtorTrampoline(Args: PRawValue; Argc: Integer; var Res: TRawValue): Boolean; forward;

{ a class method's Self: the registered class this is (or a JS subclass of), as Pascal's TSub.Method }
function ClassFromThis(This: TRawValue; Base: TClass): TClass;
var V: TRawValue; O: PObj; Depth: Integer; C: TClass;
begin
  V := This;
  for Depth := 0 to 63 do
  begin
    if not G2.Core.IsObject(V) then Break;
    O := ObjOf(V);
    if (O^.OKind = okNative) and (O^.Native = Pointer(@CtorTrampoline)) then
    begin
      C := Bindings[O^.Pad].Cls;
      if C.InheritsFrom(Base) then Exit(C);
      Break;
    end;
    V := ProtoOf(O);
  end;
  Result := Base;
end;

function ReceiverOf(P: TPlan; This: TRawValue): Pointer;
var O: TObject;
begin
  if P.IsStatic then Exit(nil);
  if P.IsClassMethod then Exit(ClassFromThis(This, P.Owner));
  if not G2.Core.IsObject(This) or (ObjOf(This)^.OKind <> okHost) then
    raise EG2TypeError.Create(UnicodeString(P.Desc) + ': this is not a ' + ClassLabel(P.Owner));
  O := TObject(ObjOf(This)^.Native);
  if O = nil then raise EG2TypeError.Create(UnicodeString(P.Desc) + ': the object was released by the host');
  if not O.InheritsFrom(P.Owner) then
    raise EG2TypeError.Create(UnicodeString(P.Desc) + ': this is not a ' + ClassLabel(P.Owner));
  Result := O;
end;

function MethodTrampoline(Args: PRawValue; Argc: Integer; var Res: TRawValue): Boolean;
var P: TPlan; Saved: Integer;
begin
  P := Plans[NativeData];
  Saved := TempTop;
  if PendingFreeCount + PendingFnCount > 0 then FlushFinalizers;
  try
    P := PickOverload(P, Argc);
    Res := InvokePlan(P, Args, Argc, ReceiverOf(P, Args[0]));
    Result := True;
  except
    on E: Exception do Result := HostFail(E, Saved);
    else Result := HostFail(nil, Saved);
  end;
end;

function CtorTrampoline(Args: PRawValue; Argc: Integer; var Res: TRawValue): Boolean;
var B: TBinding; Saved: Integer; Proto: TG2Value; Obj: TObject; W: TRawValue;
begin
  B := Bindings[NativeData];
  Saved := TempTop;
  if PendingFreeCount + PendingFnCount > 0 then FlushFinalizers;
  try
    if CurNewTarget = VUndefined then raise EG2TypeError.Create('Class constructor ' + B.JSName + ' requires ''new''');
    Proto.SetRaw(FunctionPrototypeOf(CurNewTarget));    { subclasses and Reflect.construct }
    if B.Ctors <> nil then
      W := InvokePlan(PickOverload(B.Ctors, Argc), Args, Argc, B.Cls)
    else if B.HasHiddenCtor then
      raise EG2TypeError.Create(B.JSName + ' has no constructor available to JS')
    else
    begin
      Obj := B.Cls.Create;   { no constructor declared anywhere: TObject.Create }
      W := WrapObject(Obj, True);
    end;
    if not G2.Core.IsObject(W) then raise EG2TypeError.Create(B.JSName + ' constructor did not return an object');
    if G2.Core.IsObject(Proto.Raw) and (Proto.Raw <> B.Proto.Raw) then SetProto(ObjOf(W), Proto.Raw);
    Res := W;
    Result := True;
  except
    on E: Exception do Result := HostFail(E, Saved);
    else Result := HostFail(nil, Saved);
  end;
end;

function PropThis(PP: TPropPlan; This: TRawValue): TObject;
begin
  Result := HostObjectOf(This);
  if (Result = nil) or not Result.InheritsFrom(PP.Owner) then
    raise EG2TypeError.Create(UnicodeString(PP.Desc) + ': this is not a ' + ClassLabel(PP.Owner));
end;

function PropGetTrampoline(Args: PRawValue; Argc: Integer; var Res: TRawValue): Boolean;
var PP: TPropPlan; Saved: Integer; O: TObject;
begin
  PP := PropPlans[NativeData];
  Saved := TempTop;
  try
    O := PropThis(PP, Args[0]);
    case PP.Kind of
      rkDouble, rkSingle: Res := MkNumber(GetFloatProp(O, PP.Info));
      rkInt32: Res := MkNumber(GetOrdProp(O, PP.Info));
      rkInt64: Res := MkNumber(GetInt64Prop(O, PP.Info));
      rkBool: Res := MkBool(GetOrdProp(O, PP.Info) <> 0);
      rkUStr: Res := StrFromUnicode(GetUnicodeStrProp(O, PP.Info));
      rkAStr: Res := StrFromUnicode(UTF8Decode(GetStrProp(O, PP.Info)));
      rkObject: Res := WrapObject(GetObjectProp(O, PP.Info), False, @Args[0]);   { owned by its parent object }
    end;
    Result := True;
  except
    on E: Exception do Result := HostFail(E, Saved);
    else Result := HostFail(nil, Saved);
  end;
end;

function PropSetTrampoline(Args: PRawValue; Argc: Integer; var Res: TRawValue): Boolean;
var PP: TPropPlan; Saved: Integer; O, X: TObject; V: TRawValue; S: UnicodeString;
begin
  PP := PropPlans[NativeData];
  Saved := TempTop;
  try
    if Argc >= 1 then V := Args[1] else V := VUndefined;
    case PP.Kind of
      rkDouble, rkSingle: SetFloatProp(PropThis(PP, Args[0]), PP.Info, NumOrThrow(V));
      rkInt32: SetOrdProp(PropThis(PP, Args[0]), PP.Info, ToInt32JS(NumOrThrow(V)));
      rkInt64: SetInt64Prop(PropThis(PP, Args[0]), PP.Info, ToInt64JS(NumOrThrow(V)));
      rkBool: SetOrdProp(PropThis(PP, Args[0]), PP.Info, Ord(ToBool(V)));
      rkUStr: begin S := StrOrThrow(V); SetUnicodeStrProp(PropThis(PP, Args[0]), PP.Info, S); end;
      rkAStr: begin S := StrOrThrow(V); SetStrProp(PropThis(PP, Args[0]), PP.Info, UTF8Encode(S)); end;
      rkObject:
        begin
          O := PropThis(PP, Args[0]);
          if IsNullish(V) then X := nil else X := HostObjectOf(V);
          if (X = nil) and not IsNullish(V) or (X <> nil) and not X.InheritsFrom(PP.ObjClass) then
            raise EG2TypeError.Create(UnicodeString(PP.Desc) + ' must be a ' + ClassLabel(PP.ObjClass));
          SetObjectProp(O, PP.Info, X);
          { the parent now points at X: while the parent's wrapper lives, X's must too, or the engine
            would free X under the parent }
          DefineOwn(Args[0], PP.KeepKey, V, pfWritable or pfConfigurable);
        end;
    end;
    Res := VUndefined;
    Result := True;
  except
    on E: Exception do Result := HostFail(E, Saved);
    else Result := HostFail(nil, Saved);
  end;
end;

procedure DefineOn(Target: TG2Value; const Name: UnicodeString; const V: TG2Value; Flags: Byte);
var Key: PStr;
begin
  Key := AtomOf(Name);
  DefineOwn(Target.Raw, Key, V.Raw, Flags);
end;

function RegisterClassRtti(AClass: TClass; const AName: UnicodeString): TG2Value;
var B, Parent: TBinding; C: TClass; T: TRttiType; M: TRttiMethod; Pr: TRttiProperty;
  Names: TDictionary<UnicodeString, TPlan>; Seen: TDictionary<UnicodeString, Boolean>;
  P, Q: TPlan; PP: TPropPlan; JSName: UnicodeString; Getter, Setter: TG2Value; RK: TRetKind; RC: TClass;
  FT: TGocciaFFIType; Pair: TPair<UnicodeString, TPlan>; Target: TG2Value; CtorLevel: TClass;
  PlanMark, PropMark: Integer;
begin
  if BindingExact(AClass) <> nil then raise EG2BindError.CreateFmt('%s is already registered', [AClass.ClassName]);
  Parent := FindBinding(AClass.ClassParent);
  B := TBinding.Create;
  B.Cls := AClass;
  if AName <> '' then B.JSName := AName else B.JSName := JSClassName(AClass);
  B.Index := Bindings.Count;
  Bindings.Add(B);
  if Parent <> nil then B.Proto.SetRaw(MkPtr(NewPlainObject(Parent.Proto.Raw, 4)))
  else B.Proto.SetRaw(MkPtr(NewPlainObject(ObjectProto, 4)));
  PlanMark := Plans.Count;
  PropMark := PropPlans.Count;
  Names := TDictionary<UnicodeString, TPlan>.Create;
  Seen := TDictionary<UnicodeString, Boolean>.Create;
  try
  try
    { members of this class and of its unregistered ancestors (a registered ancestor's are reached
      through the prototype chain); the nearest declaration of a name wins, as in Pascal }
    C := AClass;
    CtorLevel := nil;
    while (C <> nil) and (C <> TObject) and ((C = AClass) or (BindingExact(C) = nil)) do
    begin
      T := Ctx.GetType(C);
      for M in T.GetDeclaredMethods do
      begin
        if not (M.Visibility in [mvPublic, mvPublished]) then
        begin
          if M.IsConstructor then B.HasHiddenCtor := True;
          Continue;
        end;
        if M.IsDestructor or (M.MethodKind in [mkClassConstructor, mkClassDestructor, mkOperatorOverload]) then Continue;
        if HasAttr(M.GetAttributes, JSHiddenAttribute) then
        begin
          if M.IsConstructor then B.HasHiddenCtor := True;
          Continue;
        end;
        if M.IsConstructor then
        begin
          if (CtorLevel <> nil) and (CtorLevel <> C) then Continue;   { constructors come from one level }
          CtorLevel := C;
          P := BuildPlan(M, B, B.JSName);
          Plans.Add(P);
          P.Next := B.Ctors;
          B.Ctors := P;
          Continue;
        end;
        JSName := AttrName(M.GetAttributes, JSMemberName(M.Name));
        if Seen.ContainsKey(JSName) and not Names.ContainsKey(JSName) then Continue;   { overridden below }
        if Names.TryGetValue(JSName, Q) and (Q.Owner <> TRttiInstanceType(M.Parent).MetaclassType) then Continue;
        P := BuildPlan(M, B, JSName);
        Plans.Add(P);
        if Names.TryGetValue(JSName, Q) then
        begin
          P.Next := Q;
          Names[JSName] := P;
        end
        else
          Names.Add(JSName, P);
      end;
      for Pr in T.GetDeclaredProperties do
      begin
        if not (Pr.Visibility in [mvPublic, mvPublished]) then Continue;
        if HasAttr(Pr.GetAttributes, JSHiddenAttribute) then Continue;
        JSName := AttrName(Pr.GetAttributes, JSMemberName(Pr.Name));
        if Seen.ContainsKey(JSName) or Names.ContainsKey(JSName) then Continue;
        if (Pr.PropertyType <> nil) and (Pr.PropertyType.TypeKind = tkUString) then RK := rkUStr
        else if (Pr.PropertyType <> nil) and (Pr.PropertyType.TypeKind = tkAString) then RK := rkAStr
        else if not RetKindOf(Pr.PropertyType, RK, RC, FT) or (RK = rkVoid) then
          raise EG2BindError.CreateFmt('%s.%s: property type cannot be passed to JS; mark it [JSHidden]',
            [AClass.ClassName, Pr.Name]);
        PP := TPropPlan.Create;
        PP.JSName := JSName;
        PP.Desc := AClass.ClassName + '.' + Pr.Name;
        PP.Owner := C;
        PP.Info := PPropInfo(Pr.Handle);
        PP.Kind := RK;
        if RK = rkObject then
        begin
          PP.ObjClass := TRttiInstanceType(Pr.PropertyType).MetaclassType;
          PP.KeepKey := AtomOf(#0'keep:' + JSName);
        end;
        PropPlans.Add(PP);
        Seen.Add(JSName, True);
        Getter.SetRaw(VUndefined);
        Setter.SetRaw(VUndefined);
        if Pr.IsReadable then Getter.SetRaw(NewNativeFunctionData(PropGetTrampoline, 'get ' + JSName, 0, PropPlans.Count - 1));
        if Pr.IsWritable then Setter.SetRaw(NewNativeFunctionData(PropSetTrampoline, 'set ' + JSName, 1, PropPlans.Count - 1));
        Target.SetRaw(MkPtr(NewAccessor(Getter.Raw, Setter.Raw)));
        DefineOn(B.Proto, JSName, Target, pfConfigurable or pfAccessor);
      end;
      for Pair in Names do Seen.AddOrSetValue(Pair.Key, True);
      C := C.ClassParent;
    end;
    { constructors inherited from further up (Pascal constructors are inherited) }
    if B.Ctors = nil then
    begin
      C := AClass.ClassParent;
      while (B.Ctors = nil) and (C <> nil) and (C <> TObject) do
      begin
        for M in Ctx.GetType(C).GetDeclaredMethods do
          if M.IsConstructor then
            if (M.Visibility in [mvPublic, mvPublished]) and not HasAttr(M.GetAttributes, JSHiddenAttribute) then
            begin
              P := BuildPlan(M, B, B.JSName);
              Plans.Add(P);
              P.Next := B.Ctors;
              B.Ctors := P;
            end
            else B.HasHiddenCtor := True;
        C := C.ClassParent;
      end;
    end;
    B.Ctor.SetRaw(NewNativeFunctionData(CtorTrampoline, B.JSName, 0, B.Index));
    if B.Ctors <> nil then ObjOf(B.Ctor.Raw)^.Aux2 := MkInt(PickOverload(B.Ctors, 0).JSArgc);
    if Parent <> nil then SetProto(ObjOf(B.Ctor.Raw), Parent.Ctor.Raw);
    DefineOn(B.Ctor, 'prototype', B.Proto, 0);
    DefineOn(B.Proto, 'constructor', B.Ctor, pfHidden);
    for Pair in Names do
    begin
      P := Pair.Value;
      Target.SetRaw(NewNativeFunctionData(MethodTrampoline, P.JSName, PickOverload(P, 0).JSArgc, Plans.IndexOf(P)));
      if P.IsClassMethod or P.IsStatic then DefineOn(B.Ctor, P.JSName, Target, pfHidden)
      else DefineOn(B.Proto, P.JSName, Target, pfHidden);
    end;
    DefineOwn(GlobalObj, AtomOf(B.JSName), B.Ctor.Raw, pfHidden);
    Result := B.Ctor;
  except
    { leave nothing half-registered: the class can be fixed and registered again }
    while Plans.Count > PlanMark do Plans.Delete(Plans.Count - 1);
    while PropPlans.Count > PropMark do PropPlans.Delete(PropPlans.Count - 1);
    Bindings.Remove(B);
    raise;
  end;
  finally
    Names.Free;
    Seen.Free;
  end;
end;

{ ================= engine ================= }

var
  EngineLive: Boolean;
  EngineUsed: Boolean;

constructor JSNameAttribute.Create(const AName: UnicodeString);
begin
  Name := AName;
end;

constructor JSHiddenAttribute.Create;
begin
end;

constructor JSBorrowedAttribute.Create;
begin
end;

constructor TG2Engine.Create(NurseryBytes: SizeInt);
begin
  if EngineUsed then raise EG2Error.Create('Error', 'G2 supports one engine per process');
  EngineUsed := True;
  EngineLive := True;
  HostTrace := GetEnvironmentVariable('G2_HOSTTRACE') <> '';
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  HeapInit(NurseryBytes);
  VMInit;
  InitBuiltins;
  InitBuiltins2;
  InitRegExp;
  InitEval;
  InitPromise;
  InitBuiltins3;
  InitBuiltins4;
  InitObjModel;
  InitArrayProto;
  InitIterHelpers;
  InitTypedArrays;
  InitDisposal;
  InitModules;
  InitBuiltins5;
  InitTemporal;
  HostRootScanner := HostScanRoots;
  GCWeakHook := HostWeakHook;
  ObjMap := TDictionary<Pointer, PWeak>.Create;
  Plans := TObjectList<TPlan>.Create(True);
  PropPlans := TObjectList<TPropPlan>.Create(True);
  Bindings := TObjectList<TBinding>.Create(True);
  Ctx := TRttiContext.Create(False);   { the RTL default sees published members only }
end;

destructor TG2Engine.Destroy;
var I: Integer;
  procedure Drop(W: PWeak);
  begin
    if (W^.Obj <> nil) and W^.Owned then
    begin
      Dec(LiveObjects);
      try W^.Obj.Free; except end;
    end;
    Dispose(W);
  end;
begin
  FlushFinalizers;
  for I := 0 to YoungCount - 1 do Drop(YoungWeak[I]);
  for I := 0 to OldCount - 1 do Drop(OldWeak[I]);
  YoungCount := 0;
  OldCount := 0;
  ObjMap.Free;
  for I := 0 to HostFnCount - 1 do HostFns[I] := nil;
  Bindings.Free;
  PropPlans.Free;
  Plans.Free;
  Ctx.Free;
  GCWeakHook := nil;
  HostRootScanner := nil;
  EngineLive := False;
  inherited;
end;

function TG2Engine.Eval(const Source: UnicodeString; const FileName: string): TG2Value;
var L: TGocciaLexer; P: TGocciaParser; Prog: TGocciaProgram; Opts: TGocciaParserOptions; T: PFuncTemplate;
  Res: TRawValue; Ok: Boolean;
begin
  if FrameTop > 0 then raise EG2Error.Create('Error', 'Eval cannot run while a script is running');
  L := TGocciaLexer.Create(Source, FileName);
  P := nil;
  Prog := nil;
  try
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
    try
      Prog := P.Parse;
    except
      on E: Exception do raise EG2SyntaxError.Create(E.Message);
    end;
    T := CompileProgram(Prog, UnicodeString(FileName), True);
  finally
    Prog.Free;
    P.Free;
    L.Free;
  end;
  Ok := RunScriptRes(T, Res);
  StackTop := 0;
  if not Ok then RaisePending;
  Result.SetRaw(Res);
  if not RunMicrotasks then RaisePending;
  FlushFinalizers;
end;

function TG2Engine.Global: TG2Value;
begin
  Result.SetRaw(GlobalObj);
end;

procedure TG2Engine.SetGlobal(const Name: UnicodeString; const V: TG2Value);
var Key: PStr;
begin
  Key := AtomOf(Name);
  DefineOwn(GlobalObj, Key, V.Raw, pfHidden);
end;

procedure TG2Engine.DefineFunction(const Name: UnicodeString; Length: Integer; const Fn: TG2Function);
begin
  SetGlobal(Name, G2Func(Name, Length, Fn));
end;

function TG2Engine.RegisterClass(AClass: TClass; const AName: UnicodeString): TG2Value;
begin
  Result := RegisterClassRtti(AClass, AName);
end;

function TG2Engine.Wrap(Obj: TObject; Owned: Boolean): TG2Value;
begin
  Result.SetRaw(WrapObject(Obj, Owned));
end;

procedure TG2Engine.Forget(Obj: TObject);
var W: PWeak;
begin
  if not ObjMap.TryGetValue(Obj, W) then Exit;
  ObjMap.Remove(Obj);
  if W^.Owned then Dec(LiveObjects);
  PObj(W^.Cell)^.Native := nil;
  W^.Obj := nil;
end;

procedure TG2Engine.CollectGarbage;
begin
  CollectMinor;
  CollectMajor;
  FlushFinalizers;
end;

function TG2Engine.LiveHostObjects: Integer;
begin
  Result := LiveObjects;
end;

end.

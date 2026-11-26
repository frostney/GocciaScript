unit Goccia.FFI;
{ The FFI global (docs/built-ins-ffi.md): FFI.open, FFILibrary (bind, symbol, close, path, closed), FFIPointer,
  FFI.nullptr and FFI.suffix. The native calls go through Goccia's engine-independent FFI units: the descriptors and
  their layout (Goccia.FFI.Types), the compiled call plans (Goccia.FFI.ABI, Goccia.FFI.Call) and the library guard
  (Goccia.FFI.LibraryGuard). This unit is their JS face, as Goccia.Builtins.GlobalFFI and Goccia.Values.FFILibrary /
  FFIPointer are GocciaScript's.

  FFI objects are ordinary objects with internal slots holding ids into this thread's tables (libraries, bound
  functions). Nothing here is finalized by the collector: the libraries and the bound functions' plans are released
  when the engine is torn down (FFIDone), so a closed library is unloaded no earlier than its dependents go. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}

interface

uses
  Goccia.Capabilities;

{ installs the FFI global (a const binding of the global scope, as GocciaScript's runtime extension makes it) for the
  engine's realm, FFI.open judging every library by Caps' ffi scopes; False with Err when it could not }
function InstallFFI(const Caps: TGocciaCapabilities; var Err: UnicodeString): Boolean;
{ releases this thread's libraries and bound functions (engine teardown) }
procedure FFIDone;

implementation

uses
  {$IFDEF LINUX}BaseUnix,{$ENDIF}
  SysUtils, Math,
  Goccia.CapabilityAudit, Goccia.Error.Messages, Goccia.Error.Suggestions, Goccia.FFI.ABI, Goccia.FFI.Call, Goccia.FFI.LibraryGuard,
  Goccia.FFI.CallbackSlots, Goccia.FFI.Types, Goccia.FFI.UTF8String, Goccia.URI,
  Goccia.Core, Goccia.Instructions, Goccia.VM, Goccia.Eval, Goccia.Typed, Goccia.ThreadCleanupRegistry;

const
  {$IFDEF DARWIN}
  SHARED_LIBRARY_SUFFIX = '.dylib';
  {$ELSE}
  {$IFDEF MSWINDOWS}
  SHARED_LIBRARY_SUFFIX = '.dll';
  {$ELSE}
  SHARED_LIBRARY_SUFFIX = '.so';
  {$ENDIF}
  {$ENDIF}

type
  TLib = record
    Guard: TGocciaFFILibraryGuard;
    { dependents (bound functions, symbol pointers) retained on Guard, released at FFIDone }
    Retained: Integer;
  end;
  TBound = record
    Symbol: Pointer;
    Sig: TGocciaFFICompiledSignature;
    Lib: Integer;
    Name: UnicodeString;
    Variadic: Boolean;
  end;
  TFFITypeArray = array of TGocciaFFITypeDescriptor;
  TTypePosition = (tpBoundArgument, tpBoundReturn, tpStorage, tpCallbackArgument, tpCallbackReturn);
  { an aggregate field accessor's data: the aggregate's descriptor and the field (or element) }
  TFieldRef = record
    Desc: TGocciaFFITypeDescriptor;
    Index: Integer;
  end;
  { a pointer stored in an aggregate's bytes at Off (from the root's start) that came from library Lib (or from
    callback -2 - Lib): it is refused once that is closed, until something else is written over it }
  TTie = record
    Off, Lib: Integer;
  end;

threadvar
  Caps: TGocciaCapabilities;
  Libs: array of TLib;
  Bounds: array of TBound;
  { the realm's FFI object, its prototypes and FFI.nullptr (rooted once per engine) }
  FFIObj, LibProto, PtrProto, NullPtr: TValue;
  RootedGen: Integer;
  { internal slots: a library's id; a pointer's address and its library (-1 none) }
  KLib, KPtr, KPtrLib: PStr;
  { descriptors by id (each referenced until FFIDone), the aggregate accessors' data, the pointer ties of each
    root aggregate }
  Descs: array of TGocciaFFITypeDescriptor;
  FieldRefs: array of TFieldRef;
  Ties: array of array of TTie;
  { FFIType's and FFIAggregate's shared prototypes, ArrayBuffer.prototype, and per descriptor id the array of its
    fields' getters and setters (getter 2i, setter 2i + 1) }
  TypeProto, AggProto, ABProto, AccFns: TValue;
  { internal slots: a descriptor's id; an aggregate's descriptor id, buffer, byte offset and root id }
  KType, KAgg, KBuf, KOff, KRoot: PStr;

type
  TCb = record
    Desc: TGocciaFFITypeDescriptor;
    Sig: TGocciaFFICompiledSignature;
    Slot: Integer;
    Code: Pointer;
    Closed: Boolean;
  end;

threadvar
  { callback handles by id (each slot released when closed or at FFIDone), their callables (CbFns, an array by
    id), FFICallback's prototype and its internal slot }
  Cbs: array of TCb;
  { the callbacks open now: native code can call any of them during any call }
  OpenCbs: Integer;
  CbFns, CbProto, CbCloseFn: TValue;
  KCb: PStr;
  { the bound calls running now, innermost last: whether a callback of each failed, and its exception (CallPending,
    an array by depth) }
  CallFailed: array of Boolean;
  CallDepth: Integer;
  CallPending: TValue;
  { FFI.varargs markers: their types by id (each referenced), FFIVarArgs's prototype and the internal slots }
  VATypes: array of array of TGocciaFFITypeDescriptor;
  VAProto: TValue;
  KVA, KVAVals: PStr;

{ ---------------- helpers ---------------- }

function InternalOf(V: TValue; Key: PStr; out Slot: TValue): Boolean;
var I: Integer;
begin
  Result := False;
  if not IsObject(V) or (ObjOf(V)^.OKind <> okPlain) then Exit;
  I := GetOwnSlot(ObjOf(V), Key);
  if I < 0 then Exit;
  Slot := GetSlot(ObjOf(V), I);
  Result := True;
end;

function LibOf(V: TValue; out Id: Integer): Boolean;
var S: TValue;
begin
  Result := InternalOf(V, KLib, S);
  if Result then Id := Trunc(NumOf(S));
end;

function ArgOf(Args: PValue; Argc, I: Integer): TValue; inline;
begin
  if I <= Argc then Result := Args[I] else Result := VUndefined;
end;

procedure DefineGetterFn(O: TValue; const Name: UnicodeString; Fn: TNativeFn);
var F: TValue;
begin
  PushRoot(O);
  F := NewNativeFunction(Fn, 'get ' + Name, 0);
  F := MkPtr(NewAccessor(F, VUndefined));
  DefineOwn(RootAt(TempTop - 1), AtomOf(Name), F, pfConfigurable or pfAccessor);
  PopRoots(1);
end;

{ the PermissionDenied a capability check throws: "ffi: <scope>", with the host's suggestion of the grant to ask
  for }
function ThrowPermissionDenied(const Scope, Suggestion: UnicodeString): Boolean;
var E: TValue;
begin
  if NewPermissionDenied('ffi', Scope, Suggestion, E) then Pending := E;
  Result := False;
end;

{ a string argument's text (ToString) }
function TextOf(V: TValue; out Text: UnicodeString): Boolean;
var S: TValue;
begin
  Result := ToStringV(V, S);
  if Result then Text := StrToUnicode(StrOf(S));
end;

function NewPointer(Address: Pointer; Lib: Integer): TValue;
var O: PObj;
begin
  O := NewPlainObject(PtrProto, 2);
  PushRoot(MkPtr(O));
  DefineOwn(RootAt(TempTop - 1), KPtr, MkNumber(Double(PtrUInt(Address))), pfHidden);
  DefineOwn(RootAt(TempTop - 1), KPtrLib, MkInt(Lib), pfHidden);
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ a pointer's address, False with a TypeError once its library (or callback) is closed }
function PointerAddress(V: TValue; out Address: Pointer): Boolean;
var A, L: TValue; Lib: Integer;
begin
  Result := InternalOf(V, KPtr, A) and InternalOf(V, KPtrLib, L);
  if not Result then Exit;
  Lib := Trunc(NumOf(L));
  if (Lib >= 0) and Libs[Lib].Guard.IsClosed then Exit(ThrowTypeError(SErrorFFIPointerLibraryClosed));
  { a callback's pointer (tie -2 - id) is refused once the callback is closed }
  if (Lib <= -2) and Cbs[-2 - Lib].Closed then Exit(ThrowTypeError(SErrorFFICallbackClosed));
  Address := Pointer(PtrUInt(Trunc(NumOf(A))));
end;

{ ---------------- FFIPointer ---------------- }

function Ptr_Address(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var P: Pointer; A: TValue;
begin
  if not InternalOf(Args[0], KPtr, A) then Exit(ThrowTypeError('FFIPointer.address requires an FFIPointer'));
  if not PointerAddress(Args[0], P) then Exit(False);
  Res := MkNumber(Double(PtrUInt(P)));
  Result := True;
end;

function Ptr_IsNull(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var P: Pointer; A: TValue;
begin
  if not InternalOf(Args[0], KPtr, A) then Exit(ThrowTypeError('FFIPointer.isNull requires an FFIPointer'));
  if not PointerAddress(Args[0], P) then Exit(False);
  Res := MkBool(P = nil);
  Result := True;
end;

{ ---------------- descriptors and aggregates ---------------- }

function PointerArg(V: TValue; Index: Integer; out Address: Pointer): Boolean; forward;
function MarshalScalar(AType: TGocciaFFITypeDescriptor; V: TValue; Index: Integer; Dest: PByte;
  var Str: TBytes): Boolean; forward;
function UnmarshalScalar(AType: TGocciaFFITypeDescriptor; Data: PByte; Lib: Integer; var Res: TValue): Boolean;
  forward;
function CbOf(V: TValue; out Id: Integer): Boolean; forward;
function ParseType(V: TValue; Position: TTypePosition; out AType: TGocciaFFITypeDescriptor): Boolean; forward;

const
  KindNames: array[TGocciaFFITypeKind] of UnicodeString = ('scalar', 'struct', 'union', 'array', 'callback',
    'nullable');

function DescIdOf(D: TGocciaFFITypeDescriptor): Integer;
var I: Integer;
begin
  for I := 0 to High(Descs) do
    if Descs[I] = D then Exit(I);
  Result := -1;
end;

{ a descriptor object's descriptor }
function TypeOf(V: TValue; out D: TGocciaFFITypeDescriptor): Boolean;
var S: TValue;
begin
  Result := InternalOf(V, KType, S);
  if Result then D := Descs[Trunc(NumOf(S))];
end;

{ an aggregate value's parts }
function AggOf(V: TValue; out Id: Integer; out Buf: TValue; out Off, Root: Integer): Boolean;
var S: TValue;
begin
  Result := InternalOf(V, KAgg, S);
  if not Result then Exit;
  Id := Trunc(NumOf(S));
  InternalOf(V, KBuf, Buf);
  InternalOf(V, KOff, S);
  Off := Trunc(NumOf(S));
  InternalOf(V, KRoot, S);
  Root := Trunc(NumOf(S));
end;

{ the bytes of an aggregate of Size at Off in Buf, nil once the buffer is detached or shrunk past them }
function AggData(Buf: TValue; Off, Size: Integer): PByte;
var B: PObj;
begin
  B := ObjOf(Buf);
  if BufferDetached(B) or (Off + Size > BufferLen(B)) then Exit(nil);
  Result := BufferData(B) + Off;
end;

{ an aggregate's data, False with a TypeError once its backing store is gone }
function AggValueData(V: TValue; out D: TGocciaFFITypeDescriptor; out P: PByte): Boolean;
var Id, Off, Root: Integer; Buf: TValue;
begin
  if not AggOf(V, Id, Buf, Off, Root) then Exit(False);
  D := Descs[Id];
  P := AggData(Buf, Off, D.Size);
  if P = nil then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
  Result := True;
end;

{ the ties of a root aggregate: one dropped where [Off, Off + Size) overlaps its pointer, one added }
procedure DropTies(Root, Off, Size: Integer);
var I, J: Integer;
begin
  J := 0;
  for I := 0 to High(Ties[Root]) do
    if (Ties[Root][I].Off + SizeOf(Pointer) <= Off) or (Ties[Root][I].Off >= Off + Size) then
    begin
      Ties[Root][J] := Ties[Root][I];
      Inc(J);
    end;
  SetLength(Ties[Root], J);
end;

procedure AddTie(Root, Off, Lib: Integer);
var N: Integer;
begin
  DropTies(Root, Off, SizeOf(Pointer));
  if Lib = -1 then Exit;
  N := Length(Ties[Root]);
  SetLength(Ties[Root], N + 1);
  Ties[Root][N].Off := Off;
  Ties[Root][N].Lib := Lib;
end;

function TieAt(Root, Off: Integer): Integer;
var I: Integer;
begin
  for I := 0 to High(Ties[Root]) do
    if Ties[Root][I].Off = Off then Exit(Ties[Root][I].Lib);
  Result := -1;
end;

{ every pointer field of D at Base in a root aggregate tied to Lib (an aggregate a native function returned) }
procedure TieAll(Root, Base: Integer; D: TGocciaFFITypeDescriptor; Lib: Integer);
var I: Integer;
begin
  case D.Kind of
    ftkScalar: if D.ScalarType = fftPointer then AddTie(Root, Base, Lib);
    ftkCallback: AddTie(Root, Base, Lib);
    ftkStruct, ftkUnion:
      for I := 0 to D.FieldCount - 1 do TieAll(Root, Base + D.FieldOffsetAt(I), D.FieldTypeAt(I), Lib);
    ftkArray:
      for I := 0 to D.ElementCount - 1 do TieAll(Root, Base + I * D.ElementType.Size, D.ElementType, Lib);
  end;
end;

function NewRoot: Integer;
begin
  Result := Length(Ties);
  SetLength(Ties, Result + 1);
end;

function FieldName(D: TGocciaFFITypeDescriptor; I: Integer): UnicodeString;
begin
  if D.Kind = ftkArray then Result := IntToStr(I) else Result := D.FieldAt(I).Name;
end;

function FieldCountOf(D: TGocciaFFITypeDescriptor): Integer;
begin
  if D.Kind = ftkArray then Result := D.ElementCount else Result := D.FieldCount;
end;

{ an aggregate value of descriptor Id over Buf at Off: its fields as accessors, then buffer, byteOffset, size and
  (an array's) length where no field has the name }
function NewAggregate(Id: Integer; Buf: TValue; Off, Root: Integer): TValue;
var D: TGocciaFFITypeDescriptor; I, N: Integer; Arr, G, S: TValue;
  procedure Meta(const Name: UnicodeString; V: TValue);
  begin
    if (D.Kind <> ftkArray) and (D.FieldIndex(Name) >= 0) then Exit;
    DefineOwn(RootAt(TempTop - 1), AtomOf(Name), V, 0);
  end;
begin
  D := Descs[Id];
  PushRoot(Buf);
  PushRoot(MkPtr(NewPlainObject(AggProto, 8)));
  DefineOwn(RootAt(TempTop - 1), KAgg, MkInt(Id), pfHidden);
  DefineOwn(RootAt(TempTop - 1), KBuf, RootAt(TempTop - 2), pfHidden);
  DefineOwn(RootAt(TempTop - 1), KOff, MkInt(Off), pfHidden);
  DefineOwn(RootAt(TempTop - 1), KRoot, MkInt(Root), pfHidden);
  GetElemV(AccFns, MkInt(Id), Arr);
  PushRoot(Arr);
  N := FieldCountOf(D);
  for I := 0 to N - 1 do
  begin
    GetElemV(RootAt(TempTop - 1), MkInt(2 * I), G);
    PushRoot(G);
    GetElemV(RootAt(TempTop - 2), MkInt(2 * I + 1), S);
    PushRoot(S);
    G := MkPtr(NewAccessor(RootAt(TempTop - 2), RootAt(TempTop - 1)));
    DefineOwn(RootAt(TempTop - 4), AtomOf(FieldName(D, I)), G, pfEnumerable or pfAccessor);
    PopRoots(2);
  end;
  PopRoots(1);
  Meta('buffer', RootAt(TempTop - 2));
  Meta('byteOffset', MkInt(Off));
  Meta('size', MkInt(D.Size));
  if D.Kind = ftkArray then Meta('length', MkInt(D.ElementCount));
  Result := RootAt(TempTop - 1);
  PopRoots(2);
end;

{ a new root aggregate of descriptor Id, its bytes zeroed (Data nil) or copied from Data }
function NewRootAggregate(Id: Integer; Data: PByte): TValue;
var D: TGocciaFFITypeDescriptor; B: PObj;
begin
  D := Descs[Id];
  B := NewArrayBuffer(D.Size, ABProto);
  if (Data <> nil) and (D.Size > 0) then Move(Data^, BufferData(B)^, D.Size);
  Result := NewAggregate(Id, MkPtr(B), 0, NewRoot);
end;

function WriteField(T: TGocciaFFITypeDescriptor; V: TValue; This: TValue; FOff: Integer): Boolean; forward;

{ reads a field of type T at FOff (from the aggregate's start) of This }
function ReadField(T: TGocciaFFITypeDescriptor; This: TValue; FOff: Integer; var Res: TValue): Boolean;
var Id, Off, Root, TId: Integer; Buf: TValue; P: PByte; Q: Pointer; Lib: Integer;
begin
  AggOf(This, Id, Buf, Off, Root);
  P := AggData(Buf, Off, Descs[Id].Size);
  if P = nil then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
  if T.IsAggregate then
  begin
    TId := DescIdOf(T);
    Res := NewAggregate(TId, Buf, Off + FOff, Root);
    Exit(True);
  end;
  Inc(P, FOff);
  if (T.Kind = ftkCallback) or ((T.Kind = ftkScalar) and (T.ScalarType = fftPointer)) then
  begin
    Move(P^, Q, SizeOf(Pointer));
    Lib := TieAt(Root, Off + FOff);
    Res := NewPointer(Q, Lib);
    Exit(True);
  end;
  if (T.Kind = ftkScalar) and (T.ScalarType = fftUTF8String) then Exit(ThrowTypeError(SErrorFFITypeDescriptorExpected));
  Result := UnmarshalScalar(T, P, -1, Res);
end;

function Agg_Get(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var F: TFieldRef; Id, Off, Root: Integer; Buf: TValue; FOff: Integer; T: TGocciaFFITypeDescriptor;
begin
  F := FieldRefs[NativeData];
  if not AggOf(Args[0], Id, Buf, Off, Root) or (Descs[Id] <> F.Desc) then
    Exit(ThrowTypeError(SErrorFFIMetadataRequiresAggregate));
  if F.Desc.Kind = ftkArray then
  begin
    T := F.Desc.ElementType;
    FOff := F.Index * T.Size;
  end
  else
  begin
    T := F.Desc.FieldTypeAt(F.Index);
    FOff := F.Desc.FieldOffsetAt(F.Index);
  end;
  Result := ReadField(T, Args[0], FOff, Res);
end;

function Agg_Set(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var F: TFieldRef; Id, Off, Root: Integer; Buf: TValue; FOff: Integer; T: TGocciaFFITypeDescriptor;
begin
  F := FieldRefs[NativeData];
  if not AggOf(Args[0], Id, Buf, Off, Root) or (Descs[Id] <> F.Desc) then
    Exit(ThrowTypeError(SErrorFFIMetadataRequiresAggregate));
  if F.Desc.Kind = ftkArray then
  begin
    T := F.Desc.ElementType;
    FOff := F.Index * T.Size;
  end
  else
  begin
    T := F.Desc.FieldTypeAt(F.Index);
    FOff := F.Desc.FieldOffsetAt(F.Index);
  end;
  Result := WriteField(T, ArgOf(Args, Argc, 1), Args[0], FOff);
  Res := VUndefined;
end;

{ writes V as a field of type T at FOff of This: an aggregate field takes an aggregate value of exactly T (its
  bytes copied, its pointer ties with them), a pointer field an FFIPointer, buffer or null (tied to the pointer's
  library), a number field V converted to its width, a boolean field V's truthiness }
function WriteField(T: TGocciaFFITypeDescriptor; V: TValue; This: TValue; FOff: Integer): Boolean;
var Id, Off, Root, SId, SOff, SRoot, I, Lib: Integer; Buf, SBuf, L: TValue; P, Src: PByte; Q: Pointer;
  Str: TBytes; Scratch: array[0..15] of Byte;
begin
  AggOf(This, Id, Buf, Off, Root);
  if T.IsAggregate then
  begin
    if not AggOf(V, SId, SBuf, SOff, SRoot) then Exit(ThrowTypeError(SErrorFFIAggregateFieldValue));
    if Descs[SId] <> T then Exit(ThrowTypeError(SErrorFFIAggregateFieldType));
    Src := AggData(SBuf, SOff, T.Size);
    P := AggData(Buf, Off, Descs[Id].Size);
    if (Src = nil) or (P = nil) then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
    Move(Src^, (P + FOff)^, T.Size);
    DropTies(Root, Off + FOff, T.Size);
    for I := 0 to High(Ties[SRoot]) do
      if (Ties[SRoot][I].Off >= SOff) and (Ties[SRoot][I].Off + SizeOf(Pointer) <= SOff + T.Size) then
        AddTie(Root, Off + FOff + Ties[SRoot][I].Off - SOff, Ties[SRoot][I].Lib);
    Exit(True);
  end;
  if T.Kind = ftkCallback then
  begin
    { a persistent handle of exactly this callback type (tied to it), or null }
    Q := nil;
    Lib := -1;
    if V <> VNull then
    begin
      if not CbOf(V, I) then Exit(ThrowTypeError(SErrorFFICallbackFieldHandle));
      if Cbs[I].Desc <> T then Exit(ThrowTypeError(SErrorFFICallbackFieldType));
      if Cbs[I].Closed then Exit(ThrowTypeError(SErrorFFICallbackClosed));
      Q := Cbs[I].Code;
      Lib := -2 - I;
    end;
    P := AggData(Buf, Off, Descs[Id].Size);
    if P = nil then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
    Move(Q, (P + FOff)^, SizeOf(Pointer));
    AddTie(Root, Off + FOff, Lib);
    Exit(True);
  end;
  if (T.Kind = ftkScalar) and (T.ScalarType = fftPointer) then
  begin
    if not PointerArg(V, 0, Q) then Exit(False);
    Lib := -1;
    if InternalOf(V, KPtrLib, L) then Lib := Trunc(NumOf(L))
    else if CbOf(V, I) then Lib := -2 - I;
    P := AggData(Buf, Off, Descs[Id].Size);
    if P = nil then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
    Move(Q, (P + FOff)^, SizeOf(Pointer));
    AddTie(Root, Off + FOff, Lib);
    Exit(True);
  end;
  if (T.Kind <> ftkScalar) or (T.ScalarType = fftUTF8String) then Exit(ThrowTypeError(SErrorFFITypeDescriptorExpected));
  { the conversion first (it can run script), then the bytes, wherever the buffer is now }
  FillChar(Scratch, SizeOf(Scratch), 0);
  if not MarshalScalar(T, V, 0, @Scratch[0], Str) then Exit(False);
  P := AggData(Buf, Off, Descs[Id].Size);
  if P = nil then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
  Move(Scratch[0], (P + FOff)^, T.Size);
  DropTies(Root, Off + FOff, T.Size);
  Result := True;
end;

{ a descriptor object for D (referenced from here on): kind, size, alignment and (an array's) length; an
  aggregate's field accessors are made with it }
function NewTypeObject(D: TGocciaFFITypeDescriptor): TValue;
var Id, I, N, F: Integer; Name: UnicodeString;
begin
  Id := Length(Descs);
  SetLength(Descs, Id + 1);
  Descs[Id] := D;
  if D.IsAggregate then
  begin
    N := FieldCountOf(D);
    PushRoot(MkPtr(NewArray(0, 2 * N)));
    for I := 0 to N - 1 do
    begin
      F := Length(FieldRefs);
      SetLength(FieldRefs, F + 1);
      FieldRefs[F].Desc := D;
      FieldRefs[F].Index := I;
      Name := FieldName(D, I);
      DefineElemRaw(RootAt(TempTop - 1), 2 * I, NewNativeFunctionData(Agg_Get, 'get ' + Name, 0, F));
      DefineElemRaw(RootAt(TempTop - 1), 2 * I + 1, NewNativeFunctionData(Agg_Set, 'set ' + Name, 1, F));
    end;
    DefineElemRaw(AccFns, Id, RootAt(TempTop - 1));
    PopRoots(1);
  end;
  PushRoot(MkPtr(NewPlainObject(TypeProto, 6)));
  DefineOwn(RootAt(TempTop - 1), KType, MkInt(Id), pfHidden);
  DefineOwn(RootAt(TempTop - 1), AtomOf('kind'), StrFromUnicode(KindNames[D.Kind]), pfEnumerable);
  DefineOwn(RootAt(TempTop - 1), AtomOf('size'), MkInt(D.Size), pfEnumerable);
  DefineOwn(RootAt(TempTop - 1), AtomOf('alignment'), MkInt(D.Alignment), pfEnumerable);
  if D.Kind = ftkArray then DefineOwn(RootAt(TempTop - 1), AtomOf('length'), MkInt(D.ElementCount), pfEnumerable);
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

{ ---------------- marshalling ---------------- }

function SaturateToInt64(D: Double): Int64;
begin
  if IsNaN(D) or IsInfinite(D) then Exit(0);
  D := Int(D);
  if D >= 9223372036854775807.0 then Exit(High(Int64));
  if D <= -9223372036854775808.0 then Exit(Low(Int64));
  Result := Trunc(D);
end;

{ a pointer argument's address: an FFIPointer, an ArrayBuffer or SharedArrayBuffer (its bytes; null when empty), a
  typed array (its first element; null when it has none), an FFI aggregate (its bytes), a callback handle (its code)
  or null }
function PointerArg(V: TValue; Index: Integer; out Address: Pointer): Boolean;
var O: PObj; AType: TGocciaFFITypeDescriptor; CbId: Integer;
begin
  Address := nil;
  if V = VNull then Exit(True);
  if IsObject(V) then
  begin
    O := ObjOf(V);
    case O^.OKind of
      okArrayBuffer:
        begin
          if BufferLen(O) > 0 then Address := BufferData(O);
          Exit(True);
        end;
      okTypedArray:
        begin
          if not TAOutOfBounds(O) and (TALength(O) > 0) then Address := TAData(O);
          Exit(True);
        end;
      okPlain:
        begin
          if GetOwnSlot(O, KPtr) >= 0 then Exit(PointerAddress(V, Address));
          if GetOwnSlot(O, KAgg) >= 0 then Exit(AggValueData(V, AType, PByte(Address)));
          if CbOf(V, CbId) then
          begin
            if Cbs[CbId].Closed then Exit(ThrowTypeError(SErrorFFICallbackClosed));
            Address := Cbs[CbId].Code;
            Exit(True);
          end;
        end;
    end;
  end;
  Result := ThrowTypeError(Format(SErrorFFIArgMustBeBufferOrNull, [Index]));
end;

{ writes V as AType's scalar at Dest (zeroed by the caller); Str keeps a utf8string's bytes for the call }
function MarshalScalar(AType: TGocciaFFITypeDescriptor; V: TValue; Index: Integer; Dest: PByte;
  var Str: TBytes): Boolean;
var D: Double; P: Pointer; Text: UnicodeString;
begin
  Result := True;
  case AType.ScalarType of
    fftVoid: Exit(ThrowTypeError(SErrorFFIVoidNotValidArg));
    fftBool: if ToBool(V) then Dest^ := 1;
    fftPointer:
      begin
        if not PointerArg(V, Index, P) then Exit(False);
        Move(P, Dest^, SizeOf(Pointer));
      end;
    fftUTF8String:
      begin
        if not TextOf(V, Text) then Exit(False);
        if not TryEncodeFFIUTF8String(Text, Str) then Exit(ThrowTypeError(SErrorFFIUTF8StringArgument));
        P := @Str[0];
        Move(P, Dest^, SizeOf(Pointer));
      end;
  else
    begin
      if not ToNumberV(V, D) then Exit(False);
      case AType.ScalarType of
        fftI8: PShortInt(Dest)^ := ShortInt(DoubleToUint32Mod(D));
        fftU8: PByte(Dest)^ := Byte(DoubleToUint32Mod(D));
        fftI16: PSmallInt(Dest)^ := SmallInt(DoubleToUint32Mod(D));
        fftU16: PWord(Dest)^ := Word(DoubleToUint32Mod(D));
        fftI32: PLongInt(Dest)^ := LongInt(DoubleToUint32Mod(D));
        fftU32: PLongWord(Dest)^ := DoubleToUint32Mod(D);
        fftI64: PInt64(Dest)^ := SaturateToInt64(D);
        fftU64: PQWord(Dest)^ := QWord(SaturateToInt64(D));
        fftF32: PSingle(Dest)^ := D;
        fftF64: PDouble(Dest)^ := D;
      end;
    end;
  end;
end;

{ the JS value of a native result of AType at Data }
function UnmarshalScalar(AType: TGocciaFFITypeDescriptor; Data: PByte; Lib: Integer; var Res: TValue): Boolean;
var P: Pointer; Text: string;
begin
  Result := True;
  case AType.ScalarType of
    fftVoid: Res := VUndefined;
    fftBool: Res := MkBool(Data^ <> 0);
    fftI8: Res := MkNumber(PShortInt(Data)^);
    fftU8: Res := MkNumber(PByte(Data)^);
    fftI16: Res := MkNumber(PSmallInt(Data)^);
    fftU16: Res := MkNumber(PWord(Data)^);
    fftI32: Res := MkNumber(PLongInt(Data)^);
    fftU32: Res := MkNumber(PLongWord(Data)^);
    fftI64: Res := MkNumber(PInt64(Data)^);
    fftU64: Res := MkNumber(PQWord(Data)^);
    fftF32: Res := MkNumber(PSingle(Data)^);
    fftF64: Res := MkNumber(PDouble(Data)^);
    fftPointer:
      begin
        Move(Data^, P, SizeOf(Pointer));
        if (Lib >= 0) and Libs[Lib].Guard.IsClosed then Exit(ThrowTypeError(SErrorFFIPointerLibraryClosed));
        Res := NewPointer(P, Lib);
        if Lib >= 0 then
        begin
          Libs[Lib].Guard.RetainDependent;
          Inc(Libs[Lib].Retained);
        end;
      end;
    fftUTF8String:
      begin
        Move(Data^, P, SizeOf(Pointer));
        if P = nil then begin Res := VNull; Exit; end;
        if not TryDecodeFFIUTF8String(PAnsiChar(P), Text) then Exit(ThrowTypeError(SErrorFFIUTF8StringResult));
        Res := StrFromUnicode(Text);
      end;
  end;
end;

{ ---------------- callbacks ---------------- }

function CbOf(V: TValue; out Id: Integer): Boolean;
var S: TValue;
begin
  Result := InternalOf(V, KCb, S);
  if Result then Id := Trunc(NumOf(S));
end;

{ a callback of the running bound call failed with Pending: the first failure is kept for after native code
  returns, and the later callbacks of that call are skipped }
procedure CallbackFailed;
begin
  if (CallDepth > 0) and not CallFailed[CallDepth - 1] then
  begin
    CallFailed[CallDepth - 1] := True;
    DefineElemRaw(CallPending, CallDepth - 1, Pending);
  end;
  Pending := VUndefined;
end;

procedure CopyFromPlacement(const State: TGocciaFFICallbackMachineState; const Pl: TGocciaFFIPlacement;
  Dest: PByte);
begin
  case Pl.Kind of
    {$IFDEF CPU64}
    fpkGPR: Move(State.GPR[Pl.RegisterIndex], Dest^, Pl.Size);
    fpkFPR: Move(State.FPR[Pl.RegisterIndex], Dest^, Pl.Size);
    {$ENDIF}
    fpkStack: Move(PByte(State.StackData)[Pl.StackOffset], Dest^, Pl.Size);
  end;
end;

procedure CopyToPlacement(var State: TGocciaFFICallbackMachineState; const Pl: TGocciaFFIPlacement;
  Src: PByte; T: TGocciaFFITypeDescriptor; ABI: TGocciaFFIABI);
var S32: LongInt;
begin
  case Pl.Kind of
    fpkGPR:
      begin
        {$IFDEF CPU64}
        { Darwin ARM64 returns a signed narrow scalar sign-extended to 32 bits }
        if (ABI = fabiDarwinARM64) and (T.Kind = ftkScalar) and (T.ScalarType in [fftI8, fftI16]) then
        begin
          if T.ScalarType = fftI8 then S32 := PShortInt(Src + Pl.ValueOffset)^
          else S32 := PSmallInt(Src + Pl.ValueOffset)^;
          State.RetGPR[Pl.RegisterIndex] := 0;
          Move(S32, State.RetGPR[Pl.RegisterIndex], SizeOf(S32));
          Exit;
        end;
        {$ENDIF}
        State.RetGPR[Pl.RegisterIndex] := 0;
        Move((Src + Pl.ValueOffset)^, State.RetGPR[Pl.RegisterIndex], Pl.Size);
      end;
    fpkFPR:
      {$IFDEF CPU64}
      Move((Src + Pl.ValueOffset)^, State.RetFPR[Pl.RegisterIndex], Pl.Size);
      {$ELSE}
      begin
        Move((Src + Pl.ValueOffset)^, State.RetFPR, Pl.Size);
        State.ReturnFloatSize := Pl.Size;
      end;
      {$ENDIF}
  end;
end;

function HiddenResultPointer(ABI: TGocciaFFIABI; const State: TGocciaFFICallbackMachineState): Pointer;
begin
  Result := nil;
  case ABI of
    {$IFDEF CPU64}
    fabiSysVX64, fabiWin64: Result := Pointer(NativeUInt(State.GPR[0]));
    fabiAAPCS64, fabiDarwinARM64, fabiWinARM64: Result := State.HiddenResult;
    {$ENDIF}
    fabiI386Win: if Assigned(State.StackData) then Move(PByte(State.StackData)[0], Result, SizeOf(Pointer));
  end;
end;

{ a callback argument's JS value from its native bytes }
function CallbackArgValue(T: TGocciaFFITypeDescriptor; Data: PByte; var Res: TValue): Boolean;
var P: Pointer;
begin
  if T.IsAggregate then
  begin
    Res := NewRootAggregate(DescIdOf(T), Data);
    Exit(True);
  end;
  if T.Kind = ftkCallback then
  begin
    Move(Data^, P, SizeOf(Pointer));
    Res := NewPointer(P, -1);
    Exit(True);
  end;
  Result := UnmarshalScalar(T, Data, -1, Res);
end;

{ a callback's result as the bytes its return type takes (Dest, T.Size, zeroed) }
function CallbackResultBytes(T: TGocciaFFITypeDescriptor; V: TValue; Dest: PByte): Boolean;
var AT: TGocciaFFITypeDescriptor; P: PByte; Id: Integer; Q: Pointer; Str: TBytes; S: TValue;
begin
  if T.IsAggregate then
  begin
    if not InternalOf(V, KAgg, S) then Exit(ThrowTypeError(SErrorFFICallbackAggregateReturn));
    if not AggValueData(V, AT, P) then Exit(False);
    if AT <> T then Exit(ThrowTypeError(SErrorFFICallbackAggregateReturn));
    Move(P^, Dest^, T.Size);
    Exit(True);
  end;
  if T.Kind = ftkCallback then
  begin
    if not CbOf(V, Id) then Exit(ThrowTypeError(SErrorFFICallbackHandleReturn));
    if Cbs[Id].Desc <> T then Exit(ThrowTypeError(SErrorFFICallbackReturnType));
    if Cbs[Id].Closed then Exit(ThrowTypeError(SErrorFFICallbackClosed));
    Move(Cbs[Id].Code, Dest^, SizeOf(Pointer));
    Exit(True);
  end;
  case T.ScalarType of
    fftVoid: Exit(True);
    fftUTF8String: Exit(ThrowTypeError(SErrorFFICallbackUTF8StringReturn));
    fftPointer:
      begin
        Q := nil;
        if V <> VNull then
        begin
          if not InternalOf(V, KPtr, S) then Exit(ThrowTypeError(SErrorFFICallbackPointerReturn));
          if not PointerAddress(V, Q) then Exit(False);
        end;
        Move(Q, Dest^, SizeOf(Pointer));
        Exit(True);
      end;
  end;
  Result := MarshalScalar(T, V, 0, Dest, Str);
end;

{ runs callback Id for native code: the arguments decoded from State, the callable called, its result encoded
  into State (or the hidden result); a closed callback, or one of a call whose callback already failed, returns
  the zero value }
procedure RunCallback(Id: Integer; var State: TGocciaFFICallbackMachineState);
var Sig: TGocciaFFICompiledSignature; RP: PGocciaFFIReturnPlan; AP: PGocciaFFIArgumentPlan; Hidden, IP: Pointer;
  Data, Ret: TBytes; Base, I, J, N: Integer; Fn, R: TValue;
begin
  Sig := Cbs[Id].Sig;
  RP := Sig.ReturnPlanPointer;
  {$IF not defined(CPU64)}
  if (Length(RP^.Placements) > 0) and (RP^.Placements[0].Kind = fpkFPR) then
    State.ReturnFloatSize := RP^.Placements[0].Size;
  {$IFEND}
  Hidden := nil;
  if RP^.UsesHiddenPointer then
  begin
    Hidden := HiddenResultPointer(Sig.ABI, State);
    if Hidden = nil then Exit;
    if RP^.TypeDescriptor.Size > 0 then FillChar(Hidden^, RP^.TypeDescriptor.Size, 0);
    if Sig.ABI in [fabiSysVX64, fabiWin64, fabiI386Win] then State.RetGPR[0] := NativeUInt(Hidden);
  end;
  if Cbs[Id].Closed or ((CallDepth > 0) and CallFailed[CallDepth - 1]) then Exit;
  N := Sig.ArgumentCount;
  Base := TempTop;
  try
    for I := 0 to N - 1 do
    begin
      AP := Sig.ArgumentPlanAt(I);
      SetLength(Data, Max(AP^.TypeDescriptor.Size, 1));
      FillChar(Data[0], Length(Data), 0);
      if AP^.Indirect then
      begin
        IP := nil;
        CopyFromPlacement(State, AP^.Placements[0], @IP);
        if Assigned(IP) and (AP^.TypeDescriptor.Size > 0) then Move(IP^, Data[0], AP^.TypeDescriptor.Size);
      end
      else
        for J := 0 to High(AP^.Placements) do
          CopyFromPlacement(State, AP^.Placements[J], @Data[AP^.Placements[J].ValueOffset]);
      if not CallbackArgValue(AP^.TypeDescriptor, @Data[0], R) then
      begin
        CallbackFailed;
        Exit;
      end;
      PushRoot(R);
    end;
    GetElemV(CbFns, MkInt(Id), Fn);
    PushRoot(Fn);
    { the arguments from their root slots, which a collection inside the call updates }
    if not CallValue(RootAt(TempTop - 1), VUndefined, @TempRoots[Base], N, R) then
    begin
      CallbackFailed;
      Exit;
    end;
    SetLength(Ret, Max(RP^.TypeDescriptor.Size, 1) + 16);
    FillChar(Ret[0], Length(Ret), 0);
    if not CallbackResultBytes(RP^.TypeDescriptor, R, @Ret[0]) then
    begin
      CallbackFailed;
      Exit;
    end;
    if RP^.UsesHiddenPointer then
    begin
      if RP^.TypeDescriptor.Size > 0 then Move(Ret[0], Hidden^, RP^.TypeDescriptor.Size);
    end
    else
      for I := 0 to High(RP^.Placements) do
        CopyToPlacement(State, RP^.Placements[I], @Ret[0], RP^.TypeDescriptor, Sig.ABI);
  finally
    PopRoots(TempTop - Base);
  end;
end;

{ the slots' dispatch hook: no Pascal exception unwinds through the native frame below }
procedure CallbackDispatch(const AContext: Pointer; var AState: TGocciaFFICallbackMachineState);
begin
  try
    RunCallback(Integer(PtrUInt(AContext)) - 1, AState);
  except
    on E: Exception do
    begin
      ThrowTypeError(E.Message);
      CallbackFailed;
    end;
  end;
end;

{ a callback of descriptor D calling Fn: its signature compiled and a native slot taken; False with a RangeError
  for a layout or the slots past their limits }
function NewCallback(D: TGocciaFFITypeDescriptor; Fn: TValue; out Id: Integer): Boolean;
var Types: array of TGocciaFFITypeDescriptor; I: Integer; Sig: TGocciaFFICompiledSignature;
  Loc: TGocciaFFIHiddenResultLocation; HSize: NativeUInt; FSize: LongWord; Code: Pointer; Slot: Integer;
begin
  PushRoot(Fn);
  try
    SetLength(Types, D.CallbackArgumentCount);
    for I := 0 to High(Types) do Types[I] := D.CallbackArgumentAt(I);
    try
      Sig := TGocciaFFICompiledSignature.Create(CurrentFFIABI, Types, D.CallbackReturn);
    except
      on E: EArgumentOutOfRangeException do Exit(ThrowError('RangeError', SErrorFFICallLayoutLimit));
      on E: EArgumentException do Exit(ThrowTypeError(SErrorFFIInvalidCompiledSignature));
    end;
    Loc := fhrNone;
    HSize := 0;
    FSize := 0;
    if Sig.ReturnPlanPointer^.UsesHiddenPointer then
    begin
      HSize := Sig.ReturnPlanPointer^.TypeDescriptor.Size;
      case Sig.ABI of
        fabiSysVX64, fabiWin64: Loc := fhrGPR0;
        fabiAAPCS64, fabiDarwinARM64, fabiWinARM64: Loc := fhrDedicated;
        fabiI386Win: Loc := fhrStack0;
      end;
    end;
    {$IF not defined(CPU64)}
    if (Length(Sig.ReturnPlanPointer^.Placements) > 0) and (Sig.ReturnPlanPointer^.Placements[0].Kind = fpkFPR) then
      FSize := Sig.ReturnPlanPointer^.Placements[0].Size;
    {$IFEND}
    Id := Length(Cbs);
    try
      Slot := AllocateFFICallbackSlot(Pointer(PtrUInt(Id + 1)), Loc, HSize, FSize, Code);
    except
      on E: EOutOfMemory do
      begin
        Sig.Free;
        Exit(ThrowError('RangeError', Format(SErrorFFICallbackSlotLimit, [MAX_FFI_CALLBACK_SLOTS])));
      end;
    end;
    SetLength(Cbs, Id + 1);
    D.AddReference;
    Cbs[Id].Desc := D;
    Cbs[Id].Sig := Sig;
    Cbs[Id].Slot := Slot;
    Cbs[Id].Code := Code;
    Cbs[Id].Closed := False;
    Inc(OpenCbs);
    DefineElemRaw(CbFns, Id, RootAt(TempTop - 1));
    Result := True;
  finally
    PopRoots(1);
  end;
end;

procedure CloseCallback(Id: Integer);
begin
  if Cbs[Id].Closed then Exit;
  Cbs[Id].Closed := True;
  Dec(OpenCbs);
  if Cbs[Id].Slot >= 0 then
  begin
    ConsumeFFICallbackThreadViolation(Cbs[Id].Slot);
    ReleaseFFICallbackSlot(Cbs[Id].Slot);
    Cbs[Id].Slot := -1;
  end;
  DefineElemRaw(CbFns, Id, VUndefined);
end;

function NewCallbackObject(Id: Integer): TValue;
begin
  PushRoot(MkPtr(NewPlainObject(CbProto, 2)));
  DefineOwn(RootAt(TempTop - 1), KCb, MkInt(Id), pfHidden);
  { close is the handle's own, as GocciaScript's: it works whatever its prototype becomes }
  DefineOwn(RootAt(TempTop - 1), AtomOf('close'), CbCloseFn, pfHidden);
  Result := RootAt(TempTop - 1);
  PopRoots(1);
end;

function Cb_Address(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer;
begin
  if not CbOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFICallbackCloseReceiver));
  if Cbs[Id].Closed then Exit(ThrowTypeError(SErrorFFICallbackClosed));
  Res := MkNumber(Double(PtrUInt(Cbs[Id].Code)));
  Result := True;
end;

function Cb_Closed(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer;
begin
  if not CbOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFICallbackCloseReceiver));
  Res := MkBool(Cbs[Id].Closed);
  Result := True;
end;

function Cb_Close(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer; Violation: Boolean;
begin
  if not CbOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFICallbackCloseReceiver));
  Violation := (Cbs[Id].Slot >= 0) and ConsumeFFICallbackThreadViolation(Cbs[Id].Slot);
  CloseCallback(Id);
  Res := VUndefined;
  if Violation then Exit(ThrowTypeError(SErrorFFICallbackForeignThread));
  Result := True;
end;

{ ---------------- FFI.varargs ---------------- }

function FFI_VarArgs(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var TL, VL: Int64; I, Id: Integer; E: TValue; Types: array of TGocciaFFITypeDescriptor;
begin
  if Argc < 2 then Exit(ThrowTypeError(SErrorFFIVarArgsRequiresArrays));
  if not IsObject(Args[1]) or (ObjOf(Args[1])^.OKind <> okArray) or not IsObject(Args[2]) or
     (ObjOf(Args[2])^.OKind <> okArray) then Exit(ThrowTypeError(SErrorFFIVarArgsArrays));
  TL := ArrayLength(ObjOf(Args[1]));
  VL := ArrayLength(ObjOf(Args[2]));
  if TL <> VL then Exit(ThrowTypeError(SErrorFFIVarArgsLength));
  SetLength(Types, TL);
  Result := False;
  try
    for I := 0 to TL - 1 do
    begin
      if not GetElemV(Args[1], MkInt(I), E) then Exit;
      if not ParseType(E, tpBoundArgument, Types[I]) then Exit;
    end;
    { the values as they are now }
    PushRoot(MkPtr(NewArray(0, VL)));
    try
      for I := 0 to VL - 1 do
      begin
        if not GetElemV(Args[2], MkInt(I), E) then Exit;
        DefineElemRaw(RootAt(TempTop - 1), I, E);
      end;
      Id := Length(VATypes);
      SetLength(VATypes, Id + 1);
      VATypes[Id] := Types;
      Types := nil;
      PushRoot(MkPtr(NewPlainObject(VAProto, 2)));
      DefineOwn(RootAt(TempTop - 1), KVA, MkInt(Id), pfHidden);
      DefineOwn(RootAt(TempTop - 1), KVAVals, RootAt(TempTop - 2), pfHidden);
      Res := RootAt(TempTop - 1);
      PopRoots(1);
      Result := True;
    finally
      PopRoots(1);
    end;
  finally
    for I := 0 to High(Types) do
      if Assigned(Types[I]) then Types[I].ReleaseReference;
  end;
end;

{ ---------------- bound functions ---------------- }

function PromoteVariadic(T: TGocciaFFITypeDescriptor): TGocciaFFITypeDescriptor;
begin
  if T.Kind = ftkScalar then
    case T.ScalarType of
      fftBool, fftI8, fftI16, fftU8, fftU16: Exit(TGocciaFFITypeDescriptor.CreateScalar(fftI32));
      fftF32: Exit(TGocciaFFITypeDescriptor.CreateScalar(fftF64));
    end;
  T.AddReference;
  Result := T;
end;

function IsPointerType(T: TGocciaFFITypeDescriptor): Boolean; inline;
begin
  Result := (T.Kind = ftkScalar) and (T.ScalarType = fftPointer);
end;

{ a bound native function's call. The arguments marshal in two passes: first the conversions that can run script
  (numbers, strings, callbacks), then the addresses and bytes of buffers and aggregates, which no script runs
  between, so no collection can move a buffer whose address was taken. A call that can call back into script (a
  callback passed, or any open) runs a minor collection first, so the buffers it passes are in the old space,
  which never moves. }
function Bound_Call(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id, I, N, Fixed, Lib, CbId, VAId: Integer; B: ^TBound; Sig, CallSig: TGocciaFFICompiledSignature;
  ArgData, ResultStorage: TBytes; Strs: array of TBytes; TempCbs: array of Integer; T, AT: TGocciaFFITypeDescriptor;
  S, V, Tail: TValue; P: PByte; Q: Pointer; Combined: array of TGocciaFFITypeDescriptor; Name: UnicodeString;
  HasCallbacks, Failed: Boolean;

  function ArgValue(I: Integer): TValue;
  begin
    if I < Fixed then Result := Args[I + 1] else GetElemV(Tail, MkInt(I - Fixed), Result);
  end;

begin
  Id := NativeData;
  B := @Bounds[Id];
  Sig := B^.Sig;
  Lib := B^.Lib;
  Name := B^.Name;
  if Libs[Lib].Guard.IsClosed then Exit(ThrowTypeError(Format(SErrorFFICallLibraryClosed, [Name])));
  Fixed := Sig.ArgumentCount;
  if Argc < Fixed then Exit(ThrowTypeError(Format(SErrorFFIFuncArgCount, [Name, Fixed, Argc])));
  CallSig := nil;
  Tail := VUndefined;
  N := Fixed;
  if B^.Variadic then
  begin
    if (Argc <> Fixed + 1) or not InternalOf(Args[Fixed + 1], KVA, S) then
      Exit(ThrowTypeError(Format(SErrorFFIVariadicTailRequired, [Name])));
    VAId := Trunc(NumOf(S));
    InternalOf(Args[Fixed + 1], KVAVals, Tail);
    N := Fixed + Length(VATypes[VAId]);
    if N > MAX_FFI_ARGS then
      Exit(ThrowError('RangeError', Format(SErrorFFIVariadicArgumentLimit, [Name, MAX_FFI_ARGS])));
    SetLength(Combined, N);
    for I := 0 to Fixed - 1 do Combined[I] := Sig.ArgumentTypeAt(I);
    try
      for I := 0 to High(VATypes[VAId]) do Combined[Fixed + I] := PromoteVariadic(VATypes[VAId][I]);
      try
        CallSig := TGocciaFFICompiledSignature.Create(CurrentFFIABI, Combined, Sig.ReturnPlanPointer^.TypeDescriptor,
          Fixed);
      except
        on E: EArgumentOutOfRangeException do Exit(ThrowError('RangeError', SErrorFFICallLayoutLimit));
        on E: EArgumentException do Exit(ThrowTypeError(SErrorFFIInvalidCompiledSignature));
      end;
    finally
      for I := Fixed to High(Combined) do
        if Assigned(Combined[I]) then Combined[I].ReleaseReference;
    end;
    Sig := CallSig;
  end;
  PushRoot(Tail);
  SetLength(TempCbs, 0);
  try
    SetLength(ArgData, Sig.ArgumentDataSize + 1);
    SetLength(ResultStorage, Sig.ReturnPlanPointer^.TypeDescriptor.Size + FFI_BUFFER_ALIGNMENT);
    SetLength(Strs, N);
    HasCallbacks := False;
    { pass 1: the conversions }
    for I := 0 to N - 1 do
    begin
      T := Sig.ArgumentTypeAt(I);
      if T.IsAggregate or IsPointerType(T) then Continue;
      V := ArgValue(I);
      if T.Kind = ftkCallback then
      begin
        HasCallbacks := True;
        if CbOf(V, CbId) then
        begin
          if Cbs[CbId].Desc <> T then Exit(ThrowTypeError(SErrorFFICallbackArgumentType));
          if Cbs[CbId].Closed then Exit(ThrowTypeError(SErrorFFICallbackClosed));
        end
        else if IsCallable(V) then
        begin
          { call-scoped: closed when the call returns }
          if not NewCallback(T, V, CbId) then Exit(False);
          SetLength(TempCbs, Length(TempCbs) + 1);
          TempCbs[High(TempCbs)] := CbId;
        end
        else
          Exit(ThrowTypeError(SErrorFFICallbackArgumentValue));
        Move(Cbs[CbId].Code, ArgData[Sig.ArgumentDataOffset(I)], SizeOf(Pointer));
        Continue;
      end;
      if T.Kind = ftkNullable then
      begin
        Q := nil;
        if V <> VNull then
        begin
          if not IsString(V) or not TryEncodeFFIUTF8String(StrToUnicode(StrOf(V)), Strs[I]) then
            Exit(ThrowTypeError(SErrorFFINullableUTF8StringArgument));
          Q := @Strs[I][0];
        end;
        Move(Q, ArgData[Sig.ArgumentDataOffset(I)], SizeOf(Pointer));
        Continue;
      end;
      if not MarshalScalar(T, V, I, @ArgData[Sig.ArgumentDataOffset(I)], Strs[I]) then Exit(False);
    end;
    { a conversion may have run script, which can close the library or a callback }
    if Libs[Lib].Guard.IsClosed then Exit(ThrowTypeError(Format(SErrorFFICallLibraryClosed, [Name])));
    { native code can enter script through a callback this call passes, or one it kept from an earlier call }
    if HasCallbacks or (OpenCbs > 0) then CollectMinor;
    { pass 2: the addresses and the aggregates' bytes }
    for I := 0 to N - 1 do
    begin
      T := Sig.ArgumentTypeAt(I);
      if T.IsAggregate then
      begin
        V := ArgValue(I);
        if not InternalOf(V, KAgg, S) then Exit(ThrowTypeError(SErrorFFIAggregateArgumentValue));
        if not AggValueData(V, AT, P) then Exit(False);
        if AT <> T then Exit(ThrowTypeError(SErrorFFIAggregateArgumentType));
        Move(P^, ArgData[Sig.ArgumentDataOffset(I)], T.Size);
      end
      else if IsPointerType(T) then
      begin
        if not PointerArg(ArgValue(I), I, Q) then Exit(False);
        Move(Q, ArgData[Sig.ArgumentDataOffset(I)], SizeOf(Pointer));
      end;
    end;
    { the call, its callbacks' first failure rethrown after it returns }
    if CallDepth >= Length(CallFailed) then SetLength(CallFailed, CallDepth + 8);
    CallFailed[CallDepth] := False;
    Inc(CallDepth);
    try
      FFIInvokeCompiled(Bounds[Id].Symbol, Sig, @ArgData[0], FFIAlignBuffer(@ResultStorage[0]));
    finally
      Dec(CallDepth);
    end;
    Failed := CallFailed[CallDepth];
    if Failed then
    begin
      GetElemV(CallPending, MkInt(CallDepth), Pending);
      DefineElemRaw(CallPending, CallDepth, VUndefined);
      Exit(False);
    end;
    if ConsumeFFICallbackThreadViolationsForCurrentThread then Exit(ThrowTypeError(SErrorFFICallbackForeignThread));
    T := Sig.ReturnPlanPointer^.TypeDescriptor;
    if T.IsAggregate then
    begin
      { a new root aggregate holding the bytes, its pointer fields tied to the library }
      Res := NewRootAggregate(DescIdOf(T), FFIAlignBuffer(@ResultStorage[0]));
      InternalOf(Res, KRoot, S);
      TieAll(Trunc(NumOf(S)), 0, T, Lib);
      Exit(True);
    end;
    if T.Kind = ftkCallback then
    begin
      Move(FFIAlignBuffer(@ResultStorage[0])^, Q, SizeOf(Pointer));
      if Libs[Lib].Guard.IsClosed then Exit(ThrowTypeError(SErrorFFIPointerLibraryClosed));
      Res := NewPointer(Q, Lib);
      Libs[Lib].Guard.RetainDependent;
      Inc(Libs[Lib].Retained);
      Exit(True);
    end;
    Result := UnmarshalScalar(T, FFIAlignBuffer(@ResultStorage[0]), Lib, Res);
  finally
    PopRoots(1);
    for I := 0 to High(TempCbs) do CloseCallback(TempCbs[I]);
    CallSig.Free;
  end;
end;

{ ---------------- signatures ---------------- }

{ a type of a signature or an aggregate: a scalar type's name or a descriptor object (referenced for the caller) }
function ParseType(V: TValue; Position: TTypePosition; out AType: TGocciaFFITypeDescriptor): Boolean;
var Name: UnicodeString; Scalar: TGocciaFFIType; D: TGocciaFFITypeDescriptor;
begin
  AType := nil;
  if TypeOf(V, D) then
  begin
    if (D.Kind = ftkNullable) and (Position <> tpBoundArgument) then Exit(ThrowTypeError(SErrorFFINullableArgumentOnly));
    D.AddReference;
    AType := D;
    Exit(True);
  end;
  if not IsString(V) then Exit(ThrowTypeError(SErrorFFITypeDescriptorExpected));
  Name := StrToUnicode(StrOf(V));
  Scalar := ParseFFIType(Name);
  if (Scalar = fftVoid) and (Name <> FFI_TYPE_VOID) then Exit(ThrowTypeError(Format(SErrorFFIUnknownType, [Name])));
  {$IF not defined(CPU64)}
  if Scalar in [fftI64, fftU64] then Exit(ThrowTypeError(SErrorFFI64BitType32Bit));
  {$ENDIF}
  if (Scalar = fftVoid) and not (Position in [tpBoundReturn, tpCallbackReturn]) then
    Exit(ThrowTypeError(SErrorFFIVoidReturnOnly));
  AType := TGocciaFFITypeDescriptor.CreateScalar(Scalar);
  Result := True;
end;

{ a signature's args (an array, or absent) and returns (absent: void) parsed for Positions; Variadic its variadic
  flag (a boolean, or absent) }
function ParseArgsAndReturn(SigV: TValue; ArgPos, RetPos: TTypePosition; MaxArgs: Integer;
  const MaxMessage, ArgsMessage: UnicodeString; out Types: TFFITypeArray; out Ret: TGocciaFFITypeDescriptor;
  out Variadic: Boolean): Boolean;
var ArgsV, RetV, E: TValue; N, I: Integer; Len: Int64;
begin
  Ret := nil;
  Variadic := False;
  SetLength(Types, 0);
  Result := False;
  PushRoot(SigV);
  try
    if not GetV(RootAt(TempTop - 1), AtomOf('args'), ArgsV) then Exit;
    if IsObject(ArgsV) and (ObjOf(ArgsV)^.OKind = okArray) then
    begin
      PushRoot(ArgsV);
      try
        Len := ArrayLength(ObjOf(RootAt(TempTop - 1)));
        if Len > MaxArgs then Exit(ThrowTypeError(Format(MaxMessage, [MaxArgs])));
        N := Len;
        SetLength(Types, N);
        for I := 0 to N - 1 do
        begin
          if not GetElemV(RootAt(TempTop - 1), MkInt(I), E) then Exit;
          if not ParseType(E, ArgPos, Types[I]) then Exit;
        end;
      finally
        PopRoots(1);
      end;
    end
    else if ArgsV <> VUndefined then
      Exit(ThrowTypeError(ArgsMessage));
    if not GetV(RootAt(TempTop - 1), AtomOf('returns'), RetV) then Exit;
    if RetV = VUndefined then Ret := TGocciaFFITypeDescriptor.CreateScalar(fftVoid)
    else if not ParseType(RetV, RetPos, Ret) then Exit;
    if not GetV(RootAt(TempTop - 1), AtomOf('variadic'), E) then Exit;
    if E <> VUndefined then
    begin
      if (E <> VTrue) and (E <> VFalse) then Exit(ThrowTypeError(SErrorFFIVariadicFlagBoolean));
      Variadic := E = VTrue;
    end;
    Result := True;
  finally
    PopRoots(1);
  end;
end;

procedure ReleaseTypes(const Types: TFFITypeArray; Ret: TGocciaFFITypeDescriptor);
var I: Integer;
begin
  if Assigned(Ret) then Ret.ReleaseReference;
  for I := 0 to High(Types) do
    if Assigned(Types[I]) then Types[I].ReleaseReference;
end;

{ bind's signature (an object with args, returns and variadic) compiled for this platform }
function ParseSignature(SigV: TValue; out Sig: TGocciaFFICompiledSignature; out Variadic: Boolean): Boolean;
var Types: TFFITypeArray; Ret: TGocciaFFITypeDescriptor;
begin
  Sig := nil;
  if not IsObject(SigV) then Exit(ThrowTypeError(SErrorFFIBindSigObject));
  Result := False;
  try
    if not ParseArgsAndReturn(SigV, tpBoundArgument, tpBoundReturn, MAX_FFI_ARGS, SErrorFFIMaxArguments,
       SErrorFFISigArgsMustBeArray, Types, Ret, Variadic) then Exit;
    try
      Sig := TGocciaFFICompiledSignature.Create(CurrentFFIABI, Types, Ret);
    except
      on E: EArgumentOutOfRangeException do Exit(ThrowError('RangeError', SErrorFFICallLayoutLimit));
      on E: EArgumentException do Exit(ThrowTypeError(SErrorFFIInvalidCompiledSignature));
    end;
    Result := True;
  finally
    ReleaseTypes(Types, Ret);
  end;
end;

{ FFI.callback(signature): a callback descriptor }
function FFI_Callback(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Types: TFFITypeArray; Ret, D: TGocciaFFITypeDescriptor; Variadic: Boolean;
begin
  if Argc < 1 then Exit(ThrowTypeError(SErrorFFICallbackRequiresDefinition));
  if not IsObject(Args[1]) or (ObjOf(Args[1])^.OKind = okArray) then
    Exit(ThrowTypeError(SErrorFFICallbackDefinitionObject));
  Result := False;
  try
    if not ParseArgsAndReturn(Args[1], tpCallbackArgument, tpCallbackReturn, MAX_FFI_ARGS,
       SErrorFFIMaxCallbackArguments, SErrorFFICallbackArgsArray, Types, Ret, Variadic) then Exit;
    if Variadic then Exit(ThrowTypeError(SErrorFFICallbackVariadicUnsupported));
    try
      D := TGocciaFFITypeDescriptor.CreateCallback(Types, Ret);
    except
      on E: Exception do Exit(ThrowTypeError(E.Message));
    end;
    Res := NewTypeObject(D);
    Result := True;
  finally
    ReleaseTypes(Types, Ret);
  end;
end;

{ FFI.nullable("utf8string") }
function FFI_Nullable(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E, D: TGocciaFFITypeDescriptor;
begin
  if Argc < 1 then Exit(ThrowTypeError(SErrorFFINullableRequiresType));
  if not ParseType(Args[1], tpBoundArgument, E) then Exit(False);
  try
    if (E.Kind <> ftkScalar) or (E.ScalarType <> fftUTF8String) then Exit(ThrowTypeError(SErrorFFINullableUTF8StringOnly));
    try
      D := TGocciaFFITypeDescriptor.CreateNullable(E);
    except
      on Ex: Exception do Exit(ThrowTypeError(Ex.Message));
    end;
  finally
    E.ReleaseReference;
  end;
  Res := NewTypeObject(D);
  Result := True;
end;

{ ---------------- FFILibrary ---------------- }

function Lib_Bind(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id, B: Integer; Name: UnicodeString; Sig: TGocciaFFICompiledSignature; Sym: Pointer; Variadic: Boolean;
begin
  if not LibOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFIBindRequiresLibrary));
  if Libs[Id].Guard.IsClosed then Exit(ThrowTypeError(SErrorFFIBindLibraryClosed));
  if not TextOf(ArgOf(Args, Argc, 1), Name) then Exit(False);
  if Argc < 2 then Exit(ThrowTypeError(SErrorFFIBindRequiresNameAndSig));
  if not ParseSignature(Args[2], Sig, Variadic) then Exit(False);
  try
    Sym := Libs[Id].Guard.FindSymbol(Name);
  except
    on E: Exception do
    begin
      Sig.Free;
      Exit(ThrowTypeError(E.Message));
    end;
  end;
  B := Length(Bounds);
  SetLength(Bounds, B + 1);
  Bounds[B].Symbol := Sym;
  Bounds[B].Sig := Sig;
  Bounds[B].Lib := Id;
  Bounds[B].Name := Name;
  Bounds[B].Variadic := Variadic;
  Libs[Id].Guard.RetainDependent;
  Inc(Libs[Id].Retained);
  Res := NewNativeFunctionData(Bound_Call, Name, Sig.ArgumentCount, B);
  Result := True;
end;

function Lib_Symbol(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer; Name: UnicodeString; Sym: Pointer;
begin
  if not LibOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFISymbolRequiresLibrary));
  if Libs[Id].Guard.IsClosed then Exit(ThrowTypeError(SErrorFFISymbolLibraryClosed));
  if Argc < 1 then Exit(ThrowTypeError(SErrorFFISymbolRequiresName));
  if not TextOf(Args[1], Name) then Exit(False);
  try
    Sym := Libs[Id].Guard.FindSymbol(Name);
  except
    on E: Exception do Exit(ThrowTypeError(E.Message));
  end;
  Libs[Id].Guard.RetainDependent;
  Inc(Libs[Id].Retained);
  Res := NewPointer(Sym, Id);
  Result := True;
end;

function Lib_Close(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer;
begin
  if not LibOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFICloseRequiresLibrary));
  Libs[Id].Guard.Close;
  Res := VUndefined;
  Result := True;
end;

function Lib_Path(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer;
begin
  if not LibOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFIPathRequiresLibrary));
  Res := StrFromUnicode(Libs[Id].Guard.Path);
  Result := True;
end;

function Lib_Closed(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id: Integer;
begin
  if not LibOf(Args[0], Id) then Exit(ThrowTypeError(SErrorFFIClosedRequiresLibrary));
  Res := MkBool(Libs[Id].Guard.IsClosed);
  Result := True;
end;

{ ---------------- FFI.open ---------------- }

{ True when Path names a library without any directory part }
function IsBareLibraryName(const Path: UnicodeString): Boolean;
begin
  Result := (Pos('/', Path) = 0) {$IFDEF MSWINDOWS} and (Pos('\', Path) = 0) and (Pos(':', Path) = 0){$ENDIF};
end;

function AuditOpen(Allow: Boolean; const Subject, Reason: UnicodeString): Boolean;
begin
  Result := AuditDecision(akFFIOpen, Allow, Subject, Reason);
end;

{ FFI.open judges the library's canonical path, then loads it. On Linux the file is opened first and the kernel's
  path for that descriptor judged, and the loader maps the descriptor itself (/proc/self/fd/N), so what is loaded
  is what was judged; elsewhere the path is judged again after the load (GocciaScript's FFIOpen). }
function FFI_Open(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
const
  NOT_COVERED_REASON = 'the ffi capability does not cover this library';
  CHANGED_REASON = 'the library changed between the ffi check and the load';
var LibPath, LoadPath, URLPath, PinnedLoadPath, PinnedPath, Reported, Detail: UnicodeString; Allowed, IsBare: Boolean;
  Guard: TGocciaFFILibraryGuard; Id: Integer; O: PObj;
  {$IFDEF LINUX}Fd: LongInt;{$ENDIF}

  function Deny(const Suggestion, Reason: UnicodeString): Boolean;
  begin
    if not AuditOpen(False, LibPath, Reason) then Exit(False);
    Result := ThrowPermissionDenied(LibPath, Suggestion);
  end;

begin
  if Argc < 1 then Exit(ThrowTypeError(SErrorFFIOpenRequiresPath));
  if not TextOf(Args[1], LibPath) then Exit(False);
  { a file: URL (a URL object or its string) is judged as the host path it names }
  LoadPath := LibPath;
  if IsFileURL(LibPath) then
  begin
    if not TryFileURLToHostPath(LibPath, URLPath) then Exit(ThrowTypeError(Format(SErrorFFIOpenFileURL, [LibPath])));
    LoadPath := URLPath;
  end;
  IsBare := IsBareLibraryName(LoadPath);
  { a bare name is found on the loader's search path, which no scope describes: only an unscoped grant covers it }
  if IsBare then
  begin
    Allowed := Caps.AllowsUnscoped(gcFFI);
    if Caps.HasDeny(gcFFI) then Detail := SSuggestFFIBareNameDenyScope else Detail := SSuggestFFIBareName;
  end
  else
  begin
    LoadPath := CanonicalCapabilityPath(LoadPath);
    Allowed := Caps.AllowsPath(gcFFI, LoadPath);
    if Caps.DeniesPath(gcFFI, LoadPath) then Detail := Format(SSuggestFFIDenied, [LoadPath])
    else Detail := Format(SSuggestFFINotGranted, [LoadPath, ExtractFileDir(LoadPath)]);
  end;
  if not Allowed then Exit(Deny(Detail, NOT_COVERED_REASON));
  if not AuditOpen(True, LibPath, 'the ffi capability covers this library') then Exit(False);
  PinnedLoadPath := LoadPath;
  {$IFDEF LINUX}
  Fd := -1;
  if not IsBare then
  begin
    Fd := FpOpen(RawByteString(UTF8Encode(LoadPath)), O_RDONLY);
    if Fd < 0 then Exit(ThrowTypeError('Failed to load library: ' + LibPath));
    PinnedLoadPath := '/proc/self/fd/' + IntToStr(Fd);
    PinnedPath := UnicodeString(fpReadLink(RawByteString(PinnedLoadPath)));
    if (PinnedPath = '') or not Caps.AllowsCanonicalPath(gcFFI, PinnedPath) then
    begin
      FpClose(Fd);
      Exit(Deny(Format('the ffi capability does not cover %s', [PinnedPath]), CHANGED_REASON));
    end;
  end;
  {$ENDIF}
  try
    try
      Guard := TGocciaFFILibraryGuard.Create(LibPath, PinnedLoadPath);
    except
      on E: Exception do Exit(ThrowTypeError(E.Message));
    end;
  finally
    {$IFDEF LINUX}
    if Fd >= 0 then FpClose(Fd);
    {$ENDIF}
  end;
  {$IF not defined(LINUX)}
  if not IsBare then
  begin
    {$IFDEF MSWINDOWS}
    Reported := Guard.LoadedPath;
    if (Reported = '') or not Caps.AllowsPath(gcFFI, Reported) then
    {$ELSE}
    Reported := CanonicalCapabilityPath(PinnedLoadPath);
    if Reported <> LoadPath then
    {$ENDIF}
    begin
      Guard.ReleaseOwner;
      Exit(Deny(CHANGED_REASON, CHANGED_REASON));
    end;
  end;
  {$IFEND}
  Id := Length(Libs);
  SetLength(Libs, Id + 1);
  Libs[Id].Guard := Guard;
  Libs[Id].Retained := 0;
  O := NewPlainObject(LibProto, 1);
  PushRoot(MkPtr(O));
  DefineOwn(RootAt(TempTop - 1), KLib, MkInt(Id), pfHidden);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ FFI.struct and FFI.union (NativeData 1: union): the definition's own enumerable fields in order }
function FFI_Aggregate(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Def, Keys, K, FV: TValue; Fields: array of TGocciaFFIFieldDescriptor; N, I: Integer; Len: Int64;
  D: TGocciaFFITypeDescriptor;
begin
  if Argc < 1 then
  begin
    if NativeData = 1 then Exit(ThrowTypeError(SErrorFFIUnionRequiresDefinition));
    Exit(ThrowTypeError(SErrorFFIStructRequiresDefinition));
  end;
  Def := Args[1];
  if not IsObject(Def) or (ObjOf(Def)^.OKind = okArray) or IsCallable(Def) then
    Exit(ThrowTypeError(SErrorFFIAggregateDefinitionObject));
  PushRoot(Def);
  Keys := OwnKeys(ObjOf(Def), True);
  PushRoot(Keys);
  Len := ArrayLength(ObjOf(Keys));
  N := Len;
  SetLength(Fields, N);
  Result := False;
  try
    if N = 0 then Exit(ThrowTypeError(SErrorFFIAggregateDefinitionEmpty));
    for I := 0 to N - 1 do
    begin
      if not GetElemV(RootAt(TempTop - 1), MkInt(I), K) then Exit;
      Fields[I].Name := StrToUnicode(StrOf(K));
      if not GetV(RootAt(TempTop - 2), StrOf(K), FV) then Exit;
      if not ParseType(FV, tpStorage, Fields[I].TypeDescriptor) then Exit;
    end;
    try
      if NativeData = 1 then D := TGocciaFFITypeDescriptor.CreateUnion(Fields)
      else D := TGocciaFFITypeDescriptor.CreateStruct(Fields);
    except
      on E: Exception do Exit(ThrowTypeError(E.Message));
    end;
    Res := NewTypeObject(D);
    Result := True;
  finally
    PopRoots(2);
    for I := 0 to High(Fields) do
      if Assigned(Fields[I].TypeDescriptor) then Fields[I].TypeDescriptor.ReleaseReference;
  end;
end;

function FFI_Array(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var L: Double; E, D: TGocciaFFITypeDescriptor;
begin
  if Argc < 2 then Exit(ThrowTypeError(SErrorFFIArrayRequiresTypeAndLength));
  PushRoot(Args[1]);
  try
    if not ToNumberV(Args[2], L) then Exit(False);
    if IsNaN(L) or IsInfinite(L) or (L < 1) or (L > MAX_FFI_AGGREGATE_SIZE) or (L <> Int(L)) then
      Exit(ThrowTypeError(SErrorFFIArrayLengthPositive));
    if not ParseType(RootAt(TempTop - 1), tpStorage, E) then Exit(False);
  finally
    PopRoots(1);
  end;
  try
    try
      D := TGocciaFFITypeDescriptor.CreateArray(E, Trunc(L));
    except
      on Ex: Exception do Exit(ThrowTypeError(Ex.Message));
    end;
  finally
    E.ReleaseReference;
  end;
  Res := NewTypeObject(D);
  Result := True;
end;

{ FFI.metadata(value): an aggregate's buffer, byteOffset, size and (an array's) length }
function FFI_Metadata(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var Id, Off, Root: Integer; Buf: TValue; D: TGocciaFFITypeDescriptor;
begin
  if (Argc < 1) or not AggOf(Args[1], Id, Buf, Off, Root) then Exit(ThrowTypeError(SErrorFFIMetadataRequiresAggregate));
  D := Descs[Id];
  if AggData(Buf, Off, D.Size) = nil then Exit(ThrowTypeError(SErrorFFIAggregateBackingStore));
  PushRoot(Buf);
  PushRoot(MkPtr(NewPlainObject(ObjectProto, 4)));
  DefineOwn(RootAt(TempTop - 1), AtomOf('buffer'), RootAt(TempTop - 2), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('byteOffset'), MkInt(Off), pfDefault);
  DefineOwn(RootAt(TempTop - 1), AtomOf('size'), MkInt(D.Size), pfDefault);
  if D.Kind = ftkArray then DefineOwn(RootAt(TempTop - 1), AtomOf('length'), MkInt(D.ElementCount), pfDefault);
  Res := RootAt(TempTop - 1);
  PopRoots(2);
  Result := True;
end;

{ an aggregate initializer's fields written in order: those the object has (struct, union) or the indices the
  array-like has (array); its getters run with the new value rooted }
function InitAggregate(Agg, Init: TValue): Boolean;
var Id, Off, Root, I, N: Integer; Buf, V: TValue; D, T: TGocciaFFITypeDescriptor; Key: PStr; Found: Boolean;
  FOff: Integer;
begin
  AggOf(Agg, Id, Buf, Off, Root);
  D := Descs[Id];
  PushRoot(Agg);
  PushRoot(Init);
  Result := False;
  try
    N := FieldCountOf(D);
    for I := 0 to N - 1 do
    begin
      Key := AtomOf(FieldName(D, I));
      if not HasPropertyV(RootAt(TempTop - 1), Key, Found) then Exit;
      if not Found then Continue;
      if not GetV(RootAt(TempTop - 1), Key, V) then Exit;
      if D.Kind = ftkArray then
      begin
        T := D.ElementType;
        FOff := I * T.Size;
      end
      else
      begin
        T := D.FieldTypeAt(I);
        FOff := D.FieldOffsetAt(I);
      end;
      if not WriteField(T, V, RootAt(TempTop - 2), FOff) then Exit;
    end;
    Result := True;
  finally
    PopRoots(2);
  end;
end;

{ FFIType.prototype.create(initializer): an aggregate type's zeroed value with the initializer's fields }
function Type_Create(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var D: TGocciaFFITypeDescriptor; Init: TValue; Id: Integer;
begin
  if not TypeOf(Args[0], D) then Exit(ThrowTypeError(SErrorFFITypeCannotCreateValue));
  if D.Kind = ftkCallback then
  begin
    { a persistent handle, open until closed }
    if not IsCallable(ArgOf(Args, Argc, 1)) then Exit(ThrowTypeError(SErrorFFICallbackCreateCallable));
    if not NewCallback(D, Args[1], Id) then Exit(False);
    Res := NewCallbackObject(Id);
    Exit(True);
  end;
  if not D.IsAggregate then Exit(ThrowTypeError(SErrorFFITypeCannotCreateValue));
  Init := ArgOf(Args, Argc, 1);
  if (Init <> VUndefined) and not IsObject(Init) then Exit(ThrowTypeError(SErrorFFIAggregateInitializer));
  PushRoot(Init);
  Res := NewRootAggregate(DescIdOf(D), nil);
  PushRoot(Res);
  Result := (RootAt(TempTop - 2) = VUndefined) or InitAggregate(RootAt(TempTop - 1), RootAt(TempTop - 2));
  Res := RootAt(TempTop - 1);
  PopRoots(2);
end;

function FFI_Nullptr(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := NullPtr;
  Result := True;
end;

function FFI_Suffix(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  Res := StrFromUnicode(SHARED_LIBRARY_SUFFIX);
  Result := True;
end;

{ ---------------- install / teardown ---------------- }

procedure DefineTag(O: TValue; const Tag: UnicodeString);
var S: TValue;
begin
  PushRoot(O);
  { the string first: the object is read from its root after the allocation, whatever order the arguments take }
  S := StrFromUnicode(Tag);
  DefineOwn(RootAt(TempTop - 1), SymKey(SymToStringTagW), S, pfConfigurable);
  PopRoots(1);
end;

function InstallFFI(const Caps: TGocciaCapabilities; var Err: UnicodeString): Boolean;
var T: PFuncTemplate;
begin
  Goccia.FFI.Caps := Caps;
  if FirstInEngine(RootedGen) then
  begin
    AddRoot(@FFIObj); AddRoot(@LibProto); AddRoot(@PtrProto); AddRoot(@NullPtr);
    AddRoot(@TypeProto); AddRoot(@AggProto); AddRoot(@ABProto); AddRoot(@AccFns);
    AddRoot(@CbFns); AddRoot(@CallPending); AddRoot(@CbProto); AddRoot(@VAProto); AddRoot(@CbCloseFn);
  end;
  KLib := NewKeyAtom('%ffiLibrary', kfInternal);
  KPtr := NewKeyAtom('%ffiPointer', kfInternal);
  KPtrLib := NewKeyAtom('%ffiPointerLibrary', kfInternal);
  KType := NewKeyAtom('%ffiType', kfInternal);
  KAgg := NewKeyAtom('%ffiAggregate', kfInternal);
  KBuf := NewKeyAtom('%ffiBuffer', kfInternal);
  KOff := NewKeyAtom('%ffiOffset', kfInternal);
  KRoot := NewKeyAtom('%ffiRoot', kfInternal);
  if not GetV(GlobalObj, AtomOf('ArrayBuffer'), ABProto) or not GetV(ABProto, AtomPrototype, ABProto) then
  begin
    Err := 'FFI: ArrayBuffer.prototype is missing';
    Exit(False);
  end;
  AccFns := MkPtr(NewArray(0, 16));
  TypeProto := MkPtr(NewPlainObject(ObjectProto, 4));
  DefineNative(TypeProto, 'create', Type_Create, 1);
  DefineTag(TypeProto, 'FFIType');
  AggProto := MkPtr(NewPlainObject(ObjectProto, 2));
  DefineTag(AggProto, 'FFIAggregate');
  KCb := NewKeyAtom('%ffiCallback', kfInternal);
  KVA := NewKeyAtom('%ffiVarArgs', kfInternal);
  KVAVals := NewKeyAtom('%ffiVarArgsValues', kfInternal);
  CbFns := MkPtr(NewArray(0, 8));
  CallPending := MkPtr(NewArray(0, 8));
  CbProto := MkPtr(NewPlainObject(ObjectProto, 4));
  DefineGetterFn(CbProto, 'address', Cb_Address);
  DefineGetterFn(CbProto, 'closed', Cb_Closed);
  CbCloseFn := NewNativeFunction(Cb_Close, 'close', 0);
  DefineTag(CbProto, 'FFICallback');
  VAProto := MkPtr(NewPlainObject(ObjectProto, 2));
  DefineTag(VAProto, 'FFIVarArgs');
  SetFFICallbackDispatchHook(CallbackDispatch);

  LibProto := MkPtr(NewPlainObject(ObjectProto, 8));
  DefineNative(LibProto, 'bind', Lib_Bind, 2);
  DefineNative(LibProto, 'symbol', Lib_Symbol, 1);
  DefineNative(LibProto, 'close', Lib_Close, 0);
  DefineGetterFn(LibProto, 'path', Lib_Path);
  DefineGetterFn(LibProto, 'closed', Lib_Closed);
  DefineTag(LibProto, 'FFILibrary');

  PtrProto := MkPtr(NewPlainObject(ObjectProto, 4));
  DefineGetterFn(PtrProto, 'address', Ptr_Address);
  DefineGetterFn(PtrProto, 'isNull', Ptr_IsNull);
  DefineTag(PtrProto, 'FFIPointer');
  NullPtr := NewPointer(nil, -1);

  FFIObj := MkPtr(NewPlainObject(ObjectProto, 16));
  DefineNative(FFIObj, 'open', FFI_Open, 1);
  DefineOwn(FFIObj, AtomOf('struct'), NewNativeFunctionData(FFI_Aggregate, 'struct', 1, 0), pfHidden);
  DefineOwn(FFIObj, AtomOf('union'), NewNativeFunctionData(FFI_Aggregate, 'union', 1, 1), pfHidden);
  DefineNative(FFIObj, 'array', FFI_Array, 2);
  DefineNative(FFIObj, 'metadata', FFI_Metadata, 1);
  DefineNative(FFIObj, 'callback', FFI_Callback, 1);
  DefineNative(FFIObj, 'nullable', FFI_Nullable, 1);
  DefineNative(FFIObj, 'varargs', FFI_VarArgs, 2);
  DefineGetterFn(FFIObj, 'nullptr', FFI_Nullptr);
  DefineGetterFn(FFIObj, 'suffix', FFI_Suffix);

  { FFI: a const binding of the global scope, mirrored on the global object, as Goccia is }
  DefineOwn(GlobalObj, AtomOf('FFI'), FFIObj, pfHidden);
  Result := ParseAndCompile('const FFI = globalThis.FFI;', '<goccia:ffi>', False, T, Err, 1) and RunScript(T);
  if not Result and (Err = '') then Err := 'FFI binding failed';
end;

procedure FFIDone;
var I, K: Integer;
begin
  for I := 0 to High(Bounds) do Bounds[I].Sig.Free;
  Bounds := nil;
  for I := 0 to High(Cbs) do
  begin
    if Cbs[I].Slot >= 0 then ReleaseFFICallbackSlot(Cbs[I].Slot);
    Cbs[I].Sig.Free;
    Cbs[I].Desc.ReleaseReference;
  end;
  Cbs := nil;
  OpenCbs := 0;
  for I := 0 to High(VATypes) do
    for K := 0 to High(VATypes[I]) do VATypes[I][K].ReleaseReference;
  VATypes := nil;
  CallFailed := nil;
  CallDepth := 0;
  for I := 0 to High(Descs) do Descs[I].ReleaseReference;
  Descs := nil;
  FieldRefs := nil;
  Ties := nil;
  { each library closed, its dependents released (unloading it), then its owner (freeing the guard) }
  for I := 0 to High(Libs) do
  begin
    Libs[I].Guard.Close;
    for K := 1 to Libs[I].Retained do Libs[I].Guard.ReleaseDependent;
    Libs[I].Guard.ReleaseOwner;
  end;
  Libs := nil;
  FFIObj := VUndefined; LibProto := VUndefined; PtrProto := VUndefined; NullPtr := VUndefined;
  TypeProto := VUndefined; AggProto := VUndefined; ABProto := VUndefined; AccFns := VUndefined;
  CbFns := VUndefined; CallPending := VUndefined; CbProto := VUndefined; VAProto := VUndefined;
  CbCloseFn := VUndefined;
end;

{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaFFIThreadvarCleanup;
begin
  Caps := Default(TGocciaCapabilities);
  Libs := nil;
  Bounds := nil;
  Descs := nil;
  FieldRefs := nil;
  Ties := nil;
  Cbs := nil;
  CallFailed := nil;
  VATypes := nil;
end;

initialization
  RegisterThreadvarCleanup(@GocciaFFIThreadvarCleanup);
end.

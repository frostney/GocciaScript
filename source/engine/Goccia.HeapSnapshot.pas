unit Goccia.HeapSnapshot;
{ Heap snapshots on G2 (docs/heap-snapshots.md): what a value reaches, written so that a fresh engine of the
  same build, set up the same way, restores it.

  The writer copies cells as they are: a string as its text, a BigInt as its limbs, a symbol as its description
  (a registered one by its key), a value array, a closed binding, an accessor, and an object as its kind, flags,
  prototype, keys with their flags and values, elements and its two Aux values. What an object holds outside the
  heap is named instead: a closure's code by the entry or module it was compiled from and its place there, a
  built-in by its path from globalThis. An object kind whose state lives anywhere else (a Map's table, a buffer's
  bytes, a promise's reactions, a generator's frame) refuses the snapshot, as does a binding a frame still has
  open.

  Built-ins are what globalThis reaches before the run's code (SnapshotNameBuiltins). Each is written as its name
  and a fingerprint of its state; a restore refuses one whose fingerprint in the restoring engine differs, which
  is a built-in the run changed. Module records are not written (the closures carry the module bindings they
  use), so a restored engine that imports a module evaluates it again. }
{$mode delphiunicode}{$H+}
{$I Goccia.Checks.inc}
{$MINFPCONSTPREC 64}
{$POINTERMATH ON}
{$Q-}{$R-}

interface

uses
  SysUtils, Classes, Goccia.Core, Goccia.Instructions;

threadvar
  { the TestRunner's --heap-snapshots: expect() hands each value to SnapshotObserve }
  HeapObserving: Boolean;

{ names this engine's built-ins: every object and symbol globalThis reaches now. The engine a snapshot is taken
  in calls it before the run's code, and the engine that restores it before restoring, both set up the same way }
procedure SnapshotNameBuiltins;
{ the entry Script's compiled code, its file name, source and source type (Goccia.Embed, once it compiled it): a
  snapshot names the functions compiled from it by their place in it }
procedure SnapshotNoteEntry(T: PFuncTemplate; const FileName, Source: UnicodeString; Kind: Integer);
{ the entry a restore compiles when a snapshot names a function of it (not run) }
procedure SnapshotSetEntry(const FileName, Source: UnicodeString; Kind: Integer);
{ writes what Root reaches into Data; False with Refusal saying why it cannot. It does not allocate on the heap }
function SnapshotWrite(Root: TValue; var Data: TBytes; var Refusal: UnicodeString): Boolean;
{ restores Data, written by an engine of this build set up as this one: Root its root, which the caller roots at
  once; False with Refusal saying why it cannot }
function SnapshotRestore(const Data: TBytes; var Root: TValue; var Refusal: UnicodeString): Boolean;
{ a description of what Root reaches as the object model shows it (own keys in order, descriptors, prototypes,
  extensibility; built-ins by name), independent of the snapshot format: a round trip compares the two engines' }
function SnapshotDescribe(Root: TValue): UnicodeString;
{ expect()'s native (shared.snapshot.observe): an object it is given is kept for the round trip }
function SnapshotObserve(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
{ the values observed: Count with repeats, and each distinct one in the order first seen }
function SnapshotObservations: Integer;
function SnapshotObservedCount: Integer;
function SnapshotObservedAt(I: Integer): TValue;
{ drops this thread's tables and observations (the engine ends) }
procedure SnapshotDone;

implementation

uses
  Generics.Collections, Math, BigInteger, Goccia.VM, Goccia.ObjModel, Goccia.BigIntV, Goccia.Builtins, Goccia.Builtins3, Goccia.Eval,
  Goccia.Modules, Goccia.ThreadCleanupRegistry;

const
  SNAP_MAGIC = $31484732;   { '2GH1' }
  { a value: an immediate, a cell of the snapshot, a built-in }
  vtRaw = 0; vtCell = 1; vtBuiltin = 2;
  { a template's root: the entry Script, a module record }
  rkEntry = 1; rkModule = 2;
  MAX_NAMED = 1000000;
  MAX_CELLS = 16 * 1024 * 1024;

  okTypedArray = 14; okArrayBuffer = 15; okDataView = 16; okRegExp = 17; okDate = 18; okPromise = 20;
  okAsyncGen = 26;

threadvar
  { the built-ins, in the order SnapshotNameBuiltins met them (a rooted value array), and their names }
  NamedCells: TValue;
  NamedRootGen: Integer;
  Names: TStringList;
  NameIndex: TDictionary<UnicodeString, Integer>;
  { the entry Script (SnapshotNoteEntry, SnapshotSetEntry) }
  EntryTpl: PFuncTemplate;
  EntryFile, EntrySource: UnicodeString;
  EntryKind: Integer;
  EntryKnown: Boolean;
  { observed values: a rooted array of the distinct ones, and how many were observed in all }
  Observed: TValue;
  ObservedRootGen: Integer;
  ObservedTotal: Integer;

function NamedCount: Integer;
begin
  if not IsPtr(NamedCells) then Exit(0);
  Result := PValueArray(PtrOf(NamedCells))^.Len;
end;

function NamedAt(I: Integer): TValue;
begin
  Result := PValueArray(PtrOf(NamedCells))^.Data[I];
end;

function KindOf(V: TValue): Integer; inline;
begin
  if not IsPtr(V) then Exit(-1);
  Result := PCell(PtrOf(V))^.Kind;
end;

{ ---------------- naming the built-ins ---------------- }

function KeyText(K: PStr): UnicodeString;
var S: TValue;
begin
  if (K^.Hdr.Extra and kfSymbol) <> 0 then
  begin
    S := KeyPayload(K)^;
    Result := '[' + StrToUnicode(StrOf(PValue(PByte(PtrOf(S)) + 8)^)) + ']';
  end
  else Result := StrToUnicode(K);
end;

procedure SnapshotNameBuiltins;
var Cells: TList<Pointer>; Paths: TStringList; Seen: TDictionary<Pointer, Boolean>; I, J, K: Integer;
  O: PObj; VA: PValueArray; A: PAccessor; Base: UnicodeString; Arr: PValueArray;

  procedure Note(V: TValue; const Path: UnicodeString);
  var C: Integer;
  begin
    if not IsPtr(V) then Exit;
    C := PCell(PtrOf(V))^.Kind;
    if (C <> ckObject) and (C <> ckSymbol) then Exit;
    if Seen.ContainsKey(PtrOf(V)) or (Cells.Count >= MAX_NAMED) then Exit;
    Seen.Add(PtrOf(V), True);
    Cells.Add(PtrOf(V));
    Paths.Add(Path);
  end;

  { what a function's or a built-in's internal value holds: an object, or the objects in a value array, a binding
    or an accessor it points at }
  procedure NoteInner(V: TValue; const Path: UnicodeString);
  var N: Integer; U: PUpval; X: PAccessor; Y: PValueArray;
  begin
    if not IsPtr(V) then Exit;
    case PCell(PtrOf(V))^.Kind of
      ckObject, ckSymbol: Note(V, Path);
      ckValueArray:
        begin
          Y := PValueArray(PtrOf(V));
          for N := 0 to Integer(Y^.Len) - 1 do
            if IsPtr(Y^.Data[N]) and (PCell(PtrOf(Y^.Data[N]))^.Kind = ckUpval) then
            begin
              U := PUpval(PtrOf(Y^.Data[N]));
              Note(U^.Value, Path + '[' + IntToStr(N) + ']');
            end
            else Note(Y^.Data[N], Path + '[' + IntToStr(N) + ']');
        end;
      ckUpval:
        begin
          U := PUpval(PtrOf(V));
          Note(U^.Value, Path);
        end;
      ckAccessor:
        begin
          X := PAccessor(PtrOf(V));
          Note(X^.Getter, Path + '.get');
          Note(X^.Setter, Path + '.set');
        end;
    end;
  end;

begin
  if FirstInEngine(NamedRootGen) then
  begin
    NamedCells := VUndefined;
    AddRoot(@NamedCells);
  end;
  NamedCells := VUndefined;
  FreeAndNil(Names);
  FreeAndNil(NameIndex);
  { every cell the walk names is in old space, which no collection moves, so the walk keeps addresses }
  CollectMinor;
  Cells := TList<Pointer>.Create;
  Paths := TStringList.Create;
  Seen := TDictionary<Pointer, Boolean>.Create;
  try
    Note(GlobalObj, 'globalThis');
    Note(SymbolRegistryObject, '%SymbolRegistryObject%');
    I := 0;
    while I < Cells.Count do
    begin
      Base := Paths[I];
      if PCell(Cells[I])^.Kind = ckObject then
      begin
        O := PObj(Cells[I]);
        Note(O^.Shape^.Proto, Base + '.__proto__');
        for J := 0 to O^.Shape^.Count - 1 do
        begin
          if (O^.Shape^.Keys[J]^.Hdr.Extra and kfSymbol) <> 0 then
            Note(KeyPayload(O^.Shape^.Keys[J])^, Base + '.' + KeyText(O^.Shape^.Keys[J]) + '#key');
          if (O^.Shape^.PFlags[J] and pfAccessor) <> 0 then
          begin
            if KindOf(GetSlot(O, J)) = ckAccessor then
            begin
              A := PAccessor(PtrOf(GetSlot(O, J)));
              Note(A^.Getter, Base + '.' + KeyText(O^.Shape^.Keys[J]) + '.get');
              Note(A^.Setter, Base + '.' + KeyText(O^.Shape^.Keys[J]) + '.set');
            end;
          end
          else Note(GetSlot(O, J), Base + '.' + KeyText(O^.Shape^.Keys[J]));
        end;
        if O^.Elements <> nil then
        begin
          VA := O^.Elements;
          for K := 0 to Integer(VA^.Len) - 1 do Note(VA^.Data[K], Base + '[' + IntToStr(K) + ']');
        end;
        NoteInner(O^.Aux1, Base + '.[[Aux1]]');
        NoteInner(O^.Aux2, Base + '.[[Aux2]]');
      end;
      Inc(I);
    end;
    { the cells go into one rooted array: a minor collection the allocation runs moves none of them (they are old)
      and the array keeps each alive, so a built-in the run drops is never mistaken for a cell made later }
    Arr := NewValueArray(Cells.Count);
    Arr^.Len := Cells.Count;
    for I := 0 to Cells.Count - 1 do ValueArrayStore(Arr, I, MkPtr(Cells[I]));
    NamedCells := MkPtr(Arr);
    Names := TStringList.Create;
    Names.Assign(Paths);
    NameIndex := TDictionary<UnicodeString, Integer>.Create;
    for I := 0 to Names.Count - 1 do
      if not NameIndex.ContainsKey(Names[I]) then NameIndex.Add(Names[I], I);
  finally
    Seen.Free;
    Paths.Free;
    Cells.Free;
  end;
end;

{ which built-in a cell is: Map from the named cells' current addresses }
function BuiltinMap: TDictionary<Pointer, Integer>;
var I: Integer;
begin
  Result := TDictionary<Pointer, Integer>.Create;
  for I := 0 to NamedCount - 1 do
    if not Result.ContainsKey(PtrOf(NamedAt(I))) then Result.Add(PtrOf(NamedAt(I)), I);
end;

{ a built-in's state, as far as a run can change it: its kind and flags, its prototype, its keys with their flags
  and values, its elements; another built-in counts by its index, a string by its text, any other cell as one }
function Fingerprint(P: Pointer; Map: TDictionary<Pointer, Integer>): QWord;
var H: QWord; O: PObj; I: Integer; T: PFuncTemplate;

  procedure Mix(X: QWord);
  begin
    H := (H xor X) * 1099511628211;
  end;

  procedure MixText(const S: UnicodeString);
  var N: Integer;
  begin
    Mix(Length(S));
    for N := 1 to Length(S) do Mix(Ord(S[N]));
  end;

  procedure MixValue(V: TValue);
  var Ix: Integer; A: PAccessor;
  begin
    if not IsPtr(V) then begin Mix(1); Mix(V); Exit; end;
    if Map.TryGetValue(PtrOf(V), Ix) then begin Mix(2); Mix(Ix); Exit; end;
    case PCell(PtrOf(V))^.Kind of
      ckString: begin Mix(3); MixText(StrToUnicode(StrOf(V))); end;
      ckAccessor:
        begin
          A := PAccessor(PtrOf(V));
          Mix(4);
          MixValue(A^.Getter);
          MixValue(A^.Setter);
        end;
    else
      Mix(5);
    end;
  end;

begin
  H := 14695981039346656037;
  if PCell(P)^.Kind = ckSymbol then
  begin
    MixText(StrToUnicode(StrOf(PValue(PByte(P) + 8)^)));
    Exit(H);
  end;
  O := PObj(P);
  Mix(O^.OKind);
  { not ofElemGuard: an index property once defined on it leaves that set for good (element writes check the
    chain), the same built-in once the property is gone }
  Mix(O^.OFlags and not ofElemGuard);
  MixValue(O^.Shape^.Proto);
  Mix(O^.Shape^.Count);
  for I := 0 to O^.Shape^.Count - 1 do
  begin
    if (O^.Shape^.Keys[I]^.Hdr.Extra and kfSymbol) <> 0 then MixValue(KeyPayload(O^.Shape^.Keys[I])^)
    else MixText(StrToUnicode(O^.Shape^.Keys[I]));
    Mix(O^.Shape^.PFlags[I]);
    MixValue(GetSlot(O, I));
  end;
  { the elements present, by index: storage an index once took and a delete left a hole in is the same state }
  if O^.Elements <> nil then
    for I := 0 to Integer(O^.Elements^.Len) - 1 do
      if O^.Elements^.Data[I] <> VHole then
      begin
        Mix(I);
        MixValue(O^.Elements^.Data[I]);
      end;
  if O^.OKind = okNative then Mix(PtrUInt(O^.Native));
  if (O^.OKind = okFunction) and (O^.Native <> nil) then
  begin
    T := PFuncTemplate(O^.Native);
    if T^.Name <> nil then MixText(StrToUnicode(T^.Name));
    Mix(QWord(Int64(T^.DeclLine)));
    Mix(QWord(Int64(T^.DeclCol)));
  end;
  Result := H;
end;

{ ---------------- templates ---------------- }

type
  TTplPlace = record
    Root: Integer;
    Path: TArray<Integer>;
  end;
  TRootDesc = record
    Kind: Integer;
    Path: UnicodeString;
    { a module record's source hash: a module reloaded with other code is another root }
    Hash: QWord;
  end;

procedure SnapshotNoteEntry(T: PFuncTemplate; const FileName, Source: UnicodeString; Kind: Integer);
begin
  EntryTpl := T;
  EntryFile := FileName;
  EntrySource := Source;
  EntryKind := Kind;
  EntryKnown := True;
end;

procedure SnapshotSetEntry(const FileName, Source: UnicodeString; Kind: Integer);
begin
  EntryTpl := nil;
  EntryFile := FileName;
  EntrySource := Source;
  EntryKind := Kind;
  EntryKnown := True;
end;

{ every template the entry and the module records hold, by its root and its path of child indices (-1 the field
  initializer) }
procedure MapTemplates(Places: TDictionary<Pointer, TTplPlace>; Roots: TList<TRootDesc>);

  procedure Walk(T: PFuncTemplate; Root: Integer; const Path: TArray<Integer>);
  var P: TTplPlace; I: Integer; Sub: TArray<Integer>;
  begin
    if (T = nil) or Places.ContainsKey(T) then Exit;
    P.Root := Root;
    P.Path := Path;
    Places.Add(T, P);
    for I := 0 to High(T^.Children) do
    begin
      Sub := Copy(Path);
      SetLength(Sub, Length(Sub) + 1);
      Sub[High(Sub)] := I;
      Walk(T^.Children[I], Root, Sub);
    end;
    if T^.FieldInit <> nil then
    begin
      Sub := Copy(Path);
      SetLength(Sub, Length(Sub) + 1);
      Sub[High(Sub)] := -1;
      Walk(T^.FieldInit, Root, Sub);
    end;
  end;

var D: TRootDesc; I: Integer; Path: UnicodeString; T: PFuncTemplate; Hash: QWord;
begin
  if EntryKnown and (EntryTpl <> nil) then
  begin
    D.Kind := rkEntry;
    D.Path := EntryFile;
    D.Hash := 0;
    Roots.Add(D);
    Walk(EntryTpl, Roots.Count - 1, nil);
  end;
  for I := 0 to ModuleCount - 1 do
    if ModuleTemplateAt(I, Path, T, Hash) and (T <> nil) then
    begin
      D.Kind := rkModule;
      D.Path := Path;
      D.Hash := Hash;
      Roots.Add(D);
      Walk(T, Roots.Count - 1, nil);
    end;
end;

{ ---------------- writing ---------------- }

type
  ERefused = class(Exception);

  TSnapWriter = class
    Body: TMemoryStream;
    Ids: TDictionary<Pointer, Integer>;
    Order: TList<Pointer>;
    Builtins: TDictionary<Pointer, Integer>;
    BuiltinRef: TDictionary<Integer, Integer>;
    BuiltinRefs: TList<Integer>;
    TplRef: TDictionary<Pointer, Integer>;
    TplRefs: TList<Pointer>;
    Places: TDictionary<Pointer, TTplPlace>;
    Roots: TList<TRootDesc>;
    constructor Create;
    destructor Destroy; override;
    procedure I32(S: TStream; V: Integer);
    procedure U64(S: TStream; V: QWord);
    procedure U8(S: TStream; V: Byte);
    procedure Text(S: TStream; const U: UnicodeString);
    procedure Value(S: TStream; V: TValue);
    function Template(T: PFuncTemplate): Integer;
    procedure Cell(P: Pointer);
  end;

constructor TSnapWriter.Create;
begin
  Body := TMemoryStream.Create;
  Ids := TDictionary<Pointer, Integer>.Create;
  Order := TList<Pointer>.Create;
  Builtins := BuiltinMap;
  BuiltinRef := TDictionary<Integer, Integer>.Create;
  BuiltinRefs := TList<Integer>.Create;
  TplRef := TDictionary<Pointer, Integer>.Create;
  TplRefs := TList<Pointer>.Create;
end;

destructor TSnapWriter.Destroy;
begin
  Body.Free;
  Ids.Free;
  Order.Free;
  Builtins.Free;
  BuiltinRef.Free;
  BuiltinRefs.Free;
  TplRef.Free;
  TplRefs.Free;
  Places.Free;
  Roots.Free;
  inherited;
end;

procedure TSnapWriter.I32(S: TStream; V: Integer); begin S.WriteBuffer(V, 4); end;
procedure TSnapWriter.U64(S: TStream; V: QWord); begin S.WriteBuffer(V, 8); end;
procedure TSnapWriter.U8(S: TStream; V: Byte); begin S.WriteBuffer(V, 1); end;

procedure TSnapWriter.Text(S: TStream; const U: UnicodeString);
begin
  I32(S, Length(U));
  if Length(U) > 0 then S.WriteBuffer(U[1], Length(U) * 2);
end;

procedure TSnapWriter.Value(S: TStream; V: TValue);
var Ix, R: Integer;
begin
  if not IsPtr(V) then
  begin
    U8(S, vtRaw);
    U64(S, V);
    Exit;
  end;
  if Builtins.TryGetValue(PtrOf(V), Ix) then
  begin
    if not BuiltinRef.TryGetValue(Ix, R) then
    begin
      R := BuiltinRefs.Count;
      BuiltinRefs.Add(Ix);
      BuiltinRef.Add(Ix, R);
    end;
    U8(S, vtBuiltin);
    I32(S, R);
    Exit;
  end;
  if not Ids.TryGetValue(PtrOf(V), Ix) then
  begin
    if Order.Count >= MAX_CELLS then raise ERefused.Create('the snapshot holds too many cells');
    Ix := Order.Count;
    Order.Add(PtrOf(V));
    Ids.Add(PtrOf(V), Ix);
  end;
  U8(S, vtCell);
  I32(S, Ix);
end;

function TSnapWriter.Template(T: PFuncTemplate): Integer;
begin
  if TplRef.TryGetValue(T, Result) then Exit;
  if Places = nil then
  begin
    Places := TDictionary<Pointer, TTplPlace>.Create;
    Roots := TList<TRootDesc>.Create;
    MapTemplates(Places, Roots);
  end;
  if not Places.ContainsKey(T) then
    raise ERefused.Create('no snapshot codec for a function compiled outside the entry and its modules');
  Result := TplRefs.Count;
  TplRefs.Add(T);
  TplRef.Add(T, Result);
end;

function KindName(OKind: Integer): UnicodeString;
begin
  case OKind of
    okMap: Result := 'TGocciaMapValue';
    okSet: Result := 'TGocciaSetValue';
    okArrayBuffer: Result := 'TGocciaArrayBufferValue';
    okTypedArray: Result := 'TGocciaTypedArrayValue';
    okDataView: Result := 'TGocciaDataViewValue';
    okPromise: Result := 'TGocciaPromiseValue';
    okRegExp: Result := 'TGocciaRegExpValue';
    okDate: Result := 'TGocciaDateValue';
    okProxy: Result := 'TGocciaProxyValue';
    okGenerator: Result := 'TGocciaBytecodeGeneratorObjectValue';
    okAsyncGen: Result := 'TGocciaBytecodeAsyncGeneratorObjectValue';
    okModuleNS: Result := 'TGocciaModuleNamespaceObject';
    okIterator: Result := 'TGocciaArrayIteratorValue';
    okArguments: Result := 'TGocciaArgumentsObjectValue';
    okNative: Result := 'TGocciaNativeFunctionValue';
  else
    Result := 'object kind ' + IntToStr(OKind);
  end;
end;

procedure TSnapWriter.Cell(P: Pointer);
var C: PCell; O: PObj; I: Integer; K: PStr; B: PBigCell; VA: PValueArray; U: PUpval; A: PAccessor;
  Sym: TValue; Reg: PObj; Found: Boolean;
begin
  C := PCell(P);
  U8(Body, C^.Kind);
  case C^.Kind of
    ckString:
      begin
        { a key atom no source text names (a private name, an engine slot) is not a string the run can hold }
        if PStr(C)^.Hdr.Extra <> 0 then raise ERefused.Create('no snapshot codec for a private name');
        Text(Body, StrToUnicode(PStr(C)));
      end;
    ckBigInt:
      begin
        B := PBigCell(C);
        I32(Body, B^.Count);
        I32(Body, B^.Negative);
        for I := 0 to B^.Count - 1 do I32(Body, Integer(B^.Limbs[I]));
      end;
    ckSymbol:
      begin
        Sym := MkPtr(C);
        Found := False;
        Reg := ObjOf(SymbolRegistryObject);
        for I := 0 to Reg^.Shape^.Count - 1 do
          if GetSlot(Reg, I) = Sym then
          begin
            U8(Body, 1);
            Text(Body, StrToUnicode(Reg^.Shape^.Keys[I]));
            Found := True;
            Break;
          end;
        if not Found then
        begin
          U8(Body, 0);
          U8(Body, Ord(C^.Extra <> 0));
          Text(Body, StrToUnicode(StrOf(PValue(PByte(C) + 8)^)));
        end;
      end;
    ckValueArray:
      begin
        VA := PValueArray(C);
        I32(Body, Integer(VA^.Cap));
        I32(Body, Integer(VA^.Len));
        for I := 0 to Integer(VA^.Len) - 1 do Value(Body, VA^.Data[I]);
      end;
    ckUpval:
      begin
        U := PUpval(C);
        if U^.StackIndex >= 0 then raise ERefused.Create('no snapshot codec for a binding a running frame holds');
        Value(Body, U^.Value);
      end;
    ckAccessor:
      begin
        A := PAccessor(C);
        Value(Body, A^.Getter);
        Value(Body, A^.Setter);
      end;
    ckObject:
      begin
        O := PObj(C);
        if not (O^.OKind in [okPlain, okArray, okFunction, okBound, okError, okBoolean, okNumber, okStringObj,
          okSymbolObj, okArgumentsOrd]) then
          raise ERefused.Create('no snapshot codec for ' + KindName(O^.OKind));
        if (O^.OKind <> okFunction) and (O^.Native <> nil) then
          raise ERefused.Create('no snapshot codec for ' + KindName(O^.OKind) + ' with native state');
        U8(Body, O^.OKind);
        U8(Body, O^.OFlags);
        { Hdr.Extra is the engine's own (a prototype's root-shape index), not the object's state }
        U8(Body, C^.Flags and cfEmulatesUndefined);
        I32(Body, O^.InlineCap);
        I32(Body, Integer(O^.Pad));
        if O^.OKind = okFunction then I32(Body, Template(PFuncTemplate(O^.Native))) else I32(Body, -1);
        Value(Body, O^.Shape^.Proto);
        I32(Body, O^.Shape^.Count);
        for I := 0 to O^.Shape^.Count - 1 do
        begin
          K := O^.Shape^.Keys[I];
          if (K^.Hdr.Extra and kfSymbol) <> 0 then
          begin
            U8(Body, 1);
            Value(Body, KeyPayload(K)^);
          end
          else if K^.Hdr.Extra <> 0 then
            raise ERefused.Create('no snapshot codec for a private name')
          else
          begin
            U8(Body, 0);
            Text(Body, StrToUnicode(K));
          end;
          U8(Body, O^.Shape^.PFlags[I]);
          Value(Body, GetSlot(O, I));
        end;
        if O^.Elements = nil then U8(Body, 0)
        else
        begin
          U8(Body, 1);
          Value(Body, MkPtr(O^.Elements));
        end;
        Value(Body, O^.Aux1);
        Value(Body, O^.Aux2);
      end;
  else
    raise ERefused.Create('no snapshot codec for cell kind ' + IntToStr(C^.Kind));
  end;
end;

function SnapshotWrite(Root: TValue; var Data: TBytes; var Refusal: UnicodeString): Boolean;
var W: TSnapWriter; Head, RootS: TMemoryStream; I, J: Integer; T: PFuncTemplate; Pl: TTplPlace;
  RootUsed: array of Integer; RootIx: TList<Integer>;
begin
  Result := False;
  Refusal := '';
  SetLength(Data, 0);
  if NamedCount = 0 then
  begin
    Refusal := 'the engine named no built-ins';
    Exit;
  end;
  W := TSnapWriter.Create;
  Head := TMemoryStream.Create;
  RootS := TMemoryStream.Create;
  RootIx := TList<Integer>.Create;
  try
    try
      W.Value(RootS, Root);
      I := 0;
      while I < W.Order.Count do
      begin
        W.Cell(W.Order[I]);
        Inc(I);
      end;
      W.I32(Head, SNAP_MAGIC);
      { the built-ins: their names and fingerprints }
      W.I32(Head, W.BuiltinRefs.Count);
      for I := 0 to W.BuiltinRefs.Count - 1 do
      begin
        W.Text(Head, Names[W.BuiltinRefs[I]]);
        W.U64(Head, Fingerprint(PtrOf(NamedAt(W.BuiltinRefs[I])), W.Builtins));
      end;
      { the roots the templates come from, those used only, and the templates }
      if W.Roots <> nil then SetLength(RootUsed, W.Roots.Count) else SetLength(RootUsed, 0);
      for I := 0 to High(RootUsed) do RootUsed[I] := -1;
      for I := 0 to W.TplRefs.Count - 1 do
      begin
        Pl := W.Places[W.TplRefs[I]];
        if RootUsed[Pl.Root] < 0 then
        begin
          RootUsed[Pl.Root] := RootIx.Count;
          RootIx.Add(Pl.Root);
        end;
      end;
      W.I32(Head, RootIx.Count);
      for I := 0 to RootIx.Count - 1 do
      begin
        W.U8(Head, W.Roots[RootIx[I]].Kind);
        W.Text(Head, W.Roots[RootIx[I]].Path);
        W.U64(Head, W.Roots[RootIx[I]].Hash);
      end;
      W.I32(Head, W.TplRefs.Count);
      for I := 0 to W.TplRefs.Count - 1 do
      begin
        T := PFuncTemplate(W.TplRefs[I]);
        Pl := W.Places[T];
        W.I32(Head, RootUsed[Pl.Root]);
        W.I32(Head, Length(Pl.Path));
        for J := 0 to High(Pl.Path) do W.I32(Head, Pl.Path[J]);
        { what the restoring engine's template at that place must be }
        if T^.Name <> nil then W.Text(Head, StrToUnicode(T^.Name)) else W.Text(Head, '');
        W.I32(Head, T^.DeclLine);
        W.I32(Head, T^.DeclCol);
      end;
      W.I32(Head, W.Order.Count);
      Head.CopyFrom(W.Body, 0);
      Head.CopyFrom(RootS, 0);
      SetLength(Data, Head.Size);
      if Head.Size > 0 then Move(Head.Memory^, Data[0], Head.Size);
      Result := True;
    except
      on E: ERefused do
        Refusal := UnicodeString(E.Message);
    end;
  finally
    RootIx.Free;
    RootS.Free;
    Head.Free;
    W.Free;
  end;
end;

{ ---------------- reading ---------------- }

type
  TSnapReader = class
    Data: TBytes;
    Pos: Integer;
    function I32: Integer;
    function U64: QWord;
    function U8: Byte;
    function Text: UnicodeString;
    procedure Need(N: Integer);
  end;

procedure TSnapReader.Need(N: Integer);
begin
  if (N < 0) or (Pos + N > Length(Data)) then raise ERefused.Create('the snapshot is truncated');
end;

function TSnapReader.I32: Integer;
begin
  Need(4);
  Move(Data[Pos], Result, 4);
  Inc(Pos, 4);
end;

function TSnapReader.U64: QWord;
begin
  Need(8);
  Move(Data[Pos], Result, 8);
  Inc(Pos, 8);
end;

function TSnapReader.U8: Byte;
begin
  Need(1);
  Result := Data[Pos];
  Inc(Pos);
end;

function TSnapReader.Text: UnicodeString;
var N: Integer;
begin
  N := I32;
  if (N < 0) or (N > (Length(Data) - Pos) div 2) then raise ERefused.Create('the snapshot is malformed');
  SetLength(Result, N);
  if N > 0 then Move(Data[Pos], Result[1], N * 2);
  Inc(Pos, N * 2);
end;

{ the template at Path under T, compiling lazy functions on the way }
function TemplateAt(T: PFuncTemplate; const Path: TArray<Integer>): PFuncTemplate;
var I: Integer;
begin
  Result := T;
  for I := 0 to High(Path) do
  begin
    if Result = nil then Exit(nil);
    if (Result^.Lazy <> nil) and not LazyCompileHook(Result) then
    begin
      Pending := VUndefined;
      Exit(nil);
    end;
    if Path[I] = -1 then Result := Result^.FieldInit
    else if (Path[I] >= 0) and (Path[I] <= High(Result^.Children)) then Result := Result^.Children[Path[I]]
    else Exit(nil);
  end;
end;

function RootTemplate(Kind: Integer; const Path: UnicodeString; Hash: QWord): PFuncTemplate;
var IsModule: Boolean; Err: UnicodeString; T: PFuncTemplate; Have: QWord;
begin
  Result := nil;
  if Kind = rkEntry then
  begin
    if not EntryKnown or (Path <> EntryFile) then Exit;
    if EntryTpl = nil then
    begin
      T := nil;
      Err := '';
      if not HostCompileEntry(EntrySource, EntryFile, EntryKind, T, IsModule, Err) or IsModule then Exit;
      EntryTpl := T;
    end;
    Exit(EntryTpl);
  end;
  if Kind = rkModule then
  begin
    { the entry module's source as the run had it (stdin has no file to read) }
    if EntryKnown and (Path = UnicodeString(ExpandFileName(EntryFile))) then SetProvidedSource(Path, EntrySource);
    if not LoadModuleTemplate(Path, T, Have) then
    begin
      Pending := VUndefined;
      Exit;
    end;
    { the file holds other code than the run's version of the module }
    if Have <> Hash then Exit;
    Result := T;
  end;
end;

function SnapshotRestore(const Data: TBytes; var Root: TValue; var Refusal: UnicodeString): Boolean;
var R: TSnapReader; Base, NB, NR, NT, NC, I, J, K, N, Ix, RootNo, Cnt: Integer; BuiltinVals: array of Integer;
  RootTpls: array of PFuncTemplate; Tpls: array of PFuncTemplate; RootKind: Byte; RootPath, Name, Txt: UnicodeString;
  Path: TArray<Integer>; T: PFuncTemplate; Line, Col: Integer; FP: QWord; Map: TDictionary<Pointer, Integer>;
  Kinds: array of Byte; Starts: array of Integer; Table: PValueArray; V: TValue; Cell: PCell; Big: PBigCell;
  Size: SizeInt; O: PObj; Flags: Byte; Key: PStr; KeyKind: Byte; Cap, Len: Integer; Reg: PObj;
  OKind, OFlags, HFlags: Byte; InlineCap, PadV, TplNo, RootPos: Integer;

  function TableAt(Id: Integer): TValue;
  begin
    Result := PValueArray(PtrOf(RootAt(Base)))^.Data[Id];
  end;

  { what each restored object's kind keeps in Aux1, Aux2 and its elements (Goccia.Core's okXxx notes), which the
    engine reads without checking: a wrapper's primitive, a bound function's callable target and [this, args...],
    a closure's home object and closed upvalues (at least as many as its template has), an array's elements and, past them,
    its length }
  procedure CheckKinds;
  var I, J: Integer; O: PObj; A: PValueArray; D: Double; Ok: Boolean;
  begin
    for I := 0 to NC - 1 do
    begin
      if not IsObject(TableAt(I)) then Continue;
      O := ObjOf(TableAt(I));
      case O^.OKind of
        okBoolean: Ok := (O^.Aux1 = VTrue) or (O^.Aux1 = VFalse);
        okNumber: Ok := IsNumber(O^.Aux1);
        okStringObj: Ok := IsString(O^.Aux1);
        okSymbolObj: Ok := KindOf(O^.Aux1) = ckSymbol;
        okBound:
          Ok := IsCallable(O^.Aux1) and (KindOf(O^.Aux2) = ckValueArray) and
            (PValueArray(PtrOf(O^.Aux2))^.Len >= 1);
        okFunction:
          begin
            { a template without upvalues gets no array (undefined) }
            Ok := (O^.Aux1 = VUndefined) or IsObject(O^.Aux1);
            if Ok and (KindOf(O^.Aux2) <> ckValueArray) then
              Ok := (O^.Aux2 = VUndefined) and (Length(PFuncTemplate(O^.Native)^.Upvals) = 0)
            else if Ok then
            begin
              A := PValueArray(PtrOf(O^.Aux2));
              Ok := A^.Len >= Length(PFuncTemplate(O^.Native)^.Upvals);
              for J := 0 to Integer(A^.Len) - 1 do
                if Ok and (KindOf(A^.Data[J]) <> ckUpval) then Ok := False;
            end;
          end;
        okArray:
          begin
            Ok := O^.Elements <> nil;
            if Ok and ((O^.OFlags and ofBigLen) <> 0) then
            begin
              Ok := IsNumber(O^.Aux2);
              if Ok then
              begin
                if IsInt(O^.Aux2) then D := IntOf(O^.Aux2) else D := DoubleOf(O^.Aux2);
                Ok := (D = Int(D)) and (D >= O^.Elements^.Len) and (D <= 4294967295.0);
              end;
            end;
          end;
      else
        Ok := True;
      end;
      if not Ok then raise ERefused.Create('the snapshot is malformed');
    end;
  end;

  { 10.1.2.1 OrdinarySetPrototypeOf step 8 keeps an ordinary object's prototype chain from coming back to itself,
    and [[Get]], [[HasProperty]] and instanceof walk it to its end. A snapshot's prototypes are references to any
    of its objects, so a chain among them that loops is refused (a built-in's chain ends in built-ins). Each object
    is walked once: 1 while on the current walk, 2 once its chain is known to end }
  procedure CheckProtoChains;
  var Ix: TDictionary<Pointer, Integer>; State: array of Byte; I, J, P, Mode: Integer; V: TValue;
    { the next object on J's chain: its prototype (Mode 0) or a bound function's target (Mode 1, where a loop would
      run BoundChainArgs forever) }
    function Next(J: Integer): TValue;
    begin
      if Mode = 0 then Exit(ObjOf(TableAt(J))^.Shape^.Proto);
      if ObjOf(TableAt(J))^.OKind = okBound then Exit(ObjOf(TableAt(J))^.Aux1);
      Result := VUndefined;
    end;
  begin
    Ix := TDictionary<Pointer, Integer>.Create;
    try
      for I := 0 to NC - 1 do
        if IsObject(TableAt(I)) then Ix.Add(PtrOf(TableAt(I)), I);
      for Mode := 0 to 1 do
      begin
        SetLength(State, 0);
        SetLength(State, NC);
        for I := 0 to NC - 1 do
        begin
          if (State[I] <> 0) or not IsObject(TableAt(I)) then Continue;
          J := I;
          while True do
          begin
            State[J] := 1;
            V := Next(J);
            if not IsObject(V) or not Ix.TryGetValue(PtrOf(V), P) or (State[P] = 2) then Break;
            if State[P] = 1 then raise ERefused.Create('the snapshot is malformed: a prototype or bound chain loops');
            J := P;
          end;
          J := I;
          while State[J] = 1 do
          begin
            State[J] := 2;
            V := Next(J);
            if not IsObject(V) or not Ix.TryGetValue(PtrOf(V), P) then Break;
            J := P;
          end;
        end;
      end;
    finally
      Ix.Free;
    end;
  end;

  function ReadValue: TValue;
  var Tag: Byte; X: Integer;
  begin
    Tag := R.U8;
    case Tag of
      vtRaw:
        begin
          Result := R.U64;
          { only a value the engine makes: a pointer is a vtCell, a special one of the five constants, an integer
            an Int32, a NaN the canonical one (every routine that reads a value relies on these; ToString, for
            one, recurses forever on an unknown special) }
          case Result shr 48 of
            TAG_INT_HI: if (Result and $0000FFFF00000000) <> 0 then raise ERefused.Create('the snapshot is malformed');
            TAG_SPECIAL_HI: if Result > VHole then raise ERefused.Create('the snapshot is malformed');
            TAG_PTR_HI..$FFFF: raise ERefused.Create('the snapshot is malformed');
          else
            if ((Result and $7FF0000000000000) = $7FF0000000000000) and ((Result and $000FFFFFFFFFFFFF) <> 0) and
               (Result <> CANON_NAN) then raise ERefused.Create('the snapshot is malformed');
          end;
        end;
      vtCell:
        begin
          X := R.I32;
          if (X < 0) or (X >= NC) then raise ERefused.Create('the snapshot is malformed');
          Result := TableAt(X);
        end;
      vtBuiltin:
        begin
          X := R.I32;
          if (X < 0) or (X >= NB) then raise ERefused.Create('the snapshot is malformed');
          Result := NamedAt(BuiltinVals[X]);
        end;
    else
      raise ERefused.Create('the snapshot is malformed');
    end;
  end;

  { a field of a restored cell: the cell may be old by now }
  procedure Store(Container: PCell; Slot: PValue; V: TValue);
  begin
    Slot^ := V;
    WriteBarrier(Container, V);
  end;

begin
  Result := False;
  Refusal := '';
  Root := VUndefined;
  if NamedCount = 0 then
  begin
    Refusal := 'the engine named no built-ins';
    Exit;
  end;
  R := TSnapReader.Create;
  Map := nil;
  Base := TempTop;
  try
    try
      R.Data := Data;
      R.Pos := 0;
      if R.I32 <> SNAP_MAGIC then raise ERefused.Create('not a snapshot of this engine');
      { the built-ins, each the same as the run's }
      NB := R.I32;
      if (NB < 0) or (NB > NamedCount) then raise ERefused.Create('the snapshot is malformed');
      SetLength(BuiltinVals, NB);
      Map := BuiltinMap;
      for I := 0 to NB - 1 do
      begin
        Name := R.Text;
        FP := R.U64;
        if not NameIndex.TryGetValue(Name, Ix) then
          raise ERefused.Create('the snapshot names a built-in this engine does not have: ' + Name);
        if Fingerprint(PtrOf(NamedAt(Ix)), Map) <> FP then
          raise ERefused.Create('the run changed the built-in ' + Name);
        BuiltinVals[I] := Ix;
      end;
      { the code the functions run }
      NR := R.I32;
      if (NR < 0) or (NR > 4096) then raise ERefused.Create('the snapshot is malformed');
      SetLength(RootTpls, NR);
      for I := 0 to NR - 1 do
      begin
        RootKind := R.U8;
        RootPath := R.Text;
        RootTpls[I] := RootTemplate(RootKind, RootPath, R.U64);
        if RootTpls[I] = nil then raise ERefused.Create('the snapshot names code this engine cannot compile: ' +
          RootPath);
      end;
      NT := R.I32;
      if (NT < 0) or (NT > 1024 * 1024) then raise ERefused.Create('the snapshot is malformed');
      SetLength(Tpls, NT);
      for I := 0 to NT - 1 do
      begin
        RootNo := R.I32;
        if (RootNo < 0) or (RootNo >= NR) then raise ERefused.Create('the snapshot is malformed');
        N := R.I32;
        if (N < 0) or (N > 4096) then raise ERefused.Create('the snapshot is malformed');
        SetLength(Path, N);
        for J := 0 to N - 1 do Path[J] := R.I32;
        Name := R.Text;
        Line := R.I32;
        Col := R.I32;
        T := TemplateAt(RootTpls[RootNo], Path);
        if (T = nil) or (T^.DeclLine <> Line) or (T^.DeclCol <> Col) or
           ((T^.Name = nil) and (Name <> '')) or ((T^.Name <> nil) and (StrToUnicode(T^.Name) <> Name)) then
          raise ERefused.Create('the snapshot names a function this engine compiled differently');
        Tpls[I] := T;
      end;
      NC := R.I32;
      if (NC < 0) or (NC > MAX_CELLS) or (NC > Length(Data)) then raise ERefused.Create('the snapshot is malformed');
      { first every cell, empty, in a rooted table; then each cell's contents }
      Table := NewValueArray(Max(NC, 1));
      Table^.Len := NC;
      for I := 0 to NC - 1 do Table^.Data[I] := VUndefined;
      PushRoot(MkPtr(Table));
      SetLength(Kinds, NC);
      SetLength(Starts, NC);
      for I := 0 to NC - 1 do
      begin
        Kinds[I] := R.U8;
        Starts[I] := R.Pos;
        case Kinds[I] of
          ckString:
            V := StrFromUnicode(R.Text);
          ckBigInt:
            begin
              Cnt := R.I32;
              if (Cnt < 0) or (Cnt > (Length(Data) - R.Pos) div 4) then raise ERefused.Create('the snapshot is malformed');
              J := R.I32;
              Size := SizeOf(TBigCell) - SizeOf(Cardinal) + Max(Cnt, 1) * SizeOf(Cardinal);
              Big := PBigCell(AllocCell(Size, ckBigInt));
              Big^.Count := Cnt;
              Big^.Negative := Ord(J <> 0);
              for K := 0 to Cnt - 1 do Big^.Limbs[K] := Cardinal(R.I32);
              if Cnt = 0 then Big^.Limbs[0] := 0;
              V := MkPtr(Big);
            end;
          ckSymbol:
            begin
              if R.U8 = 1 then
              begin
                Txt := R.Text;
                Key := AtomOf(Txt);
                Reg := ObjOf(SymbolRegistryObject);
                K := GetOwnSlot(Reg, Key);
                if K >= 0 then V := GetSlot(Reg, K)
                else
                begin
                  V := NewSymbolValue(Txt);
                  PushRoot(V);
                  AddProperty(ObjOf(SymbolRegistryObject), AtomOf(Txt), V, pfDefault);
                  V := RootAt(TempTop - 1);
                  PopRoots(1);
                end;
              end
              else
              begin
                J := R.U8;
                Txt := R.Text;
                V := NewSymbolValue(Txt);
                if J <> 0 then PCell(PtrOf(V))^.Extra := 1;
              end;
            end;
          ckValueArray:
            begin
              Cap := R.I32;
              Len := R.I32;
              if (Len < 0) or (Cap < Len) or (Len > (Length(Data) - R.Pos) div 5) or (Cap > ArrayDenseMax) then
                raise ERefused.Create('the snapshot is malformed');
              V := MkPtr(NewValueArray(Max(Cap, 1)));
              PValueArray(PtrOf(V))^.Len := Len;
              for K := 0 to Len - 1 do PValueArray(PtrOf(V))^.Data[K] := VUndefined;
              for K := 0 to Len - 1 do
              begin
                case R.U8 of
                  vtRaw: R.U64;
                  vtCell, vtBuiltin: R.I32;
                else
                  raise ERefused.Create('the snapshot is malformed');
                end;
              end;
            end;
          ckUpval:
            begin
              V := MkPtr(NewUpval);
              PUpval(PtrOf(V))^.Value := VUndefined;
              PUpval(PtrOf(V))^.StackIndex := -1;
              case R.U8 of
                vtRaw: R.U64;
                vtCell, vtBuiltin: R.I32;
              else
                raise ERefused.Create('the snapshot is malformed');
              end;
            end;
          ckAccessor:
            begin
              V := MkPtr(NewAccessor(VUndefined, VUndefined));
              for K := 0 to 1 do
                case R.U8 of
                  vtRaw: R.U64;
                  vtCell, vtBuiltin: R.I32;
                else
                  raise ERefused.Create('the snapshot is malformed');
                end;
            end;
          ckObject:
            begin
              OKind := R.U8;
              if not (OKind in [okPlain, okArray, okFunction, okBound, okError, okBoolean, okNumber, okStringObj,
                okSymbolObj, okArgumentsOrd]) then raise ERefused.Create('the snapshot is malformed');
              R.U8; R.U8;
              InlineCap := R.I32;
              if (InlineCap < 0) or (InlineCap > 4096) then raise ERefused.Create('the snapshot is malformed');
              V := MkPtr(NewObject(RootShapeFor(VNull), InlineCap, OKind));
              { the rest is read in the second pass }
              R.Pos := Starts[I];
              K := 0;
              { skip: kind, flags, header flags, extra, inline cap, pad, template, proto, keys, elements, aux }
              R.U8; R.U8; R.U8; R.I32; R.I32; R.I32;
              case R.U8 of vtRaw: R.U64; vtCell, vtBuiltin: R.I32; else raise ERefused.Create('the snapshot is malformed'); end;
              Cnt := R.I32;
              if (Cnt < 0) or (Cnt > (Length(Data) - R.Pos) div 3) then raise ERefused.Create('the snapshot is malformed');
              for J := 0 to Cnt - 1 do
              begin
                if R.U8 = 1 then
                  case R.U8 of vtRaw: R.U64; vtCell, vtBuiltin: R.I32; else raise ERefused.Create('the snapshot is malformed'); end
                else R.Text;
                R.U8;
                case R.U8 of vtRaw: R.U64; vtCell, vtBuiltin: R.I32; else raise ERefused.Create('the snapshot is malformed'); end;
              end;
              if R.U8 = 1 then
                case R.U8 of vtRaw: R.U64; vtCell, vtBuiltin: R.I32; else raise ERefused.Create('the snapshot is malformed'); end;
              for J := 0 to 1 do
                case R.U8 of vtRaw: R.U64; vtCell, vtBuiltin: R.I32; else raise ERefused.Create('the snapshot is malformed'); end;
            end;
        else
          raise ERefused.Create('the snapshot is malformed');
        end;
        Table := PValueArray(PtrOf(RootAt(Base)));
        ValueArrayStore(Table, I, V);
      end;
      RootPos := R.Pos;
      { the contents }
      for I := 0 to NC - 1 do
      begin
        R.Pos := Starts[I];
        case Kinds[I] of
          ckValueArray:
            begin
              R.I32;
              Len := R.I32;
              for K := 0 to Len - 1 do
              begin
                V := ReadValue;
                Cell := PCell(PtrOf(TableAt(I)));
                Store(Cell, @PValueArray(Cell)^.Data[K], V);
              end;
            end;
          ckUpval:
            begin
              V := ReadValue;
              Cell := PCell(PtrOf(TableAt(I)));
              Store(Cell, @PUpval(Cell)^.Value, V);
            end;
          ckAccessor:
            begin
              V := ReadValue;
              Cell := PCell(PtrOf(TableAt(I)));
              Store(Cell, @PAccessor(Cell)^.Getter, V);
              V := ReadValue;
              Cell := PCell(PtrOf(TableAt(I)));
              Store(Cell, @PAccessor(Cell)^.Setter, V);
            end;
          ckObject:
            begin
              OKind := R.U8;
              OFlags := R.U8;
              HFlags := R.U8;
              InlineCap := R.I32;
              PadV := R.I32;
              TplNo := R.I32;
              if (OKind = okFunction) <> (TplNo >= 0) then raise ERefused.Create('the snapshot is malformed');
              if TplNo >= NT then raise ERefused.Create('the snapshot is malformed');
              V := ReadValue;
              if not (IsNullish(V) and (V = VNull)) and not IsObject(V) then
                raise ERefused.Create('the snapshot is malformed');
              O := ObjOf(TableAt(I));
              O^.Shape := RootShapeFor(V);
              O^.OFlags := OFlags;
              O^.Hdr.Flags := O^.Hdr.Flags or (HFlags and cfEmulatesUndefined);
              O^.Pad := UInt32(PadV);
              if TplNo >= 0 then O^.Native := Tpls[TplNo];
              Cnt := R.I32;
              for J := 0 to Cnt - 1 do
              begin
                KeyKind := R.U8;
                if KeyKind = 1 then
                begin
                  V := ReadValue;
                  if KindOf(V) <> ckSymbol then raise ERefused.Create('the snapshot is malformed');
                  Key := PStr(PPointer(PByte(PtrOf(V)) + 16)^);
                end
                else Key := AtomOf(R.Text);
                Flags := R.U8;
                PushRoot(MkPtr(Key));
                V := ReadValue;
                if ((Flags and pfAccessor) <> 0) <> (KindOf(V) = ckAccessor) then
                  raise ERefused.Create('the snapshot is malformed');
                O := ObjOf(TableAt(I));
                if GetOwnSlot(O, Key) >= 0 then raise ERefused.Create('the snapshot is malformed');
                AddProperty(O, Key, V, Flags);
                PopRoots(1);
              end;
              if R.U8 = 1 then
              begin
                V := ReadValue;
                if KindOf(V) <> ckValueArray then raise ERefused.Create('the snapshot is malformed');
                O := ObjOf(TableAt(I));
                O^.Elements := PValueArray(PtrOf(V));
                WriteBarrier(PCell(O), V);
              end;
              V := ReadValue;
              O := ObjOf(TableAt(I));
              Store(PCell(O), @O^.Aux1, V);
              V := ReadValue;
              O := ObjOf(TableAt(I));
              Store(PCell(O), @O^.Aux2, V);
            end;
        end;
      end;
      CheckKinds;
      CheckProtoChains;
      R.Pos := RootPos;
      Root := ReadValue;
      if R.Pos <> Length(Data) then raise ERefused.Create('the snapshot is malformed');
      Result := True;
    except
      on E: ERefused do
      begin
        Refusal := UnicodeString(E.Message);
        Pending := VUndefined;
      end;
    end;
  finally
    PopRoots(TempTop - Base);
    Map.Free;
    R.Free;
  end;
end;

{ ---------------- the canonical description ---------------- }

function NumberText(V: TValue): UnicodeString;
begin
  Result := 'n' + IntToHex(QWord(MkNumber(NumOf(V))), 16);
end;

function SnapshotDescribe(Root: TValue): UnicodeString;
var Attempt, Epoch: Integer; Map: TDictionary<Pointer, Integer>; Ids: TDictionary<Pointer, Integer>;
  Queue: TList<Pointer>; Out_: TStringBuilder; Unstable: Boolean;

  function Leaf(V: TValue): UnicodeString;
  var Ix: Integer; S: UnicodeString;
  begin
    if V = VUndefined then Exit('undefined');
    if V = VNull then Exit('null');
    if V = VTrue then Exit('true');
    if V = VFalse then Exit('false');
    if V = VHole then Exit('hole');
    if IsNumber(V) then Exit(NumberText(V));
    if not IsPtr(V) then Exit('?' + IntToHex(V, 16));
    if Map.TryGetValue(PtrOf(V), Ix) then Exit('<' + Names[Ix] + '>');
    case PCell(PtrOf(V))^.Kind of
      ckString:
        begin
          S := StrToUnicode(StrOf(V));
          Exit('"' + StringReplace(StringReplace(S, '\', '\\', [rfReplaceAll]), '"', '\"', [rfReplaceAll]) + '"');
        end;
      ckBigInt: Exit(BigToString(V) + 'n');
      ckSymbol, ckObject:
        begin
          if not Ids.TryGetValue(PtrOf(V), Ix) then
          begin
            Ix := Queue.Count;
            Queue.Add(PtrOf(V));
            Ids.Add(PtrOf(V), Ix);
          end;
          Exit('#' + IntToStr(Ix));
        end;
    end;
    Result := '?cell' + IntToStr(PCell(PtrOf(V))^.Kind);
  end;

  procedure DescribeObject(Ix: Integer);
  var V, Keys, KV: TValue; Ext: Boolean; D: TPropDesc; Found: Boolean; I, N: Integer; K: PStr; P: PObj;
    T: PFuncTemplate;
  begin
    V := MkPtr(Queue[Ix]);
    if PCell(Queue[Ix])^.Kind = ckSymbol then
    begin
      Out_.Append('#' + IntToStr(Ix) + ' symbol ' + Leaf(PValue(PByte(Queue[Ix]) + 8)^) +
        ' undef=' + IntToStr(PCell(Queue[Ix])^.Extra) + #10);
      Exit;
    end;
    P := ObjOf(V);
    Ext := (P^.OFlags and 8) = 0;
    Out_.Append('#' + IntToStr(Ix) + ' kind=' + IntToStr(P^.OKind) + ' ext=' + IntToStr(Ord(Ext)) +
      ' proto=' + Leaf(P^.Shape^.Proto));
    if P^.OKind = okFunction then
    begin
      T := PFuncTemplate(P^.Native);
      Out_.Append(' code=' + IntToStr(T^.DeclLine) + ':' + IntToStr(T^.DeclCol));
    end;
    if P^.OKind in [okBoolean, okNumber, okStringObj, okSymbolObj] then
      Out_.Append(' primitive=' + Leaf(P^.Aux1));
    if P^.OKind = okBound then Out_.Append(' target=' + Leaf(P^.Aux1));
    Out_.Append(#10);
    PushRoot(V);
    try
      if not OwnPropertyKeysV(V, Keys) then begin Pending := VUndefined; Unstable := True; Exit; end;
      PushRoot(Keys);
      N := ArrayLen(ObjOf(Keys));
      for I := 0 to N - 1 do
      begin
        KV := ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I];
        if IsString(KV) then
        begin
          K := Atomize(StrOf(KV));
          Out_.Append('  ' + Leaf(KV));
        end
        else
        begin
          K := PStr(PPointer(PByte(PtrOf(KV)) + 16)^);
          Out_.Append('  [' + Leaf(KV) + ']');
        end;
        if not GetOwnPropertyV(RootAt(TempTop - 2), K, D, Found) then
        begin
          Pending := VUndefined;
          Unstable := True;
          Break;
        end;
        if not Found then Out_.Append(' missing')
        else if D.HasGet or D.HasSet then
          Out_.Append(' get=' + Leaf(D.Get) + ' set=' + Leaf(D.Set_) + ' e=' + IntToStr(Ord(D.Enumerable)) +
            ' c=' + IntToStr(Ord(D.Configurable)))
        else
          Out_.Append(' = ' + Leaf(D.Value) + ' w=' + IntToStr(Ord(D.Writable)) + ' e=' + IntToStr(Ord(D.Enumerable)) +
            ' c=' + IntToStr(Ord(D.Configurable)));
        Out_.Append(#10);
      end;
      PopRoots(1);
    finally
      PopRoots(1);
    end;
  end;

var I: Integer;
begin
  Result := '';
  PushRoot(Root);
  try
    for Attempt := 1 to 4 do
    begin
      CollectMinor;
      Root := RootAt(TempTop - 1);
      Epoch := GCMinorCount;
      Unstable := False;
      Map := BuiltinMap;
      Ids := TDictionary<Pointer, Integer>.Create;
      Queue := TList<Pointer>.Create;
      Out_ := TStringBuilder.Create;
      try
        Out_.Append('root ' + Leaf(Root) + #10);
        I := 0;
        while (I < Queue.Count) and not Unstable do
        begin
          DescribeObject(I);
          { a collection moved what the description holds by address: describe again }
          if GCMinorCount <> Epoch then begin Unstable := True; Break; end;
          Inc(I);
        end;
        Result := UnicodeString(Out_.ToString);
      finally
        Out_.Free;
        Queue.Free;
        Ids.Free;
        Map.Free;
      end;
      if not Unstable then Exit;
    end;
    Result := 'unstable: ' + Result;
  finally
    PopRoots(1);
  end;
end;

{ ---------------- observations ---------------- }

function SnapshotObserve(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var V: TValue; A: PObj; I: Integer;
begin
  Res := VUndefined;
  Result := True;
  if Argc < 1 then Exit;
  V := Args[1];
  if not IsObject(V) then Exit;
  Inc(ObservedTotal);
  if FirstInEngine(ObservedRootGen) then
  begin
    Observed := VUndefined;
    AddRoot(@Observed);
  end;
  if not IsPtr(Observed) then
  begin
    Observed := MkPtr(NewArray(0, 8));
    { the allocation can move a young V: read it again from its rooted argument slot }
    V := Args[1];
  end;
  { the observed values stay rooted, so a value seen before is in the array: compare by identity there }
  A := ObjOf(Observed);
  if A^.Elements <> nil then
    for I := 0 to Integer(A^.Elements^.Len) - 1 do
      if A^.Elements^.Data[I] = V then Exit;
  ArrayPush(ObjOf(Observed), V);
end;

function SnapshotObservations: Integer;
begin
  Result := ObservedTotal;
end;

function SnapshotObservedCount: Integer;
begin
  if not IsPtr(Observed) then Exit(0);
  Result := ArrayLen(ObjOf(Observed));
end;

function SnapshotObservedAt(I: Integer): TValue;
begin
  Result := ObjOf(Observed)^.Elements^.Data[I];
end;

procedure SnapshotDone;
begin
  FreeAndNil(Names);
  FreeAndNil(NameIndex);
  NamedCells := VUndefined;
  Observed := VUndefined;
  ObservedTotal := 0;
  EntryTpl := nil;
  EntryFile := '';
  EntrySource := '';
  EntryKnown := False;
end;

{ the managed threadvars of this unit, released when the thread ends (Goccia.ThreadCleanupRegistry): FPC does not
  finalize a threadvar, and every engine thread leaves its strings, arrays and objects behind otherwise }
procedure GocciaHeapSnapshotThreadvarCleanup;
begin
  Names.Free;
  Names := nil;
  NameIndex.Free;
  NameIndex := nil;
  EntryFile := '';
  EntrySource := '';
end;

initialization
  RegisterThreadvarCleanup(@GocciaHeapSnapshotThreadvarCleanup);
end.

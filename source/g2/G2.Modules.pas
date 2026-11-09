unit G2.Modules;
{ 16.2.1 Modules: Source Text Module Records loaded from files, linked and evaluated; module namespace objects
  (10.4.6, G2.VM's okModuleNS); import() and import.meta.

  A module compiles (goal 3, G2.Compiler) to a generator over its cell table: calling it is InitializeEnvironment
  (its function declarations made, its exported bindings published as cells), resuming it is ExecuteModule.
  Imports are the exporters' cells in the importer's table. Linking runs every module's start first, then fills
  the import slots, so cycles see each other's functions. A module with top-level await is an async generator;
  its evaluation waits for it (a simplification of 16.2.1.5.3's asynchronous ordering). }
{$mode delphiunicode}{$H+}
{$MINFPCONSTPREC 64}

interface

uses
  G2.Core;

procedure InitModules;
{ loads, links and evaluates the module at Path (the host's entry point); False with the error pending, Phase
  0 when it is a parse or link (resolution) error, 1 an evaluation error }
function RunModuleFile(const Path: UnicodeString; var Phase: Integer): Boolean;

implementation

uses
  SysUtils, Classes, Generics.Collections, G2.Bytecode, G2.VM, G2.Compiler, G2.Eval, G2.Promise;

type
  TModStatus = (msNew, msLinking, msLinked, msEvaluating, msEvaluatingAsync, msEvaluated);
  TModRec = class
    Id: Integer;
    Path: UnicodeString;
    Info: TModuleInfo;
    Tpl: PFuncTemplate;
    Status: TModStatus;
    Errored: Boolean;   { with Status msEvaluated: [[EvaluationError]] (rError); with msNew: a load error }
    Deps: array of TModRec;
    { 16.2.1.6 Cyclic Module Record fields of the evaluation }
    DFSAnc, PendingAsync: Integer;
    CycleRoot: TModRec;
    AsyncOrder: Int64;   { [[AsyncEvaluationOrder]]: 0 unset, -1 done }
    AsyncParents: array of TModRec;
    destructor Destroy; override;
  end;
  TResKind = (rkNull, rkAmbiguous, rkBinding, rkNamespace, rkDeferNS);
  TResolution = record
    Kind: TResKind;
    Module: TModRec;
    Slot: Integer;
  end;

const
  { the values of a module kept in ModRoots at Id * RS + k }
  RS = 7; rTable = 0; rGen = 1; rNS = 2; rMeta = 3; rError = 4; rDNS = 5; rCap = 6;

threadvar
  Mods: TList<TModRec>;
  AsyncEvalCount: Int64;   { IncrementModuleAsyncEvaluationCount }
  ModRoots: TValue;     { a rooted array: RS slots per module }
  PendingImports: TValue;   { a rooted array of [promise, referrer, specifier] per import() in flight }

destructor TModRec.Destroy;
begin
  Info.Free;
  inherited;
end;

function GetRoot(M: TModRec; K: Integer): TValue; inline;
begin
  Result := ObjOf(ModRoots)^.Elements^.Data[M.Id * RS + K];
end;

procedure SetRoot(M: TModRec; K: Integer; V: TValue);
begin
  while ObjOf(ModRoots)^.Elements^.Len <= M.Id * RS + K do
  begin
    PushRoot(V);
    ArrayPush(ObjOf(ModRoots), VUndefined);
    V := RootAt(TempTop - 1);
    PopRoots(1);
  end;
  ValueArrayStore(ObjOf(ModRoots)^.Elements, M.Id * RS + K, V);
end;

{ ---- loading ---- }

function ResolvePath(const Spec, Referrer: UnicodeString): UnicodeString;
var Base: UnicodeString;
begin
  if (Spec <> '') and (Spec[1] = '/') then Exit(UnicodeString(ExpandFileName(string(Spec))));
  if (Referrer = '') or (Referrer[1] = '<') then Base := UnicodeString(GetCurrentDir) + '/'
  else Base := UnicodeString(ExtractFilePath(ExpandFileName(string(Referrer))));
  Result := UnicodeString(ExpandFileName(string(Base + Spec)));
end;

function ReadSource(const Path: UnicodeString; var Src: UnicodeString): Boolean;
var F: TFileStream; B: RawByteString;
begin
  Result := False;
  if not FileExists(string(Path)) then Exit;
  F := TFileStream.Create(string(Path), fmOpenRead or fmShareDenyNone);
  try
    SetLength(B, F.Size);
    if F.Size > 0 then F.ReadBuffer(B[1], F.Size);
  finally
    F.Free;
  end;
  SetCodePage(B, CP_UTF8, False);
  Src := UnicodeString(UTF8Decode(B));
  if (Src <> '') and (Src[1] = #$FEFF) then Delete(Src, 1, 1);
  Result := True;
end;

{ a request key (ModuleRequestKey) resolved against its referrer: the resolved path, then its type suffix }
function ResolveRequest(const Req, Referrer: UnicodeString): UnicodeString;
var P: Integer;
begin
  P := Pos(#1, Req);
  if P = 0 then Exit(ResolvePath(Req, Referrer));
  Result := ResolvePath(Copy(Req, 1, P - 1), Referrer) + Copy(Req, P, MaxInt);
end;

function ReadBytes(const Path: UnicodeString; var B: RawByteString): Boolean;
var F: TFileStream;
begin
  Result := False;
  B := '';
  if not FileExists(string(Path)) then Exit;
  F := TFileStream.Create(string(Path), fmOpenRead or fmShareDenyNone);
  try
    SetLength(B, F.Size);
    if F.Size > 0 then F.ReadBuffer(B[1], F.Size);
  finally
    F.Free;
  end;
  Result := True;
end;

function FindModule(const Path: UnicodeString): TModRec;
var M: TModRec;
begin
  for M in Mods do
    if M.Path = Path then Exit(M);
  Result := nil;
end;

function ClosedCell(V: TValue): TValue; forward;

{ a module of a non-JavaScript type, a Synthetic Module Record with the one export default, evaluated at once:
  'json' (ParseJSONModule), 'text' (the import text proposal: the source decoded as UTF-8) and 'bytes' (the
  import bytes proposal: a Uint8Array over an immutable ArrayBuffer) }
function LoadTypedModule(const Key: UnicodeString; var M: TModRec): Boolean;
var P: Integer; Path, AType, Src: UnicodeString; B: RawByteString; V, Tbl: TValue;
begin
  P := Pos(#1, Key);
  Path := Copy(Key, 1, P - 1);
  AType := Copy(Key, P + 1, MaxInt);
  if (AType <> 'json') and (AType <> 'text') and (AType <> 'bytes') then
    Exit(ThrowError('SyntaxError', 'Unsupported module type ''' + AType + ''''));
  if AType = 'bytes' then
  begin
    if not ReadBytes(Path, B) then Exit(ThrowTypeError('Cannot find module ''' + Path + ''''));
    if not BytesHook(B, V) then Exit(False);
  end
  else
  begin
    if not ReadSource(Path, Src) then Exit(ThrowTypeError('Cannot find module ''' + Path + ''''));
    if AType = 'text' then V := StrFromUnicode(Src)
    else if not JSONTextHook(Src, V) then Exit(False);
  end;
  PushRoot(V);
  M := TModRec.Create;
  M.Path := Key;
  M.Id := Mods.Count;
  Mods.Add(M);
  SetRoot(M, rError, VUndefined);
  M.Info := TModuleInfo.Create;
  SetLength(M.Info.LocalExports, 1);
  M.Info.LocalExports[0].ExportName := 'default';
  M.Info.LocalExports[0].Slot := 0;
  M.Info.SlotCount := 1;
  Tbl := MkPtr(NewArray(0, 1));
  SetRoot(M, rTable, Tbl);
  ArrayPush(ObjOf(GetRoot(M, rTable)), ClosedCell(RootAt(TempTop - 1)));
  PopRoots(1);
  M.Status := msEvaluated;
  Result := True;
end;

{ 16.2.1.5.1 LoadRequestedModules (synchronously, from files): the module at Path and everything it requests }
function LoadModule(const Path: UnicodeString; var M: TModRec): Boolean;
var Src, Err, ErrName: UnicodeString; I: Integer; D: TModRec;
begin
  M := FindModule(Path);
  if Assigned(M) then
  begin
    if M.Errored and (M.Status = msNew) then begin Pending := GetRoot(M, rError); Exit(False); end;
    Exit(True);
  end;
  if Pos(#1, Path) > 0 then Exit(LoadTypedModule(Path, M));
  if not ReadSource(Path, Src) then Exit(ThrowTypeError('Cannot find module ''' + Path + ''''));
  M := TModRec.Create;
  M.Path := Path;
  M.Id := Mods.Count;
  Mods.Add(M);
  SetRoot(M, rError, VUndefined);
  if not ParseAndCompileModule(Src, Path, M.Id + 1, M.Tpl, M.Info, Err, ErrName) then
  begin
    M.Errored := True;
    ThrowError(ErrName, Err);
    SetRoot(M, rError, Pending);
    Exit(False);
  end;
  SetLength(M.Deps, Length(M.Info.Requests));
  for I := 0 to High(M.Info.Requests) do
  begin
    if not LoadModule(ResolveRequest(M.Info.Requests[I], Path), D) then Exit(False);
    M.Deps[I] := D;
  end;
  Result := True;
end;

function DepOf(M: TModRec; const Req: UnicodeString): TModRec;
var I: Integer;
begin
  for I := 0 to High(M.Info.Requests) do
    if M.Info.Requests[I] = Req then Exit(M.Deps[I]);
  Result := nil;
end;

{ ---- 16.2.1.7.2.2 ResolveExport / .1 GetExportedNames ---- }

type
  TResolveSet = array of record M: TModRec; N: UnicodeString; end;

function ResolveExport(M: TModRec; const Name: UnicodeString; var RS_: TResolveSet): TResolution;
var I: Integer; R: TResolution; Star: TResolution; D: TModRec;
begin
  Result.Kind := rkNull; Result.Module := nil; Result.Slot := -1;
  for I := 0 to High(RS_) do
    if (RS_[I].M = M) and (RS_[I].N = Name) then Exit;   { a circular import request }
  SetLength(RS_, Length(RS_) + 1);
  RS_[High(RS_)].M := M; RS_[High(RS_)].N := Name;
  for I := 0 to High(M.Info.LocalExports) do
    if M.Info.LocalExports[I].ExportName = Name then
    begin
      Result.Kind := rkBinding; Result.Module := M; Result.Slot := M.Info.LocalExports[I].Slot;
      Exit;
    end;
  for I := 0 to High(M.Info.IndirectExports) do
    if M.Info.IndirectExports[I].ExportName = Name then
    begin
      D := DepOf(M, M.Info.IndirectExports[I].Request);
      if M.Info.IndirectExports[I].ImportName = ModNamespace then
      begin
        Result.Kind := rkNamespace; Result.Module := D;
        Exit;
      end;
      if M.Info.IndirectExports[I].ImportName = ModDeferNamespace then
      begin
        Result.Kind := rkDeferNS; Result.Module := D;
        Exit;
      end;
      Exit(ResolveExport(D, M.Info.IndirectExports[I].ImportName, RS_));
    end;
  if Name = 'default' then Exit;
  Star.Kind := rkNull;
  for I := 0 to High(M.Info.StarExports) do
  begin
    R := ResolveExport(DepOf(M, M.Info.StarExports[I]), Name, RS_);
    if R.Kind = rkAmbiguous then Exit(R);
    if R.Kind = rkNull then Continue;
    if Star.Kind = rkNull then Star := R
    else if (R.Module <> Star.Module) or (R.Kind <> Star.Kind) or (R.Slot <> Star.Slot) then
    begin
      Result.Kind := rkAmbiguous;
      Exit;
    end;
  end;
  Result := Star;
end;

function Resolve(M: TModRec; const Name: UnicodeString): TResolution;
var RS_: TResolveSet;
begin
  SetLength(RS_, 0);
  Result := ResolveExport(M, Name, RS_);
end;

procedure ExportedNames(M: TModRec; Seen: TList<TModRec>; Names: TStringList);
var I, J: Integer; S: TStringList;
begin
  if Seen.IndexOf(M) >= 0 then Exit;
  Seen.Add(M);
  for I := 0 to High(M.Info.LocalExports) do Names.Add(M.Info.LocalExports[I].ExportName);
  for I := 0 to High(M.Info.IndirectExports) do Names.Add(M.Info.IndirectExports[I].ExportName);
  for I := 0 to High(M.Info.StarExports) do
  begin
    S := TStringList.Create;
    S.UseLocale := False; S.CaseSensitive := True;
    try
      ExportedNames(DepOf(M, M.Info.StarExports[I]), Seen, S);
      for J := 0 to S.Count - 1 do
        if (S[J] <> 'default') and (Names.IndexOf(S[J]) < 0) then Names.Add(S[J]);
    finally
      S.Free;
    end;
  end;
end;

function CodeUnitOrder(L: TStringList; A, B: Integer): Integer;
begin
  if L[A] < L[B] then Result := -1 else if L[A] > L[B] then Result := 1 else Result := 0;
end;

function ClosedCell(V: TValue): TValue;
var U: PUpval;
begin
  PushRoot(V);
  U := NewUpval;
  U^.StackIndex := -1;
  U^.Value := RootAt(TempTop - 1);
  PopRoots(1);
  Result := MkPtr(U);
end;

function CellOf(const R: TResolution): TValue; forward;

{ 16.2.1.10 GetModuleNamespace / 10.4.6.12 ModuleNamespaceCreate }
function GetNamespace(M: TModRec): TValue;
var Names: TStringList; Seen: TList<TModRec>; I: Integer; R: TResolution; O: PObj; Arr: PObj; Base: Integer;
begin
  Result := GetRoot(M, rNS);
  if IsObject(Result) then Exit;
  Names := TStringList.Create; Names.UseLocale := False; Names.CaseSensitive := True;
  Seen := TList<TModRec>.Create;
  Base := TempTop;
  try
    ExportedNames(M, Seen, Names);
    O := NewObject(RootShapeFor(VNull), 0, okModuleNS);
    PushRoot(MkPtr(O));
    DefineOwn(RootAt(Base), SymKey(SymToStringTagW), StrFromUnicode('Module'), 0);
    PushRoot(MkPtr(NewArray(0, 8)));
    ObjOf(RootAt(Base))^.Aux1 := RootAt(Base + 1);
    ObjOf(RootAt(Base))^.OFlags := ObjOf(RootAt(Base))^.OFlags or 8;   { not extensible }
    ObjOf(RootAt(Base))^.Shape := ShapeFork(ObjOf(RootAt(Base))^.Shape);   { its own shape: no inline cache shares it }
    SetRoot(M, rNS, RootAt(Base));
    { the unambiguous names, in code unit order (TStringList.Sort compares with the locale: sorted by hand) }
    Names.CustomSort(CodeUnitOrder);
    for I := 0 to Names.Count - 1 do
    begin
      R := Resolve(M, Names[I]);
      if not (R.Kind in [rkBinding, rkNamespace, rkDeferNS]) then Continue;
      ArrayPush(ObjOf(RootAt(Base + 1)), MkPtr(AtomOf(Names[I])));
      ArrayPush(ObjOf(RootAt(Base + 1)), CellOf(R));
    end;
    Result := RootAt(Base);
  finally
    PopRoots(TempTop - Base);
    Names.Free;
    Seen.Free;
  end;
end;

{ GetModuleNamespace(module, defer): the deferred namespace, a namespace object (OFlags 16, its module's Id in
  Aux2) whose exports leave out "then", with @@toStringTag "Deferred Module" }
function GetDeferredNamespace(M: TModRec): TValue;
var Base, I: Integer; O: PObj; E: PValueArray;
begin
  Result := GetRoot(M, rDNS);
  if IsObject(Result) then Exit;
  Base := TempTop;
  PushRoot(GetNamespace(M));
  O := NewObject(RootShapeFor(VNull), 0, okModuleNS);
  PushRoot(MkPtr(O));
  DefineOwn(RootAt(Base + 1), SymKey(SymToStringTagW), StrFromUnicode('Deferred Module'), 0);
  PushRoot(MkPtr(NewArray(0, 8)));
  E := ObjOf(ObjOf(RootAt(Base))^.Aux1)^.Elements;
  I := 0;
  while I < Integer(E^.Len) div 2 do
  begin
    if StrToUnicode(PStr(PtrOf(E^.Data[I * 2]))) <> 'then' then
    begin
      ArrayPush(ObjOf(RootAt(Base + 2)), E^.Data[I * 2]);
      E := ObjOf(ObjOf(RootAt(Base))^.Aux1)^.Elements;
      ArrayPush(ObjOf(RootAt(Base + 2)), E^.Data[I * 2 + 1]);
      E := ObjOf(ObjOf(RootAt(Base))^.Aux1)^.Elements;
    end;
    Inc(I);
  end;
  O := ObjOf(RootAt(Base + 1));
  O^.Aux1 := RootAt(Base + 2);
  O^.Aux2 := MkInt(M.Id);
  O^.OFlags := O^.OFlags or 8 or 16;
  O^.Shape := ShapeFork(O^.Shape);
  SetRoot(M, rDNS, RootAt(Base + 1));
  Result := RootAt(Base + 1);
  PopRoots(3);
end;

function CellOf(const R: TResolution): TValue;
begin
  if R.Kind = rkNamespace then Exit(ClosedCell(GetNamespace(R.Module)));
  if R.Kind = rkDeferNS then Exit(ClosedCell(GetDeferredNamespace(R.Module)));
  Result := ObjOf(GetRoot(R.Module, rTable))^.Elements^.Data[R.Slot];
end;

{ ---- 16.2.1.5.2 Link ---- }

procedure CollectGraph(M: TModRec; L: TList<TModRec>);
var D: TModRec;
begin
  if (M.Status <> msNew) or (L.IndexOf(M) >= 0) then Exit;
  M.Status := msLinking;
  L.Add(M);
  for D in M.Deps do CollectGraph(D, L);
end;

function LinkFail(L: TList<TModRec>): Boolean;
var M: TModRec;
begin
  for M in L do M.Status := msNew;
  Result := False;
end;

function Link(Root: TModRec): Boolean;
var L: TList<TModRec>; M: TModRec; I, J: Integer; Clo, Gen, Tbl: TValue; R: TResolution; D: TModRec;
begin
  L := TList<TModRec>.Create;
  try
    CollectGraph(Root, L);
    { every module's InitializeEnvironment start: its table, then its function run to its first suspension }
    for M in L do
    begin
      Tbl := MkPtr(NewArray(M.Info.SlotCount, M.Info.SlotCount));
      SetRoot(M, rTable, Tbl);
      if not MakeScriptClosure(M.Tpl, Clo) then Exit(LinkFail(L));
      PushRoot(Clo);
      Tbl := GetRoot(M, rTable);
      if not CallValue(RootAt(TempTop - 1), VUndefined, @Tbl, 1, Gen) then begin PopRoots(1); Exit(LinkFail(L)); end;
      PopRoots(1);
      SetRoot(M, rGen, Gen);
    end;
    { the imports and indirect exports resolved }
    for M in L do
    begin
      for I := 0 to High(M.Info.IndirectExports) do
      begin
        if (M.Info.IndirectExports[I].ImportName = ModNamespace) or
          (M.Info.IndirectExports[I].ImportName = ModDeferNamespace) then Continue;
        R := Resolve(DepOf(M, M.Info.IndirectExports[I].Request), M.Info.IndirectExports[I].ImportName);
        if not (R.Kind in [rkBinding, rkNamespace, rkDeferNS]) then
        begin
          ThrowError('SyntaxError', 'The requested module ''' + M.Info.IndirectExports[I].Request +
            ''' does not provide an export named ''' + M.Info.IndirectExports[I].ImportName + '''');
          Exit(LinkFail(L));
        end;
      end;
      for I := 0 to High(M.Info.Imports) do
      begin
        D := DepOf(M, M.Info.Imports[I].Request);
        if M.Info.Imports[I].ImportName = ModDeferNamespace then
        begin
          Tbl := ClosedCell(GetDeferredNamespace(D));
          ValueArrayStore(ObjOf(GetRoot(M, rTable))^.Elements, M.Info.Imports[I].Slot, Tbl);
          Continue;
        end;
        if M.Info.Imports[I].ImportName = ModNamespace then
        begin
          R.Kind := rkNamespace; R.Module := D;
        end
        else
        begin
          R := Resolve(D, M.Info.Imports[I].ImportName);
          if not (R.Kind in [rkBinding, rkNamespace, rkDeferNS]) then
          begin
            if R.Kind = rkAmbiguous then
              ThrowError('SyntaxError', 'The requested module ''' + M.Info.Imports[I].Request +
                ''' contains conflicting star exports for name ''' + M.Info.Imports[I].ImportName + '''')
            else
              ThrowError('SyntaxError', 'The requested module ''' + M.Info.Imports[I].Request +
                ''' does not provide an export named ''' + M.Info.Imports[I].ImportName + '''');
            Exit(LinkFail(L));
          end;
        end;
        J := M.Info.Imports[I].Slot;
        Tbl := CellOf(R);
        ValueArrayStore(ObjOf(GetRoot(M, rTable))^.Elements, J, Tbl);
      end;
    end;
    for M in L do M.Status := msLinked;
    Result := True;
  finally
    L.Free;
  end;
end;

{ ---- 16.2.1.5.3 Evaluate (depth first; a cycle's member already evaluating is skipped) ---- }

{ ---- 16.2.1.6.1.3 Evaluate, with the import defer proposal's changes (GatherAsynchronousTransitiveDependencies,
  ReadyForSyncExecution, EvaluateModuleSync) ---- }

function IsCyclic(M: TModRec): Boolean; inline;
begin
  Result := M.Tpl <> nil;   { a typed module (LoadTypedModule) is a Synthetic Module Record }
end;

function HasCap(M: TModRec): Boolean; inline;
begin
  Result := IsObject(GetRoot(M, rCap));
end;

procedure SetEvalError(M: TModRec; E: TValue);
begin
  M.Errored := True;
  SetRoot(M, rError, E);
end;

{ IsModuleSCCEvaluated }
function SCCEvaluated(M: TModRec): Boolean;
begin
  if M.CycleRoot <> nil then Exit(M.CycleRoot.Status = msEvaluated);
  Result := M.Status = msEvaluated;
end;

{ GatherAsynchronousTransitiveDependencies: the modules with top-level await a deferred module's evaluation will
  need, gathered depth first and stopping at each }
procedure GatherAsyncDeps(M: TModRec; Seen, Res: TList<TModRec>);
var I: Integer;
begin
  if Seen.IndexOf(M) >= 0 then Exit;
  Seen.Add(M);
  if not IsCyclic(M) then Exit;
  if (M.Status = msEvaluating) or SCCEvaluated(M) then Exit;
  if M.Info.HasTLA then
  begin
    if Res.IndexOf(M) < 0 then Res.Add(M);
    Exit;
  end;
  for I := 0 to High(M.Deps) do GatherAsyncDeps(M.Deps[I], Seen, Res);
end;

function AsyncDepsOf(M: TModRec): TList<TModRec>;
var Seen: TList<TModRec>;
begin
  Result := TList<TModRec>.Create;
  Seen := TList<TModRec>.Create;
  try
    GatherAsyncDeps(M, Seen, Result);
  finally
    Seen.Free;
  end;
end;

{ ReadyForSyncExecution }
function ReadyForSync(M: TModRec; Seen: TList<TModRec>): Boolean;
var I: Integer;
begin
  Result := True;
  if not IsCyclic(M) then Exit;
  if Seen.IndexOf(M) >= 0 then Exit;
  Seen.Add(M);
  if SCCEvaluated(M) then Exit;
  if M.Status in [msEvaluating, msEvaluatingAsync] then Exit(False);
  if M.Info.HasTLA then Exit(False);
  for I := 0 to High(M.Deps) do
    if not ReadyForSync(M.Deps[I], Seen) then Exit(False);
end;

{ ExecuteModule of a module without top-level await: its function run to the end }
function ExecuteSync(M: TModRec): Boolean;
var V: TValue; Done: Boolean;
begin
  Result := GeneratorResume(GetRoot(M, rGen), VUndefined, 0, V, Done);
end;

procedure ExecuteAsyncModule(M: TModRec); forward;

{ AsyncModuleExecutionRejected }
procedure AsyncRejected(M: TModRec; E: TValue);
var I: Integer;
begin
  if M.Status = msEvaluated then Exit;
  PushRoot(E);
  SetEvalError(M, RootAt(TempTop - 1));
  M.Status := msEvaluated;
  M.AsyncOrder := -1;
  if HasCap(M) then RejectPendingPromise(GetRoot(M, rCap), RootAt(TempTop - 1));
  for I := 0 to High(M.AsyncParents) do AsyncRejected(M.AsyncParents[I], RootAt(TempTop - 1));
  PopRoots(1);
end;

{ GatherAvailableAncestors }
procedure GatherAncestors(M: TModRec; L: TList<TModRec>);
var I: Integer; P, Root: TModRec;
begin
  for I := 0 to High(M.AsyncParents) do
  begin
    P := M.AsyncParents[I];
    Root := P.CycleRoot;
    if Root = nil then Root := P;
    if (L.IndexOf(P) >= 0) or Root.Errored then Continue;
    Dec(P.PendingAsync);
    if P.PendingAsync = 0 then
    begin
      L.Add(P);
      if not P.Info.HasTLA then GatherAncestors(P, L);
    end;
  end;
end;

{ AsyncModuleExecutionFulfilled }
procedure AsyncFulfilled(M: TModRec);
var L: TList<TModRec>; I, J: Integer; P: TModRec; E: TValue;
begin
  if M.Status = msEvaluated then Exit;
  M.AsyncOrder := -1;
  M.Status := msEvaluated;
  if HasCap(M) then ResolvePendingPromise(GetRoot(M, rCap), VUndefined);
  L := TList<TModRec>.Create;
  try
    GatherAncestors(M, L);
    { sorted by [[AsyncEvaluationOrder]] }
    for I := 1 to L.Count - 1 do
    begin
      P := L[I];
      J := I - 1;
      while (J >= 0) and (L[J].AsyncOrder > P.AsyncOrder) do
      begin
        L[J + 1] := L[J];
        Dec(J);
      end;
      L[J + 1] := P;
    end;
    for I := 0 to L.Count - 1 do
    begin
      P := L[I];
      if P.Status = msEvaluated then Continue;
      if P.Info.HasTLA then ExecuteAsyncModule(P)
      else if not ExecuteSync(P) then
      begin
        E := Pending;
        Pending := VUndefined;
        AsyncRejected(P, E);
      end
      else
      begin
        P.AsyncOrder := -1;
        P.Status := msEvaluated;
        if HasCap(P) then ResolvePendingPromise(GetRoot(P, rCap), VUndefined);
      end;
    end;
  finally
    L.Free;
  end;
end;

function AsyncFulfilledFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  AsyncFulfilled(Mods[NativeData]);
  Res := VUndefined;
  Result := True;
end;

function AsyncRejectedFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E: TValue;
begin
  if Argc >= 1 then E := Args[1] else E := VUndefined;
  AsyncRejected(Mods[NativeData], E);
  Res := VUndefined;
  Result := True;
end;

{ ExecuteAsyncModule: the body runs to its first await; the promise its function returns settles when it ends }
procedure ExecuteAsyncModule(M: TModRec);
var NextF, P, F: TValue;
begin
  if not GetV(GetRoot(M, rGen), AtomNext, NextF) or not CallValue(NextF, GetRoot(M, rGen), nil, 0, P) then
  begin
    P := Pending;
    Pending := VUndefined;
    PushRoot(P);
    P := NewPendingPromise;
    RejectPendingPromise(P, RootAt(TempTop - 1));
    PopRoots(1);
  end;
  PushRoot(P);
  PushRoot(NewNativeFunctionData(AsyncFulfilledFn, '', 0, M.Id));
  F := NewNativeFunctionData(AsyncRejectedFn, '', 1, M.Id);
  PushRoot(F);
  PromiseThenNative(RootAt(TempTop - 3), RootAt(TempTop - 2), RootAt(TempTop - 1), VUndefined);
  PopRoots(3);
end;

{ IncrementModuleAsyncEvaluationCount }
function NextAsyncOrder: Int64; noinline;
begin
  Inc(AsyncEvalCount);
  Result := AsyncEvalCount;
end;

procedure AddAsyncParent(D, M: TModRec);
begin
  SetLength(D.AsyncParents, Length(D.AsyncParents) + 1);
  D.AsyncParents[High(D.AsyncParents)] := M;
end;

{ InnerModuleEvaluation }
function InnerEval(M: TModRec; Stack: TList<TModRec>; var Index: Integer): Boolean;
var L, G: TList<TModRec>; I, J, ModIndex: Integer; D: TModRec; Done: Boolean;
begin
  Result := True;
  if not IsCyclic(M) then Exit;
  if M.Status in [msEvaluatingAsync, msEvaluated] then
  begin
    if M.Errored then begin Pending := GetRoot(M, rError); Exit(False); end;
    Exit;
  end;
  if M.Status = msEvaluating then Exit;
  M.Status := msEvaluating;
  ModIndex := Index;
  M.DFSAnc := Index;
  M.PendingAsync := 0;
  Inc(Index);
  L := TList<TModRec>.Create;
  try
    for I := 0 to High(M.Info.EvalOrder) do
    begin
      D := DepOf(M, M.Info.EvalOrder[I].Req);
      if not M.Info.EvalOrder[I].Eager then
      begin
        G := AsyncDepsOf(D);
        try
          for J := 0 to G.Count - 1 do
            if L.IndexOf(G[J]) < 0 then L.Add(G[J]);
        finally
          G.Free;
        end;
      end
      else if L.IndexOf(D) < 0 then L.Add(D);
    end;
    Stack.Add(M);
    for I := 0 to L.Count - 1 do
    begin
      D := L[I];
      if not InnerEval(D, Stack, Index) then Exit(False);
      if not IsCyclic(D) then Continue;
      if D.Status = msEvaluating then
      begin
        if D.DFSAnc < M.DFSAnc then M.DFSAnc := D.DFSAnc;
      end
      else
      begin
        if D.CycleRoot <> nil then D := D.CycleRoot;
        if D.Errored then begin Pending := GetRoot(D, rError); Exit(False); end;
      end;
      if D.AsyncOrder > 0 then
      begin
        Inc(M.PendingAsync);
        AddAsyncParent(D, M);
      end;
    end;
  finally
    L.Free;
  end;
  if (M.PendingAsync > 0) or M.Info.HasTLA then
  begin
    M.AsyncOrder := NextAsyncOrder;
    if M.PendingAsync = 0 then ExecuteAsyncModule(M);
  end
  else if not ExecuteSync(M) then Exit(False);
  if M.DFSAnc = ModIndex then
    repeat
      D := Stack[Stack.Count - 1];
      Stack.Delete(Stack.Count - 1);
      if D.AsyncOrder > 0 then D.Status := msEvaluatingAsync else D.Status := msEvaluated;
      Done := D = M;
      D.CycleRoot := M;
    until Done;
end;

{ Evaluate(): the promise of the module's evaluation (its cycle root's) }
function EvaluateModule(M: TModRec): TValue;
var Stack: TList<TModRec>; Index, I: Integer;
begin
  if (M.Status in [msEvaluatingAsync, msEvaluated]) and (M.CycleRoot <> nil) then M := M.CycleRoot;
  if HasCap(M) then Exit(GetRoot(M, rCap));
  SetRoot(M, rCap, NewPendingPromise);
  Stack := TList<TModRec>.Create;
  try
    Index := 0;
    if not InnerEval(M, Stack, Index) then
    begin
      PushRoot(Pending);
      Pending := VUndefined;
      for I := 0 to Stack.Count - 1 do
      begin
        Stack[I].Status := msEvaluated;
        SetEvalError(Stack[I], RootAt(TempTop - 1));
      end;
      RejectPendingPromise(GetRoot(M, rCap), RootAt(TempTop - 1));
      PopRoots(1);
    end
    else if M.Status = msEvaluated then ResolvePendingPromise(GetRoot(M, rCap), VUndefined);
  finally
    Stack.Free;
  end;
  Result := GetRoot(M, rCap);
end;

{ NSDeferHook: EnsureDeferredNamespaceEvaluation, EvaluateModuleSync }
function DeferEval(NS: TValue): Boolean;
var M: TModRec; Seen: TList<TModRec>; Ready: Boolean; P, R: TValue;
begin
  M := Mods[IntOf(ObjOf(NS)^.Aux2)];
  Seen := TList<TModRec>.Create;
  try
    Ready := ReadyForSync(M, Seen);
  finally
    Seen.Free;
  end;
  if not Ready then
    Exit(ThrowTypeError('A deferred module cannot be evaluated while it or a dependency is evaluating or has top-level await'));
  P := EvaluateModule(M);
  case PromiseStatus(P, R) of
    1: Result := True;
    2: begin Pending := R; Result := False; end;
  else
    Result := ThrowTypeError('A deferred module''s evaluation did not complete synchronously');
  end;
end;

function LoadAndLink(const Path: UnicodeString; var M: TModRec): Boolean;
begin
  Result := LoadModule(Path, M);
  if Result and (M.Status = msNew) then Result := Link(M);
end;

{ the host's entry point: the module loaded, linked and evaluated, its evaluation's jobs run until it settles }
function RunModuleFile(const Path: UnicodeString; var Phase: Integer): Boolean;
var M: TModRec; P, R: TValue;
begin
  Phase := 0;
  if not LoadAndLink(ResolvePath(Path, ''), M) then Exit(False);
  Phase := 1;
  PushRoot(EvaluateModule(M));
  if PromiseStatus(RootAt(TempTop - 1), R) = 0 then
    if not RunMicrotasks then begin PopRoots(1); Exit(False); end;
  P := RootAt(TempTop - 1);
  PopRoots(1);
  if PromiseStatus(P, R) = 2 then begin Pending := R; Exit(False); end;
  Result := True;
end;

{ ---- 13.3.10 import() ---- }

{ a dynamic import's promise settled (once): PendingImports holds [promise, module id, deps left] per import }
procedure SettleImport(I: Integer; Ok: Boolean; V: TValue);
var P, R: TValue;
begin
  P := ObjOf(PendingImports)^.Elements^.Data[I * 3];
  if not IsObject(P) or (PromiseStatus(P, R) <> 0) then Exit;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3, VUndefined);
  if Ok then ResolvePendingPromise(P, V) else RejectPendingPromise(P, V);
end;

{ fulfilledClosure: the namespace (deferred for import.defer()); Data = the import's index, bit 23 defer }
procedure ImportFulfilled(Data: Integer);
var I: Integer; M: TModRec; NS: TValue;
begin
  I := Data and $7FFFFF;
  M := Mods[IntOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 1])];
  if (Data and $800000) <> 0 then NS := GetDeferredNamespace(M) else NS := GetNamespace(M);
  SettleImport(I, True, NS);
end;

function ImportFulfilledFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
begin
  ImportFulfilled(NativeData);
  Res := VUndefined;
  Result := True;
end;

{ import.defer(): one of the asynchronous dependencies evaluated; the namespace once all are }
function ImportDepFulfilledFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I, N: Integer;
begin
  I := NativeData and $7FFFFF;
  N := IntOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 2]) - 1;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3 + 2, MkInt(N));
  if N = 0 then ImportFulfilled(NativeData);
  Res := VUndefined;
  Result := True;
end;

function ImportRejectedFn(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var E: TValue;
begin
  if Argc >= 1 then E := Args[1] else E := VUndefined;
  SettleImport(NativeData and $7FFFFF, False, E);
  Res := VUndefined;
  Result := True;
end;

procedure ThenImport(P: TValue; OnF: TNativeFn; Data: Integer);
var F: TValue;
begin
  PushRoot(P);
  PushRoot(NewNativeFunctionData(OnF, '', 0, Data));
  F := NewNativeFunctionData(ImportRejectedFn, '', 1, Data);
  PushRoot(F);
  PromiseThenNative(RootAt(TempTop - 3), RootAt(TempTop - 2), RootAt(TempTop - 1), VUndefined);
  PopRoots(3);
end;

{ the job that loads and links a dynamic import's module, then evaluates it (ContinueDynamicImport); Data = its
  index in PendingImports, bit 23 set for import.defer() }
function ImportJob(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I, J, Data: Integer; M: TModRec; Ref, Spec: UnicodeString; G: TList<TModRec>; R: TValue;
begin
  Res := VUndefined;
  Result := True;
  { NativeData read once: the loading and linking below run other natives }
  Data := NativeData;
  I := Data and $7FFFFF;
  Ref := StrToUnicode(StrOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 1]));
  Spec := StrToUnicode(StrOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 2]));
  if not LoadAndLink(ResolveRequest(Spec, Ref), M) then
  begin
    R := Pending;
    Pending := VUndefined;
    SettleImport(I, False, R);
    Exit;
  end;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3 + 1, MkInt(M.Id));
  if (Data and $800000) = 0 then
  begin
    ThenImport(EvaluateModule(M), ImportFulfilledFn, Data);
    Exit;
  end;
  { import.defer(): only the asynchronous modules of its graph evaluated now }
  G := AsyncDepsOf(M);
  try
    if G.Count = 0 then begin ImportFulfilled(Data); Exit; end;
    ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3 + 2, MkInt(G.Count));
    for J := 0 to G.Count - 1 do ThenImport(EvaluateModule(G[J]), ImportDepFulfilledFn, Data);
  finally
    G.Free;
  end;
end;

{ 13.3.10.1 EvaluateImportCall from ToString(specifier) on: errors reject the promise }
function DoImport(const Referrer: UnicodeString; ModuleId: Integer; Spec, Opts: TValue; var Res: TValue): Boolean;
var P, S, W, Keys, V, F: TValue; I, N, Idx, Ph: Integer; Ref, AType, Key: UnicodeString;
  function Reject: Boolean;
  begin
    V := Pending;
    Pending := VUndefined;
    RejectPendingPromise(RootAt(TempTop - 1), V);
    Res := RootAt(TempTop - 1);
    PopRoots(1);
    Result := True;
  end;
begin
  { the phase read before ToString(specifier) can run code with an import() of its own }
  Ph := ImportPhase;
  ImportPhase := 0;
  PushRoot(Opts);
  PushRoot(Spec);
  P := NewPendingPromise;
  Spec := RootAt(TempTop - 1); Opts := RootAt(TempTop - 2);
  PopRoots(2);
  PushRoot(P);
  PushRoot(Opts);
  if not ToStringV(Spec, S) then begin PopRoots(1); Exit(Reject); end;
  Opts := RootAt(TempTop - 1);
  PopRoots(1);
  PushRoot(S);
  AType := '';
  if Opts <> VUndefined then
  begin
    if not IsObject(Opts) then begin ThrowTypeError('The second argument of import() must be an object'); PopRoots(1); Exit(Reject); end;
    if not GetV(Opts, AtomOf('with'), W) then begin PopRoots(1); Exit(Reject); end;
    if W <> VUndefined then
    begin
      if not IsObject(W) then begin ThrowTypeError('The ''with'' option of import() must be an object'); PopRoots(1); Exit(Reject); end;
      PushRoot(W);
      Keys := OwnKeys(ObjOf(W), True);
      if OwnKeysFailed then begin OwnKeysFailed := False; PopRoots(2); Exit(Reject); end;
      PushRoot(Keys);
      N := ArrayLen(ObjOf(Keys));
      for I := 0 to N - 1 do
      begin
        if not GetV(RootAt(TempTop - 2), StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]), V) then begin PopRoots(3); Exit(Reject); end;
        if not IsString(V) then begin ThrowTypeError('import() attribute values must be strings'); PopRoots(3); Exit(Reject); end;
        Key := StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I]));
        if Key = 'type' then AType := StrToUnicode(StrOf(V));
      end;
      { AllImportAttributesSupported: the host supports only type }
      for I := 0 to N - 1 do
        if StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I])) <> 'type' then
        begin
          ThrowError('SyntaxError', 'Unsupported import attribute ''' +
            StrToUnicode(StrOf(ObjOf(RootAt(TempTop - 1))^.Elements^.Data[I])) + '''');
          PopRoots(3);
          Exit(Reject);
        end;
      PopRoots(2);
    end;
  end;
  { import.source(): GetModuleSource of a Source Text Module Record throws (it has no source representation) }
  if Ph = 1 then
  begin
    ThrowError('SyntaxError', 'A source text module has no source phase representation');
    PopRoots(1);
    Exit(Reject);
  end;
  { the loading runs as a job: import() never evaluates synchronously }
  if AType <> '' then
  begin
    S := StrFromUnicode(ModuleRequestKey(StrToUnicode(StrOf(RootAt(TempTop - 1))), AType));
    PopRoots(1);
    PushRoot(S);
  end;
  Ref := Referrer;
  Idx := ArrayLen(ObjOf(PendingImports)) div 3;
  if Ph = 2 then Idx := Idx or $800000;
  ArrayPush(ObjOf(PendingImports), RootAt(TempTop - 2));
  ArrayPush(ObjOf(PendingImports), StrFromUnicode(Ref));
  ArrayPush(ObjOf(PendingImports), RootAt(TempTop - 1));
  F := NewNativeFunctionData(ImportJob, 'import', 0, Idx);
  EnqueueCallback(F);
  PopRoots(1);
  Res := RootAt(TempTop - 1);
  PopRoots(1);
  Result := True;
end;

{ 13.3.12.1 import.meta: an object with a null prototype, made once per module }
function DoImportMeta(ModuleId: Integer; var Res: TValue): Boolean;
var M: TModRec;
begin
  if (ModuleId < 1) or (ModuleId > Mods.Count) then Exit(ThrowError('SyntaxError', 'import.meta outside a module'));
  M := Mods[ModuleId - 1];
  Res := GetRoot(M, rMeta);
  if not IsObject(Res) then
  begin
    Res := MkPtr(NewPlainObject(VNull, 4));
    SetRoot(M, rMeta, Res);
  end;
  Result := True;
end;

procedure InitModules;
begin
  NSDeferHook := @DeferEval;
  { the module map and the pending dynamic imports belong to the agent's first realm }
  if RealmInit then Exit;
  Mods := TList<TModRec>.Create;
  AddRoot(@ModRoots);
  AddRoot(@PendingImports);
  ModRoots := MkPtr(NewArray(0, 64));
  PendingImports := MkPtr(NewArray(0, 16));
  ImportHook := @DoImport;
  ImportMetaHook := @DoImportMeta;
end;

end.

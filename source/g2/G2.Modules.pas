unit G2.Modules;
{ 16.2.1 Modules: Source Text Module Records loaded from files, linked and evaluated; module namespace objects
  (10.4.6, G2.VM's okModuleNS); import() and import.meta.

  A module compiles (goal 3, G2.Compiler) to a generator over its cell table: calling it is InitializeEnvironment
  (its function declarations made, its exported bindings published as cells), resuming it is ExecuteModule.
  Imports are the exporters' cells in the importer's table. Linking runs every module's start first, then fills
  the import slots, so cycles see each other's functions. A module with top-level await is an async generator;
  its evaluation waits for it (a simplification of 16.2.1.5.3's asynchronous ordering). }
{$mode delphiunicode}{$H+}

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
  TModStatus = (msNew, msLinking, msLinked, msEvaluating, msEvaluated);
  TModRec = class
    Id: Integer;
    Path: UnicodeString;
    Info: TModuleInfo;
    Tpl: PFuncTemplate;
    Status: TModStatus;
    Errored: Boolean;
    Deps: array of TModRec;
    destructor Destroy; override;
  end;
  TResKind = (rkNull, rkAmbiguous, rkBinding, rkNamespace);
  TResolution = record
    Kind: TResKind;
    Module: TModRec;
    Slot: Integer;
  end;

const
  { the values of a module kept in ModRoots at Id * RS + k }
  RS = 5; rTable = 0; rGen = 1; rNS = 2; rMeta = 3; rError = 4;

threadvar
  Mods: TList<TModRec>;
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

function FindModule(const Path: UnicodeString): TModRec;
var M: TModRec;
begin
  for M in Mods do
    if M.Path = Path then Exit(M);
  Result := nil;
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
    if not LoadModule(ResolvePath(M.Info.Requests[I], Path), D) then Exit(False);
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
      if not (R.Kind in [rkBinding, rkNamespace]) then Continue;
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

function CellOf(const R: TResolution): TValue;
begin
  if R.Kind = rkNamespace then Exit(ClosedCell(GetNamespace(R.Module)));
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
        if M.Info.IndirectExports[I].ImportName = ModNamespace then Continue;
        R := Resolve(DepOf(M, M.Info.IndirectExports[I].Request), M.Info.IndirectExports[I].ImportName);
        if not (R.Kind in [rkBinding, rkNamespace]) then
        begin
          ThrowError('SyntaxError', 'The requested module ''' + M.Info.IndirectExports[I].Request +
            ''' does not provide an export named ''' + M.Info.IndirectExports[I].ImportName + '''');
          Exit(LinkFail(L));
        end;
      end;
      for I := 0 to High(M.Info.Imports) do
      begin
        D := DepOf(M, M.Info.Imports[I].Request);
        if M.Info.Imports[I].ImportName = ModNamespace then
        begin
          R.Kind := rkNamespace; R.Module := D;
        end
        else
        begin
          R := Resolve(D, M.Info.Imports[I].ImportName);
          if not (R.Kind in [rkBinding, rkNamespace]) then
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

function RunAsyncBody(Gen: TValue): Boolean;
var NextF, P, R: TValue; St: Integer;
begin
  PushRoot(Gen);
  if not GetV(Gen, AtomNext, NextF) then begin PopRoots(1); Exit(False); end;
  if not CallValue(NextF, RootAt(TempTop - 1), nil, 0, P) then begin PopRoots(1); Exit(False); end;
  PopRoots(1);
  PushRoot(P);
  repeat
    St := PromiseStatus(RootAt(TempTop - 1), R);
    if St <> 0 then Break;
    RunMicrotasks;
    St := PromiseStatus(RootAt(TempTop - 1), R);
  until St <> 0;
  PopRoots(1);
  if St = 2 then begin Pending := R; Exit(False); end;
  Result := True;
end;

function Evaluate(M: TModRec): Boolean;
var D: TModRec; V: TValue; Done: Boolean;
begin
  if M.Status = msEvaluated then
  begin
    if M.Errored then begin Pending := GetRoot(M, rError); Exit(False); end;
    Exit(True);
  end;
  if M.Status = msEvaluating then Exit(True);
  M.Status := msEvaluating;
  Result := True;
  for D in M.Deps do
    if not Evaluate(D) then begin Result := False; Break; end;
  if Result then
  begin
    if M.Info.HasTLA then Result := RunAsyncBody(GetRoot(M, rGen))
    else Result := GeneratorResume(GetRoot(M, rGen), VUndefined, 0, V, Done);
  end;
  M.Status := msEvaluated;
  if not Result then
  begin
    M.Errored := True;
    SetRoot(M, rError, Pending);
  end;
end;

function LoadLinkEvaluate(const Path: UnicodeString; var M: TModRec; var Phase: Integer): Boolean;
begin
  Phase := 0;
  if not LoadModule(Path, M) then Exit(False);
  if M.Status = msNew then
    if not Link(M) then Exit(False);
  Phase := 1;
  Result := Evaluate(M);
end;

function RunModuleFile(const Path: UnicodeString; var Phase: Integer): Boolean;
var M: TModRec;
begin
  Result := LoadLinkEvaluate(ResolvePath(Path, ''), M, Phase);
end;

{ ---- 13.3.10 import() ---- }

{ the job that loads, links and evaluates a dynamic import's module (Data = its index in PendingImports) }
function ImportJob(Args: PValue; Argc: Integer; var Res: TValue): Boolean;
var I: Integer; P, R, NS: TValue; M: TModRec; Phase: Integer; Ref, Spec: UnicodeString;
begin
  I := NativeData;
  P := ObjOf(PendingImports)^.Elements^.Data[I * 3];
  Ref := StrToUnicode(StrOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 1]));
  Spec := StrToUnicode(StrOf(ObjOf(PendingImports)^.Elements^.Data[I * 3 + 2]));
  if LoadLinkEvaluate(ResolvePath(Spec, Ref), M, Phase) then
  begin
    NS := GetNamespace(M);
    P := ObjOf(PendingImports)^.Elements^.Data[I * 3];
    ResolvePendingPromise(P, NS);
  end
  else
  begin
    R := Pending;
    Pending := VUndefined;
    P := ObjOf(PendingImports)^.Elements^.Data[I * 3];
    RejectPendingPromise(P, R);
  end;
  ValueArrayStore(ObjOf(PendingImports)^.Elements, I * 3, VUndefined);
  Res := VUndefined;
  Result := True;
end;

{ 13.3.10.1 EvaluateImportCall from ToString(specifier) on: errors reject the promise }
function DoImport(const Referrer: UnicodeString; ModuleId: Integer; Spec, Opts: TValue; var Res: TValue): Boolean;
var P, S, W, Keys, V, F: TValue; I, N, Idx: Integer; Ref: UnicodeString;
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
      end;
      PopRoots(2);
    end;
  end;
  { import.source(): GetModuleSource of a Source Text Module Record throws (it has no source representation) }
  if ImportPhase = 1 then
  begin
    ImportPhase := 0;
    ThrowError('SyntaxError', 'A source text module has no source phase representation');
    PopRoots(1);
    Exit(Reject);
  end;
  { the loading runs as a job: import() never evaluates synchronously }
  Ref := Referrer;
  Idx := ArrayLen(ObjOf(PendingImports)) div 3;
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
  Mods := TList<TModRec>.Create;
  AddRoot(@ModRoots);
  AddRoot(@PendingImports);
  ModRoots := MkPtr(NewArray(0, 64));
  PendingImports := MkPtr(NewArray(0, 16));
  ImportHook := @DoImport;
  ImportMetaHook := @DoImportMeta;
end;

end.

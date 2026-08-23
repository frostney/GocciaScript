{ Engine-owned store of source the engine itself parsed, for capturing a
  runtime-error code frame at the moment the error is created.

  Security model (audited; see docs/module-resolution.md "Runtime code frames"):

  PRINCIPAL / OWNERSHIP. Each engine's module loader owns exactly one scope
  (TGocciaDiagnosticSourceScope), whose durable process-monotonic `Principal` is
  its identity (a value that is never reused, unlike a freed-then-reallocated
  pointer). Every registered source is tagged guest-owned or host-owned at LOAD
  time by the caller — ownership travels with the importing module, it is not
  inferred from ambient state at capture: a virtual/host-injected module is
  host-owned, every import it initiates is transitively host-owned, and everything
  the guest imports itself is guest-owned. Entries are keyed on CANONICAL FILE
  IDENTITY supplied by the content provider from its already-open file handle, so
  a symlink, junction, hardlink, or case-variant spelling of a registered file
  resolves to the same entry instead of minting a second, differently-owned
  copy; since the host preloads before the guest runs, the host registration
  wins and a guest cannot shadow a host file with a guest-owned copy.

  A code-frame excerpt is captured onto an error ONLY from a GUEST-owned source
  in the scope ACTIVE at capture time, AND is only rendered when the context
  doing the rendering is authorized for the excerpt's stamped principal
  (Goccia.Error.Detail enforces an explicit renderer principal = the error's).
  Both gates must pass, so neither a capture-time miss nor a
  render-time cross-principal formatting (a child error printed while its parent
  is resumed) can leak host or sibling/parent source. The error still shows the
  fault's location line; only the source excerpt is withheld across a boundary.

  EXECUTING-ENGINE BINDING. Registration always targets the loader's OWN scope,
  so an engine's loads can never land in another coexisting engine's scope.
  Capture targets the ACTIVE scope, which the engine activates around its own
  execution and restores on exit — so a ShadowRealm child that stays alive while
  its parent resumes never captures into the parent's errors, and teardown out
  of creation order is safe (a scope is freed by its owning loader, never popped
  by position).

  BUDGETS. Retained source is charged against the GC's --max-memory budget
  (TryReserveExternalBytes) and bounded per scope; a source over the per-module
  cap, or one that would exceed the scope's aggregate budget, is not retained
  (the frame degrades to location-only). A captured excerpt is bounded in both
  line count and per-line width, so a minified one-line module cannot copy an
  unbounded string per error.

  Threading: the active-scope pointer is a threadvar; scopes are owned and freed
  by their loaders (per thread), so no cross-thread sharing occurs. }

unit Goccia.Diagnostics.SourceRegistry;

{$I Goccia.inc}

interface

uses
  Classes,
  Generics.Collections,

  OrderedStringMap;

const
  { Largest single source retained for code frames. Above this a module
    contributes no excerpt — bounded, safe degradation. }
  GOCCIA_DIAGNOSTIC_SOURCE_CAP_BYTES = 256 * 1024;
  { Largest total source one scope retains. Beyond it, further modules are not
    retained, so a program importing many modules cannot accumulate unbounded
    diagnostic copies. Charged against --max-memory as it grows. }
  GOCCIA_DIAGNOSTIC_SCOPE_CAP_BYTES = 4 * 1024 * 1024;
  { Bounds on a captured excerpt so one line cannot approach the source cap. }
  GOCCIA_DIAGNOSTIC_EXCERPT_MAX_LINE_BYTES = 512;

type
  TGocciaDiagnosticSourceEntry = class
  public
    Lines: TStringList;
    IsGuest: Boolean;
    Bytes: Int64;
    destructor Destroy; override;
  end;

  TGocciaDiagnosticSourceScope = class
  private
    // path -> entry (owned). One split per module at load.
    FEntries: TOrderedStringMap<TGocciaDiagnosticSourceEntry>;
    FOwned: TObjectList<TGocciaDiagnosticSourceEntry>;
    FRetainedBytes: Int64;
    FReservedBytes: Int64;
    FIdentityResolutionFailed: Boolean;
    // Durable, process-monotonic identity of this scope. Unlike a scope
    // pointer it is never reused after the scope is freed, so an excerpt
    // stamped with a scope's principal can never be mistaken for a later
    // scope that happened to be allocated at the same address.
    FPrincipal: Int64;
  protected
    { Deterministic allocation-failure seam used by the registry transaction
      test. Production leaves it empty; a test subclass raises after an index
      insertion to prove rollback removes every partial commit and charge. }
    procedure AfterIndexCommit(const AIndex: Integer); virtual;
    { Companion seam for the alias-reconciliation path (a later identified
      registration folding into an entry already keyed by path). Production
      leaves it empty; a test subclass raises after the reconciliation has
      flipped ownership flags and repointed aliases, to prove the except arm
      restores every flag and binding and removes the canonical key. }
    procedure AfterReconcileCommit; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    { Durable identity token for render-time principal enforcement. }
    property Principal: Int64 read FPrincipal;
    property RetainedBytes: Int64 read FRetainedBytes;
    { Register APath's source text for later code-frame capture. AIsHost tags
      the source host-owned (never handed to a guest in a frame) vs guest-owned.
      Ownership is decided by the CALLER at load time (a virtual/host-injected
      module or an import inheriting a host-owned importer), never inferred from
      ambient state at capture. Keyed on canonical file identity so aliases of
      an already-registered file resolve to the same entry rather than minting a
      second, differently-owned copy. }
    procedure Register(const APath, AText: string; const AIsHost: Boolean;
      const ACanonicalIdentity: string = '';
      const AIdentityRequired: Boolean = False);
    { Copies the [ALine-ABefore .. ALine+AAfter] window of APath's source into
      AWindow (cleared first), each line truncated to the per-line byte bound,
      and sets AFirstLine to the window's first absolute line number. Returns
      True only when APath is a GUEST-owned source in this scope. }
    function TryGetGuestWindow(const APath: string; const ALine, ABefore,
      AAfter: Integer; const AWindow: TStringList;
      out AFirstLine: Integer): Boolean;
  end;

  TGocciaDiagnosticSourceRegistry = class
  public
    { Make AScope the active capture target, returning the previous active
      scope; restore it with Deactivate. The engine wraps its own execution in
      this pair so capture always targets the executing engine's scope. }
    class function Activate(const AScope: TGocciaDiagnosticSourceScope):
      TGocciaDiagnosticSourceScope;
    class procedure Deactivate(
      const APrevious: TGocciaDiagnosticSourceScope);
    { The scope currently executing, or nil. Capture uses this. }
    class function Current: TGocciaDiagnosticSourceScope;
  end;

{ Bytes retained by one non-empty UTF-16 string allocation, including the
  runtime header and terminating null code unit. Empty strings have no
  allocation and retain zero bytes. }
function DiagnosticStringRetainedBytes(const AText: string): Int64;

implementation

uses
  SysUtils,

  TextSemantics,

  Goccia.GarbageCollector;

threadvar
  GActiveScope: TGocciaDiagnosticSourceScope;

var
  // Process-monotonic principal counter. Guarded by a critical section so
  // engines on separate threads never collide on a principal value. A lock
  // rather than InterLockedIncrement64 because FPC 3.2.2 only declares the
  // 64-bit interlocked helpers under CPU64, and CI also builds i386-win32; a
  // principal is minted once per module load, so the lock cost is irrelevant.
  GPrincipalCounter: Int64 = 0;
  GPrincipalLock: TRTLCriticalSection;

type
  TUnicodeStringAllocationHeader = packed record
    CodePage: Word;
    ElementSize: Word;
    ReferenceCount: LongInt;
    Length: SizeInt;
  end;

function NextPrincipal: Int64;
begin
  EnterCriticalSection(GPrincipalLock);
  try
    Inc(GPrincipalCounter);
    Result := GPrincipalCounter;
  finally
    LeaveCriticalSection(GPrincipalLock);
  end;
end;

function DiagnosticStringRetainedBytes(const AText: string): Int64;
begin
  if AText = '' then
    Exit(0);
  Result := SizeOf(TUnicodeStringAllocationHeader) +
    Int64(Length(AText) + 1) * SizeOf(Char);
end;

function SourceEntryRetainedBytes(
  const AEntry: TGocciaDiagnosticSourceEntry): Int64;
var
  I: Integer;
begin
  Result := AEntry.InstanceSize + AEntry.Lines.InstanceSize +
    Int64(AEntry.Lines.Capacity) * (SizeOf(Pointer) + SizeOf(TObject));
  for I := 0 to AEntry.Lines.Count - 1 do
    Inc(Result, DiagnosticStringRetainedBytes(AEntry.Lines[I]));
end;

{ Longest prefix whose retained UTF-16 allocation, including its string header,
  is <= AMaxBytes. A supplementary code point is never split between its two
  surrogate code units. }
function TruncateToRetainedBytes(const AText: string;
  const AMaxBytes: Integer): string;
var
  MaximumUnits: Integer;
begin
  if (AText = '') or
     (AMaxBytes <= SizeOf(TUnicodeStringAllocationHeader)) then
    Exit('');
  MaximumUnits :=
    (AMaxBytes - SizeOf(TUnicodeStringAllocationHeader)) div SizeOf(Char) - 1;
  if MaximumUnits <= 0 then
    Exit('');
  if MaximumUnits >= Length(AText) then
    Exit(AText);
  if (MaximumUnits > 0) and
     (Word(AText[MaximumUnits]) >= $D800) and
     (Word(AText[MaximumUnits]) <= $DBFF) then
    Dec(MaximumUnits);
  Result := Copy(AText, 1, MaximumUnits);
end;

{ TGocciaDiagnosticSourceEntry }

destructor TGocciaDiagnosticSourceEntry.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

{ TGocciaDiagnosticSourceScope }

constructor TGocciaDiagnosticSourceScope.Create;
begin
  inherited Create;
  FEntries := TOrderedStringMap<TGocciaDiagnosticSourceEntry>.Create;
  FOwned := TObjectList<TGocciaDiagnosticSourceEntry>.Create(True);
  FPrincipal := NextPrincipal;
end;

destructor TGocciaDiagnosticSourceScope.Destroy;
var
  GC: TGarbageCollector;
begin
  FEntries.Free;
  FOwned.Free;
  // Release the whole scope's charged bytes back to the memory budget.
  if FReservedBytes > 0 then
  begin
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.ReleaseExternalBytes(FReservedBytes);
    FReservedBytes := 0;
  end;
  inherited Destroy;
end;

procedure TGocciaDiagnosticSourceScope.Register(const APath, AText: string;
  const AIsHost: Boolean; const ACanonicalIdentity: string;
  const AIdentityRequired: Boolean);
var
  Entry: TGocciaDiagnosticSourceEntry;
  Existing: TGocciaDiagnosticSourceEntry;
  ByPath, ByExpanded, Unified: TGocciaDiagnosticSourceEntry;
  HavePath, HaveExpanded: Boolean;
  Canonical, Expanded: string;
  Size: Int64;
  GC: TGarbageCollector;
  Reserved: Boolean;
  CanonicalAdded, LiteralAdded, ExpandedAdded: Boolean;
  ReconPriorByPathGuest, ReconPriorByExpandedGuest: Boolean;
  ReconCanonicalAdded, ReconPathRepointed, ReconExpandedRepointed: Boolean;
begin
  if APath = '' then
    Exit;

  { A filesystem provider that could not identify the already-open file must not
    mint a path key. One unresolved file also disables source lookup for this
    scope: without an identity there is no safe way to prove that an earlier or
    later spelling is not an alias of the unresolved host file. }
  if AIdentityRequired and (ACanonicalIdentity = '') then
  begin
    FIdentityResolutionFailed := True;
    Exit;
  end;

  Expanded := ExpandFileName(APath);
  if ACanonicalIdentity <> '' then
    Canonical := ACanonicalIdentity
  else
    Canonical := '#path:' + Expanded;
  // One registration per canonical identity: a later load under an alias finds
  // this entry and cannot mint a guest-owned copy. Ownership is monotonic: a
  // host enrollment upgrades an existing guest entry to host-owned, while a
  // guest load can never downgrade one.
  if FEntries.TryGetValue(Canonical, Existing) then
  begin
    if AIsHost then
      Existing.IsGuest := False;
    Exit;
  end;

  // Reconcile a pre-existing entry reached through the path spellings. When a
  // file is first registered without a canonical identity, its entry is keyed
  // under '#path:'+Expanded plus the literal and expanded paths; a later
  // identified registration under '#id:' has a different canonical key, so the
  // TryGetValue(Canonical) miss above would otherwise mint a SECOND entry and
  // leave those path aliases still pointing at the first. A host upgrade on the
  // new '#id:' entry would then never reach the alias, so TryGetGuestWindow via
  // the path spelling would keep returning the guest-owned entry — a host-source
  // downgrade. Fold this registration into the existing entry instead.
  //
  // The literal and expanded spellings can already resolve to DIFFERENT entries
  // — e.g. a relative literal keyed while the working directory was one path and
  // the absolute expansion produced (or keyed) after a cwd change, so APath
  // still names an old entry while Expanded names another. This registration
  // asserts that APath expands to Expanded and both name the file identified by
  // Canonical, so they are one open file: collapse every entry reached through
  // either spelling onto a single owned entry, upgrade ownership on ALL of them
  // when host (host wins, never downgrades), and repoint both spellings plus the
  // canonical key at that one entry. Upgrading only the first spelling would
  // leave the other guest-owned, and since TryGetGuestWindow checks the literal
  // spelling first a stale literal alias could then disclose host source.
  HavePath := FEntries.TryGetValue(APath, ByPath);
  HaveExpanded := FEntries.TryGetValue(Expanded, ByExpanded);
  if HavePath or HaveExpanded then
  begin
    if HavePath then
      Unified := ByPath
    else
      Unified := ByExpanded;

    { Transactional reconciliation. This block both flips ownership flags
      (host-wins upgrades) and repoints alias bindings onto the one owned entry;
      if any step raised partway, a previously guest-owned window could be left
      permanently suppressed (a half-applied host upgrade) or an alias left
      half-repointed. Two guarantees keep it atomic:

      1. The canonical key is inserted FIRST. It is the only NEW key here (the
         TryGetValue(Canonical) miss above proved it absent), so it is the only
         operation that can allocate and therefore the only one that can raise
         under memory pressure. The literal/expanded repoints below target keys
         that already exist (HavePath/HaveExpanded), so they set a slot in place
         and never allocate; the IsGuest writes are plain field stores. Doing the
         one allocating step before any mutation means a failure here leaves the
         scope exactly as it was found.

      2. The except arm still restores every ownership flag and alias binding
         this block touches, and removes the canonical key if it was added, so
         even a future mutation that unexpectedly allocated could not leave a
         partial host upgrade or a stale alias behind. Unified is always ByPath
         or ByExpanded, so restoring those two flags also restores Unified's. }
    ReconPriorByPathGuest := False;
    ReconPriorByExpandedGuest := False;
    if HavePath then
      ReconPriorByPathGuest := ByPath.IsGuest;
    if HaveExpanded then
      ReconPriorByExpandedGuest := ByExpanded.IsGuest;
    ReconCanonicalAdded := False;
    ReconPathRepointed := False;
    ReconExpandedRepointed := False;
    try
      if not FEntries.ContainsKey(Canonical) then
      begin
        FEntries.AddOrSetValue(Canonical, Unified);
        ReconCanonicalAdded := True;
      end;
      if AIsHost then
      begin
        Unified.IsGuest := False;
        if HavePath then
          ByPath.IsGuest := False;
        if HaveExpanded then
          ByExpanded.IsGuest := False;
      end;
      if HavePath and (ByPath <> Unified) then
      begin
        FEntries.AddOrSetValue(APath, Unified);
        ReconPathRepointed := True;
      end;
      if HaveExpanded and (ByExpanded <> Unified) then
      begin
        FEntries.AddOrSetValue(Expanded, Unified);
        ReconExpandedRepointed := True;
      end;
      AfterReconcileCommit;
    except
      { Restore in reverse: repoints target still-present keys and the flag
        writes are field stores, so none of these can raise. }
      if ReconExpandedRepointed then
        FEntries.AddOrSetValue(Expanded, ByExpanded);
      if ReconPathRepointed then
        FEntries.AddOrSetValue(APath, ByPath);
      if HaveExpanded then
        ByExpanded.IsGuest := ReconPriorByExpandedGuest;
      if HavePath then
        ByPath.IsGuest := ReconPriorByPathGuest;
      if ReconCanonicalAdded then
        FEntries.Remove(Canonical);
      raise;
    end;
    Exit;
  end;

  // Build the complete retained representation before checking either cap.
  // Many short lines retain separate UTF-16 strings and pointer slots, so the
  // content's code-unit or UTF-8 length is not the amount this registry holds.
  Entry := TGocciaDiagnosticSourceEntry.Create;
  try
    Entry.Lines := CreateECMAScriptSourceLines(AText);
  except
    Entry.Free;
    raise;
  end;
  Entry.IsGuest := not AIsHost;
  Size := SourceEntryRetainedBytes(Entry);
  Entry.Bytes := Size;
  if (Size > GOCCIA_DIAGNOSTIC_SOURCE_CAP_BYTES) or
     (FRetainedBytes + Size > GOCCIA_DIAGNOSTIC_SCOPE_CAP_BYTES) then
  begin
    Entry.Free;
    Exit;
  end;

  GC := TGarbageCollector.Instance;
  Reserved := False;
  if Assigned(GC) then
  begin
    if not GC.TryReserveExternalBytes(Size) then
    begin
      Entry.Free;
      Exit;
    end;
    Reserved := True;
  end;

  CanonicalAdded := False;
  LiteralAdded := False;
  ExpandedAdded := False;
  try
    // Key under the canonical identity plus the literal and expanded spellings,
    // so a capture lookup resolves whether the frame path arrives canonical,
    // relative, or absolute. All aliases point at the one owned entry (FOwned
    // holds it once); ownership is fixed on the first (host-wins) registration.
    FEntries.Add(Canonical, Entry);
    CanonicalAdded := True;
    AfterIndexCommit(1);
    if (APath <> Canonical) and (not FEntries.ContainsKey(APath)) then
    begin
      FEntries.AddOrSetValue(APath, Entry);
      LiteralAdded := True;
      AfterIndexCommit(2);
    end;
    if (Expanded <> Canonical) and (Expanded <> APath) and
       (not FEntries.ContainsKey(Expanded)) then
    begin
      FEntries.AddOrSetValue(Expanded, Entry);
      ExpandedAdded := True;
      AfterIndexCommit(3);
    end;

    { Ownership transfers only after every index exists. FOwned.Add is the final
      allocating operation; on failure the indexes and reservation roll back and
      Entry remains owned by this frame. }
    FOwned.Add(Entry);
    Inc(FRetainedBytes, Size);
    if Reserved then
      Inc(FReservedBytes, Size);
    Entry := nil;
  except
    if ExpandedAdded then
      FEntries.Remove(Expanded);
    if LiteralAdded then
      FEntries.Remove(APath);
    if CanonicalAdded then
      FEntries.Remove(Canonical);
    if Reserved then
      GC.ReleaseExternalBytes(Size);
    Entry.Free;
    raise;
  end;
end;

procedure TGocciaDiagnosticSourceScope.AfterIndexCommit(const AIndex: Integer);
begin
end;

procedure TGocciaDiagnosticSourceScope.AfterReconcileCommit;
begin
end;

function TGocciaDiagnosticSourceScope.TryGetGuestWindow(const APath: string;
  const ALine, ABefore, AAfter: Integer; const AWindow: TStringList;
  out AFirstLine: Integer): Boolean;
var
  Entry: TGocciaDiagnosticSourceEntry;
  Expanded: string;
  FirstLine, LastLine, I: Integer;
begin
  AWindow.Clear;
  AFirstLine := 0;
  Result := False;
  if (APath = '') or (ALine <= 0) then
    Exit;
  if FIdentityResolutionFailed then
    Exit;
  // Resolve by literal spelling first (the common case; the frame path matches
  // how the module was registered), then by canonical identity so a symlinked or
  // case-variant frame path still finds the one owned entry.
  if not FEntries.TryGetValue(APath, Entry) then
  begin
    Expanded := ExpandFileName(APath);
    if (Expanded = APath) or (not FEntries.TryGetValue(Expanded, Entry)) then
      Exit;
  end;
  // Cross-principal exclusion: host-owned source is never handed to a guest.
  if not Entry.IsGuest then
    Exit;
  if ALine > Entry.Lines.Count then
    Exit;
  FirstLine := ALine - ABefore;
  if FirstLine < 1 then
    FirstLine := 1;
  LastLine := ALine + AAfter;
  if LastLine > Entry.Lines.Count then
    LastLine := Entry.Lines.Count;
  for I := FirstLine to LastLine do
    // Per-line width bound: a minified/one-line module cannot copy a huge string
    // into the excerpt (and cannot disclose a whole line at once). Bounded on
    // actual retained UTF-16 bytes, including the per-string header, and cut on
    // a code-point boundary so truncation never splits a surrogate pair.
    AWindow.Add(TruncateToRetainedBytes(Entry.Lines[I - 1],
      GOCCIA_DIAGNOSTIC_EXCERPT_MAX_LINE_BYTES));
  AFirstLine := FirstLine;
  Result := True;
end;

{ TGocciaDiagnosticSourceRegistry }

class function TGocciaDiagnosticSourceRegistry.Activate(
  const AScope: TGocciaDiagnosticSourceScope): TGocciaDiagnosticSourceScope;
begin
  Result := GActiveScope;
  GActiveScope := AScope;
end;

class procedure TGocciaDiagnosticSourceRegistry.Deactivate(
  const APrevious: TGocciaDiagnosticSourceScope);
begin
  GActiveScope := APrevious;
end;

class function TGocciaDiagnosticSourceRegistry.Current:
  TGocciaDiagnosticSourceScope;
begin
  Result := GActiveScope;
end;

initialization
  InitCriticalSection(GPrincipalLock);

finalization
  DoneCriticalSection(GPrincipalLock);

end.

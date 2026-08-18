unit Goccia.Values.HoleValue;

{$I Goccia.inc}

interface

uses
  Goccia.GarbageCollector,
  Goccia.Values.Primitives;

type
  TGocciaHoleValue = class(TGocciaValue)
  private
    class var FHoleValue: TGocciaHoleValue;
  public
    class function HoleValue: TGocciaHoleValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
  end;

  // An array value's dense element storage, and the growth gate over it.
  //
  // The element-side twin of TGocciaShapedPropertyMap, deliberately built to
  // the same three-part shape: the gate lives on the storage container rather
  // than at the store entry points, the container carries a backref to the
  // value that owns it, and the budget decision sits behind its own virtual so
  // the rooted window and the decision taken inside it can be reasoned about —
  // and exercised — apart.
  //
  // A distinct list class rather than a plain TGocciaValueList is what makes
  // the coverage compile-time: ExtendElementsWithHoles takes this type, so the
  // only lists that can reach the gate are ones that carry an Owner to root.
  TGocciaElementList = class(TGocciaValueList)
  private
    FOwner: TGCManagedObject;
  protected
    // Growth gate: makes the growth point safe to collect at, and only then
    // consults the budget. Two things are live across that window and
    // reachable from nowhere else — see the implementation.
    //
    // Not published beyond this unit: the gate is reached from
    // ExtendElementsWithHoles and from nowhere else, which is what keeps every
    // extension path covered by construction.
    procedure RequireStorageBytes(const ABytes: Int64;
      const APendingValue: TGocciaValue);
    // The budget decision, taken inside the window RequireStorageBytes has
    // just opened. Virtual for the same reason
    // TGocciaShapedPropertyMap.ConsultStorageBudget is: a gate that collects
    // before refusing collects HERE, with the store's values already rooted.
    procedure ConsultStorageBudget(const ABytes: Int64); virtual;
  public
    // The value this list holds elements for. Rooted for the duration of a
    // gated extension so the elements already stored stay reachable across a
    // collection taken there — they are reachable only through the owner's
    // MarkReferences, which is exactly what the collection would consult.
    // nil for a standalone list with no owning value, which roots nothing.
    property Owner: TGCManagedObject read FOwner write FOwner;
  end;

// Dense hole-fill: extend AElements with holes until it holds ACount
// entries.  A single huge index (e.g. `x[2 ** 24] = v`) fills millions of
// slots in one native call, so the loop mask-polls the cooperative timeout
// to keep the engine deadline reachable.  If the deadline (or any other
// exception) fires mid-fill, the list is truncated back to its pre-fill
// count before re-raising, so callers never observe a partially extended
// list — extension stays atomic with respect to JS-visible state.
//
// APendingValue is the value the in-flight store has not written into the list
// yet, or nil where the caller carries none. It is passed rather than rooted
// by the caller because the gate is the one point in an element store that can
// collect, and it is consulted only when the list actually has to grow — so
// the cost stays on the extension path instead of on every dense write. Pass
// nil only where nothing reads a store value after the call; the callers
// document which case each of them is.
procedure ExtendElementsWithHoles(const AElements: TGocciaElementList;
  const ACount: Int64; const APendingValue: TGocciaValue);

implementation

uses
  Goccia.Constants.TypeNames,
  Goccia.MemoryLimit,
  Goccia.Timeout;

procedure TGocciaElementList.RequireStorageBytes(const ABytes: Int64;
  const APendingValue: TGocciaValue);
var
  Roots: TGocciaActiveRootFrame;
begin
  { Two things are live here and reachable from nowhere the collector can see.
    The owning array holds this list, and a native builder may be the only
    thing holding the array — `new Array(n)` runs
    InitializeNativeFromArguments on an instance that no scope, register or
    argument collection has been handed yet, so a collection taken here would
    sweep the array out from under its own constructor and free this list with
    it. Rooting the owner covers the elements already stored too: they are
    reachable through TGocciaArrayValue.MarkReferences and through nothing
    else.

    APendingValue is the second: the store that called this has not written it
    into the list yet, so it is held only in a Pascal local, or in a property
    descriptor — a plain class the collector does not trace. Callers that read
    a value back after the extension (the DefineProperty family reads the
    descriptor's value into the freshly made slot) depend on this push; reading
    it before the gate instead would not help, because the hazard is the
    object's lifetime, not when the field is loaded.

    A frame here costs nothing on the dense-write path: an append at exactly
    the element count never reaches this unit, and this method is reached only
    when the list actually has to grow.

    The frame closes when this method returns — BEFORE the hole fill and the
    caller's post-gate store. That is safe under a precise invariant: this
    gate is the only prospective collection point in the extension window,
    because allocation in this engine registers but never collects
    (TGocciaValue.AfterConstruction). If that ever changes — a collecting
    allocator, or hole construction that can collect — the window must widen
    to cover the whole extension, and the H4 layer re-audits exactly this. }
  Roots.Initialize;
  try
    Roots.Add(FOwner);
    Roots.Add(APendingValue);
    ConsultStorageBudget(ABytes);
  finally
    Roots.Clear;
  end;
end;

procedure TGocciaElementList.ConsultStorageBudget(const ABytes: Int64);
begin
  RequireNativeBytes(ABytes);
end;

procedure ExtendElementsWithHoles(const AElements: TGocciaElementList;
  const ACount: Int64; const APendingValue: TGocciaValue);
var
  StartCount: Integer;
begin
  StartCount := AElements.Count;
  { Gate the whole extension up front.
    ACount comes from a JS-controlled length, so `arr.length = 1e9` asks for
    ~8 GB of pointer storage here. Growing element by element would sail past
    the budget between GC samples and only be noticed once the process had
    already committed the memory — the budget must refuse the request before
    the first slot is allocated, not after the last.
    A gate rather than a charge: this storage belongs to the list, which has
    no hook to release a reservation when it shrinks or is freed. }
  if ACount > StartCount then
    AElements.RequireStorageBytes((ACount - StartCount) * SizeOf(Pointer),
      APendingValue);
  // Fast path: small extensions skip both the poll and the rollback frame.
  // Sequential element writes extend by one slot at a time, and an FPC
  // try/except frame per append is a measurable tax on every array-building
  // loop; a bounded extension cannot stall, so neither is needed.
  if ACount - StartCount <= 1024 then
  begin
    while AElements.Count < ACount do
      AElements.Add(TGocciaHoleValue.HoleValue);
    Exit;
  end;
  try
    while AElements.Count < ACount do
    begin
      AElements.Add(TGocciaHoleValue.HoleValue);
      if (AElements.Count and 1023) = 0 then
        CheckExecutionTimeout;
    end;
  except
    AElements.Count := StartCount;
    raise;
  end;
end;

class function TGocciaHoleValue.HoleValue: TGocciaHoleValue;
begin
  if not Assigned(FHoleValue) then
    FHoleValue := TGocciaHoleValue.Create;
  Result := FHoleValue;
end;

function TGocciaHoleValue.IsPrimitive: Boolean;
begin
  Result := False;
end;

function TGocciaHoleValue.TypeName: string;
begin
  Result := 'hole';
end;

function TGocciaHoleValue.TypeOf: string;
begin
  Result := UNDEFINED_TYPE_NAME;
end;

function TGocciaHoleValue.RuntimeCopy: TGocciaValue;
begin
  Result := HoleValue;
end;

function TGocciaHoleValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaHoleValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaHoleValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('');
end;

end.

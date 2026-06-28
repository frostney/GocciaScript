unit LazyPublishedCache;

{$I Shared.inc}

interface

type
  { Lazy one-shot load plus barrier-correct lock-free publication of an
    immutable value of any type T.

    The engine caches several immutable embedded-data tables (Unicode
    property ranges, case-fold and uppercase pairs, IANA time-zone and CLDR
    resource blobs, available-locale lists) that are loaded once and then read
    concurrently from a worker-thread pool. Each cache wants the same shape: a
    cold one-shot load under mutual exclusion, then a warm read path that never
    enters the critical section.

    The Loaded flag is published LAST. The cold path writes Data and Available
    first, issues a WriteBarrier, then sets Loaded := True; the warm path reads
    Loaded and, on a hit, issues a matching ReadBarrier before reading
    Available/Data. A reader that observes Loaded = True therefore also observes
    the fully written immutable value on weakly-ordered targets (e.g. AArch64,
    where FPC lowers the barriers to dmb ishld/ishst); on strongly-ordered
    targets (x86 TSO) the barriers still act as compiler barriers. This is the
    double-checked publication idiom introduced for the RegExp case-fold tables
    in #813, generalized so every cache shares one barrier-correct
    implementation instead of re-asserting the ordering per call site.

    Load failure is memoized as well (Loaded = True, Available = False), so an
    absent or corrupt resource is not re-attempted on every call. Bundling
    Data, the flags and the Lock into one record makes a mismatched
    (data, flag, lock) pairing a compile-time error rather than a latent
    convention. Callers that read hot tables in place pass Data as a const
    argument (TLazyPublishedCache.Data), which makes no managed copy. }
  TLazyPublishedCache<T> = record
  public type
    { Cold-load callback. Receives the entry/property/resource key the cache
      was created for, fills AData, and returns whether the load produced a
      usable value. Invoked at most once, under the cache lock. }
    TLoader = function(const AKey: string; out AData: T): Boolean;
  var
    Data: T;
    Loaded: Boolean;
    Available: Boolean;
    Lock: TRTLCriticalSection;
    { Prepare the lock. Call once from the owning unit's initialization. }
    procedure Init;
    { Release the lock. Call once from the owning unit's finalization. }
    procedure Done;
    { Ensure the value is loaded and published, then return whether it is
      available. The warm path takes no lock. }
    function Ensure(const AKey: string; const ALoader: TLoader): Boolean;
  end;

implementation

procedure TLazyPublishedCache<T>.Init;
begin
  InitCriticalSection(Lock);
  Loaded := False;
  Available := False;
end;

procedure TLazyPublishedCache<T>.Done;
begin
  DoneCriticalSection(Lock);
end;

function TLazyPublishedCache<T>.Ensure(const AKey: string;
  const ALoader: TLoader): Boolean;
begin
  if Loaded then
  begin
    ReadBarrier;
    Result := Available;
    Exit;
  end;

  EnterCriticalSection(Lock);
  try
    if Loaded then
    begin
      Result := Available;
      Exit;
    end;

    Available := ALoader(AKey, Data);
    WriteBarrier;
    Loaded := True;
    Result := Available;
  finally
    LeaveCriticalSection(Lock);
  end;
end;

end.

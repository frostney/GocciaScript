unit Goccia.HostEnvironment;

{$I Goccia.inc}

interface

uses
  CriticalSections;

type
  IGocciaHostClock = interface
    function EpochNanoseconds: Int64;
    function MonotonicNanoseconds: Int64;
    function TimeZoneIdentifier: string;
  end;

  IGocciaHostRandom = interface;

  IGocciaHostRandom = interface
    function NextDouble: Double;
    function Fork(const AStreamId: UInt64): IGocciaHostRandom;
  end;

  TGocciaSystemHostClock = class(TInterfacedObject, IGocciaHostClock)
  public
    function EpochNanoseconds: Int64;
    function MonotonicNanoseconds: Int64;
    function TimeZoneIdentifier: string;
  end;

  TGocciaFixedHostClock = class(TInterfacedObject, IGocciaHostClock)
  private
    FEpochNanoseconds: Int64;
    FMonotonicNanoseconds: Int64;
    FTimeZoneIdentifier: string;
  public
    constructor Create(const AEpochNanoseconds,
      AMonotonicNanoseconds: Int64; const ATimeZoneIdentifier: string);

    function EpochNanoseconds: Int64;
    function MonotonicNanoseconds: Int64;
    function TimeZoneIdentifier: string;
  end;

  TGocciaSeededHostRandom = class(TInterfacedObject, IGocciaHostRandom)
  private
    FSeed: UInt64;
    FState: UInt64;
    class function Mix(const AValue: UInt64): UInt64; static;
  public
    constructor Create(const ASeed: UInt64);

    function NextDouble: Double;
    function Fork(const AStreamId: UInt64): IGocciaHostRandom;
  end;

  { Engine-owned host-controlled sources of JavaScript-observable time and
    randomness. Infrastructure timing (timeouts, profiling, and benchmarks)
    deliberately remains in TimingUtils. }
  TGocciaHostEnvironment = class
  private
    FClock: IGocciaHostClock;
    FRandom: IGocciaHostRandom;
    FHasTimeZoneOverride: Boolean;
    FNextChildStreamId: UInt64;
    FChildLock: TGocciaCriticalSection;
    FHasEpochOverride: Boolean;
    FEpochOverride: Int64;
    FHasMonotonicOverride: Boolean;
    FMonotonicOverride: Int64;
    procedure SetProviders(const AClock: IGocciaHostClock;
      const ARandom: IGocciaHostRandom;
      const AHasTimeZoneOverride: Boolean);
    procedure ForkProviders(out AClock: IGocciaHostClock;
      out ARandom: IGocciaHostRandom;
      out AHasTimeZoneOverride: Boolean);
  public
    const DeterministicEpochNanoseconds: Int64 = 0;
    const DeterministicMonotonicNanoseconds: Int64 = 0;
    const DeterministicTimeZone = 'UTC';
    const DeterministicSeed: UInt64 = 0;

    constructor Create; overload;
    constructor Create(const AClock: IGocciaHostClock;
      const ARandom: IGocciaHostRandom); overload;
    destructor Destroy; override;

    procedure Configure(const AClock: IGocciaHostClock;
      const ARandom: IGocciaHostRandom);
    procedure UseDeterministicProfile;
    procedure ConfigureAsChildOf(const AParent: TGocciaHostEnvironment);

    { A mocked clock, layered over the configured providers rather than
      replacing them. The virtual timer queue installs one while fake timers
      are on, so Date, Temporal.Now and performance all read the same simulated
      instant; RealEpochNanoseconds still reaches the provider underneath,
      which is what `vi.getRealSystemTime()` needs. Each half is independent:
      freezing the date without faking timers leaves monotonic time real.

      Not inherited by ConfigureAsChildOf — a child realm gets the real clock
      until something mocks its own. }
    procedure OverrideClock(const AHasEpoch: Boolean;
      const AEpochNanoseconds: Int64; const AHasMonotonic: Boolean;
      const AMonotonicNanoseconds: Int64);
    procedure ClearClockOverride;
    function HasClockOverride: Boolean;
    { True only while monotonic time is mocked. A mocked monotonic clock is
      measured from the mock's own origin, so a reader that subtracts a time
      origin of its own — performance.now() — has to stop doing that. }
    function HasMonotonicClockOverride: Boolean;
    function RealEpochNanoseconds: Int64;

    function EpochNanoseconds: Int64; {$IFDEF FPC}inline;{$ENDIF}
    function MonotonicNanoseconds: Int64; {$IFDEF FPC}inline;{$ENDIF}
    function TimeZoneIdentifier: string; {$IFDEF FPC}inline;{$ENDIF}
    function ResolveTimeZoneIdentifier(const AFallback: string): string;
    function RandomDouble: Double; {$IFDEF FPC}inline;{$ENDIF}
  end;

implementation

uses
  SysUtils,

  TimingUtils,

  Goccia.Temporal.TimeZone,
  Goccia.Utils;

const
  SPLITMIX64_INCREMENT = UInt64($9E3779B97F4A7C15);
  SPLITMIX64_MULTIPLIER_1 = UInt64($BF58476D1CE4E5B9);
  SPLITMIX64_MULTIPLIER_2 = UInt64($94D049BB133111EB);
  RANDOM_DOUBLE_SCALE: Double = 1.0 / 9007199254740992.0;

{ TGocciaSystemHostClock }

function TGocciaSystemHostClock.EpochNanoseconds: Int64;
begin
  Result := GetEpochNanoseconds;
end;

function TGocciaSystemHostClock.MonotonicNanoseconds: Int64;
begin
  Result := GetNanoseconds;
end;

function TGocciaSystemHostClock.TimeZoneIdentifier: string;
begin
  Result := GetSystemTimeZoneId;
end;

{ TGocciaFixedHostClock }

constructor TGocciaFixedHostClock.Create(const AEpochNanoseconds,
  AMonotonicNanoseconds: Int64; const ATimeZoneIdentifier: string);
begin
  inherited Create;
  FEpochNanoseconds := AEpochNanoseconds;
  FMonotonicNanoseconds := AMonotonicNanoseconds;
  FTimeZoneIdentifier := ATimeZoneIdentifier;
end;

function TGocciaFixedHostClock.EpochNanoseconds: Int64;
begin
  Result := FEpochNanoseconds;
end;

function TGocciaFixedHostClock.MonotonicNanoseconds: Int64;
begin
  Result := FMonotonicNanoseconds;
end;

function TGocciaFixedHostClock.TimeZoneIdentifier: string;
begin
  Result := FTimeZoneIdentifier;
end;

{ TGocciaSeededHostRandom }

constructor TGocciaSeededHostRandom.Create(const ASeed: UInt64);
begin
  inherited Create;
  FSeed := ASeed;
  FState := ASeed;
end;

{$IFDEF FPC}
  {$PUSH}
{$ENDIF}
{$OVERFLOWCHECKS OFF}
class function TGocciaSeededHostRandom.Mix(const AValue: UInt64): UInt64;
begin
  Result := AValue;
  Result := (Result xor (Result shr 30)) * SPLITMIX64_MULTIPLIER_1;
  Result := (Result xor (Result shr 27)) * SPLITMIX64_MULTIPLIER_2;
  Result := Result xor (Result shr 31);
end;

function TGocciaSeededHostRandom.NextDouble: Double;
var
  RandomBits: Int64;
begin
  FState := FState + SPLITMIX64_INCREMENT;
  RandomBits := Int64(Mix(FState) shr 11);
  Result := Int64ToDouble(RandomBits) * RANDOM_DOUBLE_SCALE;
end;

function TGocciaSeededHostRandom.Fork(
  const AStreamId: UInt64): IGocciaHostRandom;
begin
  Result := TGocciaSeededHostRandom.Create(
    Mix(FSeed xor (AStreamId * SPLITMIX64_INCREMENT)));
end;
{$IFDEF FPC}
  {$POP}
{$ELSE}
  {$IFNDEF PRODUCTION}{$OVERFLOWCHECKS ON}{$ENDIF}
{$ENDIF}

{ TGocciaHostEnvironment }

constructor TGocciaHostEnvironment.Create;
var
  Clock: IGocciaHostClock;
  Seed: UInt64;
begin
  inherited Create;
  CriticalSectionInit(FChildLock);
  Clock := TGocciaSystemHostClock.Create;
  Seed := UInt64(Clock.EpochNanoseconds) xor
    (UInt64(Clock.MonotonicNanoseconds) shl 1) xor UInt64(NativeUInt(Self));
  SetProviders(Clock, TGocciaSeededHostRandom.Create(Seed), False);
end;

constructor TGocciaHostEnvironment.Create(const AClock: IGocciaHostClock;
  const ARandom: IGocciaHostRandom);
begin
  inherited Create;
  CriticalSectionInit(FChildLock);
  Configure(AClock, ARandom);
end;

destructor TGocciaHostEnvironment.Destroy;
begin
  FRandom := nil;
  FClock := nil;
  CriticalSectionDone(FChildLock);
  inherited Destroy;
end;

procedure TGocciaHostEnvironment.Configure(const AClock: IGocciaHostClock;
  const ARandom: IGocciaHostRandom);
begin
  SetProviders(AClock, ARandom, True);
end;

procedure TGocciaHostEnvironment.SetProviders(const AClock: IGocciaHostClock;
  const ARandom: IGocciaHostRandom;
  const AHasTimeZoneOverride: Boolean);
begin
  if not Assigned(AClock) then
    raise EArgumentNilException.Create('AClock');
  if not Assigned(ARandom) then
    raise EArgumentNilException.Create('ARandom');
  FClock := AClock;
  FRandom := ARandom;
  FHasTimeZoneOverride := AHasTimeZoneOverride;
  FNextChildStreamId := 0;
end;

procedure TGocciaHostEnvironment.UseDeterministicProfile;
begin
  Configure(TGocciaFixedHostClock.Create(DeterministicEpochNanoseconds,
    DeterministicMonotonicNanoseconds, DeterministicTimeZone),
    TGocciaSeededHostRandom.Create(DeterministicSeed));
end;

procedure TGocciaHostEnvironment.ForkProviders(out AClock: IGocciaHostClock;
  out ARandom: IGocciaHostRandom;
  out AHasTimeZoneOverride: Boolean);
begin
  CriticalSectionEnter(FChildLock);
  try
    Inc(FNextChildStreamId);
    AClock := FClock;
    ARandom := FRandom.Fork(FNextChildStreamId);
    AHasTimeZoneOverride := FHasTimeZoneOverride;
  finally
    CriticalSectionLeave(FChildLock);
  end;
end;

procedure TGocciaHostEnvironment.ConfigureAsChildOf(
  const AParent: TGocciaHostEnvironment);
var
  HasTimeZoneOverride: Boolean;
  Clock: IGocciaHostClock;
  Random: IGocciaHostRandom;
begin
  if not Assigned(AParent) then
    raise EArgumentNilException.Create('AParent');
  AParent.ForkProviders(Clock, Random, HasTimeZoneOverride);
  SetProviders(Clock, Random, HasTimeZoneOverride);
end;

procedure TGocciaHostEnvironment.OverrideClock(const AHasEpoch: Boolean;
  const AEpochNanoseconds: Int64; const AHasMonotonic: Boolean;
  const AMonotonicNanoseconds: Int64);
begin
  FHasEpochOverride := AHasEpoch;
  FEpochOverride := AEpochNanoseconds;
  FHasMonotonicOverride := AHasMonotonic;
  FMonotonicOverride := AMonotonicNanoseconds;
end;

procedure TGocciaHostEnvironment.ClearClockOverride;
begin
  FHasEpochOverride := False;
  FHasMonotonicOverride := False;
end;

function TGocciaHostEnvironment.HasClockOverride: Boolean;
begin
  Result := FHasEpochOverride or FHasMonotonicOverride;
end;

function TGocciaHostEnvironment.HasMonotonicClockOverride: Boolean;
begin
  Result := FHasMonotonicOverride;
end;

function TGocciaHostEnvironment.RealEpochNanoseconds: Int64;
begin
  Result := FClock.EpochNanoseconds;
end;

function TGocciaHostEnvironment.EpochNanoseconds: Int64;
begin
  if FHasEpochOverride then
    Exit(FEpochOverride);
  Result := FClock.EpochNanoseconds;
end;

function TGocciaHostEnvironment.MonotonicNanoseconds: Int64;
begin
  if FHasMonotonicOverride then
    Exit(FMonotonicOverride);
  Result := FClock.MonotonicNanoseconds;
end;

function TGocciaHostEnvironment.TimeZoneIdentifier: string;
begin
  Result := FClock.TimeZoneIdentifier;
end;

function TGocciaHostEnvironment.ResolveTimeZoneIdentifier(
  const AFallback: string): string;
begin
  if FHasTimeZoneOverride then
    Result := FClock.TimeZoneIdentifier
  else
    Result := AFallback;
end;

function TGocciaHostEnvironment.RandomDouble: Double;
begin
  Result := FRandom.NextDouble;
end;

end.

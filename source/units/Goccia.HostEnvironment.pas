unit Goccia.HostEnvironment;

{$I Goccia.inc}

interface

type
  IGocciaHostClock = interface
    function EpochNanoseconds: Int64;
    function MonotonicNanoseconds: Int64;
    function TimeZoneIdentifier: string;
  end;

  IGocciaHostRandom = interface;

  IGocciaHostRandom = interface
    function NextDouble: Double;
    function Fork(const AStreamId: QWord): IGocciaHostRandom;
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
    FSeed: QWord;
    FState: QWord;
    class function Mix(const AValue: QWord): QWord; static;
  public
    constructor Create(const ASeed: QWord);

    function NextDouble: Double;
    function Fork(const AStreamId: QWord): IGocciaHostRandom;
  end;

  { Engine-owned host-controlled sources of JavaScript-observable time and
    randomness. Infrastructure timing (timeouts, profiling, and benchmarks)
    deliberately remains in TimingUtils. }
  TGocciaHostEnvironment = class
  private
    FClock: IGocciaHostClock;
    FRandom: IGocciaHostRandom;
    FNextChildStreamId: QWord;
    FChildLock: TRTLCriticalSection;
    procedure ForkProviders(out AClock: IGocciaHostClock;
      out ARandom: IGocciaHostRandom);
  public
    const DeterministicEpochNanoseconds: Int64 = 0;
    const DeterministicMonotonicNanoseconds: Int64 = 0;
    const DeterministicTimeZone = 'UTC';
    const DeterministicSeed: QWord = 0;

    constructor Create; overload;
    constructor Create(const AClock: IGocciaHostClock;
      const ARandom: IGocciaHostRandom); overload;
    destructor Destroy; override;

    procedure Configure(const AClock: IGocciaHostClock;
      const ARandom: IGocciaHostRandom);
    procedure UseDeterministicProfile;
    procedure ConfigureAsChildOf(const AParent: TGocciaHostEnvironment);

    function EpochNanoseconds: Int64; inline;
    function MonotonicNanoseconds: Int64; inline;
    function TimeZoneIdentifier: string; inline;
    function RandomDouble: Double; inline;
  end;

implementation

uses
  SysUtils,

  TimingUtils,

  Goccia.Temporal.TimeZone;

const
  SPLITMIX64_INCREMENT = QWord($9E3779B97F4A7C15);
  SPLITMIX64_MULTIPLIER_1 = QWord($BF58476D1CE4E5B9);
  SPLITMIX64_MULTIPLIER_2 = QWord($94D049BB133111EB);
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

constructor TGocciaSeededHostRandom.Create(const ASeed: QWord);
begin
  inherited Create;
  FSeed := ASeed;
  FState := ASeed;
end;

{$PUSH}
{$OVERFLOWCHECKS OFF}
class function TGocciaSeededHostRandom.Mix(const AValue: QWord): QWord;
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
  Result := RandomBits * RANDOM_DOUBLE_SCALE;
end;

function TGocciaSeededHostRandom.Fork(
  const AStreamId: QWord): IGocciaHostRandom;
begin
  Result := TGocciaSeededHostRandom.Create(
    Mix(FSeed xor (AStreamId * SPLITMIX64_INCREMENT)));
end;
{$POP}

{ TGocciaHostEnvironment }

constructor TGocciaHostEnvironment.Create;
var
  Clock: IGocciaHostClock;
  Seed: QWord;
begin
  inherited Create;
  InitCriticalSection(FChildLock);
  Clock := TGocciaSystemHostClock.Create;
  Seed := QWord(Clock.EpochNanoseconds) xor
    (QWord(Clock.MonotonicNanoseconds) shl 1) xor QWord(PtrUInt(Self));
  Configure(Clock, TGocciaSeededHostRandom.Create(Seed));
end;

constructor TGocciaHostEnvironment.Create(const AClock: IGocciaHostClock;
  const ARandom: IGocciaHostRandom);
begin
  inherited Create;
  InitCriticalSection(FChildLock);
  Configure(AClock, ARandom);
end;

destructor TGocciaHostEnvironment.Destroy;
begin
  FRandom := nil;
  FClock := nil;
  DoneCriticalSection(FChildLock);
  inherited Destroy;
end;

procedure TGocciaHostEnvironment.Configure(const AClock: IGocciaHostClock;
  const ARandom: IGocciaHostRandom);
begin
  if not Assigned(AClock) then
    raise EArgumentNilException.Create('AClock');
  if not Assigned(ARandom) then
    raise EArgumentNilException.Create('ARandom');
  FClock := AClock;
  FRandom := ARandom;
  FNextChildStreamId := 0;
end;

procedure TGocciaHostEnvironment.UseDeterministicProfile;
begin
  Configure(TGocciaFixedHostClock.Create(DeterministicEpochNanoseconds,
    DeterministicMonotonicNanoseconds, DeterministicTimeZone),
    TGocciaSeededHostRandom.Create(DeterministicSeed));
end;

procedure TGocciaHostEnvironment.ForkProviders(out AClock: IGocciaHostClock;
  out ARandom: IGocciaHostRandom);
begin
  EnterCriticalSection(FChildLock);
  try
    Inc(FNextChildStreamId);
    AClock := FClock;
    ARandom := FRandom.Fork(FNextChildStreamId);
  finally
    LeaveCriticalSection(FChildLock);
  end;
end;

procedure TGocciaHostEnvironment.ConfigureAsChildOf(
  const AParent: TGocciaHostEnvironment);
var
  Clock: IGocciaHostClock;
  Random: IGocciaHostRandom;
begin
  if not Assigned(AParent) then
    raise EArgumentNilException.Create('AParent');
  AParent.ForkProviders(Clock, Random);
  Configure(Clock, Random);
end;

function TGocciaHostEnvironment.EpochNanoseconds: Int64;
begin
  Result := FClock.EpochNanoseconds;
end;

function TGocciaHostEnvironment.MonotonicNanoseconds: Int64;
begin
  Result := FClock.MonotonicNanoseconds;
end;

function TGocciaHostEnvironment.TimeZoneIdentifier: string;
begin
  Result := FClock.TimeZoneIdentifier;
end;

function TGocciaHostEnvironment.RandomDouble: Double;
begin
  Result := FRandom.NextDouble;
end;

end.

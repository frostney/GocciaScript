program LazyPublishedCache.Test;

{$I Shared.inc}

uses
  SysUtils,

  LazyPublishedCache,
  TestingPascalLibrary;

type
  TIntArray = array of Integer;

var
  GBytesLoads: Integer;
  GIntLoads: Integer;
  GFailLoads: Integer;
  GLastKey: string;

function LoadBytesOk(const AKey: string; out AData: TBytes): Boolean;
begin
  Inc(GBytesLoads);
  GLastKey := AKey;
  SetLength(AData, 3);
  AData[0] := 10;
  AData[1] := 20;
  AData[2] := 30;
  Result := True;
end;

function LoadIntsOk(const AKey: string; out AData: TIntArray): Boolean;
begin
  Inc(GIntLoads);
  SetLength(AData, 2);
  AData[0] := 100;
  AData[1] := 200;
  Result := True;
end;

function LoadFails(const AKey: string; out AData: TBytes): Boolean;
begin
  Inc(GFailLoads);
  SetLength(AData, 0);
  Result := False;
end;

function LoadPartialThenFails(const AKey: string; out AData: TBytes): Boolean;
begin
  // Writes a payload into the out slot, then reports failure — mimics a
  // resource read that sizes its buffer before a failing ReadBuffer.
  SetLength(AData, 4);
  AData[0] := 1;
  Result := False;
end;

type
  TLazyPublishedCacheTests = class(TTestSuite)
  private
    procedure TestLoadsOnceAndPublishes;
    procedure TestMemoizesFailureWithoutRetrying;
    procedure TestFailedLoadDropsPartialData;
    procedure TestPassesKeyToLoader;
    procedure TestWorksForAnyPayloadType;
  public
    procedure SetupTests; override;
  end;

procedure TLazyPublishedCacheTests.SetupTests;
begin
  Test('Loads once and publishes data for warm reads', TestLoadsOnceAndPublishes);
  Test('Memoizes load failure and does not retry', TestMemoizesFailureWithoutRetrying);
  Test('Drops partial data when the loader fails', TestFailedLoadDropsPartialData);
  Test('Passes the key through to the loader', TestPassesKeyToLoader);
  Test('Works for any payload type', TestWorksForAnyPayloadType);
end;

procedure TLazyPublishedCacheTests.TestLoadsOnceAndPublishes;
var
  Cache: TLazyPublishedCache<TBytes>;
begin
  GBytesLoads := 0;
  Cache.Init;
  try
    Expect<Boolean>(Cache.Ensure('k', @LoadBytesOk)).ToBe(True);
    // Warm read: still available, loader is not invoked again.
    Expect<Boolean>(Cache.Ensure('k', @LoadBytesOk)).ToBe(True);
    Expect<Integer>(GBytesLoads).ToBe(1);
    Expect<Integer>(Length(Cache.Data)).ToBe(3);
    Expect<Integer>(Cache.Data[1]).ToBe(20);
  finally
    Cache.Done;
  end;
end;

procedure TLazyPublishedCacheTests.TestMemoizesFailureWithoutRetrying;
var
  Cache: TLazyPublishedCache<TBytes>;
begin
  GFailLoads := 0;
  Cache.Init;
  try
    Expect<Boolean>(Cache.Ensure('k', @LoadFails)).ToBe(False);
    // Failure is memoized, so the loader is not re-attempted on the next call.
    Expect<Boolean>(Cache.Ensure('k', @LoadFails)).ToBe(False);
    Expect<Integer>(GFailLoads).ToBe(1);
    Expect<Integer>(Length(Cache.Data)).ToBe(0);
  finally
    Cache.Done;
  end;
end;

procedure TLazyPublishedCacheTests.TestFailedLoadDropsPartialData;
var
  Cache: TLazyPublishedCache<TBytes>;
begin
  Cache.Init;
  try
    Expect<Boolean>(Cache.Ensure('k', @LoadPartialThenFails)).ToBe(False);
    // The payload the loader wrote into Data before failing must not survive.
    Expect<Integer>(Length(Cache.Data)).ToBe(0);
  finally
    Cache.Done;
  end;
end;

procedure TLazyPublishedCacheTests.TestPassesKeyToLoader;
var
  Cache: TLazyPublishedCache<TBytes>;
begin
  GBytesLoads := 0;
  GLastKey := '';
  Cache.Init;
  try
    Cache.Ensure('case-fold-key', @LoadBytesOk);
    Expect<string>(GLastKey).ToBe('case-fold-key');
  finally
    Cache.Done;
  end;
end;

procedure TLazyPublishedCacheTests.TestWorksForAnyPayloadType;
var
  Cache: TLazyPublishedCache<TIntArray>;
begin
  GIntLoads := 0;
  Cache.Init;
  try
    Expect<Boolean>(Cache.Ensure('k', @LoadIntsOk)).ToBe(True);
    Expect<Boolean>(Cache.Ensure('k', @LoadIntsOk)).ToBe(True);
    Expect<Integer>(GIntLoads).ToBe(1);
    Expect<Integer>(Cache.Data[0]).ToBe(100);
    Expect<Integer>(Cache.Data[1]).ToBe(200);
  finally
    Cache.Done;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TLazyPublishedCacheTests.Create('LazyPublishedCache'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.

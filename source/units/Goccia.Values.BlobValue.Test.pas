program Goccia.Values.BlobValue.Test;

{$I Goccia.inc}

{ TGocciaBlobValue's embedder API: CreateFromBytes, Bytes and BlobType, which
  script code cannot reach. Its script surface is tests/built-ins/Blob. }

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.TestSetup,
  Goccia.Values.BlobValue,
  Goccia.Values.Error;

type
  TTestBlobValue = class(TTestSuite)
  private
    FRealm, FPreviousRealm: TGocciaRealm;
    procedure EnterRealm;
    procedure LeaveRealm;
  public
    procedure SetupTests; override;

    procedure TestCreateFromBytesCopiesTheBytes;
    procedure TestCreateFromBytesNormalisesTheType;
    procedure TestCreateFromBytesReservesAndReleases;
    procedure TestCreateFromBytesRefusedPastTheLimit;
    procedure TestCreateFromBytesEmpty;
  end;

procedure TTestBlobValue.SetupTests;
begin
  Test('CreateFromBytes copies the bytes: a later change to the source is not seen',
    TestCreateFromBytesCopiesTheBytes);
  Test('CreateFromBytes lower-cases the type, and empties one with a character outside U+0020-U+007E',
    TestCreateFromBytesNormalisesTheType);
  Test('CreateFromBytes reserves the bytes, and collecting the Blob gives them back',
    TestCreateFromBytesReservesAndReleases);
  Test('CreateFromBytes past the memory limit raises a RangeError and keeps no reservation',
    TestCreateFromBytesRefusedPastTheLimit);
  Test('CreateFromBytes of no bytes is an empty Blob', TestCreateFromBytesEmpty);
end;

procedure TTestBlobValue.EnterRealm;
begin
  FPreviousRealm := CurrentRealm;
  FRealm := TGocciaRealm.Create('blob-value-test');
  SetCurrentRealm(FRealm);
end;

procedure TTestBlobValue.LeaveRealm;
begin
  SetCurrentRealm(FPreviousRealm);
  FRealm.Free;
  FRealm := nil;
end;

procedure TTestBlobValue.TestCreateFromBytesCopiesTheBytes;
var
  Source: TBytes;
  Blob: TGocciaBlobValue;
begin
  EnterRealm;
  try
    Source := TBytes.Create(1, 2, 3);
    Blob := TGocciaBlobValue.CreateFromBytes(Source, 'image/png');
    Source[0] := 9;
    Expect<Integer>(Length(Blob.Bytes)).ToBe(3);
    Expect<Integer>(Blob.Bytes[0]).ToBe(1);
    Expect<Integer>(Blob.Bytes[2]).ToBe(3);
    Expect<string>(Blob.BlobType).ToBe('image/png');
  finally
    LeaveRealm;
  end;
end;

procedure TTestBlobValue.TestCreateFromBytesNormalisesTheType;
begin
  EnterRealm;
  try
    Expect<string>(TGocciaBlobValue.CreateFromBytes(nil, 'Image/PNG').BlobType)
      .ToBe('image/png');
    Expect<string>(TGocciaBlobValue.CreateFromBytes(nil, 'text/pl'#$00E4'in').BlobType)
      .ToBe('');
    Expect<string>(TGocciaBlobValue.CreateFromBytes(nil).BlobType).ToBe('');
    Expect<string>(NormalizeBlobType('A/B;C=D')).ToBe('a/b;c=d');
  finally
    LeaveRealm;
  end;
end;

procedure TTestBlobValue.TestCreateFromBytesReservesAndReleases;
const
  SIZE = 1024 * 1024;
var
  GC: TGarbageCollector;
  Before: Int64;
  Bytes: TBytes;
  Blob: TGocciaBlobValue;
begin
  GC := TGarbageCollector.Instance;
  EnterRealm;
  try
    // The first Blob in a realm hosts the shared prototype; make it first so
    // the measurement below is of the embedder's Blob alone.
    TGocciaBlobValue.CreateFromBytes(nil);
    GC.Collect;
    Before := GC.BytesAllocated;
    SetLength(Bytes, SIZE);
    Blob := TGocciaBlobValue.CreateFromBytes(Bytes, '');
    GC.AddTempRoot(Blob);
    try
      Expect<Boolean>(GC.BytesAllocated - Before >= SIZE).ToBe(True);
    finally
      GC.RemoveTempRoot(Blob);
    end;
    GC.Collect;
    Expect<Boolean>(GC.BytesAllocated - Before < SIZE).ToBe(True);
  finally
    LeaveRealm;
  end;
end;

procedure TTestBlobValue.TestCreateFromBytesRefusedPastTheLimit;
const
  SIZE = 8 * 1024 * 1024;
var
  GC: TGarbageCollector;
  PreviousMaxBytes, Before: Int64;
  Bytes: TBytes;
  Raised: Boolean;
begin
  GC := TGarbageCollector.Instance;
  PreviousMaxBytes := GC.MaxBytes;
  EnterRealm;
  try
    TGocciaBlobValue.CreateFromBytes(nil);
    GC.Collect;
    Before := GC.BytesAllocated;
    GC.MaxBytes := Before + 1024 * 1024;
    SetLength(Bytes, SIZE);
    Raised := False;
    try
      TGocciaBlobValue.CreateFromBytes(Bytes, 'a/b');
    except
      on E: TGocciaThrowValue do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    GC.MaxBytes := PreviousMaxBytes;
    GC.Collect;
    Expect<Boolean>(GC.BytesAllocated - Before < SIZE).ToBe(True);
  finally
    GC.MaxBytes := PreviousMaxBytes;
    LeaveRealm;
  end;
end;

procedure TTestBlobValue.TestCreateFromBytesEmpty;
var
  Blob: TGocciaBlobValue;
begin
  EnterRealm;
  try
    Blob := TGocciaBlobValue.CreateFromBytes(nil, 'text/plain');
    Expect<Integer>(Length(Blob.Bytes)).ToBe(0);
    Expect<string>(Blob.BlobType).ToBe('text/plain');
  finally
    LeaveRealm;
  end;
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TTestBlobValue.Create('Blob Value'));
    RunGocciaTests;
  finally
    TGarbageCollector.Shutdown;
  end;
  ExitCode := TestResultToExitCode;
end.

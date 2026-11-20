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
  public
    procedure SetupTests; override;

    procedure TestCreateFromBytesCopiesTheBytes;
    procedure TestCreateFromBytesNormalisesTheType;
    procedure TestCreateFromBytesReservesAndReleases;
    procedure TestCreateFromBytesRefusedPastTheLimit;
    procedure TestCreateFromBytesEmpty;
    procedure TestFirstBlobOfARealmIsNotPinned;
  end;

procedure TTestBlobValue.SetupTests;
begin
  // First: it needs the suite's realm before any Blob has been made in it.
  Test('the first Blob of a realm, made by CreateFromBytes, is not kept as the prototype host',
    TestFirstBlobOfARealmIsNotPinned);
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

procedure TTestBlobValue.TestCreateFromBytesCopiesTheBytes;
var
  Source: TBytes;
  Blob: TGocciaBlobValue;
begin
  Source := TBytes.Create(1, 2, 3);
  Blob := TGocciaBlobValue.CreateFromBytes(Source, 'image/png');
  Source[0] := 9;
  Expect<Integer>(Length(Blob.Bytes)).ToBe(3);
  Expect<Integer>(Blob.Bytes[0]).ToBe(1);
  Expect<Integer>(Blob.Bytes[2]).ToBe(3);
  Expect<string>(Blob.BlobType).ToBe('image/png');
end;

procedure TTestBlobValue.TestCreateFromBytesNormalisesTheType;
begin
  Expect<string>(TGocciaBlobValue.CreateFromBytes(nil, 'Image/PNG').BlobType)
    .ToBe('image/png');
  Expect<string>(TGocciaBlobValue.CreateFromBytes(nil, 'text/pl'#$00E4'in').BlobType)
    .ToBe('');
  Expect<string>(TGocciaBlobValue.CreateFromBytes(nil).BlobType).ToBe('');
  Expect<string>(NormalizeBlobType('A/B;C=D')).ToBe('a/b;c=d');
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
  end;
end;

procedure TTestBlobValue.TestCreateFromBytesEmpty;
var
  Blob: TGocciaBlobValue;
begin
  Blob := TGocciaBlobValue.CreateFromBytes(nil, 'text/plain');
  Expect<Integer>(Length(Blob.Bytes)).ToBe(0);
  Expect<string>(Blob.BlobType).ToBe('text/plain');
end;

procedure TTestBlobValue.TestFirstBlobOfARealmIsNotPinned;
const
  SIZE = 4 * 1024 * 1024;
var
  GC: TGarbageCollector;
  Before: Int64;
  Bytes: TBytes;
begin
  GC := TGarbageCollector.Instance;
  GC.Collect;
  Before := GC.BytesAllocated;
  // No Blob exists in this realm yet: the shared prototype's method host
  // is made here, and must be an empty Blob, not this one.
  SetLength(Bytes, SIZE);
  TGocciaBlobValue.CreateFromBytes(Bytes, '');
  Bytes := nil;
  GC.Collect;
  Expect<Boolean>(GC.BytesAllocated - Before < SIZE).ToBe(True);
end;

var
  Realm: TGocciaRealm;
begin
  TGarbageCollector.Initialize;
  { One realm for the suite: a value made in a realm that has since been
    freed would reach that realm's intrinsics when a later collection marks
    it, which no engine does but separate bare realms in one process can. }
  Realm := TGocciaRealm.Create('blob-value-test');
  SetCurrentRealm(Realm);
  try
    TestRunnerProgram.AddSuite(TTestBlobValue.Create('Blob Value'));
    RunGocciaTests;
  finally
    SetCurrentRealm(nil);
    Realm.Free;
    TGarbageCollector.Shutdown;
  end;
  ExitCode := TestResultToExitCode;
end.

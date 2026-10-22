program SHA256.Test;

{$I Shared.inc}

uses
  SysUtils,

  SHA256,
  TestingPascalLibrary,
  TextEncoding;

type
  TSHA256Tests = class(TTestSuite)
  private
    function Bytes(const AText: string): TBytes;
    procedure TestEmptyInput;
    procedure TestShortNISTVector;
    procedure TestMultiBlockNISTVector;
    procedure TestMillionAs;
    procedure TestPiecesHashAsOne;
  public
    procedure SetupTests; override;
  end;

procedure TSHA256Tests.SetupTests;
begin
  Test('SHA256 hashes empty input', TestEmptyInput);
  Test('SHA256 hashes the short NIST vector', TestShortNISTVector);
  Test('SHA256 hashes the multi-block NIST vector', TestMultiBlockNISTVector);
  Test('SHA256 hashes a million a characters', TestMillionAs);
  Test('SHA256 in pieces gives the digest of the whole',
    TestPiecesHashAsOne);
end;

function TSHA256Tests.Bytes(const AText: string): TBytes;
var
  ErrorOffset: Integer;
begin
  if not TryEncodeUTF8(AText, Result, ErrorOffset) then
    raise Exception.CreateFmt('Fixture encoding failed at %d', [ErrorOffset]);
end;

procedure TSHA256Tests.TestEmptyInput;
begin
  Expect<string>(SHA256Hex(Bytes(''))).ToBe(
    'e3b0c44298fc1c149afbf4c8996fb924' +
    '27ae41e4649b934ca495991b7852b855');
end;

procedure TSHA256Tests.TestShortNISTVector;
begin
  Expect<string>(SHA256Hex(Bytes('abc'))).ToBe(
    'ba7816bf8f01cfea414140de5dae2223' +
    'b00361a396177a9cb410ff61f20015ad');
end;

procedure TSHA256Tests.TestMultiBlockNISTVector;
begin
  Expect<string>(SHA256Hex(Bytes(
    'abcdbcdecdefdefgefghfghighijhijk' +
    'ijkljklmklmnlmnomnopnopq'))).ToBe(
    '248d6a61d20638b8e5c026930c3e6039' +
    'a33ce45964ff2167f6ecedd419db06c1');
end;

procedure TSHA256Tests.TestMillionAs;
var
  Data: TBytes;
begin
  SetLength(Data, 1000000);
  FillChar(Data[0], Length(Data), Ord('a'));
  Expect<string>(SHA256Hex(Data)).ToBe(
    'cdc76e5c9914fb9281a1c7e284d73e67' +
    'f1809a48a497200e046d39ccc7112cd0');
end;

procedure TSHA256Tests.TestPiecesHashAsOne;
const
  PIECES: array[0..5] of Integer = (1, 3, 55, 63, 64, 65);
var
  Data: TBytes;
  Context: TSHA256Context;
  Len, P, At, Take: Integer;
begin
  // Every length across a few blocks, in pieces that straddle blocks and the
  // padding's boundary at 56 bytes.
  for Len := 0 to 200 do
  begin
    SetLength(Data, Len);
    for At := 0 to Len - 1 do
      Data[At] := Byte(At * 31 + Len);
    for P := Low(PIECES) to High(PIECES) do
    begin
      SHA256Init(Context);
      At := 0;
      while At < Len do
      begin
        Take := PIECES[P];
        if Take > Len - At then
          Take := Len - At;
        SHA256Update(Context, @Data[At], Take);
        Inc(At, Take);
      end;
      Expect<string>(SHA256FinalHex(Context)).ToBe(SHA256Hex(Data));
    end;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TSHA256Tests.Create('SHA256'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.

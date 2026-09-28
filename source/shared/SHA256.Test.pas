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
  public
    procedure SetupTests; override;
  end;

procedure TSHA256Tests.SetupTests;
begin
  Test('SHA256 hashes empty input', TestEmptyInput);
  Test('SHA256 hashes the short NIST vector', TestShortNISTVector);
  Test('SHA256 hashes the multi-block NIST vector', TestMultiBlockNISTVector);
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

begin
  TestRunnerProgram.AddSuite(TSHA256Tests.Create('SHA256'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.

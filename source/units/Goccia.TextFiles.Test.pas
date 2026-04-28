program Goccia.TextFiles.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.TextFiles;

type
  TTextFilesTests = class(TTestSuite)
  private
    procedure TestReadUTF8FileTextPreservesUTF8Bytes;
  public
    procedure SetupTests; override;
  end;

procedure WriteUTF8File(const APath: string; const AText: UTF8String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(APath, fmCreate);
  try
    if Length(AText) > 0 then
      Stream.WriteBuffer(Pointer(AText)^, Length(AText));
  finally
    Stream.Free;
  end;
end;

procedure TTextFilesTests.SetupTests;
begin
  Test('ReadUTF8FileText preserves UTF-8 bytes',
    TestReadUTF8FileTextPreservesUTF8Bytes);
end;

procedure TTextFilesTests.TestReadUTF8FileTextPreservesUTF8Bytes;
const
  UTF8_WORD_BYTES = 'na' + #$C3#$AF + 've';
var
  FilePath: string;
  FileText: RawByteString;
  I: Integer;
  ReadText: UTF8String;
begin
  FilePath := GetTempFileName;
  FileText := RawByteString('cafe' + #13#10 + UTF8_WORD_BYTES + #13#10);
  SetCodePage(FileText, CP_UTF8, False);
  WriteUTF8File(FilePath, UTF8String(FileText));
  try
    ReadText := ReadUTF8FileText(FilePath);
    Expect<Integer>(Length(ReadText)).ToBe(Length(FileText));
    for I := 1 to Length(ReadText) do
      Expect<Integer>(Ord(ReadText[I])).ToBe(Ord(FileText[I]));
  finally
    DeleteFile(FilePath);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTextFilesTests.Create('TextFiles'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.

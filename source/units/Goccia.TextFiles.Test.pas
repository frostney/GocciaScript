program Goccia.TextFiles.Test;

{$I Goccia.inc}

uses
  SysUtils,

  FileUtils,
  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.TextFiles;

type
  TTextFilesTests = class(TTestSuite)
  private
    procedure TestReadUTF8FileTextDecodesUnicode;
  public
    procedure SetupTests; override;
  end;

procedure TTextFilesTests.SetupTests;
begin
  Test('ReadUTF8FileText decodes Unicode text',
    TestReadUTF8FileTextDecodesUnicode);
end;

procedure TTextFilesTests.TestReadUTF8FileTextDecodesUnicode;
var
  FilePath, FileText, ReadText: string;
begin
  FilePath := GetTempFileName;
  FileText := 'cafe' + #13#10 + 'na' + #$00EF + 've' + #13#10;
  WriteUTF8FileText(FilePath, FileText);
  try
    ReadText := ReadUTF8FileText(FilePath);
    Expect<string>(ReadText).ToBe(FileText);
  finally
    DeleteFile(FilePath);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTextFilesTests.Create('TextFiles'));
  RunGocciaTests;

  ExitCode := TestResultToExitCode;
end.

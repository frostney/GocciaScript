program Goccia.TextFiles.Test;

{$I Goccia.inc}

uses
  Classes,
  SysUtils,

  TestRunner,

  Goccia.TestSetup,
  Goccia.TextFiles;

type
  TTextFilesTests = class(TTestSuite)
  private
    procedure TestCreateUTF8StringListSplitsMixedNewlines;
    procedure TestReadUTF8FileLinesCanonicalizesParserTextToLF;
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
  Test('CreateUTF8StringList splits mixed newlines',
    TestCreateUTF8StringListSplitsMixedNewlines);
  Test('ReadUTF8FileLines canonicalizes parser text to LF',
    TestReadUTF8FileLinesCanonicalizesParserTextToLF);
end;

procedure TTextFilesTests.TestCreateUTF8StringListSplitsMixedNewlines;
var
  Lines: TStringList;
begin
  Lines := CreateUTF8StringList('alpha' + #13#10 + 'beta' + #10 + 'gamma' +
    #13 + 'delta');
  try
    Expect<Integer>(Lines.Count).ToBe(4);
    Expect<string>(Lines[0]).ToBe('alpha');
    Expect<string>(Lines[1]).ToBe('beta');
    Expect<string>(Lines[2]).ToBe('gamma');
    Expect<string>(Lines[3]).ToBe('delta');
    Expect<string>(StringListToLFText(Lines)).ToBe(
      'alpha' + #10 +
      'beta' + #10 +
      'gamma' + #10 +
      'delta' + #10);
  finally
    Lines.Free;
  end;
end;

procedure TTextFilesTests.TestReadUTF8FileLinesCanonicalizesParserTextToLF;
const
  UTF8_WORD_BYTES = 'na' + #$C3#$AF + 've';
var
  FilePath: string;
  FileText: RawByteString;
  I: Integer;
  JoinedText: string;
  Lines: TStringList;
begin
  FilePath := GetTempFileName;
  FileText := RawByteString('cafe' + #13#10 + UTF8_WORD_BYTES + #13#10);
  SetCodePage(FileText, CP_UTF8, False);
  WriteUTF8File(FilePath, UTF8String(FileText));
  try
    Lines := ReadUTF8FileLines(FilePath);
    try
      Expect<Integer>(Lines.Count).ToBe(3);
      Expect<string>(Lines[0]).ToBe('cafe');
      Expect<Integer>(Length(Lines[1])).ToBe(Length(UTF8_WORD_BYTES));
      for I := 1 to Length(UTF8_WORD_BYTES) do
        Expect<Integer>(Ord(Lines[1][I])).ToBe(Ord(UTF8_WORD_BYTES[I]));
      Expect<string>(Lines[2]).ToBe('');
      JoinedText := StringListToLFText(Lines);
      Expect<Integer>(Length(JoinedText)).ToBe(
        Length('cafe' + #10 + UTF8_WORD_BYTES + #10 + #10));
      for I := 1 to Length(JoinedText) do
        Expect<Integer>(Ord(JoinedText[I])).ToBe(
          Ord(('cafe' + #10 + UTF8_WORD_BYTES + #10 + #10)[I]));
    finally
      Lines.Free;
    end;
  finally
    DeleteFile(FilePath);
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTextFilesTests.Create('TextFiles'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.

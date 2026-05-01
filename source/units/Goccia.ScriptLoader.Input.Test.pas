program Goccia.ScriptLoader.Input.Test;

{$I Goccia.inc}

uses
  Classes,
  Generics.Collections,
  SysUtils,

  TestingPascalLibrary,

  Goccia.GarbageCollector,
  Goccia.ScriptLoader.Input,
  Goccia.TestSetup;

type
  TScriptLoaderInputTests = class(TTestSuite)
  private
    procedure TestReadSourceFromTextPreservesLines;
    procedure TestReadSourceFromTextHandlesEmptyInput;
    procedure TestIsStdinPath;
    procedure TestIsMultifileSeparator;
    procedure TestSplitMultifileSourceSingleSection;
    procedure TestSplitMultifileSourceTwoSections;
    procedure TestSplitMultifileSourceThreeSections;
    procedure TestSplitMultifileSourceLeadingSeparatorDropped;
    procedure TestSplitMultifileSourceTrailingSeparatorDropped;
    procedure TestSplitMultifileSourceConsecutiveSeparatorsDropped;
    procedure TestBuildMultifileSectionNameFile;
    procedure TestBuildMultifileSectionNameStdin;
    procedure TestBuildMultifileSectionNamePreservesExtractFileExt;
    procedure TestIsMultifileSectionName;
    procedure TestMultifileOriginalPath;
    procedure TestMultifileOriginalPathIdempotent;
  public
    procedure SetupTests; override;
  end;

procedure TScriptLoaderInputTests.SetupTests;
begin
  Test('Read source from text preserves lines', TestReadSourceFromTextPreservesLines);
  Test('Read source from text handles empty input', TestReadSourceFromTextHandlesEmptyInput);
  Test('Is stdin path', TestIsStdinPath);
  Test('IsMultifileSeparator matches only canonical separator',
    TestIsMultifileSeparator);
  Test('SplitMultifileSource returns single section when no separator',
    TestSplitMultifileSourceSingleSection);
  Test('SplitMultifileSource returns two sections separated by ---',
    TestSplitMultifileSourceTwoSections);
  Test('SplitMultifileSource returns three sections',
    TestSplitMultifileSourceThreeSections);
  Test('SplitMultifileSource drops empty leading section',
    TestSplitMultifileSourceLeadingSeparatorDropped);
  Test('SplitMultifileSource drops empty trailing section',
    TestSplitMultifileSourceTrailingSeparatorDropped);
  Test('SplitMultifileSource drops empty sections between consecutive separators',
    TestSplitMultifileSourceConsecutiveSeparatorsDropped);
  Test('BuildMultifileSectionName inserts before extension for files',
    TestBuildMultifileSectionNameFile);
  Test('BuildMultifileSectionName appends suffix for extension-less names',
    TestBuildMultifileSectionNameStdin);
  Test('BuildMultifileSectionName preserves ExtractFileExt result',
    TestBuildMultifileSectionNamePreservesExtractFileExt);
  Test('IsMultifileSectionName recognises only the canonical form',
    TestIsMultifileSectionName);
  Test('MultifileOriginalPath strips section marker',
    TestMultifileOriginalPath);
  Test('MultifileOriginalPath is idempotent on non-section names',
    TestMultifileOriginalPathIdempotent);
end;

procedure TScriptLoaderInputTests.TestReadSourceFromTextPreservesLines;
var
  TempFileName: string;
  InputFile: Text;
  Source: TStringList;
  RawSource: TFileStream;
  Text: string;
begin
  TempFileName := GetTempFileName;
  Text := 'const x = 2 + 2;' + sLineBreak + 'x;';
  RawSource := TFileStream.Create(TempFileName, fmCreate);
  try
    RawSource.WriteBuffer(Pointer(Text)^, Length(Text));
  finally
    RawSource.Free;
  end;

  AssignFile(InputFile, TempFileName);
  Reset(InputFile);
  try
    Source := ReadSourceFromText(InputFile);
    try
      Expect<Integer>(Source.Count).ToBe(2);
      Expect<string>(Source[0]).ToBe('const x = 2 + 2;');
      Expect<string>(Source[1]).ToBe('x;');
      Expect<string>(Source.Text).ToBe('const x = 2 + 2;' + sLineBreak + 'x;' + sLineBreak);
    finally
      Source.Free;
    end;
  finally
    CloseFile(InputFile);
    DeleteFile(TempFileName);
  end;
end;

procedure TScriptLoaderInputTests.TestReadSourceFromTextHandlesEmptyInput;
var
  TempFileName: string;
  InputFile: Text;
  Source: TStringList;
  EmptySource: TStringList;
begin
  TempFileName := GetTempFileName;
  EmptySource := TStringList.Create;
  try
    EmptySource.SaveToFile(TempFileName);
  finally
    EmptySource.Free;
  end;

  AssignFile(InputFile, TempFileName);
  Reset(InputFile);
  try
    Source := ReadSourceFromText(InputFile);
    try
      Expect<Integer>(Source.Count).ToBe(0);
      Expect<string>(Source.Text).ToBe('');
    finally
      Source.Free;
    end;
  finally
    CloseFile(InputFile);
    DeleteFile(TempFileName);
  end;
end;

procedure TScriptLoaderInputTests.TestIsStdinPath;
begin
  Expect<Boolean>(IsStdinPath('-')).ToBe(True);
  Expect<Boolean>(IsStdinPath(' - ')).ToBe(True);
  Expect<Boolean>(IsStdinPath('script.js')).ToBe(False);
end;

procedure TScriptLoaderInputTests.TestIsMultifileSeparator;
begin
  Expect<Boolean>(IsMultifileSeparator('---')).ToBe(True);
  Expect<Boolean>(IsMultifileSeparator('  ---  ')).ToBe(True);
  Expect<Boolean>(IsMultifileSeparator('----')).ToBe(False);
  Expect<Boolean>(IsMultifileSeparator('--- comment')).ToBe(False);
  Expect<Boolean>(IsMultifileSeparator('---abc')).ToBe(False);
  Expect<Boolean>(IsMultifileSeparator('--')).ToBe(False);
  Expect<Boolean>(IsMultifileSeparator('')).ToBe(False);
end;

procedure TScriptLoaderInputTests.TestSplitMultifileSourceSingleSection;
var
  Source: TStringList;
  Sections: TObjectList<TStringList>;
begin
  Source := TStringList.Create;
  try
    Source.Add('const x = 1;');
    Source.Add('const y = 2;');
    Sections := SplitMultifileSource(Source);
    try
      Expect<Integer>(Sections.Count).ToBe(1);
      Expect<Integer>(Sections[0].Count).ToBe(2);
      Expect<string>(Sections[0][0]).ToBe('const x = 1;');
      Expect<string>(Sections[0][1]).ToBe('const y = 2;');
    finally
      Sections.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderInputTests.TestSplitMultifileSourceTwoSections;
var
  Source: TStringList;
  Sections: TObjectList<TStringList>;
begin
  Source := TStringList.Create;
  try
    Source.Add('section A line 1');
    Source.Add('section A line 2');
    Source.Add('---');
    Source.Add('section B line 1');
    Sections := SplitMultifileSource(Source);
    try
      Expect<Integer>(Sections.Count).ToBe(2);
      Expect<Integer>(Sections[0].Count).ToBe(2);
      Expect<string>(Sections[0][0]).ToBe('section A line 1');
      Expect<string>(Sections[0][1]).ToBe('section A line 2');
      Expect<Integer>(Sections[1].Count).ToBe(1);
      Expect<string>(Sections[1][0]).ToBe('section B line 1');
    finally
      Sections.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderInputTests.TestSplitMultifileSourceThreeSections;
var
  Source: TStringList;
  Sections: TObjectList<TStringList>;
begin
  Source := TStringList.Create;
  try
    Source.Add('A');
    Source.Add('---');
    Source.Add('B');
    Source.Add('---');
    Source.Add('C');
    Sections := SplitMultifileSource(Source);
    try
      Expect<Integer>(Sections.Count).ToBe(3);
      Expect<string>(Sections[0][0]).ToBe('A');
      Expect<string>(Sections[1][0]).ToBe('B');
      Expect<string>(Sections[2][0]).ToBe('C');
    finally
      Sections.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderInputTests.TestSplitMultifileSourceLeadingSeparatorDropped;
var
  Source: TStringList;
  Sections: TObjectList<TStringList>;
begin
  Source := TStringList.Create;
  try
    Source.Add('---');
    Source.Add('A');
    Source.Add('---');
    Source.Add('B');
    Sections := SplitMultifileSource(Source);
    try
      Expect<Integer>(Sections.Count).ToBe(2);
      Expect<string>(Sections[0][0]).ToBe('A');
      Expect<string>(Sections[1][0]).ToBe('B');
    finally
      Sections.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderInputTests.TestSplitMultifileSourceTrailingSeparatorDropped;
var
  Source: TStringList;
  Sections: TObjectList<TStringList>;
begin
  Source := TStringList.Create;
  try
    Source.Add('A');
    Source.Add('---');
    Source.Add('B');
    Source.Add('---');
    Sections := SplitMultifileSource(Source);
    try
      Expect<Integer>(Sections.Count).ToBe(2);
      Expect<string>(Sections[0][0]).ToBe('A');
      Expect<string>(Sections[1][0]).ToBe('B');
    finally
      Sections.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderInputTests.TestSplitMultifileSourceConsecutiveSeparatorsDropped;
var
  Source: TStringList;
  Sections: TObjectList<TStringList>;
begin
  Source := TStringList.Create;
  try
    Source.Add('A');
    Source.Add('---');
    Source.Add('---');
    Source.Add('B');
    Sections := SplitMultifileSource(Source);
    try
      Expect<Integer>(Sections.Count).ToBe(2);
      Expect<string>(Sections[0][0]).ToBe('A');
      Expect<string>(Sections[1][0]).ToBe('B');
    finally
      Sections.Free;
    end;
  finally
    Source.Free;
  end;
end;

procedure TScriptLoaderInputTests.TestBuildMultifileSectionNameFile;
begin
  Expect<string>(BuildMultifileSectionName('/abs/foo.jsx', 1))
    .ToBe('/abs/foo[part1].jsx');
  Expect<string>(BuildMultifileSectionName('/abs/foo.jsx', 2))
    .ToBe('/abs/foo[part2].jsx');
  Expect<string>(BuildMultifileSectionName('foo.js', 3))
    .ToBe('foo[part3].js');
end;

procedure TScriptLoaderInputTests.TestBuildMultifileSectionNameStdin;
begin
  Expect<string>(BuildMultifileSectionName('<stdin>', 1)).ToBe('<stdin>[part1]');
  Expect<string>(BuildMultifileSectionName('<stdin>', 2)).ToBe('<stdin>[part2]');
end;

procedure TScriptLoaderInputTests.TestBuildMultifileSectionNamePreservesExtractFileExt;
begin
  // Sanity check: ExtractFileExt must keep working unchanged on
  // multifile section names so all existing extension-dispatch sites
  // (JSX, .gbc bundler output, module loader) function without
  // per-site updates.
  Expect<string>(LowerCase(ExtractFileExt(BuildMultifileSectionName('foo.jsx', 1))))
    .ToBe('.jsx');
  Expect<string>(LowerCase(ExtractFileExt(BuildMultifileSectionName('foo.js', 5))))
    .ToBe('.js');
  Expect<string>(LowerCase(ExtractFileExt(BuildMultifileSectionName('foo.tsx', 9))))
    .ToBe('.tsx');
end;

procedure TScriptLoaderInputTests.TestIsMultifileSectionName;
begin
  Expect<Boolean>(IsMultifileSectionName('/abs/foo[part1].jsx')).ToBe(True);
  Expect<Boolean>(IsMultifileSectionName('foo[part42].js')).ToBe(True);
  Expect<Boolean>(IsMultifileSectionName('<stdin>[part1]')).ToBe(True);
  Expect<Boolean>(IsMultifileSectionName('/abs/foo.jsx')).ToBe(False);
  Expect<Boolean>(IsMultifileSectionName('foo[part].js')).ToBe(False);
  Expect<Boolean>(IsMultifileSectionName('foo[partABC].js')).ToBe(False);
  Expect<Boolean>(IsMultifileSectionName('<stdin>')).ToBe(False);
end;

procedure TScriptLoaderInputTests.TestMultifileOriginalPath;
begin
  Expect<string>(MultifileOriginalPath('/abs/foo[part2].jsx'))
    .ToBe('/abs/foo.jsx');
  Expect<string>(MultifileOriginalPath('foo[part1].js'))
    .ToBe('foo.js');
  Expect<string>(MultifileOriginalPath('<stdin>[part1]'))
    .ToBe('<stdin>');
end;

procedure TScriptLoaderInputTests.TestMultifileOriginalPathIdempotent;
begin
  Expect<string>(MultifileOriginalPath('/abs/foo.jsx'))
    .ToBe('/abs/foo.jsx');
  Expect<string>(MultifileOriginalPath('<stdin>'))
    .ToBe('<stdin>');
  Expect<string>(MultifileOriginalPath('foo.js'))
    .ToBe('foo.js');
end;

begin
  TGarbageCollector.Initialize;
  try
    TestRunnerProgram.AddSuite(TScriptLoaderInputTests.Create('ScriptLoader Input'));
    TestRunnerProgram.Run;
    ExitCode := TestResultToExitCode;
  finally
    TGarbageCollector.Shutdown;
  end;
end.

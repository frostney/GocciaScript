program UnicodeNormalization.Test;

{$I Shared.inc}

uses
  TestingPascalLibrary,
  UnicodeNormalization;

type
  TUnicodeNormalizationTests = class(TTestSuite)
  private
    procedure TestCanonicalDecomposition;
    procedure TestCanonicalComposition;
    procedure TestCompatibilityForms;
    procedure TestCanonicalOrdering;
    procedure TestHangul;
    procedure TestSurrogates;
  public
    procedure SetupTests; override;
  end;

procedure TUnicodeNormalizationTests.SetupTests;
begin
  Test('NFD recursively applies canonical decomposition',
    TestCanonicalDecomposition);
  Test('NFC applies canonical composition', TestCanonicalComposition);
  Test('NFKD and NFKC apply compatibility decomposition',
    TestCompatibilityForms);
  Test('all forms apply canonical combining-class ordering',
    TestCanonicalOrdering);
  Test('Hangul syllables decompose and compose algorithmically', TestHangul);
  Test('unpaired UTF-16 surrogates remain unchanged', TestSurrogates);
end;

procedure TUnicodeNormalizationTests.TestCanonicalDecomposition;
begin
  Expect<string>(NormalizeUnicode(#$01FA, unfNFD)).ToBe(
    'A' + #$030A + #$0301);
  Expect<string>(NormalizeUnicode(#$212B, unfNFD)).ToBe('A' + #$030A);
end;

procedure TUnicodeNormalizationTests.TestCanonicalComposition;
begin
  Expect<string>(NormalizeUnicode('e' + #$0301, unfNFC)).ToBe(#$00E9);
  Expect<string>(NormalizeUnicode(#$212B, unfNFC)).ToBe(#$00C5);
end;

procedure TUnicodeNormalizationTests.TestCompatibilityForms;
begin
  Expect<string>(NormalizeUnicode(#$FB03, unfNFKD)).ToBe('ffi');
  Expect<string>(NormalizeUnicode(#$212B, unfNFKC)).ToBe(#$00C5);
  Expect<string>(NormalizeUnicode(#$FB03, unfNFC)).ToBe(#$FB03);
end;

procedure TUnicodeNormalizationTests.TestCanonicalOrdering;
begin
  Expect<string>(NormalizeUnicode('q' + #$0307 + #$0323, unfNFD)).ToBe(
    'q' + #$0323 + #$0307);
end;

procedure TUnicodeNormalizationTests.TestHangul;
begin
  Expect<string>(NormalizeUnicode(#$AC01, unfNFD)).ToBe(
    #$1100 + #$1161 + #$11A8);
  Expect<string>(NormalizeUnicode(#$1100 + #$1161 + #$11A8, unfNFC))
    .ToBe(#$AC01);
end;

procedure TUnicodeNormalizationTests.TestSurrogates;
begin
  Expect<string>(NormalizeUnicode(#$D800 + 'a' + #$DC00, unfNFKC)).ToBe(
    #$D800 + 'a' + #$DC00);
end;

begin
  TestRunnerProgram.AddSuite(
    TUnicodeNormalizationTests.Create('UnicodeNormalization'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.

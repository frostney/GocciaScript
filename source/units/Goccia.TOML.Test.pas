program Goccia.TOML.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestingPascalLibrary,

  Goccia.TestSetup,
  Goccia.TOML;

type
  TTOMLParserTests = class(TTestSuite)
  private
    function GetChildOrFail(const AParent: TGocciaTOMLNode;
      const AKey: string): TGocciaTOMLNode;
    procedure TestParseDocumentNormalizesCRLFInMultilineStrings;
    procedure TestParseDocumentPreservesUTF8EscapeSequences;
    procedure TestParseDocumentPreservesUTF8KeysAndValues;
    procedure TestParseDocumentTracksScalarKinds;
    procedure TestParseDocumentTracksArrayElementKinds;
  public
    procedure SetupTests; override;
  end;

procedure TTOMLParserTests.SetupTests;
begin
  Test('ParseDocument tracks scalar kinds',
    TestParseDocumentTracksScalarKinds);
  Test('ParseDocument tracks array element kinds',
    TestParseDocumentTracksArrayElementKinds);
  Test('ParseDocument normalizes CRLF in multiline strings',
    TestParseDocumentNormalizesCRLFInMultilineStrings);
  Test('ParseDocument preserves UTF-8 escape sequences',
    TestParseDocumentPreservesUTF8EscapeSequences);
  Test('ParseDocument preserves UTF-8 keys and values',
    TestParseDocumentPreservesUTF8KeysAndValues);
end;

function TTOMLParserTests.GetChildOrFail(const AParent: TGocciaTOMLNode;
  const AKey: string): TGocciaTOMLNode;
begin
  if not AParent.Children.TryGetValue(AKey, Result) then
    raise Exception.Create('Missing TOML key: ' + AKey);
end;

procedure TTOMLParserTests.TestParseDocumentTracksScalarKinds;
var
  CountNode: TGocciaTOMLNode;
  Parser: TGocciaTOMLParser;
  RatioNode: TGocciaTOMLNode;
  ReleaseNode: TGocciaTOMLNode;
  Root: TGocciaTOMLNode;
  TitleNode: TGocciaTOMLNode;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    Root := Parser.ParseDocument(
      'title = "goccia"' + LineEnding +
      'count = 42' + LineEnding +
      'ratio = 3.5' + LineEnding +
      'release = 2026-04-04 12:30:45Z' + LineEnding);
    try
      TitleNode := GetChildOrFail(Root, 'title');
      CountNode := GetChildOrFail(Root, 'count');
      RatioNode := GetChildOrFail(Root, 'ratio');
      ReleaseNode := GetChildOrFail(Root, 'release');

      Expect<Integer>(Ord(TitleNode.ScalarKind)).ToBe(Ord(tskString));
      Expect<string>(TitleNode.CanonicalValue).ToBe('goccia');

      Expect<Integer>(Ord(CountNode.ScalarKind)).ToBe(Ord(tskInteger));
      Expect<string>(CountNode.CanonicalValue).ToBe('42');

      Expect<Integer>(Ord(RatioNode.ScalarKind)).ToBe(Ord(tskFloat));
      Expect<string>(RatioNode.CanonicalValue).ToBe('3.5');

      Expect<Integer>(Ord(ReleaseNode.ScalarKind)).ToBe(Ord(tskDateTime));
      Expect<string>(ReleaseNode.CanonicalValue).ToBe('2026-04-04T12:30:45Z');
    finally
      Root.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTOMLParserTests.TestParseDocumentTracksArrayElementKinds;
var
  ItemsNode: TGocciaTOMLNode;
  NestedNode: TGocciaTOMLNode;
  Parser: TGocciaTOMLParser;
  Root: TGocciaTOMLNode;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    Root := Parser.ParseDocument(
      'items = [1, 1.5, 07:32, { nested = true }]' + LineEnding);
    try
      ItemsNode := GetChildOrFail(Root, 'items');
      Expect<Integer>(Ord(ItemsNode.Kind)).ToBe(Ord(tnkArray));
      Expect<Integer>(ItemsNode.Items.Count).ToBe(4);

      Expect<Integer>(Ord(ItemsNode.Items[0].ScalarKind)).ToBe(Ord(tskInteger));
      Expect<string>(ItemsNode.Items[0].CanonicalValue).ToBe('1');

      Expect<Integer>(Ord(ItemsNode.Items[1].ScalarKind)).ToBe(Ord(tskFloat));
      Expect<string>(ItemsNode.Items[1].CanonicalValue).ToBe('1.5');

      Expect<Integer>(Ord(ItemsNode.Items[2].ScalarKind)).ToBe(Ord(tskTimeLocal));
      Expect<string>(ItemsNode.Items[2].CanonicalValue).ToBe('07:32:00');

      NestedNode := GetChildOrFail(ItemsNode.Items[3], 'nested');
      Expect<Integer>(Ord(NestedNode.ScalarKind)).ToBe(Ord(tskBool));
      Expect<string>(NestedNode.CanonicalValue).ToBe('true');
    finally
      Root.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTOMLParserTests.TestParseDocumentNormalizesCRLFInMultilineStrings;
var
  BasicNode: TGocciaTOMLNode;
  LiteralNode: TGocciaTOMLNode;
  Parser: TGocciaTOMLParser;
  Root: TGocciaTOMLNode;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    Root := Parser.ParseDocument(
      'basic = """' + #13#10 +
      'Roses are red' + #13#10 +
      'Violets are blue' + #13#10 +
      '"""' + #13#10 +
      'literal = ''''''' + #13#10 +
      'C:\Users\nodejs\templates' + #13#10 +
      'Line two' + #13#10 +
      '''''''' + #13#10);
    try
      BasicNode := GetChildOrFail(Root, 'basic');
      LiteralNode := GetChildOrFail(Root, 'literal');

      Expect<string>(BasicNode.CanonicalValue).ToBe(
        'Roses are red' + #10 + 'Violets are blue' + #10);
      Expect<string>(LiteralNode.CanonicalValue).ToBe(
        'C:\Users\nodejs\templates' + #10 + 'Line two' + #10);
    finally
      Root.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTOMLParserTests.TestParseDocumentPreservesUTF8KeysAndValues;
var
  NameKey: UTF8String;
  NameNode: TGocciaTOMLNode;
  NameValue: UTF8String;
  Parser: TGocciaTOMLParser;
  QuotedKey: UTF8String;
  QuotedNode: TGocciaTOMLNode;
  Root: TGocciaTOMLNode;
  SourceText: UTF8String;
begin
  NameKey := 'name';
  NameValue := 'Jos' + #$C3#$A9;
  QuotedKey := 'd' + #$C3#$A9 + 'j' + #$C3#$A0;
  SourceText :=
    NameKey + ' = "' + NameValue + '"' + #10 +
    '"' + QuotedKey + '" = "vu"' + #10;

  Parser := TGocciaTOMLParser.Create;
  try
    Root := Parser.ParseDocument(SourceText);
    try
      NameNode := GetChildOrFail(Root, NameKey);
      QuotedNode := GetChildOrFail(Root, QuotedKey);

      Expect<string>(NameNode.CanonicalValue).ToBe(NameValue);
      Expect<string>(QuotedNode.CanonicalValue).ToBe('vu');
    finally
      Root.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TTOMLParserTests.TestParseDocumentPreservesUTF8EscapeSequences;
var
  AstralNode: TGocciaTOMLNode;
  DeltaNode: TGocciaTOMLNode;
  EscapedKeyNode: TGocciaTOMLNode;
  Parser: TGocciaTOMLParser;
  Root: TGocciaTOMLNode;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    Root := Parser.ParseDocument(
      'delta = "\u03B4"' + #10 +
      '"caf\u00E9" = "ok"' + #10 +
      'astral = "\U00010AF1"' + #10);
    try
      DeltaNode := GetChildOrFail(Root, 'delta');
      EscapedKeyNode := GetChildOrFail(Root, 'caf' + #$C3#$A9);
      AstralNode := GetChildOrFail(Root, 'astral');

      Expect<string>(DeltaNode.CanonicalValue).ToBe(#$CE#$B4);
      Expect<string>(EscapedKeyNode.CanonicalValue).ToBe('ok');
      Expect<string>(AstralNode.CanonicalValue).ToBe(#$F0#$90#$AB#$B1);
    finally
      Root.Free;
    end;
  finally
    Parser.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TTOMLParserTests.Create('TOML Parser'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.

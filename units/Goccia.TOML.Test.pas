program Goccia.TOML.Test;

{$I Goccia.inc}

uses
  SysUtils,

  TestRunner,

  Goccia.TestSetup,
  Goccia.TOML;

type
  TTOMLParserTests = class(TTestSuite)
  private
    function GetChildOrFail(const AParent: TGocciaTOMLNode;
      const AKey: string): TGocciaTOMLNode;
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

begin
  TestRunnerProgram.AddSuite(TTOMLParserTests.Create('TOML Parser'));
  TestRunnerProgram.Run;
  ExitCode := TestResultToExitCode;
end.

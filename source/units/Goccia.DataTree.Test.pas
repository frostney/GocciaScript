program Goccia.DataTree.Test;

{$I Goccia.inc}

uses
  Math,
  SysUtils,

  TestingPascalLibrary,

  Goccia.DataTree,
  Goccia.TestSetup,
  Goccia.TOML,
  Goccia.YAML;

type
  TDataTreeTests = class(TTestSuite)
  private
    procedure TestJSONKeepsSharedNodesOneEntry;
    procedure TestJSONMarksNumbersJSONCannotHold;
    procedure TestJSONEscapesStrings;
    procedure TestOwnKeysListIndicesFirst;
    procedure TestYAMLAliasIsTheAnchoredNode;
    procedure TestYAMLSelfAliasIsACycle;
    procedure TestYAMLTaggedNodeKeepsItsTag;
    procedure TestTOMLParsesIntoATree;
    procedure TestDepthLimitWithoutEngineGuards;
    procedure TestObjectHasNoText;
  public
    procedure SetupTests; override;
  end;

procedure TDataTreeTests.SetupTests;
begin
  Test('DataTreeToJSON keeps a shared node one entry',
    TestJSONKeepsSharedNodesOneEntry);
  Test('DataTreeToJSON marks numbers JSON cannot hold',
    TestJSONMarksNumbersJSONCannotHold);
  Test('DataTreeToJSON escapes strings', TestJSONEscapesStrings);
  Test('OwnKeys lists array indices first', TestOwnKeysListIndicesFirst);
  Test('a YAML alias is the anchored node', TestYAMLAliasIsTheAnchoredNode);
  Test('a YAML self-alias is a cycle', TestYAMLSelfAliasIsACycle);
  Test('a YAML tagged node keeps its tag', TestYAMLTaggedNodeKeepsItsTag);
  Test('TOML parses into a tree', TestTOMLParsesIntoATree);
  Test('nesting past the depth limit raises without engine guards',
    TestDepthLimitWithoutEngineGuards);
  Test('an object node has no string form', TestObjectHasNoText);
end;

procedure TDataTreeTests.TestJSONKeepsSharedNodesOneEntry;
var
  Root, Shared: TGocciaDataNode;
  Tree: TGocciaDataTree;
begin
  Tree := TGocciaDataTree.Create;
  try
    Root := Tree.NewObject;
    Shared := Tree.NewArray;
    Shared.Add(Tree.NewString('x'));
    Root.SetProperty('a', Shared);
    Root.SetProperty('b', Shared);
    Expect<string>(DataTreeToJSON(Root)).ToBe('[[2,"a",1,"b",1],[1,2],"x"]');
  finally
    Tree.Free;
  end;
end;

procedure TDataTreeTests.TestJSONMarksNumbersJSONCannotHold;
var
  Root: TGocciaDataNode;
  Tree: TGocciaDataTree;
begin
  Tree := TGocciaDataTree.Create;
  try
    Root := Tree.NewArray;
    Root.Add(Tree.NewNumber(NaN));
    Root.Add(Tree.NewNumber(Infinity));
    Root.Add(Tree.NewNumber(NegInfinity));
    Root.Add(Tree.NewNumber(-0.0));
    Root.Add(Tree.NewNumber(0.1));
    Root.Add(Tree.NewNumber(1e21));
    Expect<string>(DataTreeToJSON(Root)).ToBe(
      '[[1,1,2,3,4,5,6],[0,"NaN"],[0,"Infinity"],[0,"-Infinity"],[0,"-0"],0.1,1e+21]');
  finally
    Tree.Free;
  end;
end;

procedure TDataTreeTests.TestJSONEscapesStrings;
var
  Tree: TGocciaDataTree;
begin
  Tree := TGocciaDataTree.Create;
  try
    Expect<string>(DataTreeToJSON(Tree.NewString('q"\'#10#1#$D800#$00E9)))
      .ToBe('["q\"\\\n\u0001\ud800' + #$00E9 + '"]');
  finally
    Tree.Free;
  end;
end;

procedure TDataTreeTests.TestOwnKeysListIndicesFirst;
var
  Keys: TArray<string>;
  Root: TGocciaDataNode;
  Tree: TGocciaDataTree;
begin
  Tree := TGocciaDataTree.Create;
  try
    Root := Tree.NewObject;
    Root.SetProperty('b', Tree.NewNull);
    Root.SetProperty('10', Tree.NewNull);
    Root.SetProperty('01', Tree.NewNull);
    Root.SetProperty('2', Tree.NewNull);
    Root.SetProperty('4294967295', Tree.NewNull);
    Keys := Root.OwnKeys;
    Expect<Integer>(Length(Keys)).ToBe(5);
    Expect<string>(Keys[0]).ToBe('2');
    Expect<string>(Keys[1]).ToBe('10');
    Expect<string>(Keys[2]).ToBe('b');
    Expect<string>(Keys[3]).ToBe('01');
    Expect<string>(Keys[4]).ToBe('4294967295');
  finally
    Tree.Free;
  end;
end;

procedure TDataTreeTests.TestYAMLAliasIsTheAnchoredNode;
var
  Parser: TGocciaYAMLParser;
  Root: TGocciaDataNode;
  Tree: TGocciaDataTree;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Tree := Parser.Parse('base: &b {x: 1}' + #10 + 'copy: *b' + #10 +
      'merged:' + #10 + '  <<: *b' + #10 + '  y: 2' + #10);
    try
      Root := Tree.Root;
      Expect<Boolean>(Root.GetProperty('copy') = Root.GetProperty('base'))
        .ToBe(True);
      Expect<Boolean>(Root.GetProperty('merged').GetProperty('x') =
        Root.GetProperty('base').GetProperty('x')).ToBe(True);
      Expect<Integer>(Length(Root.GetProperty('merged').OwnKeys)).ToBe(2);
    finally
      Tree.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDataTreeTests.TestYAMLSelfAliasIsACycle;
var
  Node: TGocciaDataNode;
  Parser: TGocciaYAMLParser;
  Tree: TGocciaDataTree;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Tree := Parser.Parse('a: &a' + #10 + '  self: *a' + #10);
    try
      Node := Tree.Root.GetProperty('a');
      Expect<Boolean>(Node.GetProperty('self') = Node).ToBe(True);
      Expect<string>(DataTreeToJSON(Tree.Root)).ToBe('[[2,"a",1],[2,"self",1]]');
    finally
      Tree.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDataTreeTests.TestYAMLTaggedNodeKeepsItsTag;
var
  Node: TGocciaDataNode;
  Parser: TGocciaYAMLParser;
  Tree: TGocciaDataTree;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Tree := Parser.Parse('%TAG !e! tag:example.com,2026:' + #10 + '---' + #10 +
      'a: !e!widget 1' + #10 + 'b: !!binary SGk=' + #10);
    try
      // one document is the value itself, "---" or not
      Expect<Integer>(Ord(Tree.Root.Kind)).ToBe(Ord(dnkObject));
      Node := Tree.Root.GetProperty('a');
      Expect<Integer>(Ord(Node.Kind)).ToBe(Ord(dnkTagged));
      Expect<string>(Node.TagName).ToBe('tag:example.com,2026:widget');
      Expect<Double>(Node.TaggedValue.NumberValue).ToBe(1);
      Node := Tree.Root.GetProperty('b');
      // !!binary keeps its base64 text
      Expect<string>(Node.TaggedValue.StringValue).ToBe('SGk=');
    finally
      Tree.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDataTreeTests.TestTOMLParsesIntoATree;
var
  Parser: TGocciaTOMLParser;
  Tree: TGocciaDataTree;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    Tree := Parser.Parse('x = -0.0' + #10 + '[[t]]' + #10 + 'n = nan' + #10);
    try
      Expect<string>(DataTreeToJSON(Tree.Root))
        .ToBe('[[2,"x",1,"t",2],[0,"-0"],[1,3],[2,"n",4],[0,"NaN"]]');
    finally
      Tree.Free;
    end;
  finally
    Parser.Free;
  end;
end;

procedure TDataTreeTests.TestDepthLimitWithoutEngineGuards;
var
  Parser: TGocciaYAMLParser;
  Raised: Boolean;
  Tree: TGocciaDataTree;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Tree := Parser.Parse(StringOfChar('[', DATA_TREE_MAX_DEPTH - 1) +
      StringOfChar(']', DATA_TREE_MAX_DEPTH - 1));
    Tree.Free;
    Raised := False;
    try
      Parser.Parse(StringOfChar('[', DATA_TREE_MAX_DEPTH + 1) +
        StringOfChar(']', DATA_TREE_MAX_DEPTH + 1)).Free;
    except
      on EGocciaDataDepthError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
    // the failed parse left the depth count where it was
    Parser.Parse('[[1]]').Free;
  finally
    Parser.Free;
  end;
end;

procedure TDataTreeTests.TestObjectHasNoText;
var
  Raised: Boolean;
  Tree: TGocciaDataTree;
begin
  Tree := TGocciaDataTree.Create;
  try
    Raised := False;
    try
      Tree.NewObject.ToText;
    except
      on EGocciaDataTypeError do
        Raised := True;
    end;
    Expect<Boolean>(Raised).ToBe(True);
  finally
    Tree.Free;
  end;
end;

begin
  TestRunnerProgram.AddSuite(TDataTreeTests.Create('Data Tree'));
  RunGocciaTests;
  ExitCode := TestResultToExitCode;
end.

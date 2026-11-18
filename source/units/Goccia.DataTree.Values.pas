unit Goccia.DataTree.Values;

{ The engine's values for a Goccia.DataTree tree, and the YAML and TOML parses
  as values. One value per node, so a node the tree shares (a YAML alias) is
  one value wherever it stands, cycles included. Objects are created without a
  prototype, as the parsers have always built them.

  Loading this unit points the tree's recursion and work guards at the
  engine's native limits (Goccia.NativeLimits). }

{$I Goccia.inc}

interface

uses
  Goccia.DataTree,
  Goccia.Values.ArrayValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

{ the value of ANode and everything it reaches }
function DataNodeToValue(const ANode: TGocciaDataNode): TGocciaValue;
{ the value of ATree's root; frees ATree }
function DataTreeToValue(const ATree: TGocciaDataTree): TGocciaValue;

{ Goccia.YAML's parses as values }
function YAMLTextToValue(const AText: string): TGocciaValue;
function YAMLDocumentsToValue(const AText: string): TGocciaArrayValue;
{ Goccia.TOML's parse as a value }
function TOMLTextToValue(const AText: string): TGocciaObjectValue;

implementation

uses
  Generics.Collections,
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.NativeLimits,
  Goccia.TOML,
  Goccia.Values.ErrorHelper,
  Goccia.YAML;

type
  { a YAML node with a tag outside the core schema, or a !!timestamp or
    !!binary scalar: the value it wraps, with the tag under tagName }
  TGocciaYAMLTaggedValue = class(TGocciaValue)
  private
    FTagName: string;
    FValue: TGocciaValue;
  public
    constructor Create(const ATagName: string; const AValue: TGocciaValue);
    procedure TraceReferences; override;
    function RuntimeCopy: TGocciaValue; override;
    function IsPrimitive: Boolean; override;
    function IsCallable: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
    function GetProperty(const AName: string): TGocciaValue; override;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); override;
    property TagName: string read FTagName;
    property Value: TGocciaValue read FValue;
  end;

constructor TGocciaYAMLTaggedValue.Create(const ATagName: string;
  const AValue: TGocciaValue);
begin
  inherited Create;
  FTagName := ATagName;
  FValue := AValue;
end;

procedure TGocciaYAMLTaggedValue.TraceReferences;
begin
  inherited;
  if Assigned(FValue) then
    FValue.MarkReferences;
end;

function TGocciaYAMLTaggedValue.RuntimeCopy: TGocciaValue;
begin
  Result := TGocciaYAMLTaggedValue.Create(FTagName, FValue.RuntimeCopy);
end;

function TGocciaYAMLTaggedValue.IsPrimitive: Boolean;
begin
  Result := FValue.IsPrimitive;
end;

function TGocciaYAMLTaggedValue.IsCallable: Boolean;
begin
  Result := FValue.IsCallable;
end;

function TGocciaYAMLTaggedValue.TypeName: string;
begin
  Result := FValue.TypeName;
end;

function TGocciaYAMLTaggedValue.TypeOf: string;
begin
  Result := FValue.TypeOf;
end;

function TGocciaYAMLTaggedValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := FValue.ToBooleanLiteral;
end;

function TGocciaYAMLTaggedValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := FValue.ToNumberLiteral;
end;

function TGocciaYAMLTaggedValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := FValue.ToStringLiteral;
end;

function TGocciaYAMLTaggedValue.GetProperty(const AName: string): TGocciaValue;
begin
  if AName = PROP_TAG_NAME then
    Exit(TGocciaStringLiteralValue.Create(FTagName));
  if AName = PROP_VALUE then
    Exit(FValue);
  Result := FValue.GetProperty(AName);
end;

procedure TGocciaYAMLTaggedValue.SetProperty(const AName: string;
  const AValue: TGocciaValue);
begin
  FValue.SetProperty(AName, AValue);
end;

function DataNodeToValue(const ANode: TGocciaDataNode): TGocciaValue;
var
  Keeper: TGocciaArrayValue;
  KeeperRoot: TGocciaTempRoot;
  Unfilled: TList<TGocciaDataNode>;
  Values: TDictionary<TGocciaDataNode, TGocciaValue>;

  { the node's value, made on first use; a container is made empty and queued
    to be filled, so the walk needs no native recursion however deep the tree }
  function ValueOf(const ANode: TGocciaDataNode): TGocciaValue;
  begin
    if Values.TryGetValue(ANode, Result) then
      Exit;
    case ANode.Kind of
      dnkBoolean:
        if ANode.BooleanValue then
          Result := TGocciaBooleanLiteralValue.TrueValue
        else
          Result := TGocciaBooleanLiteralValue.FalseValue;
      dnkNumber:
        Result := TGocciaNumberLiteralValue.Create(ANode.NumberValue);
      dnkString:
        Result := TGocciaStringLiteralValue.Create(ANode.StringValue);
      dnkArray:
        begin
          Result := TGocciaArrayValue.Create;
          Unfilled.Add(ANode);
        end;
      dnkObject:
        begin
          Result := TGocciaObjectValue.Create;
          Unfilled.Add(ANode);
        end;
      dnkTagged:
        Result := TGocciaYAMLTaggedValue.Create(ANode.TagName,
          ValueOf(ANode.TaggedValue));
    else
      Result := TGocciaNullLiteralValue.NullValue;
    end;
    // every value is reachable from the rooted keeper until the caller takes
    // the result, whatever a later allocation collects
    Keeper.Elements.Add(Result);
    Values.Add(ANode, Result);
  end;

var
  Index, I: Integer;
  Node: TGocciaDataNode;
  Pair: TGocciaDataNodeMap.TKeyValuePair;
  Target: TGocciaValue;
begin
  Values := TDictionary<TGocciaDataNode, TGocciaValue>.Create;
  Unfilled := TList<TGocciaDataNode>.Create;
  InitializeTempRoot(KeeperRoot);
  try
    Keeper := TGocciaArrayValue.Create;
    AddTempRootIfNeeded(KeeperRoot, Keeper);
    Result := ValueOf(ANode);
    Index := 0;
    while Index < Unfilled.Count do
    begin
      Node := Unfilled[Index];
      Target := Values[Node];
      if Node.Kind = dnkArray then
      begin
        for I := 0 to Node.Count - 1 do
          TGocciaArrayValue(Target).Elements.Add(ValueOf(Node.Items[I]));
      end
      else
        for Pair in Node.Properties do
          TGocciaObjectValue(Target).AssignProperty(Pair.Key, ValueOf(Pair.Value));
      Inc(Index);
    end;
  finally
    RemoveTempRootIfNeeded(KeeperRoot);
    Unfilled.Free;
    Values.Free;
  end;
end;

function DataTreeToValue(const ATree: TGocciaDataTree): TGocciaValue;
begin
  try
    Result := DataNodeToValue(ATree.Root);
  finally
    ATree.Free;
  end;
end;

function YAMLTextToValue(const AText: string): TGocciaValue;
var
  Parser: TGocciaYAMLParser;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Result := DataTreeToValue(Parser.Parse(AText));
  finally
    Parser.Free;
  end;
end;

function YAMLDocumentsToValue(const AText: string): TGocciaArrayValue;
var
  Parser: TGocciaYAMLParser;
begin
  Parser := TGocciaYAMLParser.Create;
  try
    Result := TGocciaArrayValue(DataTreeToValue(Parser.ParseDocuments(AText)));
  finally
    Parser.Free;
  end;
end;

function TOMLTextToValue(const AText: string): TGocciaObjectValue;
var
  Parser: TGocciaTOMLParser;
begin
  Parser := TGocciaTOMLParser.Create;
  try
    Result := TGocciaObjectValue(DataTreeToValue(Parser.Parse(AText)));
  finally
    Parser.Free;
  end;
end;

procedure EnterEngineDataDepth(const AOperation: string);
begin
  EnterNativeDataDepth(AOperation);
end;

procedure ThrowEngineTypeError(const AMessage: string);
begin
  ThrowTypeError(AMessage, SSuggestToPrimitive);
end;

initialization
  DataTreeGuards.EnterDepth := EnterEngineDataDepth;
  DataTreeGuards.LeaveDepth := LeaveNativeDataDepth;
  DataTreeGuards.CheckWork := CheckNativeWork;
  DataTreeGuards.TypeError := ThrowEngineTypeError;

end.

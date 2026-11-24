unit Goccia.DataTree.JSON;

{ JSON text as a data tree (Goccia.DataTree), free of any engine's values, for
  host-side readers of JSON files such as import maps: the strict JSON grammar
  of the shared parser (JSONParser), its errors (EJSONParseError) and its
  nesting limit. A repeated key keeps its first position and takes the last
  value, as JSON.parse's object does (ES2026 §25.5.1.1, CreateDataProperty). }

{$I Goccia.inc}

interface

uses
  Goccia.DataTree;

{ The tree of AText; the caller owns it. Raises EJSONParseError for text that
  is not JSON. }
function ParseJSONDataTree(const AText: string): TGocciaDataTree;

implementation

uses
  JSONParser;

type
  TDataTreeJSONParser = class(TAbstractJSONParser)
  private
    FTree: TGocciaDataTree;
    { the open containers, innermost last }
    FStack: array of TGocciaDataNode;
    FDepth: Integer;
    { the key the next value of each open object takes }
    FKeys: array of string;
    procedure Push(const ANode: TGocciaDataNode);
    procedure Place(const ANode: TGocciaDataNode);
  protected
    procedure OnNull; override;
    procedure OnBoolean(const AValue: Boolean); override;
    procedure OnString(const AValue: string); override;
    procedure OnInteger(const AValue: Int64); override;
    procedure OnFloat(const AValue: Double); override;
    procedure OnBeginObject; override;
    procedure OnObjectKey(const AKey: string); override;
    procedure OnEndObject; override;
    procedure OnBeginArray; override;
    procedure OnEndArray; override;
  public
    function Parse(const AText: string): TGocciaDataTree;
  end;

procedure TDataTreeJSONParser.Place(const ANode: TGocciaDataNode);
var
  Parent: TGocciaDataNode;
begin
  if FDepth = 0 then
  begin
    FTree.Root := ANode;
    Exit;
  end;
  Parent := FStack[FDepth - 1];
  if Parent.Kind = dnkArray then
    Parent.Add(ANode)
  else
    Parent.SetProperty(FKeys[FDepth - 1], ANode);
end;

procedure TDataTreeJSONParser.Push(const ANode: TGocciaDataNode);
begin
  Place(ANode);
  if FDepth >= Length(FStack) then
  begin
    SetLength(FStack, FDepth * 2 + 8);
    SetLength(FKeys, Length(FStack));
  end;
  FStack[FDepth] := ANode;
  FKeys[FDepth] := '';
  Inc(FDepth);
end;

procedure TDataTreeJSONParser.OnNull;
begin
  Place(FTree.NewNull);
end;

procedure TDataTreeJSONParser.OnBoolean(const AValue: Boolean);
begin
  Place(FTree.NewBoolean(AValue));
end;

procedure TDataTreeJSONParser.OnString(const AValue: string);
begin
  Place(FTree.NewString(AValue));
end;

procedure TDataTreeJSONParser.OnInteger(const AValue: Int64);
var
  Value: Double;
begin
  Value := AValue;
  Place(FTree.NewNumber(Value));
end;

procedure TDataTreeJSONParser.OnFloat(const AValue: Double);
begin
  Place(FTree.NewNumber(AValue));
end;

procedure TDataTreeJSONParser.OnBeginObject;
begin
  Push(FTree.NewObject);
end;

procedure TDataTreeJSONParser.OnObjectKey(const AKey: string);
begin
  FKeys[FDepth - 1] := AKey;
end;

procedure TDataTreeJSONParser.OnEndObject;
begin
  Dec(FDepth);
end;

procedure TDataTreeJSONParser.OnBeginArray;
begin
  Push(FTree.NewArray);
end;

procedure TDataTreeJSONParser.OnEndArray;
begin
  Dec(FDepth);
end;

function TDataTreeJSONParser.Parse(const AText: string): TGocciaDataTree;
begin
  FTree := TGocciaDataTree.Create;
  FDepth := 0;
  try
    DoParse(AText);
  except
    FTree.Free;
    FTree := nil;
    raise;
  end;
  Result := FTree;
  FTree := nil;
end;

function ParseJSONDataTree(const AText: string): TGocciaDataTree;
var
  Parser: TDataTreeJSONParser;
begin
  Parser := TDataTreeJSONParser.Create;
  try
    Result := Parser.Parse(AText);
  finally
    Parser.Free;
  end;
end;

end.

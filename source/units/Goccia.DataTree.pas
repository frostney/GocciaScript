unit Goccia.DataTree;

{ The tree the data-format parsers (Goccia.YAML, Goccia.TOML) produce, free of
  any engine's values so every engine builds its own from the same parse: the
  old engine through Goccia.DataTree.Values, G2 through DataTreeToJSON and its
  prelude.

  A tree owns its nodes as an arena. A node may be referenced from more than
  one place, and from inside itself: a YAML alias names the anchored node, not
  a copy, so an engine that keeps one value per node keeps the alias's
  identity, cycles included.

  The parsers recurse once per nesting level. They guard it with
  EnterDataDepth / LeaveDataDepth and poll CheckDataWork, which an engine
  points at its own limits (DataTreeGuards); without one, nesting deeper than
  DATA_TREE_MAX_DEPTH raises EGocciaDataDepthError. A node that has no string
  form (ToText of an object) raises the engine's TypeError the same way, or
  EGocciaDataTypeError. }

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  SysUtils,

  OrderedStringMap;

const
  DATA_TREE_MAX_DEPTH = 256;

type
  EGocciaDataDepthError = class(Exception);
  EGocciaDataTypeError = class(Exception);

  TGocciaDataNodeKind = (dnkNull, dnkBoolean, dnkNumber, dnkString, dnkArray,
    dnkObject, dnkTagged);

  TGocciaDataNode = class;
  TGocciaDataNodeMap = TOrderedStringMap<TGocciaDataNode>;
  TGocciaDataNodeList = TList<TGocciaDataNode>;

  TGocciaDataNode = class
  private
    FBoolean: Boolean;
    FItems: TGocciaDataNodeList;
    FKind: TGocciaDataNodeKind;
    FNumber: Double;
    FProperties: TGocciaDataNodeMap;
    FText: string;
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TGocciaDataNode;
    function GetTaggedValue: TGocciaDataNode;
  public
    constructor Create(const AKind: TGocciaDataNodeKind);
    destructor Destroy; override;

    { an array's next element }
    procedure Add(const ANode: TGocciaDataNode);
    { an object's own key }
    function HasProperty(const AKey: string): Boolean;
    function GetProperty(const AKey: string): TGocciaDataNode;
    { adds the key, or replaces its node where it already stands }
    procedure SetProperty(const AKey: string; const ANode: TGocciaDataNode);
    { an object's keys as an engine lists them (ES2026 §10.1.11.1
      OrdinaryOwnPropertyKeys): array indices ascending, then the others in
      the order they were added }
    function OwnKeys: TArray<string>;
    { ECMAScript ToString of the value the node becomes (ES2026 §7.1.17); an
      array joins its elements as Array.prototype.join, a tagged node is its
      value's, and an object, made without a prototype, has none (a
      TypeError) }
    function ToText: string;
    function IsContainer: Boolean;

    property BooleanValue: Boolean read FBoolean;
    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TGocciaDataNode read GetItem;
    property Kind: TGocciaDataNodeKind read FKind;
    property NumberValue: Double read FNumber;
    property Properties: TGocciaDataNodeMap read FProperties;
    property StringValue: string read FText;
    property TagName: string read FText;
    property TaggedValue: TGocciaDataNode read GetTaggedValue;
  end;

  TGocciaDataTree = class
  private
    FFalse: TGocciaDataNode;
    FNodes: TObjectList<TGocciaDataNode>;
    FNull: TGocciaDataNode;
    FRoot: TGocciaDataNode;
    FTrue: TGocciaDataNode;
    function Own(const ANode: TGocciaDataNode): TGocciaDataNode;
  public
    constructor Create;
    destructor Destroy; override;

    function NewNull: TGocciaDataNode;
    function NewBoolean(const AValue: Boolean): TGocciaDataNode;
    function NewNumber(const AValue: Double): TGocciaDataNode;
    function NewString(const AValue: string): TGocciaDataNode;
    function NewArray: TGocciaDataNode;
    function NewObject: TGocciaDataNode;
    function NewTagged(const ATagName: string;
      const AValue: TGocciaDataNode): TGocciaDataNode;

    { the parse's result }
    property Root: TGocciaDataNode read FRoot write FRoot;
  end;

  TGocciaDataTextProc = procedure(const AOperation: string);
  TGocciaDataProc = procedure;

  TGocciaDataTreeGuards = record
    EnterDepth: TGocciaDataTextProc;
    LeaveDepth: TGocciaDataProc;
    CheckWork: TGocciaDataProc;
    { raises the engine's TypeError with the message }
    TypeError: TGocciaDataTextProc;
  end;

var
  { the embedding engine's recursion and work limits; nil members fall back to
    the tree's own depth count }
  DataTreeGuards: TGocciaDataTreeGuards;

procedure EnterDataDepth(const AOperation: string);
procedure LeaveDataDepth;
procedure CheckDataWork;

{ The tree under ANode as compact JSON for an engine that rebuilds it in
  script: an array of entries, the node first and each other node it reaches
  once, numbered in the order the entries before it first name them. A container names its
  children by entry index, so a shared node stays one entry:
    null, true, false, a finite number other than -0, a string: itself
    [0, "NaN" | "Infinity" | "-Infinity" | "-0"]: a number JSON cannot hold
    [1, i, ...]: an array of the entries i
    [2, key, i, ...]: an object, its keys in order, each with its entry
    [3, tagName, i]: a YAML tagged value wrapping entry i }
function DataTreeToJSON(const ANode: TGocciaDataNode): string;

implementation

uses
  Math,

  NumberBits,
  NumericText,
  StringBuffer;

threadvar
  GDataDepth: Integer;

procedure EnterDataDepth(const AOperation: string);
begin
  if Assigned(DataTreeGuards.EnterDepth) then
  begin
    DataTreeGuards.EnterDepth(AOperation);
    Exit;
  end;
  if GDataDepth >= DATA_TREE_MAX_DEPTH then
    raise EGocciaDataDepthError.Create('Maximum call stack size exceeded');
  Inc(GDataDepth);
end;

procedure LeaveDataDepth;
begin
  if Assigned(DataTreeGuards.LeaveDepth) then
  begin
    DataTreeGuards.LeaveDepth;
    Exit;
  end;
  if GDataDepth > 0 then
    Dec(GDataDepth);
end;

procedure CheckDataWork;
begin
  if Assigned(DataTreeGuards.CheckWork) then
    DataTreeGuards.CheckWork;
end;

{ TGocciaDataNode }

constructor TGocciaDataNode.Create(const AKind: TGocciaDataNodeKind);
begin
  inherited Create;
  FKind := AKind;
  case AKind of
    dnkArray, dnkTagged:
      FItems := TGocciaDataNodeList.Create;
    dnkObject:
      FProperties := TGocciaDataNodeMap.Create;
  end;
end;

destructor TGocciaDataNode.Destroy;
begin
  FItems.Free;
  FProperties.Free;
  inherited;
end;

function TGocciaDataNode.GetCount: Integer;
begin
  if Assigned(FItems) then
    Result := FItems.Count
  else
    Result := 0;
end;

function TGocciaDataNode.GetItem(const AIndex: Integer): TGocciaDataNode;
begin
  Result := FItems[AIndex];
end;

function TGocciaDataNode.GetTaggedValue: TGocciaDataNode;
begin
  if (FKind = dnkTagged) and (FItems.Count > 0) then
    Result := FItems[0]
  else
    Result := nil;
end;

procedure TGocciaDataNode.Add(const ANode: TGocciaDataNode);
begin
  FItems.Add(ANode);
end;

function TGocciaDataNode.HasProperty(const AKey: string): Boolean;
begin
  Result := Assigned(FProperties) and FProperties.ContainsKey(AKey);
end;

function TGocciaDataNode.GetProperty(const AKey: string): TGocciaDataNode;
begin
  if not Assigned(FProperties) or
     not FProperties.TryGetValue(AKey, Result) then
    Result := nil;
end;

procedure TGocciaDataNode.SetProperty(const AKey: string;
  const ANode: TGocciaDataNode);
begin
  FProperties.AddOrSetValue(AKey, ANode);
end;

{ ES2026 §6.1.7 array index: a canonical numeric string below 2^32 - 1 }
function TryParseArrayIndex(const AKey: string; out AIndex: Cardinal): Boolean;
var
  Digit, I: Integer;
  Value: UInt64;
begin
  AIndex := 0;
  Result := False;
  if (AKey = '') or ((Length(AKey) > 1) and (AKey[1] = '0')) then
    Exit;
  Value := 0;
  for I := 1 to Length(AKey) do
  begin
    if (AKey[I] < '0') or (AKey[I] > '9') then
      Exit;
    Digit := Ord(AKey[I]) - Ord('0');
    Value := Value * 10 + UInt64(Digit);
    if Value >= High(Cardinal) then
      Exit;
  end;
  AIndex := Cardinal(Value);
  Result := True;
end;

function TGocciaDataNode.OwnKeys: TArray<string>;
var
  Count, I, J: Integer;
  Index: Cardinal;
  Indices: TArray<Cardinal>;
  Others: TArray<string>;
  Pair: TGocciaDataNodeMap.TKeyValuePair;
begin
  SetLength(Result, 0);
  if not Assigned(FProperties) then
    Exit;
  SetLength(Indices, FProperties.Count);
  SetLength(Others, FProperties.Count);
  Count := 0;
  J := 0;
  for Pair in FProperties do
    if TryParseArrayIndex(Pair.Key, Index) then
    begin
      Indices[Count] := Index;
      Inc(Count);
    end
    else
    begin
      Others[J] := Pair.Key;
      Inc(J);
    end;
  SetLength(Indices, Count);
  TArrayHelper<Cardinal>.Sort(Indices);
  SetLength(Result, Count + J);
  for I := 0 to Count - 1 do
    Result[I] := IntToStr(Indices[I]);
  for I := 0 to J - 1 do
    Result[Count + I] := Others[I];
end;

function TGocciaDataNode.IsContainer: Boolean;
begin
  Result := FKind in [dnkArray, dnkObject];
end;

function TGocciaDataNode.ToText: string;
var
  I: Integer;
begin
  case FKind of
    dnkNull:
      Result := 'null';
    dnkBoolean:
      if FBoolean then
        Result := 'true'
      else
        Result := 'false';
    dnkNumber:
      Result := NumberToString(FNumber);
    dnkString:
      Result := FText;
    dnkArray:
      begin
        // ES2026 §23.1.3.18 Array.prototype.join: null and undefined
        // elements join as the empty string
        Result := '';
        for I := 0 to FItems.Count - 1 do
        begin
          if I > 0 then
            Result := Result + ',';
          if FItems[I].Kind <> dnkNull then
            Result := Result + FItems[I].ToText;
        end;
      end;
    dnkObject:
      begin
        // ES2026 §7.1.1.1 OrdinaryToPrimitive finds neither toString nor
        // valueOf on an object without a prototype
        if Assigned(DataTreeGuards.TypeError) then
          DataTreeGuards.TypeError('Cannot convert object to primitive value');
        raise EGocciaDataTypeError.Create('Cannot convert object to primitive value');
      end;
    dnkTagged:
      Result := TaggedValue.ToText;
  end;
end;

{ TGocciaDataTree }

constructor TGocciaDataTree.Create;
begin
  inherited Create;
  FNodes := TObjectList<TGocciaDataNode>.Create(True);
end;

destructor TGocciaDataTree.Destroy;
begin
  FNodes.Free;
  inherited;
end;

function TGocciaDataTree.Own(const ANode: TGocciaDataNode): TGocciaDataNode;
begin
  try
    FNodes.Add(ANode);
  except
    ANode.Free;
    raise;
  end;
  Result := ANode;
end;

function TGocciaDataTree.NewNull: TGocciaDataNode;
begin
  if not Assigned(FNull) then
    FNull := Own(TGocciaDataNode.Create(dnkNull));
  Result := FNull;
end;

function TGocciaDataTree.NewBoolean(const AValue: Boolean): TGocciaDataNode;
begin
  if AValue then
  begin
    if not Assigned(FTrue) then
    begin
      FTrue := Own(TGocciaDataNode.Create(dnkBoolean));
      FTrue.FBoolean := True;
    end;
    Result := FTrue;
  end
  else
  begin
    if not Assigned(FFalse) then
      FFalse := Own(TGocciaDataNode.Create(dnkBoolean));
    Result := FFalse;
  end;
end;

function TGocciaDataTree.NewNumber(const AValue: Double): TGocciaDataNode;
begin
  Result := Own(TGocciaDataNode.Create(dnkNumber));
  Result.FNumber := AValue;
end;

function TGocciaDataTree.NewString(const AValue: string): TGocciaDataNode;
begin
  Result := Own(TGocciaDataNode.Create(dnkString));
  Result.FText := AValue;
end;

function TGocciaDataTree.NewArray: TGocciaDataNode;
begin
  Result := Own(TGocciaDataNode.Create(dnkArray));
end;

function TGocciaDataTree.NewObject: TGocciaDataNode;
begin
  Result := Own(TGocciaDataNode.Create(dnkObject));
end;

function TGocciaDataTree.NewTagged(const ATagName: string;
  const AValue: TGocciaDataNode): TGocciaDataNode;
begin
  Result := Own(TGocciaDataNode.Create(dnkTagged));
  Result.FText := ATagName;
  Result.FItems.Add(AValue);
end;

{ DataTreeToJSON }

procedure AppendJSONString(var ABuffer: TStringBuffer; const AText: string);
const
  HEX_DIGITS: array[0..15] of Char = '0123456789abcdef';
var
  C: Char;
  I: Integer;
begin
  ABuffer.AppendChar('"');
  for I := 1 to Length(AText) do
  begin
    C := AText[I];
    case C of
      '"':
        ABuffer.Append('\"');
      '\':
        ABuffer.Append('\\');
      #10:
        ABuffer.Append('\n');
      #13:
        ABuffer.Append('\r');
      #9:
        ABuffer.Append('\t');
    else
      // control characters, and the surrogates JSON text cannot carry
      // unpaired, as \u escapes
      if (Ord(C) < 32) or ((Ord(C) >= $D800) and (Ord(C) <= $DFFF)) then
      begin
        ABuffer.Append('\u');
        ABuffer.AppendChar(HEX_DIGITS[(Ord(C) shr 12) and 15]);
        ABuffer.AppendChar(HEX_DIGITS[(Ord(C) shr 8) and 15]);
        ABuffer.AppendChar(HEX_DIGITS[(Ord(C) shr 4) and 15]);
        ABuffer.AppendChar(HEX_DIGITS[Ord(C) and 15]);
      end
      else
        ABuffer.AppendChar(C);
    end;
  end;
  ABuffer.AppendChar('"');
end;

function DataTreeToJSON(const ANode: TGocciaDataNode): string;
var
  Buffer: TStringBuffer;
  Ids: TDictionary<TGocciaDataNode, Integer>;
  Order: TList<TGocciaDataNode>;

  function IdOf(const ANode: TGocciaDataNode): Integer;
  begin
    if not Ids.TryGetValue(ANode, Result) then
    begin
      Result := Order.Count;
      Ids.Add(ANode, Result);
      Order.Add(ANode);
    end;
  end;

var
  Child, Node: TGocciaDataNode;
  I, Index: Integer;
  Pair: TGocciaDataNodeMap.TKeyValuePair;
begin
  Buffer := TStringBuffer.Create;
  Ids := TDictionary<TGocciaDataNode, Integer>.Create;
  Order := TList<TGocciaDataNode>.Create;
  try
    IdOf(ANode);
    Buffer.AppendChar('[');
    // a node is numbered when an entry first names it, so the entry list is
    // its own work queue
    Index := 0;
    while Index < Order.Count do
    begin
      Node := Order[Index];
      if Index > 0 then
        Buffer.AppendChar(',');
      case Node.Kind of
        dnkNull:
          Buffer.Append('null');
        dnkBoolean:
          if Node.BooleanValue then
            Buffer.Append('true')
          else
            Buffer.Append('false');
        dnkNumber:
          if IsNan(Node.NumberValue) or IsInfinite(Node.NumberValue) or
             IsNegativeZero(Node.NumberValue) then
          begin
            if IsNan(Node.NumberValue) then
              Buffer.Append('[0,"NaN"]')
            else if IsInfinite(Node.NumberValue) and (Node.NumberValue > 0) then
              Buffer.Append('[0,"Infinity"]')
            else if IsInfinite(Node.NumberValue) then
              Buffer.Append('[0,"-Infinity"]')
            else
              Buffer.Append('[0,"-0"]');
          end
          else
            Buffer.Append(NumberToString(Node.NumberValue));
        dnkString:
          AppendJSONString(Buffer, Node.StringValue);
        dnkArray:
          begin
            Buffer.Append('[1');
            for I := 0 to Node.Count - 1 do
            begin
              Buffer.AppendChar(',');
              Buffer.Append(IntToStr(IdOf(Node.Items[I])));
            end;
            Buffer.AppendChar(']');
          end;
        dnkObject:
          begin
            Buffer.Append('[2');
            for Pair in Node.Properties do
            begin
              Buffer.AppendChar(',');
              AppendJSONString(Buffer, Pair.Key);
              Buffer.AppendChar(',');
              Buffer.Append(IntToStr(IdOf(Pair.Value)));
            end;
            Buffer.AppendChar(']');
          end;
        dnkTagged:
          begin
            Child := Node.TaggedValue;
            Buffer.Append('[3,');
            AppendJSONString(Buffer, Node.TagName);
            Buffer.AppendChar(',');
            Buffer.Append(IntToStr(IdOf(Child)));
            Buffer.AppendChar(']');
          end;
      end;
      Inc(Index);
    end;
    Buffer.AppendChar(']');
    Result := Buffer.ToString;
  finally
    Order.Free;
    Ids.Free;
  end;
end;

end.

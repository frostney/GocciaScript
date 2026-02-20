unit Goccia.Builtins.GlobalMap;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalMap = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);

    function MapGroupBy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.ArrayValue,
  Goccia.Values.FunctionBase,
  Goccia.Values.MapValue,
  Goccia.Values.NativeFunction;

constructor TGocciaGlobalMap.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(MapGroupBy, 'groupBy', 2));
end;

// ES2026 §24.1.2.1 Map.groupBy(items, callbackfn)
function TGocciaGlobalMap.MapGroupBy(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Items: TGocciaArrayValue;
  Callback: TGocciaValue;
  ResultMap: TGocciaMapValue;
  GroupKey: TGocciaValue;
  GroupArray: TGocciaArrayValue;
  CallArgs: TGocciaArgumentsCollection;
  I, J: Integer;
  Found: Boolean;
begin
  // Step 1: Let groups be GroupBy(items, callbackfn, zero)
  // (Validate arguments first)
  if not (AArgs.GetElement(0) is TGocciaArrayValue) then
    ThrowError('Map.groupBy requires an iterable as first argument', 0, 0);
  if not AArgs.GetElement(1).IsCallable then
    ThrowError('Map.groupBy requires a callback function as second argument', 0, 0);

  Items := TGocciaArrayValue(AArgs.GetElement(0));
  Callback := AArgs.GetElement(1);
  // Step 2: Let map be a new empty Map
  ResultMap := TGocciaMapValue.Create;

  // GroupBy abstract operation: iterate items, call callbackfn for each element
  I := 0;
  while I < Items.Elements.Count do
  begin
    // Step 1a (GroupBy): Let value be items[k]
    // Step 1b (GroupBy): Let key be Call(callbackfn, undefined, « value, k »)
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      CallArgs.Add(Items.Elements[I]);
      CallArgs.Add(TGocciaNumberLiteralValue.SmallInt(I));
      GroupKey := TGocciaFunctionBase(Callback).Call(CallArgs, TGocciaUndefinedLiteralValue.UndefinedValue);
    finally
      CallArgs.Free;
    end;

    // Step 1c (GroupBy): If key matches an existing group, use that group
    Found := False;
    for J := 0 to ResultMap.Entries.Count - 1 do
    begin
      if ResultMap.Entries[J].Key.ToStringLiteral.Value = GroupKey.ToStringLiteral.Value then
      begin
        GroupArray := TGocciaArrayValue(ResultMap.Entries[J].Value);
        Found := True;
        Break;
      end;
    end;

    // Step 1d (GroupBy): Otherwise, create a new group for this key
    if not Found then
    begin
      GroupArray := TGocciaArrayValue.Create;
      ResultMap.SetEntry(GroupKey, GroupArray);
    end;

    // Step 1e (GroupBy): Append value to the group's elements list
    GroupArray.Elements.Add(Items.Elements[I]);
    Inc(I);
  end;

  // Step 3: For each Record { [[Key]], [[Elements]] } g of groups, do
  //   Perform ! CreateDataPropertyOrThrow(map, g.[[Key]], CreateArrayFromList(g.[[Elements]]))
  //   (Already done inline above via SetEntry)
  // Step 4: Return map
  Result := ResultMap;
end;

end.

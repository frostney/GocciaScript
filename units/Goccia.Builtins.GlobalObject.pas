unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.UndefinedValue, Goccia.Values.ObjectValue, Generics.Collections, Goccia.Builtins.Base;

type
  TGocciaGlobalObject = class(TGocciaBuiltin)
  protected
    // Native methods
    function ObjectKeys(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectAssign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectCreate(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectHasOwn(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
  end;

implementation

uses
  Goccia.Values.ArrayValue, Goccia.Values.StringValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue;

constructor TGocciaGlobalObject.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinObject.SetProperty('keys', TGocciaNativeFunctionValue.Create(ObjectKeys, 'keys', 1));
  FBuiltinObject.SetProperty('values', TGocciaNativeFunctionValue.Create(ObjectValues, 'values', 1));
  FBuiltinObject.SetProperty('entries', TGocciaNativeFunctionValue.Create(ObjectEntries, 'entries', 1));
  FBuiltinObject.SetProperty('assign', TGocciaNativeFunctionValue.Create(ObjectAssign, 'assign', -1));
  FBuiltinObject.SetProperty('create', TGocciaNativeFunctionValue.Create(ObjectCreate, 'create', 1));
  FBuiltinObject.SetProperty('hasOwn', TGocciaNativeFunctionValue.Create(ObjectHasOwn, 'hasOwn', 1));

  AScope.SetValue(AName, FBuiltinObject);
end;

function TGocciaGlobalObject.ObjectKeys(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Keys: TGocciaArrayValue;
  Key: string;
begin
  if Args.Count <> 1 then
    ThrowError('Object.keys expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.keys called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Keys := TGocciaArrayValue.Create;

  for Key in Obj.Properties.Keys do
    Keys.Elements.Add(TGocciaStringValue.Create(Key));

  Result := Keys;
end;

function TGocciaGlobalObject.ObjectValues(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Values: TGocciaArrayValue;
  Value: TGocciaValue;
begin
  if Args.Count <> 1 then
    ThrowError('Object.values expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.values called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Values := TGocciaArrayValue.Create;

  for Value in Obj.Properties.Values do
    Values.Elements.Add(Value);

  Result := Values;
end;

function TGocciaGlobalObject.ObjectEntries(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Entries: TGocciaArrayValue;
  Entry: TGocciaArrayValue;
  Pair: TPair<string, TGocciaValue>;
begin
  if Args.Count <> 1 then
    ThrowError('Object.entries expects exactly 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.entries called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  Entries := TGocciaArrayValue.Create;

  for Pair in Obj.Properties do
  begin
    Entry := TGocciaArrayValue.Create;
    Entry.Elements.Add(TGocciaStringValue.Create(Pair.Key));
    Entry.Elements.Add(Pair.Value);
    Entries.Elements.Add(Entry);
  end;

  Result := Entries;
end;

function TGocciaGlobalObject.ObjectAssign(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  InitialObj: TGocciaObjectValue;
  Source: TGocciaObjectValue;
  Pair: TPair<string, TGocciaValue>;
  I: Integer;
begin
  if Args.Count < 2 then
    ThrowError('Object.assign expects at least 2 arguments', 0, 0);

  // TODO: Should check for the first object or filter out non-objects
  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.assign called on non-object', 0, 0);

  InitialObj := TGocciaObjectValue(Args[0]);

  for I := 1 to Args.Count - 1 do
  begin
    if (Args[I] is TGocciaObjectValue) then
    begin
      Source := TGocciaObjectValue(Args[I]);

      // Use pair iteration and SetProperty to safely copy properties
      for Pair in Source.Properties do
        InitialObj.SetProperty(Pair.Key, Pair.Value);
    end;
  end;

  Result := InitialObj;
end;

function TGocciaGlobalObject.ObjectCreate(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  Key: string;
begin
  if Args.Count < 1 then
    ThrowError('Object.create expects at least 1 argument', 0, 0);

  if not (Args[0] is TGocciaObjectValue or Args[0] is TGocciaNullValue) then
    ThrowError('Object.create called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);

  Result := Obj;
end;

function TGocciaGlobalObject.ObjectHasOwn(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaObjectValue;
  PropertyName: string;
begin
  if Args.Count <> 2 then
    ThrowError('Object.hasOwn expects exactly 2 arguments', 0, 0);

  if not (Args[0] is TGocciaObjectValue) then
    ThrowError('Object.hasOwn called on non-object', 0, 0);

  Obj := TGocciaObjectValue(Args[0]);
  PropertyName := Args[1].ToString;

  Result := TGocciaBooleanValue.Create(Obj.HasOwnProperty(PropertyName));
end;

end.
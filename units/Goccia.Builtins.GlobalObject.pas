unit Goccia.Builtins.GlobalObject;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Scope, Goccia.Error, Goccia.Values.NativeFunction, Goccia.Values.Undefined, Goccia.Values.ObjectValue, Generics.Collections;

type
  TGocciaGlobalObject = class
  private
    FName: string;
    FGlobalObject: TGocciaObjectValue;
    FThrowError: TGocciaThrowError;
  public
    constructor Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
    destructor Destroy; override;

    // Native methods
    function ObjectKeys(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectValues(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ObjectEntries(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;

    property Name: string read FName;
    property ThrowError: TGocciaThrowError read FThrowError;
  end;

implementation

uses
  Goccia.Values.ArrayValue, Goccia.Values.StringValue;

constructor TGocciaGlobalObject.Create(const AName: string; AScope: TGocciaScope; AThrowError: TGocciaThrowError);
begin
  FName := AName;
  FGlobalObject := TGocciaObjectValue.Create;

  FGlobalObject.SetProperty('keys', TGocciaNativeFunctionValue.Create(ObjectKeys, 'keys', 1));
  FGlobalObject.SetProperty('values', TGocciaNativeFunctionValue.Create(ObjectValues, 'values', 1));
  FGlobalObject.SetProperty('entries', TGocciaNativeFunctionValue.Create(ObjectEntries, 'entries', 1));

  AScope.SetValue(AName, FGlobalObject);
end;

destructor TGocciaGlobalObject.Destroy;
begin
  FGlobalObject.Free;
  inherited;
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


end.
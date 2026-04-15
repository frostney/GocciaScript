unit Goccia.Values.URLSearchParamsValue;

// WHATWG URL Standard §6 URLSearchParams
// https://url.spec.whatwg.org/#interface-urlsearchparams

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // A single name=value pair in URLSearchParams
  TGocciaURLSearchParam = record
    Name: string;
    Value: string;
  end;

  TGocciaURLSearchParamList = TList<TGocciaURLSearchParam>;

  TGocciaURLSearchParamsValue = class(TGocciaInstanceValue)
  private
    FList: TGocciaURLSearchParamList;
    // Back-reference to the owning URL object for update notifications.
    // Stored as TObject to avoid a circular interface-section dependency.
    // In practice this is a TGocciaURLValue; cast in the implementation.
    FURLRef: TObject;
  private
    procedure InitializePrototype;
    procedure UpdateURL;
  public
    function URLSearchParamsAppend(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsDelete(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsGet(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsGetAll(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsHas(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsSet(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsSort(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsToString(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsKeys(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsValues(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsEntries(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsForEach(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsSymbolIterator(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function URLSearchParamsSizeGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    // Initialize from a query string (without or with leading '?')
    procedure ParseFromString(const AQuery: string);
    // Initialize from an object's own enumerable properties
    procedure ParseFromObject(const AObj: TGocciaObjectValue);
    // Initialize from an array of [name, value] pairs
    procedure ParseFromArray(const AArr: TGocciaValue);

    // Serialize to application/x-www-form-urlencoded
    function Serialize: string;

    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string;
      const AThisContext: TGocciaValue): TGocciaValue; override;
    function ToStringTag: string; override;

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property List: TGocciaURLSearchParamList read FList;
    // Set to a TGocciaURLValue to receive mutation notifications
    property URLRef: TObject read FURLRef write FURLRef;
  end;

implementation

uses
  SysUtils,

  StringBuffer,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.URL.Parser,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.URLValue;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

{ TGocciaURLSearchParamsValue }

constructor TGocciaURLSearchParamsValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create(AClass);
  FList := TGocciaURLSearchParamList.Create;
  FURLRef := nil;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

destructor TGocciaURLSearchParamsValue.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TGocciaURLSearchParamsValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod(PROP_APPEND, URLSearchParamsAppend, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_DELETE, URLSearchParamsDelete, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_GET, URLSearchParamsGet, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_GET_ALL, URLSearchParamsGetAll, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_HAS, URLSearchParamsHas, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_SET, URLSearchParamsSet, 2,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_SORT, URLSearchParamsSort, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_TO_STRING, URLSearchParamsToString, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_KEYS, URLSearchParamsKeys, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_VALUES, URLSearchParamsValues, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_ENTRIES, URLSearchParamsEntries, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_FOR_EACH, URLSearchParamsForEach, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      // WHATWG URLSearchParams: size is a non-enumerable prototype accessor
      Members.AddAccessor(PROP_SIZE,
        URLSearchParamsSizeGetter, nil,
        [pfConfigurable]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]',
        URLSearchParamsSymbolIterator,
        0,
        [pfConfigurable, pfWritable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaURLSearchParamsValue.ExposePrototype(
  const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaURLSearchParamsValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

// ---------------------------------------------------------------------------
// Serialization / parsing
// ---------------------------------------------------------------------------

// WHATWG URL §5.1 application/x-www-form-urlencoded serializer
function TGocciaURLSearchParamsValue.Serialize: string;
var
  I: Integer;
  Buffer: TStringBuffer;
begin
  if FList.Count = 0 then
  begin
    Result := '';
    Exit;
  end;
  Buffer := TStringBuffer.Create(FList.Count * 16);
  for I := 0 to FList.Count - 1 do
  begin
    if I > 0 then Buffer.AppendChar('&');
    Buffer.Append(SerializeFormEncoded(FList[I].Name, FList[I].Value));
  end;
  Result := Buffer.ToString;
end;

// WHATWG URL §5.2 application/x-www-form-urlencoded parser
procedure TGocciaURLSearchParamsValue.ParseFromString(const AQuery: string);
var
  Names, Values: TArray<string>;
  I: Integer;
  Param: TGocciaURLSearchParam;
  Input: string;
begin
  FList.Clear;
  Input := AQuery;
  // Strip leading '?'
  if (Length(Input) > 0) and (Input[1] = '?') then
    Delete(Input, 1, 1);

  ParseFormEncoded(Input, Names, Values);
  for I := 0 to High(Names) do
  begin
    Param.Name := Names[I];
    Param.Value := Values[I];
    FList.Add(Param);
  end;
end;

procedure TGocciaURLSearchParamsValue.ParseFromObject(
  const AObj: TGocciaObjectValue);
var
  Key: string;
  Param: TGocciaURLSearchParam;
  Val: TGocciaValue;
begin
  FList.Clear;
  for Key in AObj.GetOwnPropertyKeys do
  begin
    Val := AObj.GetProperty(Key);
    Param.Name := Key;
    if Assigned(Val) then
      Param.Value := Val.ToStringLiteral.Value
    else
      Param.Value := '';
    FList.Add(Param);
  end;
end;

procedure TGocciaURLSearchParamsValue.ParseFromArray(const AArr: TGocciaValue);
var
  Outer, Inner: TGocciaArrayValue;
  I: Integer;
  Param: TGocciaURLSearchParam;
begin
  FList.Clear;
  if not (AArr is TGocciaArrayValue) then Exit;
  Outer := TGocciaArrayValue(AArr);
  // WHATWG URL §5.2: each pair must contain exactly 2 elements per sequence<sequence<USVString>>
  for I := 0 to Outer.Elements.Count - 1 do
  begin
    if not Assigned(Outer.Elements[I]) or not (Outer.Elements[I] is TGocciaArrayValue) then
      ThrowTypeError('Failed to construct URLSearchParams: The provided value cannot be converted to a sequence');
    Inner := TGocciaArrayValue(Outer.Elements[I]);
    // WHATWG URL §5.2: each pair must contain exactly two items
    if Inner.Elements.Count <> 2 then
      ThrowTypeError('Failed to construct URLSearchParams: Sequence initializer must contain pairs with exactly two items');
    Param.Name := '';
    Param.Value := '';
    if Assigned(Inner.Elements[0]) then
      Param.Name := Inner.Elements[0].ToStringLiteral.Value;
    if Assigned(Inner.Elements[1]) then
      Param.Value := Inner.Elements[1].ToStringLiteral.Value;
    FList.Add(Param);
  end;
end;

// Notify the owning URL that our params have changed
procedure TGocciaURLSearchParamsValue.UpdateURL;
begin
  if Assigned(FURLRef) and (FURLRef is TGocciaURLValue) then
    TGocciaURLValue(FURLRef).SetSearchFromParams(Self);
end;

// ---------------------------------------------------------------------------
// Prototype methods
// ---------------------------------------------------------------------------

// WHATWG URL §6.2 URLSearchParams.prototype.append(name, value)
function TGocciaURLSearchParamsValue.URLSearchParamsAppend(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Param: TGocciaURLSearchParam;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.append: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  if AArgs.Length < 2 then Exit;
  Param.Name := AArgs.GetElement(0).ToStringLiteral.Value;
  Param.Value := AArgs.GetElement(1).ToStringLiteral.Value;
  Self_.FList.Add(Param);
  Self_.UpdateURL;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.delete(name[, value])
function TGocciaURLSearchParamsValue.URLSearchParamsDelete(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Name, Value: string;
  HasValue: Boolean;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.delete: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  if AArgs.Length = 0 then Exit;
  Name := AArgs.GetElement(0).ToStringLiteral.Value;
  HasValue := AArgs.Length >= 2;
  if HasValue then
    Value := AArgs.GetElement(1).ToStringLiteral.Value
  else
    Value := '';

  I := 0;
  while I < Self_.FList.Count do
  begin
    if Self_.FList[I].Name = Name then
    begin
      if HasValue and (Self_.FList[I].Value <> Value) then
      begin
        Inc(I);
        Continue;
      end;
      Self_.FList.Delete(I);
    end
    else
      Inc(I);
  end;
  Self_.UpdateURL;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.get(name) → string or null
function TGocciaURLSearchParamsValue.URLSearchParamsGet(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Name: string;
  I: Integer;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.get: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  if AArgs.Length = 0 then
  begin
    Result := TGocciaNullLiteralValue.NullValue;
    Exit;
  end;
  Name := AArgs.GetElement(0).ToStringLiteral.Value;
  for I := 0 to Self_.FList.Count - 1 do
    if Self_.FList[I].Name = Name then
    begin
      Result := TGocciaStringLiteralValue.Create(Self_.FList[I].Value);
      Exit;
    end;
  Result := TGocciaNullLiteralValue.NullValue;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.getAll(name) → string[]
function TGocciaURLSearchParamsValue.URLSearchParamsGetAll(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Name: string;
  Arr: TGocciaArrayValue;
  I: Integer;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.getAll: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  Arr := TGocciaArrayValue.Create;
  if AArgs.Length > 0 then
  begin
    Name := AArgs.GetElement(0).ToStringLiteral.Value;
    for I := 0 to Self_.FList.Count - 1 do
      if Self_.FList[I].Name = Name then
        Arr.Elements.Add(TGocciaStringLiteralValue.Create(Self_.FList[I].Value));
  end;
  Result := Arr;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.has(name[, value]) → boolean
function TGocciaURLSearchParamsValue.URLSearchParamsHas(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Name, Value: string;
  HasValue: Boolean;
  I: Integer;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.has: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  if AArgs.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;
  Name := AArgs.GetElement(0).ToStringLiteral.Value;
  HasValue := AArgs.Length >= 2;
  if HasValue then
    Value := AArgs.GetElement(1).ToStringLiteral.Value
  else
    Value := '';

  for I := 0 to Self_.FList.Count - 1 do
    if Self_.FList[I].Name = Name then
    begin
      if not HasValue or (Self_.FList[I].Value = Value) then
      begin
        Result := TGocciaBooleanLiteralValue.TrueValue;
        Exit;
      end;
    end;
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.set(name, value)
function TGocciaURLSearchParamsValue.URLSearchParamsSet(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Name, Value: string;
  Found: Boolean;
  Param: TGocciaURLSearchParam;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.set: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  if AArgs.Length < 2 then Exit;
  Name := AArgs.GetElement(0).ToStringLiteral.Value;
  Value := AArgs.GetElement(1).ToStringLiteral.Value;

  Found := False;
  I := 0;
  while I < Self_.FList.Count do
  begin
    if Self_.FList[I].Name = Name then
    begin
      if not Found then
      begin
        Param := Self_.FList[I];
        Param.Value := Value;
        Self_.FList[I] := Param;
        Found := True;
        Inc(I);
      end
      else
        Self_.FList.Delete(I);
    end
    else
      Inc(I);
  end;

  if not Found then
  begin
    Param.Name := Name;
    Param.Value := Value;
    Self_.FList.Add(Param);
  end;
  Self_.UpdateURL;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.sort()
function TGocciaURLSearchParamsValue.URLSearchParamsSort(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  I, J: Integer;
  Temp: TGocciaURLSearchParam;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.sort: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);

  // Stable sort by name (Unicode code unit order) — insertion sort
  for I := 1 to Self_.FList.Count - 1 do
  begin
    Temp := Self_.FList[I];
    J := I - 1;
    while (J >= 0) and (Self_.FList[J].Name > Temp.Name) do
    begin
      Self_.FList[J + 1] := Self_.FList[J];
      Dec(J);
    end;
    Self_.FList[J + 1] := Temp;
  end;
  Self_.UpdateURL;
end;

// WHATWG URL §6.2 URLSearchParams.prototype.toString()
function TGocciaURLSearchParamsValue.URLSearchParamsToString(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.toString: not a URLSearchParams');
  Result := TGocciaStringLiteralValue.Create(
    TGocciaURLSearchParamsValue(AThisValue).Serialize);
end;

// WHATWG URL §6.2 URLSearchParams.prototype.keys()
function TGocciaURLSearchParamsValue.URLSearchParamsKeys(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.keys: not a URLSearchParams');
  Result := TGocciaURLSearchParamsIteratorValue.Create(AThisValue, spikKeys);
end;

// WHATWG URL §6.2 URLSearchParams.prototype.values()
function TGocciaURLSearchParamsValue.URLSearchParamsValues(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.values: not a URLSearchParams');
  Result := TGocciaURLSearchParamsIteratorValue.Create(AThisValue, spikValues);
end;

// WHATWG URL §6.2 URLSearchParams.prototype.entries()
function TGocciaURLSearchParamsValue.URLSearchParamsEntries(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.entries: not a URLSearchParams');
  Result := TGocciaURLSearchParamsIteratorValue.Create(AThisValue, spikEntries);
end;

// WHATWG URL §6.2 URLSearchParams.prototype.forEach(callbackFn[, thisArg])
function TGocciaURLSearchParamsValue.URLSearchParamsForEach(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Self_: TGocciaURLSearchParamsValue;
  Callback, ThisArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  Pair: TGocciaURLSearchParam;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams.prototype.forEach: not a URLSearchParams');
  Self_ := TGocciaURLSearchParamsValue(AThisValue);
  // §6.2: callbackFn is required; GetElement(0) returns undefined when absent,
  // which then fails the IsCallable check and throws TypeError as required.
  Callback := AArgs.GetElement(0);
  // §6.2 step 3: if callbackFn is not callable, throw TypeError
  if not Callback.IsCallable then
    ThrowTypeError('URLSearchParams.prototype.forEach: callback is not a function');

  // §6.2 step 4: optional thisArg (second argument)
  if AArgs.Length >= 2 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  // §6.2: snapshot each pair before invoking the callback so that
  // mutations to FList during the callback (append/delete/set) do not
  // cause out-of-bounds access on a now-shorter list.
  I := 0;
  while I < Self_.FList.Count do
  begin
    Pair := Self_.FList[I];
    CallArgs := TGocciaArgumentsCollection.Create([
      TGocciaStringLiteralValue.Create(Pair.Value),
      TGocciaStringLiteralValue.Create(Pair.Name),
      AThisValue]);
    try
      InvokeCallable(Callback, CallArgs, ThisArg);
    finally
      CallArgs.Free;
    end;
    Inc(I);
  end;
end;

// WHATWG URL §6.2 size getter
function TGocciaURLSearchParamsValue.URLSearchParamsSizeGetter(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams size getter: not a URLSearchParams');
  Result := TGocciaNumberLiteralValue.Create(
    TGocciaURLSearchParamsValue(AThisValue).FList.Count);
end;

// [Symbol.iterator] → same as entries()
function TGocciaURLSearchParamsValue.URLSearchParamsSymbolIterator(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaURLSearchParamsValue) then
    ThrowTypeError('URLSearchParams[Symbol.iterator]: not a URLSearchParams');
  Result := TGocciaURLSearchParamsIteratorValue.Create(AThisValue, spikEntries);
end;

// ---------------------------------------------------------------------------
// Property access
// ---------------------------------------------------------------------------

function TGocciaURLSearchParamsValue.GetProperty(
  const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaURLSearchParamsValue.GetPropertyWithContext(const AName: string;
  const AThisContext: TGocciaValue): TGocciaValue;
begin
  Result := inherited GetPropertyWithContext(AName, AThisContext);
end;

function TGocciaURLSearchParamsValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_URL_SEARCH_PARAMS;
end;

procedure TGocciaURLSearchParamsValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  Init: TGocciaValue;
begin
  if AArguments.Length = 0 then Exit;
  Init := AArguments.GetElement(0);
  // WHATWG Web IDL: explicit undefined receives the default value (empty string),
  // producing an empty list — same as calling the constructor with no argument.
  if Init is TGocciaUndefinedLiteralValue then Exit;
  if Init is TGocciaStringLiteralValue then
    ParseFromString(TGocciaStringLiteralValue(Init).Value)
  else if Init is TGocciaURLSearchParamsValue then
  begin
    // Copy from existing URLSearchParams
    FList.AddRange(TGocciaURLSearchParamsValue(Init).FList);
  end
  else if Init is TGocciaArrayValue then
    ParseFromArray(Init)
  else if Init is TGocciaObjectValue then
    ParseFromObject(TGocciaObjectValue(Init))
  else
  begin
    // §5.2: coerce non-object, non-string, non-array init to USVString (e.g. numbers)
    ParseFromString(Init.ToStringLiteral.Value);
  end;
end;

procedure TGocciaURLSearchParamsValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  // Mark the owning URL so it stays live as long as this URLSearchParams is
  // reachable, preventing use-after-free when the user holds only a direct
  // reference to a url.searchParams object.
  if Assigned(FURLRef) and (FURLRef is TGocciaURLValue) then
    TGocciaURLValue(FURLRef).MarkReferences;
end;

end.

unit Goccia.Values.HeadersValue;

// Fetch API Headers
// https://fetch.spec.whatwg.org/#headers-class

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
  TGocciaHeaderEntry = record
    Name: string;   // stored lowercase
    Value: string;
  end;

  TGocciaHeadersValue = class(TGocciaInstanceValue)
  private
    FEntries: TList<TGocciaHeaderEntry>;
    FImmutable: Boolean;

    // Prototype methods
    function HeadersGet(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HeadersHas(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HeadersForEach(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HeadersEntries(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HeadersKeys(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HeadersValues(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function HeadersSymbolIterator(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    // Internal helpers for populating from HTTP response
    procedure AddHeader(const AName, AValue: string);
    function GetHeader(const AName: string): string;

    function ToStringTag: string; override;
    procedure MarkReferences; override;

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Entries: TList<TGocciaHeaderEntry> read FEntries;
    property Immutable: Boolean read FImmutable write FImmutable;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue;

threadvar
  FShared: TGocciaSharedPrototype;
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

{ TGocciaHeadersValue }

constructor TGocciaHeadersValue.Create(const AClass: TGocciaClassValue);
begin
  inherited Create(AClass);
  FEntries := TList<TGocciaHeaderEntry>.Create;
  FImmutable := False;
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

destructor TGocciaHeadersValue.Destroy;
begin
  FEntries.Free;
  inherited;
end;

procedure TGocciaHeadersValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddNamedMethod(PROP_GET, HeadersGet, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_HAS, HeadersHas, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_FOR_EACH, HeadersForEach, 1,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_ENTRIES, HeadersEntries, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_KEYS, HeadersKeys, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_VALUES, HeadersValues, 0,
        gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolMethod(
        TGocciaSymbolValue.WellKnownIterator,
        '[Symbol.iterator]', HeadersSymbolIterator, 0,
        [pfConfigurable, pfWritable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaHeadersValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaHeadersValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

procedure TGocciaHeadersValue.AddHeader(const AName, AValue: string);
var
  Entry: TGocciaHeaderEntry;
begin
  Entry.Name := LowerCase(AName);
  Entry.Value := AValue;
  FEntries.Add(Entry);
end;

function TGocciaHeadersValue.GetHeader(const AName: string): string;
var
  I: Integer;
  Lower: string;
begin
  Result := '';
  Lower := LowerCase(AName);
  for I := 0 to FEntries.Count - 1 do
    if FEntries[I].Name = Lower then
    begin
      // Fetch spec: combine multiple values with ', '
      if Result = '' then
        Result := FEntries[I].Value
      else
        Result := Result + ', ' + FEntries[I].Value;
    end;
end;

// ---------------------------------------------------------------------------
// Constructor
// ---------------------------------------------------------------------------

procedure TGocciaHeadersValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  InitArg: TGocciaValue;
  Obj: TGocciaObjectValue;
  PropNames: TArray<string>;
  I: Integer;
  Entry: TGocciaHeaderEntry;
begin
  if AArguments.Length = 0 then Exit;

  InitArg := AArguments.GetElement(0);
  if (InitArg is TGocciaUndefinedLiteralValue) or
     (InitArg is TGocciaNullLiteralValue) then
    Exit;

  if InitArg is TGocciaHeadersValue then
  begin
    // Copy from another Headers instance
    for I := 0 to TGocciaHeadersValue(InitArg).FEntries.Count - 1 do
      FEntries.Add(TGocciaHeadersValue(InitArg).FEntries[I]);
  end
  else if InitArg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(InitArg);
    PropNames := Obj.GetAllPropertyNames;
    for I := 0 to High(PropNames) do
    begin
      Entry.Name := LowerCase(PropNames[I]);
      Entry.Value := Obj.GetProperty(PropNames[I]).ToStringLiteral.Value;
      FEntries.Add(Entry);
    end;
  end;
end;

// ---------------------------------------------------------------------------
// Utility
// ---------------------------------------------------------------------------

function HeaderExists(const AHeaders: TGocciaHeadersValue; const AName: string): Boolean;
var
  I: Integer;
  Lower: string;
begin
  Result := False;
  Lower := LowerCase(AName);
  for I := 0 to AHeaders.Entries.Count - 1 do
    if AHeaders.Entries[I].Name = Lower then
    begin
      Result := True;
      Exit;
    end;
end;

// ---------------------------------------------------------------------------
// Prototype methods
// ---------------------------------------------------------------------------

function TGocciaHeadersValue.HeadersGet(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  H: TGocciaHeadersValue;
  Name, Combined: string;
begin
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  H := TGocciaHeadersValue(AThisValue);
  Name := AArgs.GetElement(0).ToStringLiteral.Value;
  Combined := H.GetHeader(Name);
  if Combined = '' then
  begin
    // Check if the header actually exists with an empty value
    if not HeaderExists(H, Name) then
    begin
      Result := TGocciaNullLiteralValue.NullValue;
      Exit;
    end;
  end;
  Result := TGocciaStringLiteralValue.Create(Combined);
end;

function TGocciaHeadersValue.HeadersHas(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  H: TGocciaHeadersValue;
begin
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  H := TGocciaHeadersValue(AThisValue);
  if HeaderExists(H, AArgs.GetElement(0).ToStringLiteral.Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaHeadersValue.HeadersForEach(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  H: TGocciaHeadersValue;
  Callback, ThisArg: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  I: Integer;
begin
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  H := TGocciaHeadersValue(AThisValue);

  Callback := AArgs.GetElement(0);
  if not Callback.IsCallable then
    ThrowTypeError(SErrorHeadersForEachNotCallable, SSuggestCallbackRequired);

  if AArgs.Length >= 2 then
    ThisArg := AArgs.GetElement(1)
  else
    ThisArg := TGocciaUndefinedLiteralValue.UndefinedValue;

  I := 0;
  while I < H.FEntries.Count do
  begin
    CallArgs := TGocciaArgumentsCollection.Create([
      TGocciaStringLiteralValue.Create(H.FEntries[I].Value),
      TGocciaStringLiteralValue.Create(H.FEntries[I].Name),
      AThisValue]);
    try
      InvokeCallable(Callback, CallArgs, ThisArg);
    finally
      CallArgs.Free;
    end;
    Inc(I);
  end;
end;

function TGocciaHeadersValue.HeadersEntries(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  Result := TGocciaHeadersIteratorValue.Create(AThisValue, hikEntries);
end;

function TGocciaHeadersValue.HeadersKeys(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  Result := TGocciaHeadersIteratorValue.Create(AThisValue, hikKeys);
end;

function TGocciaHeadersValue.HeadersValues(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  Result := TGocciaHeadersIteratorValue.Create(AThisValue, hikValues);
end;

function TGocciaHeadersValue.HeadersSymbolIterator(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaHeadersValue) then
    ThrowTypeError(SErrorHeadersNotHeaders, SSuggestHeadersThisType);
  Result := TGocciaHeadersIteratorValue.Create(AThisValue, hikEntries);
end;

// ---------------------------------------------------------------------------
// GC / Boilerplate
// ---------------------------------------------------------------------------

function TGocciaHeadersValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_HEADERS;
end;

procedure TGocciaHeadersValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  // FEntries contains only plain strings — no TGocciaValue references
end;

end.

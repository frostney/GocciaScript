unit Goccia.Values.ObjectPropertyDescriptor;

{$I Goccia.inc}

interface

uses
  OrderedStringMap,

  Goccia.Values.Primitives;

type
  TGocciaLazyPropertyFactory = function: TGocciaValue of object;

  // Flags store resolved descriptor attribute values; Fields store which
  // descriptor keys were explicitly present in a partial descriptor.
  TPropertyFlag = (pfEnumerable, pfConfigurable, pfWritable);
  TPropertyFlags = set of TPropertyFlag;
  TPropertyDescriptorField = (
    pdfEnumerable, pdfConfigurable, pdfWritable,
    pdfValue, pdfGet, pdfSet
  );
  TPropertyDescriptorFields = set of TPropertyDescriptorField;

  TGocciaPropertyDescriptor = class
  private
    FFlags: TPropertyFlags;
    FFields: TPropertyDescriptorFields;
    function GetEnumerable: Boolean; inline;
    function GetConfigurable: Boolean; inline;
    function GetWritable: Boolean; inline;
    function GetHasEnumerableField: Boolean; inline;
    function GetHasConfigurableField: Boolean; inline;
    function GetHasWritableField: Boolean; inline;
    function GetHasValue: Boolean; inline;
    function GetHasGet: Boolean; inline;
    function GetHasSet: Boolean; inline;
  public
    constructor Create(const AFlags: TPropertyFlags;
      const AFields: TPropertyDescriptorFields);
    procedure MarkValues; virtual;

    property Flags: TPropertyFlags read FFlags;
    property Fields: TPropertyDescriptorFields read FFields;
    property Enumerable: Boolean read GetEnumerable;
    property Configurable: Boolean read GetConfigurable;
    property Writable: Boolean read GetWritable;
    property HasEnumerableField: Boolean read GetHasEnumerableField;
    property HasConfigurableField: Boolean read GetHasConfigurableField;
    property HasWritableField: Boolean read GetHasWritableField;
    property HasValue: Boolean read GetHasValue;
    property HasGet: Boolean read GetHasGet;
    property HasSet: Boolean read GetHasSet;
  end;

  TGocciaPropertyDescriptorData = class(TGocciaPropertyDescriptor)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);
    constructor CreatePartial(const AValue: TGocciaValue;
      const AFlags: TPropertyFlags; const AFields: TPropertyDescriptorFields);
    procedure MarkValues; override;

    property Value: TGocciaValue read FValue write FValue;
  end;

  TGocciaLazyPropertyDescriptorData = class(TGocciaPropertyDescriptorData)
  private
    FFactory: TGocciaLazyPropertyFactory;
    FMaterialized: Boolean;
  public
    constructor Create(const AFactory: TGocciaLazyPropertyFactory;
      const AFlags: TPropertyFlags);
    function Materialize: TGocciaValue;
  end;

  TGocciaPropertyDescriptorAccessor = class(TGocciaPropertyDescriptor)
  private
    FGetter: TGocciaValue;
    FSetter: TGocciaValue;

    function GetWritable: Boolean; inline;
  public
    constructor Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);
    constructor CreatePartial(const AGetter: TGocciaValue;
      const ASetter: TGocciaValue; const AFlags: TPropertyFlags;
      const AFields: TPropertyDescriptorFields);
    procedure MarkValues; override;

    property Getter: TGocciaValue read FGetter;
    property Setter: TGocciaValue read FSetter;
  end;

  TGocciaPropertyMap = TOrderedStringMap<TGocciaPropertyDescriptor>;

// ES2026 §6.2.5.5 ToPropertyDescriptor(Obj)
// ADescriptorObject must be a TGocciaObjectValue (caller validates).
function ToPropertyDescriptor(
  const ADescriptorObject: TGocciaValue;
  const AExistingDescriptor: TGocciaPropertyDescriptor
): TGocciaPropertyDescriptor;
function IsDataDescriptor(const ADescriptor: TGocciaPropertyDescriptor): Boolean;
function IsAccessorDescriptor(const ADescriptor: TGocciaPropertyDescriptor): Boolean;
function IsGenericDescriptor(const ADescriptor: TGocciaPropertyDescriptor): Boolean;
function ClonePropertyDescriptor(const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue;

constructor TGocciaPropertyDescriptor.Create(const AFlags: TPropertyFlags;
  const AFields: TPropertyDescriptorFields);
begin
  FFlags := AFlags;
  FFields := AFields;
end;

function TGocciaPropertyDescriptor.GetEnumerable: Boolean;
begin
  Result := pfEnumerable in FFlags;
end;

function TGocciaPropertyDescriptor.GetConfigurable: Boolean;
begin
  Result := pfConfigurable in FFlags;
end;

function TGocciaPropertyDescriptor.GetWritable: Boolean;
begin
  Result := pfWritable in FFlags;
end;

function TGocciaPropertyDescriptor.GetHasEnumerableField: Boolean;
begin
  Result := pdfEnumerable in FFields;
end;

function TGocciaPropertyDescriptor.GetHasConfigurableField: Boolean;
begin
  Result := pdfConfigurable in FFields;
end;

function TGocciaPropertyDescriptor.GetHasWritableField: Boolean;
begin
  Result := pdfWritable in FFields;
end;

function TGocciaPropertyDescriptor.GetHasValue: Boolean;
begin
  Result := pdfValue in FFields;
end;

function TGocciaPropertyDescriptor.GetHasGet: Boolean;
begin
  Result := pdfGet in FFields;
end;

function TGocciaPropertyDescriptor.GetHasSet: Boolean;
begin
  Result := pdfSet in FFields;
end;

procedure TGocciaPropertyDescriptor.MarkValues;
begin
  // No-op base: subclasses override to mark their value references
end;

constructor TGocciaPropertyDescriptorData.Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags,
    [pdfEnumerable, pdfConfigurable, pdfWritable, pdfValue]);
  FValue := AValue;
end;

constructor TGocciaPropertyDescriptorData.CreatePartial(
  const AValue: TGocciaValue; const AFlags: TPropertyFlags;
  const AFields: TPropertyDescriptorFields);
begin
  inherited Create(AFlags, AFields);
  FValue := AValue;
end;

procedure TGocciaPropertyDescriptorData.MarkValues;
begin
  if Assigned(FValue) then
    FValue.MarkReferences;
end;

constructor TGocciaLazyPropertyDescriptorData.Create(
  const AFactory: TGocciaLazyPropertyFactory; const AFlags: TPropertyFlags);
begin
  inherited Create(TGocciaUndefinedLiteralValue.UndefinedValue, AFlags);
  FFactory := AFactory;
  FMaterialized := False;
end;

function TGocciaLazyPropertyDescriptorData.Materialize: TGocciaValue;
begin
  if not FMaterialized then
  begin
    if TMethod(FFactory).Code <> nil then
      Value := FFactory()
    else
      Value := TGocciaUndefinedLiteralValue.UndefinedValue;
    FMaterialized := True;
  end;
  Result := Value;
end;

constructor TGocciaPropertyDescriptorAccessor.Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags, [pdfEnumerable, pdfConfigurable, pdfGet, pdfSet]);
  FGetter := AGetter;
  FSetter := ASetter;
end;

constructor TGocciaPropertyDescriptorAccessor.CreatePartial(
  const AGetter: TGocciaValue; const ASetter: TGocciaValue;
  const AFlags: TPropertyFlags; const AFields: TPropertyDescriptorFields);
begin
  inherited Create(AFlags, AFields);
  FGetter := AGetter;
  FSetter := ASetter;
end;

procedure TGocciaPropertyDescriptorAccessor.MarkValues;
begin
  if Assigned(FGetter) then
    FGetter.MarkReferences;
  if Assigned(FSetter) then
    FSetter.MarkReferences;
end;

function TGocciaPropertyDescriptorAccessor.GetWritable: Boolean;
begin
  Result := FSetter <> nil;
end;

function IsDataDescriptor(const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := Assigned(ADescriptor) and
    ((pdfValue in ADescriptor.Fields) or (pdfWritable in ADescriptor.Fields));
end;

function IsAccessorDescriptor(const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := Assigned(ADescriptor) and
    ((pdfGet in ADescriptor.Fields) or (pdfSet in ADescriptor.Fields));
end;

function IsGenericDescriptor(const ADescriptor: TGocciaPropertyDescriptor): Boolean;
begin
  Result := Assigned(ADescriptor) and
    not IsDataDescriptor(ADescriptor) and
    not IsAccessorDescriptor(ADescriptor);
end;

function ClonePropertyDescriptor(
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
begin
  if ADescriptor is TGocciaLazyPropertyDescriptorData then
    Result := TGocciaPropertyDescriptorData.CreatePartial(
      TGocciaLazyPropertyDescriptorData(ADescriptor).Materialize,
      ADescriptor.Flags,
      ADescriptor.Fields)
  else if ADescriptor is TGocciaPropertyDescriptorData then
    Result := TGocciaPropertyDescriptorData.CreatePartial(
      TGocciaPropertyDescriptorData(ADescriptor).Value,
      ADescriptor.Flags,
      ADescriptor.Fields)
  else if ADescriptor is TGocciaPropertyDescriptorAccessor then
    Result := TGocciaPropertyDescriptorAccessor.CreatePartial(
      TGocciaPropertyDescriptorAccessor(ADescriptor).Getter,
      TGocciaPropertyDescriptorAccessor(ADescriptor).Setter,
      ADescriptor.Flags,
      ADescriptor.Fields)
  else
    Result := TGocciaPropertyDescriptor.Create(ADescriptor.Flags,
      ADescriptor.Fields);
end;

// ES2026 §6.2.5.5 ToPropertyDescriptor(Obj)
function ToPropertyDescriptor(
  const ADescriptorObject: TGocciaValue;
  const AExistingDescriptor: TGocciaPropertyDescriptor
): TGocciaPropertyDescriptor;
var
  DescObj: TGocciaObjectValue;
  Enumerable, Configurable, Writable: Boolean;
  Value, Getter, Setter: TGocciaValue;
  PropertyFlags: TPropertyFlags;
  DescriptorFields: TPropertyDescriptorFields;
  HasValue, HasGet, HasSet, HasWritableField: Boolean;
  HasEnumerableField, HasConfigurableField: Boolean;
begin
  // ES2026 §6.2.5.5 step 1: If Desc is not an Object, throw a TypeError
  if not (ADescriptorObject is TGocciaObjectValue) then
    ThrowTypeError(SErrorPropertyDescriptorMustBeObject, SSuggestPropertyDescriptorObject);

  DescObj := TGocciaObjectValue(ADescriptorObject);

  Enumerable := False;
  Configurable := False;
  Writable := False;
  Value := nil;
  Getter := nil;
  Setter := nil;

  // ES2026 §6.2.5.5 steps 3–9: extract descriptor fields
  HasEnumerableField := DescObj.HasProperty(PROP_ENUMERABLE);
  HasConfigurableField := DescObj.HasProperty(PROP_CONFIGURABLE);
  HasValue := DescObj.HasProperty(PROP_VALUE);
  HasGet := DescObj.HasProperty(PROP_GET);
  HasSet := DescObj.HasProperty(PROP_SET);
  HasWritableField := DescObj.HasProperty(PROP_WRITABLE);

  if HasEnumerableField then
    Enumerable := DescObj.GetProperty(PROP_ENUMERABLE).ToBooleanLiteral.Value;
  if HasConfigurableField then
    Configurable := DescObj.GetProperty(PROP_CONFIGURABLE).ToBooleanLiteral.Value;
  if HasValue then
    Value := DescObj.GetProperty(PROP_VALUE);
  if HasWritableField then
    Writable := DescObj.GetProperty(PROP_WRITABLE).ToBooleanLiteral.Value;

  // ES2026 §6.2.5.5 step 7: validate getter
  if HasGet then
  begin
    Getter := DescObj.GetProperty(PROP_GET);
    if not (Getter is TGocciaUndefinedLiteralValue) and not Getter.IsCallable then
      ThrowTypeError(SErrorGetterMustBeFunctionOrUndefined, SSuggestNotFunctionType);
    if Getter is TGocciaUndefinedLiteralValue then
      Getter := nil;
  end;

  // ES2026 §6.2.5.5 step 8: validate setter
  if HasSet then
  begin
    Setter := DescObj.GetProperty(PROP_SET);
    if not (Setter is TGocciaUndefinedLiteralValue) and not Setter.IsCallable then
      ThrowTypeError(SErrorSetterMustBeFunctionOrUndefined, SSuggestNotFunctionType);
    if Setter is TGocciaUndefinedLiteralValue then
      Setter := nil;
  end;

  // ES2026 §6.2.5.5 step 10: mixed data+accessor is invalid
  if (HasValue or HasWritableField) and (HasGet or HasSet) then
    ThrowTypeError(SErrorDescriptorMixedAccessorData, SSuggestPropertyDescriptorObject);

  // Build flags
  PropertyFlags := [];
  if Enumerable then
    Include(PropertyFlags, pfEnumerable);
  if Configurable then
    Include(PropertyFlags, pfConfigurable);
  if Writable then
    Include(PropertyFlags, pfWritable);

  DescriptorFields := [];
  if HasEnumerableField then
    Include(DescriptorFields, pdfEnumerable);
  if HasConfigurableField then
    Include(DescriptorFields, pdfConfigurable);
  if HasValue then
    Include(DescriptorFields, pdfValue);
  if HasWritableField then
    Include(DescriptorFields, pdfWritable);
  if HasGet then
    Include(DescriptorFields, pdfGet);
  if HasSet then
    Include(DescriptorFields, pdfSet);

  if HasValue or HasWritableField then
    Result := TGocciaPropertyDescriptorData.CreatePartial(Value,
      PropertyFlags, DescriptorFields)
  else if HasGet or HasSet then
  begin
    Exclude(PropertyFlags, pfWritable);
    Result := TGocciaPropertyDescriptorAccessor.CreatePartial(Getter,
      Setter, PropertyFlags, DescriptorFields);
  end
  else
    Result := TGocciaPropertyDescriptor.Create(PropertyFlags, DescriptorFields);
end;

end.

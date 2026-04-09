unit Goccia.Values.ObjectPropertyDescriptor;

{$I Goccia.inc}

interface

uses
  OrderedStringMap,

  Goccia.Values.Primitives;

type
  TPropertyFlag = (pfEnumerable, pfConfigurable, pfWritable);
  TPropertyFlags = set of TPropertyFlag;

  TGocciaPropertyDescriptor = class
  private
    FFlags: TPropertyFlags;
    function GetEnumerable: Boolean; inline;
    function GetConfigurable: Boolean; inline;
    function GetWritable: Boolean; inline;
  public
    constructor Create(const AFlags: TPropertyFlags);

    property Flags: TPropertyFlags read FFlags;
    property Enumerable: Boolean read GetEnumerable;
    property Configurable: Boolean read GetConfigurable;
    property Writable: Boolean read GetWritable;
  end;

  TGocciaPropertyDescriptorData = class(TGocciaPropertyDescriptor)
  private
    FValue: TGocciaValue;
  public
    constructor Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);

    property Value: TGocciaValue read FValue write FValue;
  end;

  TGocciaPropertyDescriptorAccessor = class(TGocciaPropertyDescriptor)
  private
    FGetter: TGocciaValue;
    FSetter: TGocciaValue;

    function GetWritable: Boolean; inline;
  public
    constructor Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);

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

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue;

constructor TGocciaPropertyDescriptor.Create(const AFlags: TPropertyFlags);
begin
  FFlags := AFlags;
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

constructor TGocciaPropertyDescriptorData.Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
  FValue := AValue;
end;

constructor TGocciaPropertyDescriptorAccessor.Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
  FGetter := AGetter;
  FSetter := ASetter;
end;

function TGocciaPropertyDescriptorAccessor.GetWritable: Boolean;
begin
  Result := FSetter <> nil;
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
  HasValue, HasGet, HasSet: Boolean;
begin
  DescObj := TGocciaObjectValue(ADescriptorObject);

  Enumerable := False;
  Configurable := False;
  Writable := False;
  Value := nil;
  Getter := nil;
  Setter := nil;

  // Inherit from existing descriptor
  if Assigned(AExistingDescriptor) then
  begin
    Enumerable := AExistingDescriptor.Enumerable;
    Configurable := AExistingDescriptor.Configurable;
    if AExistingDescriptor is TGocciaPropertyDescriptorData then
    begin
      Writable := AExistingDescriptor.Writable;
      Value := TGocciaPropertyDescriptorData(AExistingDescriptor).Value;
    end
    else if AExistingDescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      Getter := TGocciaPropertyDescriptorAccessor(AExistingDescriptor).Getter;
      Setter := TGocciaPropertyDescriptorAccessor(AExistingDescriptor).Setter;
    end;
  end;

  // ES2026 §6.2.5.5 steps 3–9: extract descriptor fields
  HasValue := DescObj.HasProperty(PROP_VALUE);
  HasGet := DescObj.HasProperty(PROP_GET);
  HasSet := DescObj.HasProperty(PROP_SET);

  if DescObj.HasProperty(PROP_ENUMERABLE) then
    Enumerable := DescObj.GetProperty(PROP_ENUMERABLE).ToBooleanLiteral.Value;
  if DescObj.HasProperty(PROP_CONFIGURABLE) then
    Configurable := DescObj.GetProperty(PROP_CONFIGURABLE).ToBooleanLiteral.Value;
  if DescObj.HasProperty(PROP_WRITABLE) then
    Writable := DescObj.GetProperty(PROP_WRITABLE).ToBooleanLiteral.Value;
  if HasValue then
    Value := DescObj.GetProperty(PROP_VALUE);

  // ES2026 §6.2.5.5 step 7: validate getter
  if HasGet then
  begin
    Getter := DescObj.GetProperty(PROP_GET);
    if not (Getter is TGocciaUndefinedLiteralValue) and not Getter.IsCallable then
      ThrowTypeError('getter must be a function or undefined');
    if Getter is TGocciaUndefinedLiteralValue then
      Getter := nil;
  end;

  // ES2026 §6.2.5.5 step 8: validate setter
  if HasSet then
  begin
    Setter := DescObj.GetProperty(PROP_SET);
    if not (Setter is TGocciaUndefinedLiteralValue) and not Setter.IsCallable then
      ThrowTypeError('setter must be a function or undefined');
    if Setter is TGocciaUndefinedLiteralValue then
      Setter := nil;
  end;

  // ES2026 §6.2.5.5 step 10: mixed data+accessor is invalid
  if (HasValue or DescObj.HasProperty(PROP_WRITABLE)) and (HasGet or HasSet) then
    ThrowTypeError('descriptor cannot have both accessor and data properties');

  // Build flags
  PropertyFlags := [];
  if Enumerable then
    Include(PropertyFlags, pfEnumerable);
  if Configurable then
    Include(PropertyFlags, pfConfigurable);
  if Writable then
    Include(PropertyFlags, pfWritable);

  // Construct the appropriate descriptor type
  if HasValue or DescObj.HasProperty(PROP_WRITABLE) or
     (Assigned(AExistingDescriptor) and (AExistingDescriptor is TGocciaPropertyDescriptorData) and not HasGet and not HasSet) or
     (not Assigned(AExistingDescriptor) and not HasGet and not HasSet) then
    Result := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags)
  else
    Result := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);
end;

end.

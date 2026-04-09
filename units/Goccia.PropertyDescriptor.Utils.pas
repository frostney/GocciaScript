unit Goccia.PropertyDescriptor.Utils;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// ES2026 §6.2.5.5 ToPropertyDescriptor(Obj)
function ToPropertyDescriptor(
  const ADescriptorObject: TGocciaObjectValue;
  const AExistingDescriptor: TGocciaPropertyDescriptor
): TGocciaPropertyDescriptor;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Values.ErrorHelper;

// ES2026 §6.2.5.5 ToPropertyDescriptor(Obj)
function ToPropertyDescriptor(
  const ADescriptorObject: TGocciaObjectValue;
  const AExistingDescriptor: TGocciaPropertyDescriptor
): TGocciaPropertyDescriptor;
var
  Enumerable, Configurable, Writable: Boolean;
  Value, Getter, Setter: TGocciaValue;
  PropertyFlags: TPropertyFlags;
  HasValue, HasGet, HasSet: Boolean;
begin
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
  HasValue := ADescriptorObject.HasProperty(PROP_VALUE);
  HasGet := ADescriptorObject.HasProperty(PROP_GET);
  HasSet := ADescriptorObject.HasProperty(PROP_SET);

  if ADescriptorObject.HasProperty(PROP_ENUMERABLE) then
    Enumerable := ADescriptorObject.GetProperty(PROP_ENUMERABLE).ToBooleanLiteral.Value;
  if ADescriptorObject.HasProperty(PROP_CONFIGURABLE) then
    Configurable := ADescriptorObject.GetProperty(PROP_CONFIGURABLE).ToBooleanLiteral.Value;
  if ADescriptorObject.HasProperty(PROP_WRITABLE) then
    Writable := ADescriptorObject.GetProperty(PROP_WRITABLE).ToBooleanLiteral.Value;
  if HasValue then
    Value := ADescriptorObject.GetProperty(PROP_VALUE);

  // ES2026 §6.2.5.5 step 7: validate getter
  if HasGet then
  begin
    Getter := ADescriptorObject.GetProperty(PROP_GET);
    if not (Getter is TGocciaUndefinedLiteralValue) and not Getter.IsCallable then
      ThrowTypeError('getter must be a function or undefined');
    if Getter is TGocciaUndefinedLiteralValue then
      Getter := nil;
  end;

  // ES2026 §6.2.5.5 step 8: validate setter
  if HasSet then
  begin
    Setter := ADescriptorObject.GetProperty(PROP_SET);
    if not (Setter is TGocciaUndefinedLiteralValue) and not Setter.IsCallable then
      ThrowTypeError('setter must be a function or undefined');
    if Setter is TGocciaUndefinedLiteralValue then
      Setter := nil;
  end;

  // ES2026 §6.2.5.5 step 10: mixed data+accessor is invalid
  if (HasValue or ADescriptorObject.HasProperty(PROP_WRITABLE)) and (HasGet or HasSet) then
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
  if HasValue or ADescriptorObject.HasProperty(PROP_WRITABLE) or
     (Assigned(AExistingDescriptor) and (AExistingDescriptor is TGocciaPropertyDescriptorData) and not HasGet and not HasSet) or
     (not Assigned(AExistingDescriptor) and not HasGet and not HasSet) then
    Result := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags)
  else
    Result := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);
end;

end.

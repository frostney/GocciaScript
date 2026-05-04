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
    procedure MarkValues; virtual;

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
    procedure MarkValues; override;

    property Value: TGocciaValue read FValue write FValue;
  end;

  TGocciaPropertyDescriptorAccessor = class(TGocciaPropertyDescriptor)
  private
    FGetter: TGocciaValue;
    FSetter: TGocciaValue;

    function GetWritable: Boolean; inline;
  public
    constructor Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);
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

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
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

procedure TGocciaPropertyDescriptor.MarkValues;
begin
  // No-op base: subclasses override to mark their value references
end;

constructor TGocciaPropertyDescriptorData.Create(const AValue: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
  FValue := AValue;
end;

procedure TGocciaPropertyDescriptorData.MarkValues;
begin
  if Assigned(FValue) then
    FValue.MarkReferences;
end;

constructor TGocciaPropertyDescriptorAccessor.Create(const AGetter: TGocciaValue; const ASetter: TGocciaValue; const AFlags: TPropertyFlags);
begin
  inherited Create(AFlags);
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
  HasValue, HasGet, HasSet, HasWritable: Boolean;
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
  HasWritable := DescObj.HasProperty(PROP_WRITABLE);

  if DescObj.HasProperty(PROP_ENUMERABLE) then
    Enumerable := DescObj.GetProperty(PROP_ENUMERABLE).ToBooleanLiteral.Value;
  if DescObj.HasProperty(PROP_CONFIGURABLE) then
    Configurable := DescObj.GetProperty(PROP_CONFIGURABLE).ToBooleanLiteral.Value;
  if HasWritable then
    Writable := DescObj.GetProperty(PROP_WRITABLE).ToBooleanLiteral.Value;
  if HasValue then
    Value := DescObj.GetProperty(PROP_VALUE);

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
  if (HasValue or HasWritable) and (HasGet or HasSet) then
    ThrowTypeError(SErrorDescriptorMixedAccessorData, SSuggestPropertyDescriptorObject);

  // Build flags
  PropertyFlags := [];
  if Enumerable then
    Include(PropertyFlags, pfEnumerable);
  if Configurable then
    Include(PropertyFlags, pfConfigurable);
  if Writable then
    Include(PropertyFlags, pfWritable);

  // Construct the appropriate descriptor type
  if HasValue or HasWritable or
     (Assigned(AExistingDescriptor) and (AExistingDescriptor is TGocciaPropertyDescriptorData) and not HasGet and not HasSet) or
     (not Assigned(AExistingDescriptor) and not HasGet and not HasSet) then
    Result := TGocciaPropertyDescriptorData.Create(Value, PropertyFlags)
  else
  begin
    // Accessor descriptors determine writability via FSetter, not pfWritable
    Exclude(PropertyFlags, pfWritable);
    Result := TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, PropertyFlags);
  end;
end;

end.

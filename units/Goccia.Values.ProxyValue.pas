unit Goccia.Values.ProxyValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives,
  Goccia.Values.SymbolValue;

type
  TGocciaProxyValue = class(TGocciaObjectValue)
  private
    FTarget: TGocciaValue;
    FHandler: TGocciaObjectValue;
    FRevoked: Boolean;

    procedure CheckRevoked;
    function GetTrap(const ATrapName: string): TGocciaValue;
    function InvokeTrap(const ATrap: TGocciaValue;
      const AArgs: TGocciaArgumentsCollection): TGocciaValue;
  public
    constructor Create(const ATarget: TGocciaValue;
      const AHandler: TGocciaObjectValue);

    // ES2026 §28.1.1 [[Get]](P, Receiver)
    function GetProperty(const AName: string): TGocciaValue; override;

    // ES2026 §28.1.1 [[Set]](P, V, Receiver)
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue;
      const ACanCreate: Boolean = True); override;

    // ES2026 §28.1.1 [[HasProperty]](P)
    function HasTrap(const AName: string): Boolean;
    function HasSymbolTrap(const ASymbol: TGocciaSymbolValue): Boolean;

    // ES2026 §28.1.1 [[Delete]](P)
    function DeleteProperty(const AName: string): Boolean; override;

    // ES2026 §28.1.1 [[GetOwnProperty]](P)
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;

    // ES2026 §28.1.1 [[DefineOwnProperty]](P, Desc)
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;

    // ES2026 §28.1.1 [[OwnPropertyKeys]]()
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetOwnPropertyNames: TArray<string>; override;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function HasOwnProperty(const AName: string): Boolean; override;

    // Symbol-keyed property access via get/has traps
    function GetSymbolProperty(
      const ASymbol: TGocciaSymbolValue): TGocciaValue; override;
    function HasSymbolProperty(
      const ASymbol: TGocciaSymbolValue): Boolean; override;

    // ES2026 §28.1.1 [[GetPrototypeOf]]()
    function GetPrototypeTrap: TGocciaValue;

    // ES2026 §28.1.1 [[SetPrototypeOf]](V)
    function SetPrototypeTrap(const AProto: TGocciaValue): Boolean;

    // ES2026 §28.1.1 [[IsExtensible]]()
    function IsExtensibleTrap: Boolean;

    // ES2026 §28.1.1 [[PreventExtensions]]()
    procedure PreventExtensions; override;

    // ES2026 §28.1.1 [[Call]](thisArgument, argumentsList)
    function ApplyTrap(const AArguments: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    // ES2026 §28.1.1 [[Construct]](argumentsList, newTarget)
    function ConstructTrap(
      const AArguments: TGocciaArgumentsCollection): TGocciaValue;

    function TypeOf: string; override;
    function IsCallable: Boolean; override;
    function ToStringTag: string; override;

    procedure MarkReferences; override;

    procedure Revoke;

    property Target: TGocciaValue read FTarget;
    property Handler: TGocciaObjectValue read FHandler;
    property Revoked: Boolean read FRevoked;
  end;

  { Helper class for Proxy.revocable — captures the proxy reference
    so the revoke callback can set FRevoked without closures. }
  TGocciaProxyRevoker = class(TGocciaObjectValue)
  private
    FProxy: TGocciaProxyValue;
  public
    constructor Create(const AProxy: TGocciaProxyValue);
    function RevokeCallback(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    procedure MarkReferences; override;
  end;

  { Result object for Proxy.revocable — holds the Revoker via an
    internal field (not a JS-visible property) so the GC can trace it
    without exposing implementation state through reflection APIs. }
  TGocciaRevocableProxyResult = class(TGocciaObjectValue)
  private
    FRevoker: TGocciaProxyRevoker;
  public
    constructor Create(const ARevoker: TGocciaProxyRevoker);
    procedure MarkReferences; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Evaluator.Comparison,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.HoleValue,
  Goccia.Values.NativeFunction;

{ TGocciaProxyValue }

constructor TGocciaProxyValue.Create(const ATarget: TGocciaValue;
  const AHandler: TGocciaObjectValue);
begin
  inherited Create;
  FTarget := ATarget;
  FHandler := AHandler;
  FRevoked := False;
end;

procedure TGocciaProxyValue.CheckRevoked;
begin
  if FRevoked then
    ThrowTypeError('Cannot perform operation on a revoked proxy');
end;

function TGocciaProxyValue.GetTrap(const ATrapName: string): TGocciaValue;
var
  TrapValue: TGocciaValue;
begin
  TrapValue := FHandler.GetProperty(ATrapName);
  if (TrapValue is TGocciaUndefinedLiteralValue) or
     (TrapValue is TGocciaNullLiteralValue) then
    Result := nil
  else if not TrapValue.IsCallable then
    ThrowTypeError('Proxy handler trap ''' + ATrapName + ''' is not a function')
  else
    Result := TrapValue;
end;

function TGocciaProxyValue.InvokeTrap(const ATrap: TGocciaValue;
  const AArgs: TGocciaArgumentsCollection): TGocciaValue;
begin
  // Mirror the VM dispatch order: proxy-wrapped traps first, then
  // functions, then classes.
  if ATrap is TGocciaProxyValue then
    Result := TGocciaProxyValue(ATrap).ApplyTrap(AArgs, FHandler)
  else if ATrap is TGocciaFunctionBase then
    Result := TGocciaFunctionBase(ATrap).Call(AArgs, FHandler)
  else if ATrap is TGocciaClassValue then
    Result := TGocciaClassValue(ATrap).Call(AArgs, FHandler)
  else
    ThrowTypeError('Proxy trap is not callable');
end;

// ES2026 §28.1.1 [[Get]](P, Receiver)
function TGocciaProxyValue.GetProperty(const AName: string): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      Args.Add(Self);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 8-9: Invariant validation.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data: result must be SameValue
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError('Proxy get: value mismatch for non-configurable, non-writable property ''' + AName + '''');
        // Non-configurable accessor without getter: result must be undefined
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError('Proxy get: must return undefined for non-configurable accessor without getter ''' + AName + '''');
      end;
    end;
  end
  else
    Result := FTarget.GetProperty(AName);
end;

// ES2026 §28.1.1 [[Set]](P, V, Receiver)
procedure TGocciaProxyValue.AssignProperty(const AName: string;
  const AValue: TGocciaValue; const ACanCreate: Boolean = True);
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_SET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      Args.Add(AValue);
      Args.Add(Self);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        ThrowTypeError('Proxy set handler returned false for property ''' +
          AName + '''');
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 11-12: Invariant validation after truthy result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data property: value must match
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, AValue) then
          ThrowTypeError('Proxy set: cannot change value of non-configurable, non-writable property ''' + AName + '''');
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError('Proxy set: cannot set non-configurable accessor property ''' + AName + ''' without a setter');
      end;
    end;
  end
  else
    FTarget.SetProperty(AName, AValue);
end;

// ES2026 §28.1.1 [[HasProperty]](P)
function TGocciaProxyValue.HasTrap(const AName: string): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_HAS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 9-10: Invariant checks when trap returns false.
    if (not Result) and (FTarget is TGocciaObjectValue) then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
      // Cannot hide non-configurable own property
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
        ThrowTypeError('Proxy has trap returned false for non-configurable property ''' + AName + '''');
      // Cannot hide own property on non-extensible target
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError('Proxy has trap returned false for property on non-extensible target');
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).HasProperty(AName)
    else
      Result := False;
  end;
end;

// ES2026 §28.1.1 [[HasProperty]](P) — symbol key overload
function TGocciaProxyValue.HasSymbolTrap(
  const ASymbol: TGocciaSymbolValue): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_HAS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 9-10: Invariant checks (mirror HasTrap).
    if (not Result) and (FTarget is TGocciaObjectValue) then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
        ThrowTypeError('Proxy has trap returned false for non-configurable symbol property');
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError('Proxy has trap returned false for symbol property on non-extensible target');
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).HasSymbolProperty(ASymbol)
    else
      Result := False;
  end;
end;

function TGocciaProxyValue.HasOwnProperty(const AName: string): Boolean;
var
  Descriptor: TGocciaPropertyDescriptor;
begin
  // ES2026 §28.1.1: Own-property check uses [[GetOwnProperty]], not
  // [[HasProperty]], so Object.hasOwn(proxy, key) only reports true
  // for own properties, not inherited ones.
  Descriptor := GetOwnPropertyDescriptor(AName);
  Result := Assigned(Descriptor);
end;

// ES2026 §28.1.1 [[Get]](P, Receiver) — symbol key
function TGocciaProxyValue.GetSymbolProperty(
  const ASymbol: TGocciaSymbolValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      Args.Add(Self);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 8-9: Invariant validation for symbol keys.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError('Proxy get: value mismatch for non-configurable, non-writable symbol property');
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError('Proxy get: must return undefined for non-configurable accessor without getter');
      end;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).GetSymbolProperty(ASymbol)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §28.1.1 [[HasProperty]](P) — symbol key (virtual override)
function TGocciaProxyValue.HasSymbolProperty(
  const ASymbol: TGocciaSymbolValue): Boolean;
begin
  Result := HasSymbolTrap(ASymbol);
end;

// ES2026 §28.1.1 [[Delete]](P)
function TGocciaProxyValue.DeleteProperty(const AName: string): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_DELETE_PROPERTY);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 11-12: Invariant checks when trap returns true.
    if Result and (FTarget is TGocciaObjectValue) then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
      // Cannot delete non-configurable own property
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
        ThrowTypeError('Proxy deleteProperty trap returned true for non-configurable property ''' + AName + '''');
      // Cannot delete own property on non-extensible target (property still exists)
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError('Proxy deleteProperty trap returned true for property on non-extensible target');
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).DeleteProperty(AName)
    else
      Result := True;
  end;
end;

// ES2026 §28.1.1 [[GetOwnProperty]](P)
function TGocciaProxyValue.GetOwnPropertyDescriptor(
  const AName: string): TGocciaPropertyDescriptor;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  ResultObj: TGocciaObjectValue;
  ValueProp, WritableProp, EnumProp, ConfigProp: TGocciaValue;
  GetterProp, SetterProp: TGocciaValue;
  HasGetter, HasSetter: Boolean;
  Flags: TPropertyFlags;
  TargetDesc: TGocciaPropertyDescriptor;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET_OWN_PROPERTY_DESCRIPTOR);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      TrapResult := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // Per spec, trap must return an object or undefined (not null)
    if TrapResult is TGocciaUndefinedLiteralValue then
    begin
      // ES2026 §28.1.1 step 10-11: Cannot hide non-configurable property
      if FTarget is TGocciaObjectValue then
      begin
        TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
        if Assigned(TargetDesc) and not TargetDesc.Configurable then
          ThrowTypeError('Proxy getOwnPropertyDescriptor returned undefined for non-configurable property ''' + AName + '''');
        if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
          ThrowTypeError('Proxy getOwnPropertyDescriptor returned undefined for property on non-extensible target');
      end;
      Exit(nil);
    end;

    if not (TrapResult is TGocciaObjectValue) then
      ThrowTypeError('Proxy getOwnPropertyDescriptor must return an object or undefined');

    ResultObj := TGocciaObjectValue(TrapResult);
    Flags := [];

    EnumProp := ResultObj.GetProperty(PROP_ENUMERABLE);
    if Assigned(EnumProp) and EnumProp.ToBooleanLiteral.Value then
      Include(Flags, pfEnumerable);

    ConfigProp := ResultObj.GetProperty(PROP_CONFIGURABLE);
    if Assigned(ConfigProp) and ConfigProp.ToBooleanLiteral.Value then
      Include(Flags, pfConfigurable);

    // Detect accessor vs data descriptor by field presence, not value.
    // { get: undefined } is an accessor descriptor per ES2026 §6.2.6.
    HasGetter := ResultObj.HasOwnProperty(PROP_GET);
    HasSetter := ResultObj.HasOwnProperty(PROP_SET);

    if HasGetter or HasSetter then
    begin
      // Accessor descriptor — preserve explicit undefined get/set
      GetterProp := ResultObj.GetProperty(PROP_GET);
      SetterProp := ResultObj.GetProperty(PROP_SET);
      Result := TGocciaPropertyDescriptorAccessor.Create(
        GetterProp, SetterProp, Flags);
    end
    else
    begin
      // Data descriptor
      WritableProp := ResultObj.GetProperty(PROP_WRITABLE);
      if Assigned(WritableProp) and WritableProp.ToBooleanLiteral.Value then
        Include(Flags, pfWritable);

      ValueProp := ResultObj.GetProperty(PROP_VALUE);
      Result := TGocciaPropertyDescriptorData.Create(ValueProp, Flags);
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName)
    else
      Result := nil;
  end;
end;

// ES2026 §28.1.1 [[DefineOwnProperty]](P, Desc)
procedure TGocciaProxyValue.DefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor);
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  DescObj: TGocciaObjectValue;
  TrapResult: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_DEFINE_PROPERTY);
  if Assigned(Trap) then
  begin
    // Build descriptor object — preserve accessor vs data shape
    DescObj := TGocciaObjectValue.Create;
    // Emit get/set when the descriptor IS an accessor, regardless of
    // whether the stored function is nil — presence matters per spec.
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      DescObj.AssignProperty(PROP_GET,
        TGocciaPropertyDescriptorAccessor(ADescriptor).Getter);
      DescObj.AssignProperty(PROP_SET,
        TGocciaPropertyDescriptorAccessor(ADescriptor).Setter);
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      DescObj.AssignProperty(PROP_VALUE,
        TGocciaPropertyDescriptorData(ADescriptor).Value);
      DescObj.AssignProperty(PROP_WRITABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Writable));
    end;
    DescObj.AssignProperty(PROP_ENUMERABLE,
      TGocciaBooleanLiteralValue.Create(ADescriptor.Enumerable));
    DescObj.AssignProperty(PROP_CONFIGURABLE,
      TGocciaBooleanLiteralValue.Create(ADescriptor.Configurable));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      Args.Add(DescObj);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        ThrowTypeError(
          'Proxy defineProperty handler returned false for property ''' +
          AName + '''');
    finally
      Args.Free;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      TGocciaObjectValue(FTarget).DefineProperty(AName, ADescriptor)
    else
      ThrowTypeError('Cannot define property on non-object target');
  end;
end;

// ES2026 §28.1.1 [[OwnPropertyKeys]]()
function TGocciaProxyValue.GetOwnPropertyKeys: TArray<string>;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  ResultArray: TGocciaArrayValue;
  Keys: TArray<string>;
  I, J: Integer;
  Element: TGocciaValue;
  TargetKeys: TArray<string>;
  TargetDesc: TGocciaPropertyDescriptor;
  Found: Boolean;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_OWN_KEYS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      TrapResult := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    if not (TrapResult is TGocciaArrayValue) then
      ThrowTypeError('Proxy ownKeys must return an array');

    ResultArray := TGocciaArrayValue(TrapResult);
    SetLength(Keys, ResultArray.Elements.Count);
    for I := 0 to ResultArray.Elements.Count - 1 do
    begin
      Element := ResultArray.Elements[I];
      // ES2026 §28.1.1 step 8: Each element must be a String or Symbol.
      if not (Element is TGocciaStringLiteralValue) and
         not (Element is TGocciaSymbolValue) then
        ThrowTypeError('Proxy ownKeys trap result must contain only strings and symbols');
      Keys[I] := Element.ToStringLiteral.Value;
    end;

    // ES2026 §28.1.1 step 17-18: Non-configurable target properties
    // must appear in the trap result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetKeys := TGocciaObjectValue(FTarget).GetOwnPropertyKeys;
      for I := 0 to Length(TargetKeys) - 1 do
      begin
        TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(TargetKeys[I]);
        if Assigned(TargetDesc) and not TargetDesc.Configurable then
        begin
          Found := False;
          for J := 0 to Length(Keys) - 1 do
            if Keys[J] = TargetKeys[I] then begin Found := True; Break; end;
          if not Found then
            ThrowTypeError('Proxy ownKeys trap result must include non-configurable property ''' + TargetKeys[I] + '''');
        end;
      end;
    end;

    Result := Keys;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).GetOwnPropertyKeys
    else
      SetLength(Result, 0);
  end;
end;

function TGocciaProxyValue.GetOwnPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaProxyValue.GetEnumerablePropertyNames: TArray<string>;
var
  AllKeys: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  FilteredKeys: TArray<string>;
  I, Count: Integer;
begin
  AllKeys := GetOwnPropertyKeys;
  SetLength(FilteredKeys, Length(AllKeys));
  Count := 0;
  for I := 0 to Length(AllKeys) - 1 do
  begin
    Descriptor := GetOwnPropertyDescriptor(AllKeys[I]);
    if Assigned(Descriptor) and Descriptor.Enumerable then
    begin
      FilteredKeys[Count] := AllKeys[I];
      Inc(Count);
    end;
  end;
  SetLength(FilteredKeys, Count);
  Result := FilteredKeys;
end;

function TGocciaProxyValue.GetAllPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

// ES2026 §28.1.1 [[GetPrototypeOf]]()
function TGocciaProxyValue.GetPrototypeTrap: TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TargetProto: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET_PROTOTYPE_OF);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.1 step 5: Result must be an Object or null.
    if not (Result is TGocciaObjectValue) and
       not (Result is TGocciaNullLiteralValue) then
      ThrowTypeError('Proxy getPrototypeOf trap must return an object or null');

    // ES2026 §28.1.1 step 8: If target is non-extensible, trap result
    // must be the same as the target's actual prototype.
    if (FTarget is TGocciaObjectValue) and
       not TGocciaObjectValue(FTarget).Extensible then
    begin
      if Assigned(TGocciaObjectValue(FTarget).Prototype) then
        TargetProto := TGocciaObjectValue(FTarget).Prototype
      else
        TargetProto := TGocciaNullLiteralValue.NullValue;
      if Result <> TargetProto then
        ThrowTypeError(
          'Proxy getPrototypeOf trap result does not match non-extensible target prototype');
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
    begin
      if Assigned(TGocciaObjectValue(FTarget).Prototype) then
        Result := TGocciaObjectValue(FTarget).Prototype
      else
        Result := TGocciaNullLiteralValue.NullValue;
    end
    else
      Result := TGocciaNullLiteralValue.NullValue;
  end;
end;

// ES2026 §28.1.1 [[SetPrototypeOf]](V)
function TGocciaProxyValue.SetPrototypeTrap(
  const AProto: TGocciaValue): Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_SET_PROTOTYPE_OF);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(AProto);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 12: If trap returns true and target is
    // non-extensible, the new prototype must match the target's current one.
    if Result and (FTarget is TGocciaObjectValue) and
       not TGocciaObjectValue(FTarget).Extensible then
    begin
      if AProto is TGocciaObjectValue then
      begin
        if TGocciaObjectValue(FTarget).Prototype <> TGocciaObjectValue(AProto) then
          ThrowTypeError('Proxy setPrototypeOf trap returned true but target is non-extensible');
      end
      else if AProto is TGocciaNullLiteralValue then
      begin
        if Assigned(TGocciaObjectValue(FTarget).Prototype) then
          ThrowTypeError('Proxy setPrototypeOf trap returned true but target is non-extensible');
      end;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
    begin
      // ES2026 §10.1.2 step 4: If target is not extensible and the new
      // prototype differs from the current one, return false.
      if not TGocciaObjectValue(FTarget).Extensible then
      begin
        if AProto is TGocciaObjectValue then
          Result := TGocciaObjectValue(FTarget).Prototype = TGocciaObjectValue(AProto)
        else if AProto is TGocciaNullLiteralValue then
          Result := not Assigned(TGocciaObjectValue(FTarget).Prototype)
        else
          Result := False;
      end
      else
      begin
        if AProto is TGocciaObjectValue then
          TGocciaObjectValue(FTarget).Prototype := TGocciaObjectValue(AProto)
        else if (AProto is TGocciaNullLiteralValue) then
          TGocciaObjectValue(FTarget).Prototype := nil;
        Result := True;
      end;
    end
    else
      Result := False;
  end;
end;

// ES2026 §28.1.1 [[IsExtensible]]()
function TGocciaProxyValue.IsExtensibleTrap: Boolean;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetExtensible: Boolean;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_IS_EXTENSIBLE);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      TrapResult := InvokeTrap(Trap, Args);
      Result := TrapResult.ToBooleanLiteral.Value;
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 7: Validate against target extensibility
    if FTarget is TGocciaObjectValue then
      TargetExtensible := TGocciaObjectValue(FTarget).Extensible
    else
      TargetExtensible := False;
    if Result <> TargetExtensible then
      ThrowTypeError(
        'Proxy isExtensible trap result does not match target extensibility');
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).Extensible
    else
      Result := False;
  end;
end;

// ES2026 §28.1.1 [[PreventExtensions]]()
procedure TGocciaProxyValue.PreventExtensions;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_PREVENT_EXTENSIONS);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        ThrowTypeError('Proxy preventExtensions handler returned false');
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 8: Invariant — if trap returned true, target
    // must actually be non-extensible now.
    if (FTarget is TGocciaObjectValue) and
       TGocciaObjectValue(FTarget).Extensible then
      ThrowTypeError(
        'Proxy preventExtensions trap returned true but target is still extensible');
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      TGocciaObjectValue(FTarget).PreventExtensions;
  end;
end;

// ES2026 §28.1.1 [[Call]](thisArgument, argumentsList)
function TGocciaProxyValue.ApplyTrap(
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  ArgsArray: TGocciaArrayValue;
  I: Integer;
begin
  CheckRevoked;

  if not FTarget.IsCallable then
    ThrowTypeError('Proxy apply trap called on non-function target');

  Trap := GetTrap(PROP_APPLY);
  if Assigned(Trap) then
  begin
    // Build arguments array
    ArgsArray := TGocciaArrayValue.Create;
    for I := 0 to AArguments.Length - 1 do
      ArgsArray.Elements.Add(AArguments.GetElement(I));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(AThisValue);
      Args.Add(ArgsArray);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;
  end
  else
  begin
    // No apply trap: call the target directly (nested proxies first)
    if FTarget is TGocciaProxyValue then
      Result := TGocciaProxyValue(FTarget).ApplyTrap(AArguments, AThisValue)
    else if FTarget is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(FTarget).Call(AArguments, AThisValue)
    else if FTarget is TGocciaClassValue then
      Result := TGocciaClassValue(FTarget).Call(AArguments, AThisValue)
    else
      ThrowTypeError('Proxy target is not callable');
  end;
end;

// ES2026 §28.1.1 [[Construct]](argumentsList, newTarget)
function TGocciaProxyValue.ConstructTrap(
  const AArguments: TGocciaArgumentsCollection): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  ArgsArray: TGocciaArrayValue;
  I: Integer;
begin
  CheckRevoked;

  // ES2026 §28.1.1 step 1: Proxy [[Construct]] only exists when target
  // is constructable. Validate before dispatching to the trap.
  if not ((FTarget is TGocciaProxyValue) or
          (FTarget is TGocciaClassValue) or
          (FTarget is TGocciaNativeFunctionValue) or
          (FTarget is TGocciaFunctionBase)) then
    ThrowTypeError('Proxy target is not a constructor');

  Trap := GetTrap(PROP_CONSTRUCT);
  if Assigned(Trap) then
  begin
    // Build arguments array
    ArgsArray := TGocciaArrayValue.Create;
    for I := 0 to AArguments.Length - 1 do
      ArgsArray.Elements.Add(AArguments.GetElement(I));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ArgsArray);
      Args.Add(Self);
      Result := InvokeTrap(Trap, Args);
      if Result.IsPrimitive then
        ThrowTypeError('Proxy construct handler must return an object');
    finally
      Args.Free;
    end;
  end
  else
  begin
    // No construct trap: construct the target directly (nested proxies first).
    // Use Instantiate() for class values — it is virtual, so
    // TGocciaVMClassValue dispatches to its bytecode-aware override.
    if FTarget is TGocciaProxyValue then
      Result := TGocciaProxyValue(FTarget).ConstructTrap(AArguments)
    else if FTarget is TGocciaClassValue then
      Result := TGocciaClassValue(FTarget).Instantiate(AArguments)
    else if FTarget is TGocciaNativeFunctionValue then
      Result := TGocciaNativeFunctionValue(FTarget).Call(AArguments,
        TGocciaHoleValue.HoleValue)
    else if FTarget is TGocciaFunctionBase then
      Result := TGocciaFunctionBase(FTarget).Call(AArguments,
        TGocciaHoleValue.HoleValue)
    else
      ThrowTypeError('Proxy target is not a constructor');
  end;
end;

function TGocciaProxyValue.TypeOf: string;
begin
  // ES2026 §28.1.1: Revocation disables operations, not type
  // inspection. typeof and IsCallable always reflect the target.
  Result := FTarget.TypeOf;
end;

function TGocciaProxyValue.IsCallable: Boolean;
begin
  Result := FTarget.IsCallable;
end;

function TGocciaProxyValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_PROXY;
end;

procedure TGocciaProxyValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;

  if Assigned(FTarget) then
    FTarget.MarkReferences;
  if Assigned(FHandler) then
    FHandler.MarkReferences;
end;

procedure TGocciaProxyValue.Revoke;
begin
  FRevoked := True;
end;

{ TGocciaProxyRevoker }

constructor TGocciaProxyRevoker.Create(const AProxy: TGocciaProxyValue);
begin
  inherited Create;
  FProxy := AProxy;
end;

function TGocciaProxyRevoker.RevokeCallback(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(FProxy) then
  begin
    FProxy.Revoke;
    FProxy := nil;
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaProxyRevoker.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FProxy) then
    FProxy.MarkReferences;
end;

{ TGocciaRevocableProxyResult }

constructor TGocciaRevocableProxyResult.Create(
  const ARevoker: TGocciaProxyRevoker);
begin
  inherited Create;
  FRevoker := ARevoker;
end;

procedure TGocciaRevocableProxyResult.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FRevoker) then
    FRevoker.MarkReferences;
end;

end.

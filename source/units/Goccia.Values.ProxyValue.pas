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
    procedure CollectOwnPropertyTrapKeys(out AStringKeys: TArray<string>;
      out ASymbolKeys: TArray<TGocciaSymbolValue>;
      out AOrderedKeys: TArray<TGocciaValue>);
    function GetTrap(const ATrapName: string): TGocciaValue;
    function InvokeTrap(const ATrap: TGocciaValue;
      const AArgs: TGocciaArgumentsCollection): TGocciaValue;
  public
    constructor Create(const ATarget: TGocciaValue;
      const AHandler: TGocciaObjectValue);

    // ES2026 §28.1.1 [[Get]](P, Receiver)
    function GetProperty(const AName: string): TGocciaValue; override;
    function GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue; override;

    // ES2026 §28.1.1 [[Set]](P, V, Receiver)
    procedure AssignProperty(const AName: string; const AValue: TGocciaValue;
      const ACanCreate: Boolean = True); override;

    // ES2026 §28.1.1 [[HasProperty]](P)
    function HasProperty(const AName: string): Boolean; override;
    function HasTrap(const AName: string): Boolean;
    function HasSymbolTrap(const ASymbol: TGocciaSymbolValue): Boolean;

    // ES2026 §28.1.1 [[Delete]](P)
    function DeleteProperty(const AName: string): Boolean; override;

    // ES2026 §28.1.1 [[GetOwnProperty]](P)
    function GetOwnPropertyDescriptor(
      const AName: string): TGocciaPropertyDescriptor; override;
    function GetOwnSymbolPropertyDescriptor(
      const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor; override;

    // ES2026 §28.1.1 [[DefineOwnProperty]](P, Desc)
    procedure DefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    procedure DefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor); override;
    function TryDefineProperty(const AName: string;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;
    function TryDefineSymbolProperty(const ASymbol: TGocciaSymbolValue;
      const ADescriptor: TGocciaPropertyDescriptor): Boolean; override;

    // ES2026 §28.1.1 [[OwnPropertyKeys]]()
    function GetOwnPropertyKeys: TArray<string>; override;
    function GetOwnPropertyNames: TArray<string>; override;
    function GetOwnPropertyKeyValues: TArray<TGocciaValue>;
    function GetEnumerablePropertyNames: TArray<string>; override;
    function GetAllPropertyNames: TArray<string>; override;
    function GetOwnSymbols: TArray<TGocciaSymbolValue>; override;
    function HasOwnProperty(const AName: string): Boolean; override;

    // ES2026 §10.5.9 [[Set]](P, V, Receiver) — receiver-aware
    function AssignPropertyWithReceiver(const AName: string;
      const AValue: TGocciaValue;
      const AReceiver: TGocciaValue): Boolean; override;

    // Symbol-keyed property access via get/has traps
    function GetSymbolProperty(
      const ASymbol: TGocciaSymbolValue): TGocciaValue; override;
    function GetSymbolPropertyWithReceiver(
      const ASymbol: TGocciaSymbolValue;
      const AReceiver: TGocciaValue): TGocciaValue; override;
    function HasSymbolProperty(
      const ASymbol: TGocciaSymbolValue): Boolean; override;
    function AssignSymbolPropertyWithReceiver(
      const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue;
      const AReceiver: TGocciaValue): Boolean; override;

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

    // ES2026 §28.1.1 [[Construct]](argumentsList, newTarget). ANewTarget is
    // forwarded to the construct trap and to a nested proxy fallback. nil
    // means "use this proxy as newTarget" — the default for `new proxy(...)`.
    function ConstructTrap(
      const AArguments: TGocciaArgumentsCollection;
      const ANewTarget: TGocciaValue = nil): TGocciaValue;

    function TypeOf: string; override;
    function IsCallable: Boolean; override;
    function IsConstructable: Boolean; override;
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

  Goccia.Arithmetic,
  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ArrayValue,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ToObject;

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
    ThrowTypeError(SErrorProxyRevoked, SSuggestProxyRevoked);
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
    ThrowTypeError(Format(SErrorProxyTrapNotFunction, [ATrapName]), SSuggestProxyTargetType)
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
    ThrowTypeError(SErrorProxyTrapNotCallable, SSuggestProxyTargetType);
end;

function CompleteProxyTrapPropertyDescriptor(
  const ADescriptor: TGocciaPropertyDescriptor): TGocciaPropertyDescriptor;
var
  Flags: TPropertyFlags;
  Value: TGocciaValue;
  Getter: TGocciaValue;
  Setter: TGocciaValue;
begin
  Flags := [];
  if ADescriptor.HasEnumerableField and ADescriptor.Enumerable then
    Include(Flags, pfEnumerable);
  if ADescriptor.HasConfigurableField and ADescriptor.Configurable then
    Include(Flags, pfConfigurable);

  if IsAccessorDescriptor(ADescriptor) then
  begin
    Getter := nil;
    Setter := nil;
    if ADescriptor.HasGet then
      Getter := TGocciaPropertyDescriptorAccessor(ADescriptor).Getter;
    if ADescriptor.HasSet then
      Setter := TGocciaPropertyDescriptorAccessor(ADescriptor).Setter;
    Exit(TGocciaPropertyDescriptorAccessor.Create(Getter, Setter, Flags));
  end;

  Value := TGocciaUndefinedLiteralValue.UndefinedValue;
  if IsDataDescriptor(ADescriptor) and ADescriptor.HasValue then
    Value := TGocciaPropertyDescriptorData(ADescriptor).Value;
  if IsDataDescriptor(ADescriptor) and ADescriptor.HasWritableField and
     ADescriptor.Writable then
    Include(Flags, pfWritable);

  Result := TGocciaPropertyDescriptorData.Create(Value, Flags);
end;

function IsCompatibleProxyTrapPropertyDescriptor(
  const AExtensible: Boolean; const AResultDesc: TGocciaPropertyDescriptor;
  const ATargetDesc: TGocciaPropertyDescriptor): Boolean;
var
  ResultData: TGocciaPropertyDescriptorData;
  ResultAccessor: TGocciaPropertyDescriptorAccessor;
  TargetData: TGocciaPropertyDescriptorData;
  TargetAccessor: TGocciaPropertyDescriptorAccessor;
begin
  if not Assigned(ATargetDesc) then
    Exit(AExtensible);

  if ATargetDesc.Configurable then
    Exit(True);

  if AResultDesc.Configurable then
    Exit(False);
  if ATargetDesc.Enumerable <> AResultDesc.Enumerable then
    Exit(False);
  if IsDataDescriptor(ATargetDesc) <> IsDataDescriptor(AResultDesc) then
    Exit(False);

  if IsAccessorDescriptor(ATargetDesc) then
  begin
    if not IsAccessorDescriptor(AResultDesc) then
      Exit(False);
    TargetAccessor := TGocciaPropertyDescriptorAccessor(ATargetDesc);
    ResultAccessor := TGocciaPropertyDescriptorAccessor(AResultDesc);
    Exit((TargetAccessor.Getter = ResultAccessor.Getter) and
      (TargetAccessor.Setter = ResultAccessor.Setter));
  end;

  if IsDataDescriptor(ATargetDesc) then
  begin
    if not IsDataDescriptor(AResultDesc) then
      Exit(False);
    TargetData := TGocciaPropertyDescriptorData(ATargetDesc);
    ResultData := TGocciaPropertyDescriptorData(AResultDesc);
    if not TargetData.Writable then
    begin
      if ResultData.Writable then
        Exit(False);
      if not IsSameValue(TargetData.Value, ResultData.Value) then
        Exit(False);
    end;
  end;

  Result := True;
end;

procedure ValidateProxyGetOwnTrapDescriptor(const APropertyLabel: string;
  const ATargetObject: TGocciaObjectValue;
  const ATargetDesc, AResultDesc: TGocciaPropertyDescriptor);
begin
  if not IsCompatibleProxyTrapPropertyDescriptor(ATargetObject.Extensible,
    AResultDesc, ATargetDesc) then
    ThrowTypeError(Format(SErrorProxyGetOwnIncompatible, [APropertyLabel]),
      SSuggestProxyTrapInvariant);

  if not AResultDesc.Configurable then
  begin
    if not Assigned(ATargetDesc) or ATargetDesc.Configurable then
      ThrowTypeError(Format(SErrorProxyGetOwnInvalidNonConfigurable,
        [APropertyLabel]), SSuggestProxyTrapInvariant);

    if IsDataDescriptor(AResultDesc) and not AResultDesc.Writable then
    begin
      if (not IsDataDescriptor(ATargetDesc)) or ATargetDesc.Writable then
        ThrowTypeError(Format(SErrorProxyGetOwnInvalidNonWritable,
          [APropertyLabel]), SSuggestProxyTrapInvariant);
    end;
  end;
end;

procedure TGocciaProxyValue.CollectOwnPropertyTrapKeys(
  out AStringKeys: TArray<string>;
  out ASymbolKeys: TArray<TGocciaSymbolValue>;
  out AOrderedKeys: TArray<TGocciaValue>);
var
  Args: TGocciaArgumentsCollection;
  Count: Integer;
  Element: TGocciaValue;
  Found: Boolean;
  I, J: Integer;
  OrderedCount: Integer;
  ResultArray: TGocciaArrayValue;
  ResultObject: TGocciaObjectValue;
  ResultLength: Integer;
  SymbolCount: Integer;
  SymbolElement: TGocciaSymbolValue;
  TargetDesc: TGocciaPropertyDescriptor;
  TargetKeys: TArray<string>;
  TargetObj: TGocciaObjectValue;
  TargetSymbols: TArray<TGocciaSymbolValue>;
  Trap: TGocciaValue;
  TrapResult: TGocciaValue;
begin
  CheckRevoked;
  SetLength(AStringKeys, 0);
  SetLength(ASymbolKeys, 0);
  SetLength(AOrderedKeys, 0);

  Trap := GetTrap(PROP_OWN_KEYS);
  if not Assigned(Trap) then
  begin
    if FTarget is TGocciaObjectValue then
    begin
      TargetObj := TGocciaObjectValue(FTarget);
      AStringKeys := TargetObj.GetOwnPropertyKeys;
      ASymbolKeys := TargetObj.GetOwnSymbols;
      SetLength(AOrderedKeys, Length(AStringKeys) + Length(ASymbolKeys));
      OrderedCount := 0;
      for I := 0 to High(AStringKeys) do
      begin
        AOrderedKeys[OrderedCount] :=
          TGocciaStringLiteralValue.Create(AStringKeys[I]);
        Inc(OrderedCount);
      end;
      for I := 0 to High(ASymbolKeys) do
      begin
        AOrderedKeys[OrderedCount] := ASymbolKeys[I];
        Inc(OrderedCount);
      end;
    end;
    Exit;
  end;

  Args := TGocciaArgumentsCollection.Create;
  try
    Args.Add(FTarget);
    TrapResult := InvokeTrap(Trap, Args);
  finally
    Args.Free;
  end;

  if not (TrapResult is TGocciaObjectValue) then
    ThrowTypeError(SErrorProxyOwnKeysArray, SSuggestProxyTrapReturnType);

  ResultObject := TGocciaObjectValue(TrapResult);
  if TrapResult is TGocciaArrayValue then
  begin
    ResultArray := TGocciaArrayValue(TrapResult);
    ResultLength := ResultArray.Elements.Count;
  end
  else
  begin
    ResultArray := nil;
    ResultLength := LengthOfArrayLike(ResultObject);
  end;

  SetLength(AStringKeys, ResultLength);
  SetLength(ASymbolKeys, ResultLength);
  SetLength(AOrderedKeys, ResultLength);
  Count := 0;
  SymbolCount := 0;
  OrderedCount := 0;
  for I := 0 to ResultLength - 1 do
  begin
    if Assigned(ResultArray) then
      Element := ResultArray.Elements[I]
    else
      Element := ResultObject.GetProperty(IntToStr(I));
    if not (Element is TGocciaStringLiteralValue) and
       not (Element is TGocciaSymbolValue) then
      ThrowTypeError(SErrorProxyOwnKeysTypes, SSuggestProxyTrapReturnType);
    if Element is TGocciaStringLiteralValue then
    begin
      for J := 0 to Count - 1 do
        if AStringKeys[J] = TGocciaStringLiteralValue(Element).Value then
          ThrowTypeError(SErrorProxyOwnKeysDuplicate,
            SSuggestProxyTrapInvariant);
      AStringKeys[Count] := TGocciaStringLiteralValue(Element).Value;
      Inc(Count);
    end
    else
    begin
      SymbolElement := TGocciaSymbolValue(Element);
      for J := 0 to SymbolCount - 1 do
        if ASymbolKeys[J] = SymbolElement then
          ThrowTypeError(SErrorProxyOwnKeysDuplicate,
            SSuggestProxyTrapInvariant);
      ASymbolKeys[SymbolCount] := SymbolElement;
      Inc(SymbolCount);
    end;
    AOrderedKeys[OrderedCount] := Element;
    Inc(OrderedCount);
  end;
  SetLength(AStringKeys, Count);
  SetLength(ASymbolKeys, SymbolCount);
  SetLength(AOrderedKeys, OrderedCount);

  if not (FTarget is TGocciaObjectValue) then
    Exit;

  TargetObj := TGocciaObjectValue(FTarget);
  TargetKeys := TargetObj.GetOwnPropertyKeys;
  for I := 0 to Length(TargetKeys) - 1 do
  begin
    TargetDesc := TargetObj.GetOwnPropertyDescriptor(TargetKeys[I]);
    if Assigned(TargetDesc) and not TargetDesc.Configurable then
    begin
      Found := False;
      for J := 0 to Length(AStringKeys) - 1 do
        if AStringKeys[J] = TargetKeys[I] then
        begin
          Found := True;
          Break;
        end;
      if not Found then
        ThrowTypeError(Format(SErrorProxyOwnKeysMissing, [TargetKeys[I]]),
          SSuggestProxyTrapInvariant);
    end;
  end;

  TargetSymbols := TargetObj.GetOwnSymbols;
  for I := 0 to Length(TargetSymbols) - 1 do
  begin
    TargetDesc := TargetObj.GetOwnSymbolPropertyDescriptor(TargetSymbols[I]);
    if Assigned(TargetDesc) and not TargetDesc.Configurable then
    begin
      Found := False;
      for J := 0 to Length(ASymbolKeys) - 1 do
        if ASymbolKeys[J] = TargetSymbols[I] then
        begin
          Found := True;
          Break;
        end;
      if not Found then
        ThrowTypeError(Format(SErrorProxyOwnKeysMissing,
          [TargetSymbols[I].ToStringLiteral.Value]),
          SSuggestProxyTrapInvariant);
    end;
  end;

  if TargetObj.Extensible then
    Exit;

  for I := 0 to Length(TargetKeys) - 1 do
  begin
    Found := False;
    for J := 0 to Length(AStringKeys) - 1 do
      if AStringKeys[J] = TargetKeys[I] then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      ThrowTypeError(Format(SErrorProxyOwnKeysMissing, [TargetKeys[I]]),
        SSuggestProxyTrapInvariant);
  end;
  for I := 0 to Length(TargetSymbols) - 1 do
  begin
    Found := False;
    for J := 0 to Length(ASymbolKeys) - 1 do
      if ASymbolKeys[J] = TargetSymbols[I] then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      ThrowTypeError(Format(SErrorProxyOwnKeysMissing,
        [TargetSymbols[I].ToStringLiteral.Value]),
        SSuggestProxyTrapInvariant);
  end;
  for I := 0 to Length(AStringKeys) - 1 do
  begin
    Found := False;
    for J := 0 to Length(TargetKeys) - 1 do
      if TargetKeys[J] = AStringKeys[I] then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      ThrowTypeError(SErrorProxyOwnKeysExtra, SSuggestProxyTrapInvariant);
  end;
  for I := 0 to Length(ASymbolKeys) - 1 do
  begin
    Found := False;
    for J := 0 to Length(TargetSymbols) - 1 do
      if TargetSymbols[J] = ASymbolKeys[I] then
      begin
        Found := True;
        Break;
      end;
    if not Found then
      ThrowTypeError(SErrorProxyOwnKeysExtra, SSuggestProxyTrapInvariant);
  end;
end;

// ES2026 §28.1.1 [[Get]](P, Receiver)
function TGocciaProxyValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := GetPropertyWithContext(AName, Self);
end;

function TGocciaProxyValue.GetPropertyWithContext(const AName: string; const AThisContext: TGocciaValue): TGocciaValue;
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
      // ES2026 §10.5.8 step 7: Pass the receiver, not the proxy itself
      Args.Add(AThisContext);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.8 step 8-9: Invariant validation.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data: result must be SameValue
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError(Format(SErrorProxyGetNonConfigurableValue, [AName]), SSuggestProxyTrapInvariant);
        // Non-configurable accessor without getter: result must be undefined
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError(Format(SErrorProxyGetNoGetter, [AName]), SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else if FTarget is TGocciaObjectValue then
    // ES2026 §10.5.8 step 10: Return ? target.[[Get]](P, Receiver)
    Result := TGocciaObjectValue(FTarget).GetPropertyWithContext(AName, AThisContext)
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
        ThrowTypeError(Format(SErrorProxySetReturnedFalse, [AName]), SSuggestProxyTrapInvariant);
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
          ThrowTypeError(Format(SErrorProxySetNonConfigurableValue, [AName]), SSuggestProxyTrapInvariant);
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError(Format(SErrorProxySetNoSetter, [AName]), SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else
  begin
    if (FTarget is TGocciaObjectValue) and
       TGocciaObjectValue(FTarget).AssignPropertyWithReceiver(AName, AValue, Self) then
      Exit;
    ThrowTypeError(Format(SErrorProxySetReturnedFalse, [AName]), SSuggestProxyTrapInvariant);
  end;
end;

// ES2026 §10.5.9 [[Set]](P, V, Receiver) — receiver-aware, returns Boolean
function TGocciaProxyValue.AssignPropertyWithReceiver(const AName: string;
  const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
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
      Args.Add(AReceiver);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        Exit(False);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.9 step 11-12: Invariant validation after truthy result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnPropertyDescriptor(AName);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data property: value must match
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, AValue) then
          ThrowTypeError(Format(SErrorProxySetNonConfigurableValue, [AName]), SSuggestProxyTrapInvariant);
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError(Format(SErrorProxySetNoSetter, [AName]), SSuggestProxyTrapInvariant);
      end;
    end;

    Result := True;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).AssignPropertyWithReceiver(AName, AValue, AReceiver)
    else
      Result := False;
  end;
end;

function TGocciaProxyValue.HasProperty(const AName: string): Boolean;
begin
  Result := HasTrap(AName);
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
        ThrowTypeError(Format(SErrorProxyHasNonConfigurable, [AName]), SSuggestProxyTrapInvariant);
      // Cannot hide own property on non-extensible target
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError(SErrorProxyHasNonExtensible, SSuggestProxyTrapInvariant);
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
        ThrowTypeError(SErrorProxyHasSymbolNonConfigurable, SSuggestProxyTrapInvariant);
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError(SErrorProxyHasSymbolNonExtensible, SSuggestProxyTrapInvariant);
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
          ThrowTypeError(SErrorProxyGetSymbolNonConfigurable, SSuggestProxyTrapInvariant);
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError(SErrorProxyGetSymbolNoGetter, SSuggestProxyTrapInvariant);
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

// ES2026 §10.5.8 [[Get]](P, Receiver) — symbol key, receiver-aware
function TGocciaProxyValue.GetSymbolPropertyWithReceiver(
  const ASymbol: TGocciaSymbolValue;
  const AReceiver: TGocciaValue): TGocciaValue;
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
      Args.Add(AReceiver);
      Result := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.8 step 8-9: Invariant validation for symbol keys.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, Result) then
          ThrowTypeError(SErrorProxyGetSymbolNonConfigurable, SSuggestProxyTrapInvariant);
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Getter) and
           not (Result is TGocciaUndefinedLiteralValue) then
          ThrowTypeError(SErrorProxyGetSymbolNoGetter, SSuggestProxyTrapInvariant);
      end;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).GetSymbolPropertyWithReceiver(ASymbol, AReceiver)
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

// ES2026 §10.5.9 [[Set]](P, V, Receiver) — symbol key, receiver-aware
function TGocciaProxyValue.AssignSymbolPropertyWithReceiver(
  const ASymbol: TGocciaSymbolValue; const AValue: TGocciaValue;
  const AReceiver: TGocciaValue): Boolean;
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
      Args.Add(ASymbol);
      Args.Add(AValue);
      Args.Add(AReceiver);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        Exit(False);
    finally
      Args.Free;
    end;

    // ES2026 §10.5.9 step 11-12: Invariant validation after truthy result.
    if FTarget is TGocciaObjectValue then
    begin
      TargetDesc := TGocciaObjectValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol);
      if Assigned(TargetDesc) and not TargetDesc.Configurable then
      begin
        // Non-configurable, non-writable data property: value must match
        if (TargetDesc is TGocciaPropertyDescriptorData) and
           not TargetDesc.Writable and
           not IsSameValue(TGocciaPropertyDescriptorData(TargetDesc).Value, AValue) then
          ThrowTypeError(SErrorProxySetSymbolNonConfigurable, SSuggestProxyTrapInvariant);
        // Non-configurable accessor without setter
        if (TargetDesc is TGocciaPropertyDescriptorAccessor) and
           not Assigned(TGocciaPropertyDescriptorAccessor(TargetDesc).Setter) then
          ThrowTypeError(SErrorProxySetSymbolNoSetter, SSuggestProxyTrapInvariant);
      end;
    end;

    Result := True;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).AssignSymbolPropertyWithReceiver(ASymbol, AValue, AReceiver)
    else
      Result := False;
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
        ThrowTypeError(Format(SErrorProxyDeleteNonConfigurable, [AName]), SSuggestProxyTrapInvariant);
      // Cannot delete own property on non-extensible target (property still exists)
      if Assigned(TargetDesc) and not TGocciaObjectValue(FTarget).Extensible then
        ThrowTypeError(SErrorProxyDeleteNonExtensible, SSuggestProxyTrapInvariant);
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
  TargetObject: TGocciaObjectValue;
  TargetDesc: TGocciaPropertyDescriptor;
  TrapDesc: TGocciaPropertyDescriptor;
  CompletedDesc: TGocciaPropertyDescriptor;
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

    TargetObject := nil;
    TargetDesc := nil;
    if FTarget is TGocciaObjectValue then
    begin
      TargetObject := TGocciaObjectValue(FTarget);
      TargetDesc := TargetObject.GetOwnPropertyDescriptor(AName);
    end;

    // Per spec, trap must return an object or undefined (not null)
    if TrapResult is TGocciaUndefinedLiteralValue then
    begin
      // ES2026 §28.1.1 step 10-11: Cannot hide non-configurable property
      if Assigned(TargetObject) then
      begin
        if Assigned(TargetDesc) and not TargetDesc.Configurable then
          ThrowTypeError(Format(SErrorProxyGetOwnNonConfigurable, [AName]), SSuggestProxyTrapInvariant);
        if Assigned(TargetDesc) and not TargetObject.Extensible then
          ThrowTypeError(SErrorProxyGetOwnNonExtensible, SSuggestProxyTrapInvariant);
      end;
      Exit(nil);
    end;

    if not (TrapResult is TGocciaObjectValue) then
      ThrowTypeError(SErrorProxyGetOwnReturnType, SSuggestProxyTrapReturnType);

    TrapDesc := ToPropertyDescriptor(TrapResult, TargetDesc);
    try
      CompletedDesc := CompleteProxyTrapPropertyDescriptor(TrapDesc);
    finally
      TrapDesc.Free;
    end;
    try
      if Assigned(TargetObject) then
        ValidateProxyGetOwnTrapDescriptor(AName, TargetObject, TargetDesc,
          CompletedDesc);
      Result := CompletedDesc;
    except
      CompletedDesc.Free;
      raise;
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

// ES2026 §10.5.5 [[GetOwnProperty]](P) — symbol key
function TGocciaProxyValue.GetOwnSymbolPropertyDescriptor(
  const ASymbol: TGocciaSymbolValue): TGocciaPropertyDescriptor;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  TrapResult: TGocciaValue;
  TargetObject: TGocciaObjectValue;
  TargetDesc: TGocciaPropertyDescriptor;
  TrapDesc: TGocciaPropertyDescriptor;
  CompletedDesc: TGocciaPropertyDescriptor;
  PropertyLabel: string;
begin
  CheckRevoked;
  Trap := GetTrap(PROP_GET_OWN_PROPERTY_DESCRIPTOR);
  if Assigned(Trap) then
  begin
    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      TrapResult := InvokeTrap(Trap, Args);
    finally
      Args.Free;
    end;

    TargetObject := nil;
    TargetDesc := nil;
    PropertyLabel := ASymbol.ToDisplayString.Value;
    if FTarget is TGocciaObjectValue then
    begin
      TargetObject := TGocciaObjectValue(FTarget);
      TargetDesc := TargetObject.GetOwnSymbolPropertyDescriptor(ASymbol);
    end;

    if TrapResult is TGocciaUndefinedLiteralValue then
    begin
      if Assigned(TargetObject) then
      begin
        if Assigned(TargetDesc) and not TargetDesc.Configurable then
          ThrowTypeError(Format(SErrorProxyGetOwnNonConfigurable,
            [PropertyLabel]), SSuggestProxyTrapInvariant);
        if Assigned(TargetDesc) and not TargetObject.Extensible then
          ThrowTypeError(SErrorProxyGetOwnNonExtensible,
            SSuggestProxyTrapInvariant);
      end;
      Exit(nil);
    end;

    if not (TrapResult is TGocciaObjectValue) then
      ThrowTypeError(SErrorProxyGetOwnReturnType, SSuggestProxyTrapReturnType);

    TrapDesc := ToPropertyDescriptor(TrapResult, TargetDesc);
    try
      CompletedDesc := CompleteProxyTrapPropertyDescriptor(TrapDesc);
    finally
      TrapDesc.Free;
    end;
    try
      if Assigned(TargetObject) then
        ValidateProxyGetOwnTrapDescriptor(PropertyLabel, TargetObject,
          TargetDesc, CompletedDesc);
      Result := CompletedDesc;
    except
      CompletedDesc.Free;
      raise;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).GetOwnSymbolPropertyDescriptor(ASymbol)
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
    // Build descriptor object — preserve descriptor field presence.
    DescObj := TGocciaObjectValue.Create;
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if ADescriptor.HasGet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
          DescObj.AssignProperty(PROP_GET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
        else
          DescObj.AssignProperty(PROP_GET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
      if ADescriptor.HasSet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
          DescObj.AssignProperty(PROP_SET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
        else
          DescObj.AssignProperty(PROP_SET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      if ADescriptor.HasValue then
        DescObj.AssignProperty(PROP_VALUE,
          TGocciaPropertyDescriptorData(ADescriptor).Value);
      if ADescriptor.HasWritableField then
        DescObj.AssignProperty(PROP_WRITABLE,
          TGocciaBooleanLiteralValue.Create(ADescriptor.Writable));
    end;
    if ADescriptor.HasEnumerableField then
      DescObj.AssignProperty(PROP_ENUMERABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Enumerable));
    if ADescriptor.HasConfigurableField then
      DescObj.AssignProperty(PROP_CONFIGURABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Configurable));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(TGocciaStringLiteralValue.Create(AName));
      Args.Add(DescObj);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [AName]), SSuggestProxyTrapInvariant);
      ADescriptor.Free;
    finally
      Args.Free;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      TGocciaObjectValue(FTarget).DefineProperty(AName, ADescriptor)
    else
      ThrowTypeError(SErrorProxyDefineNonObject, SSuggestProxyTargetType);
  end;
end;

// ES2026 §10.5.6 [[DefineOwnProperty]](P, Desc) — symbol key
procedure TGocciaProxyValue.DefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
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
    // Build descriptor object — preserve descriptor field presence.
    DescObj := TGocciaObjectValue.Create;
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if ADescriptor.HasGet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
          DescObj.AssignProperty(PROP_GET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
        else
          DescObj.AssignProperty(PROP_GET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
      if ADescriptor.HasSet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
          DescObj.AssignProperty(PROP_SET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
        else
          DescObj.AssignProperty(PROP_SET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      if ADescriptor.HasValue then
        DescObj.AssignProperty(PROP_VALUE,
          TGocciaPropertyDescriptorData(ADescriptor).Value);
      if ADescriptor.HasWritableField then
        DescObj.AssignProperty(PROP_WRITABLE,
          TGocciaBooleanLiteralValue.Create(ADescriptor.Writable));
    end;
    if ADescriptor.HasEnumerableField then
      DescObj.AssignProperty(PROP_ENUMERABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Enumerable));
    if ADescriptor.HasConfigurableField then
      DescObj.AssignProperty(PROP_CONFIGURABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Configurable));

    Args := TGocciaArgumentsCollection.Create;
    try
      Args.Add(FTarget);
      Args.Add(ASymbol);
      Args.Add(DescObj);
      TrapResult := InvokeTrap(Trap, Args);
      if not TrapResult.ToBooleanLiteral.Value then
        ThrowTypeError(Format(SErrorProxyDefineReturnedFalse, [ASymbol.ToDisplayString.Value]), SSuggestProxyTrapInvariant);
      ADescriptor.Free;
    finally
      Args.Free;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      TGocciaObjectValue(FTarget).DefineSymbolProperty(ASymbol, ADescriptor)
    else
      ThrowTypeError(SErrorProxyDefineNonObject, SSuggestProxyTargetType);
  end;
end;

// ES2026 §10.5.6 [[DefineOwnProperty]](P, Desc) — boolean variant
// Returns false instead of throwing when the trap returns a falsy value.
function TGocciaProxyValue.TryDefineProperty(const AName: string;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
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
    // Build descriptor object — preserve descriptor field presence.
    DescObj := TGocciaObjectValue.Create;
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if ADescriptor.HasGet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
          DescObj.AssignProperty(PROP_GET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
        else
          DescObj.AssignProperty(PROP_GET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
      if ADescriptor.HasSet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
          DescObj.AssignProperty(PROP_SET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
        else
          DescObj.AssignProperty(PROP_SET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      if ADescriptor.HasValue then
        DescObj.AssignProperty(PROP_VALUE,
          TGocciaPropertyDescriptorData(ADescriptor).Value);
      if ADescriptor.HasWritableField then
        DescObj.AssignProperty(PROP_WRITABLE,
          TGocciaBooleanLiteralValue.Create(ADescriptor.Writable));
    end;
    if ADescriptor.HasEnumerableField then
      DescObj.AssignProperty(PROP_ENUMERABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Enumerable));
    if ADescriptor.HasConfigurableField then
      DescObj.AssignProperty(PROP_CONFIGURABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Configurable));

    Args := TGocciaArgumentsCollection.Create;
    try
      try
        Args.Add(FTarget);
        Args.Add(TGocciaStringLiteralValue.Create(AName));
        Args.Add(DescObj);
        TrapResult := InvokeTrap(Trap, Args);
        Result := TrapResult.ToBooleanLiteral.Value;
      finally
        ADescriptor.Free;
      end;
    finally
      Args.Free;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).TryDefineProperty(AName, ADescriptor)
    else
    begin
      ADescriptor.Free;
      Result := False;
    end;
  end;
end;

// ES2026 §10.5.6 [[DefineOwnProperty]](P, Desc) — symbol key, boolean variant
function TGocciaProxyValue.TryDefineSymbolProperty(
  const ASymbol: TGocciaSymbolValue;
  const ADescriptor: TGocciaPropertyDescriptor): Boolean;
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
    // Build descriptor object — preserve descriptor field presence.
    DescObj := TGocciaObjectValue.Create;
    if ADescriptor is TGocciaPropertyDescriptorAccessor then
    begin
      if ADescriptor.HasGet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Getter) then
          DescObj.AssignProperty(PROP_GET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Getter)
        else
          DescObj.AssignProperty(PROP_GET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
      if ADescriptor.HasSet then
      begin
        if Assigned(TGocciaPropertyDescriptorAccessor(ADescriptor).Setter) then
          DescObj.AssignProperty(PROP_SET,
            TGocciaPropertyDescriptorAccessor(ADescriptor).Setter)
        else
          DescObj.AssignProperty(PROP_SET,
            TGocciaUndefinedLiteralValue.UndefinedValue);
      end;
    end
    else if ADescriptor is TGocciaPropertyDescriptorData then
    begin
      if ADescriptor.HasValue then
        DescObj.AssignProperty(PROP_VALUE,
          TGocciaPropertyDescriptorData(ADescriptor).Value);
      if ADescriptor.HasWritableField then
        DescObj.AssignProperty(PROP_WRITABLE,
          TGocciaBooleanLiteralValue.Create(ADescriptor.Writable));
    end;
    if ADescriptor.HasEnumerableField then
      DescObj.AssignProperty(PROP_ENUMERABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Enumerable));
    if ADescriptor.HasConfigurableField then
      DescObj.AssignProperty(PROP_CONFIGURABLE,
        TGocciaBooleanLiteralValue.Create(ADescriptor.Configurable));

    Args := TGocciaArgumentsCollection.Create;
    try
      try
        Args.Add(FTarget);
        Args.Add(ASymbol);
        Args.Add(DescObj);
        TrapResult := InvokeTrap(Trap, Args);
        Result := TrapResult.ToBooleanLiteral.Value;
      finally
        ADescriptor.Free;
      end;
    finally
      Args.Free;
    end;
  end
  else
  begin
    if FTarget is TGocciaObjectValue then
      Result := TGocciaObjectValue(FTarget).TryDefineSymbolProperty(ASymbol, ADescriptor)
    else
    begin
      ADescriptor.Free;
      Result := False;
    end;
  end;
end;

// ES2026 §28.1.1 [[OwnPropertyKeys]]()
function TGocciaProxyValue.GetOwnPropertyKeys: TArray<string>;
var
  SymbolKeys: TArray<TGocciaSymbolValue>;
  OrderedKeys: TArray<TGocciaValue>;
begin
  CollectOwnPropertyTrapKeys(Result, SymbolKeys, OrderedKeys);
end;

function TGocciaProxyValue.GetOwnPropertyNames: TArray<string>;
begin
  Result := GetOwnPropertyKeys;
end;

function TGocciaProxyValue.GetOwnPropertyKeyValues: TArray<TGocciaValue>;
var
  StringKeys: TArray<string>;
  SymbolKeys: TArray<TGocciaSymbolValue>;
begin
  CollectOwnPropertyTrapKeys(StringKeys, SymbolKeys, Result);
end;

function TGocciaProxyValue.GetOwnSymbols: TArray<TGocciaSymbolValue>;
var
  OrderedKeys: TArray<TGocciaValue>;
  StringKeys: TArray<string>;
begin
  CollectOwnPropertyTrapKeys(StringKeys, Result, OrderedKeys);
end;

function TGocciaProxyValue.GetEnumerablePropertyNames: TArray<string>;
var
  AllKeys: TArray<string>;
  Descriptor: TGocciaPropertyDescriptor;
  DescriptorTrap: TGocciaValue;
  FilteredKeys: TArray<string>;
  I, Count: Integer;
  TargetObj: TGocciaObjectValue;
begin
  AllKeys := GetOwnPropertyKeys;
  SetLength(FilteredKeys, Length(AllKeys));
  Count := 0;
  DescriptorTrap := GetTrap(PROP_GET_OWN_PROPERTY_DESCRIPTOR);
  if FTarget is TGocciaObjectValue then
    TargetObj := TGocciaObjectValue(FTarget)
  else
    TargetObj := nil;
  for I := 0 to Length(AllKeys) - 1 do
  begin
    if Assigned(DescriptorTrap) then
      Descriptor := GetOwnPropertyDescriptor(AllKeys[I])
    else if Assigned(TargetObj) then
      Descriptor := TargetObj.GetOwnPropertyDescriptor(AllKeys[I])
    else
      Descriptor := nil;
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
      ThrowTypeError(SErrorProxyGetProtoReturnType, SSuggestProxyTrapReturnType);

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
        ThrowTypeError(SErrorProxyGetProtoMismatch, SSuggestProxyTrapInvariant);
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
          ThrowTypeError(SErrorProxySetProtoNonExtensible, SSuggestProxyTrapInvariant);
      end
      else if AProto is TGocciaNullLiteralValue then
      begin
        if Assigned(TGocciaObjectValue(FTarget).Prototype) then
          ThrowTypeError(SErrorProxySetProtoNonExtensible, SSuggestProxyTrapInvariant);
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
      ThrowTypeError(SErrorProxyIsExtensibleMismatch, SSuggestProxyTrapInvariant);
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
        ThrowTypeError(SErrorProxyPreventExtensionsFalse, SSuggestProxyTrapInvariant);
    finally
      Args.Free;
    end;

    // ES2026 §28.1.1 step 8: Invariant — if trap returned true, target
    // must actually be non-extensible now.
    if (FTarget is TGocciaObjectValue) and
       TGocciaObjectValue(FTarget).Extensible then
      ThrowTypeError(SErrorProxyPreventExtensionsStillExtensible, SSuggestProxyTrapInvariant);
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
    ThrowTypeError(SErrorProxyApplyNonFunction, SSuggestProxyTargetType);

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
      ThrowTypeError(SErrorProxyTargetNotCallable, SSuggestProxyTargetType);
  end;
end;

// ES2026 §28.1.1 [[Construct]](argumentsList, newTarget)
function TGocciaProxyValue.ConstructTrap(
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
var
  Trap: TGocciaValue;
  Args: TGocciaArgumentsCollection;
  ArgsArray: TGocciaArrayValue;
  EffectiveNewTarget: TGocciaValue;
  I: Integer;
begin
  CheckRevoked;

  // ES2026 §28.1.1 step 1: Proxy [[Construct]] only exists when target
  // is constructable. Validate before dispatching to the trap.
  if not FTarget.IsConstructable then
    ThrowTypeError(SErrorProxyTargetNotConstructor, SSuggestProxyTargetType);

  // Default newTarget for `new proxy(...)` is the proxy itself per ES2026.
  if Assigned(ANewTarget) then
    EffectiveNewTarget := ANewTarget
  else
    EffectiveNewTarget := Self;

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
      Args.Add(EffectiveNewTarget);
      Result := InvokeTrap(Trap, Args);
      if Result.IsPrimitive then
        ThrowTypeError(SErrorProxyConstructReturnType, SSuggestProxyTrapReturnType);
    finally
      Args.Free;
    end;
  end
  else
    // No construct trap: forward to target's [[Construct]](args, EffectiveNewTarget)
    // through the shared dispatch — handles bound chain unwrap (so
    // `new Proxy(F.bind(obj), {})()` does not leak the bound `this` into the
    // synthetic receiver), nested proxies, classes, ordinary functions, and
    // native constructors. Native-constructor newTarget propagation remains
    // tracked in #530.
    Result := ConstructValue(FTarget, AArguments, EffectiveNewTarget);
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

function TGocciaProxyValue.IsConstructable: Boolean;
begin
  Result := FTarget.IsConstructable;
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

function IsProxyDispatchValue(const AValue: TGocciaValue): Boolean;
begin
  Result := AValue is TGocciaProxyValue;
end;

function DispatchProxyApply(const AProxy: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaProxyValue(AProxy).ApplyTrap(AArguments, AThisValue);
end;

function DispatchProxyConstruct(const AProxy: TGocciaValue;
  const AArguments: TGocciaArgumentsCollection;
  const ANewTarget: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaProxyValue(AProxy).ConstructTrap(AArguments, ANewTarget);
end;

function DispatchProxyGetPrototype(const AProxy: TGocciaObjectValue): TGocciaValue;
begin
  Result := TGocciaProxyValue(AProxy).GetPrototypeTrap;
end;

initialization
  RegisterProxyDispatchHooks(IsProxyDispatchValue, DispatchProxyApply,
    DispatchProxyConstruct, DispatchProxyGetPrototype);

end.

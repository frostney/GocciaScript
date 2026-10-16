unit Goccia.Decorators;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.GarbageCollector,
  Goccia.Values.NativeFunction,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives;

type
  { The list of extra initializers a class's decorators of one placement add
    through context.addInitializer (decorators proposal,
    CreateAddInitializerFunction's _initializers_). Managed by the garbage
    collector: each addInitializer function made for it keeps it alive, so a
    decorator that keeps its context can still reach it after the class is
    defined. }
  TGocciaInitializerCollector = class(TGCManagedObject)
  private
    FInitializers: array of TGocciaValue;
  public
    procedure AfterConstruction; override;
    procedure Add(const AInitializer: TGocciaValue);
    function GetInitializers: TArray<TGocciaValue>;
    procedure MarkReferences; override;
  end;

  { An initializer addInitializer accepted that is callable but not a
    function object: a callable Proxy, or a class (calling which throws a
    TypeError). A class runs its initializers as function objects, so this
    stands in for the value, keeps it alive as its CapturedRoot, and performs
    Call(value, receiver) on it. Script never sees it. }
  TGocciaCallableInitializer = class(TGocciaNativeFunctionValue)
  private
    function Invoke(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ATarget: TGocciaValue);
  end;

  { The addInitializer function of one decorator's context object
    (decorators proposal, CreateAddInitializerFunction). It holds that
    decorator's decorationState: Finish sets [[Finished]] once the decorator
    has returned, after which a call throws a TypeError. }
  TGocciaAddInitializerFunction = class(TGocciaNativeFunctionValue)
  private
    FCollector: TGocciaInitializerCollector;
    FFinished: Boolean;
    function AddInitializer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
  public
    constructor Create(const ACollector: TGocciaInitializerCollector);
    procedure Finish;
    procedure MarkReferences; override;
  end;

  TGocciaAccessGetter = class
  private
    FTarget: TGocciaValue;
    FPropertyName: string;
    FPropertyKey: TGocciaValue;
  public
    constructor Create(const ATarget: TGocciaValue; const APropertyName: string);
    constructor CreateWithKey(const ATarget: TGocciaValue; const APropertyKey: TGocciaValue);
    function Get(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAccessSetter = class
  private
    FPropertyName: string;
    FPropertyKey: TGocciaValue;
  public
    constructor Create(const APropertyName: string);
    constructor CreateWithKey(const APropertyKey: TGocciaValue);
    function SetValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ClassValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

{ TGocciaInitializerCollector }

procedure TGocciaInitializerCollector.AfterConstruction;
var
  GC: TGarbageCollector;
begin
  inherited;
  GCIndex := -1;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.RegisterObject(Self);
end;

procedure TGocciaInitializerCollector.Add(const AInitializer: TGocciaValue);
begin
  SetLength(FInitializers, Length(FInitializers) + 1);
  FInitializers[High(FInitializers)] := AInitializer;
end;

function TGocciaInitializerCollector.GetInitializers: TArray<TGocciaValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(FInitializers));
  for I := 0 to High(FInitializers) do
    Result[I] := FInitializers[I];
end;

procedure TGocciaInitializerCollector.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then
    Exit;
  inherited;
  for I := 0 to High(FInitializers) do
    if Assigned(FInitializers[I]) then
      FInitializers[I].MarkReferences;
end;

{ TGocciaCallableInitializer }

constructor TGocciaCallableInitializer.Create(const ATarget: TGocciaValue);
begin
  inherited CreateWithoutPrototype(Invoke, '', 0);
  CapturedRoot := ATarget;
end;

function TGocciaCallableInitializer.Invoke(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := DispatchCall(CapturedRoot, AArgs, AThisValue);
end;

{ TGocciaAddInitializerFunction }

constructor TGocciaAddInitializerFunction.Create(
  const ACollector: TGocciaInitializerCollector);
begin
  inherited CreateWithoutPrototype(AddInitializer, PROP_ADD_INITIALIZER, 1);
  FCollector := ACollector;
  FFinished := False;
end;

// Decorators proposal, CreateAddInitializerFunction, the closure's steps 1-4.
function TGocciaAddInitializerFunction.AddInitializer(
  const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Initializer: TGocciaValue;
begin
  if FFinished then
    ThrowTypeError(SErrorAddInitializerAfterDecoration,
      SSuggestAddInitializerDuringDecoration);
  if AArgs.Length > 0 then
    Initializer := AArgs.GetElement(0)
  else
    Initializer := TGocciaUndefinedLiteralValue.UndefinedValue;
  if not Initializer.IsCallable then
    ThrowTypeError(SErrorAddInitializerNotCallable,
      SSuggestAddInitializerFunction);
  if Initializer is TGocciaFunctionBase then
    FCollector.Add(Initializer)
  else
    FCollector.Add(TGocciaCallableInitializer.Create(Initializer));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

procedure TGocciaAddInitializerFunction.Finish;
begin
  FFinished := True;
end;

procedure TGocciaAddInitializerFunction.MarkReferences;
begin
  if GCMarked then
    Exit;
  inherited;
  if Assigned(FCollector) then
    FCollector.MarkReferences;
end;

{ TGocciaAccessGetter }

constructor TGocciaAccessGetter.Create(const ATarget: TGocciaValue; const APropertyName: string);
begin
  FTarget := ATarget;
  FPropertyName := APropertyName;
  FPropertyKey := nil;
end;

constructor TGocciaAccessGetter.CreateWithKey(const ATarget: TGocciaValue; const APropertyKey: TGocciaValue);
begin
  FTarget := ATarget;
  FPropertyKey := APropertyKey;
  if Assigned(APropertyKey) and not (APropertyKey is TGocciaSymbolValue) then
    FPropertyName := APropertyKey.ToStringLiteral.Value
  else
    FPropertyName := '';
end;

function TGocciaAccessGetter.Get(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
begin
  if AArgs.Length > 0 then
    Target := AArgs.GetElement(0)
  else if Assigned(AThisValue) and (AThisValue is TGocciaObjectValue) then
    Target := AThisValue
  else
    Target := FTarget;

  if FPropertyKey is TGocciaSymbolValue then
  begin
    if Target is TGocciaClassValue then
      Result := TGocciaClassValue(Target).GetSymbolProperty(
        TGocciaSymbolValue(FPropertyKey))
    else if Target is TGocciaObjectValue then
      Result := TGocciaObjectValue(Target).GetSymbolProperty(
        TGocciaSymbolValue(FPropertyKey))
    else
      Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end
  else if Assigned(Target) then
    Result := Target.GetProperty(FPropertyName)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ TGocciaAccessSetter }

constructor TGocciaAccessSetter.Create(const APropertyName: string);
begin
  FPropertyName := APropertyName;
  FPropertyKey := nil;
end;

constructor TGocciaAccessSetter.CreateWithKey(const APropertyKey: TGocciaValue);
begin
  FPropertyKey := APropertyKey;
  if Assigned(APropertyKey) and not (APropertyKey is TGocciaSymbolValue) then
    FPropertyName := APropertyKey.ToStringLiteral.Value
  else
    FPropertyName := '';
end;

function TGocciaAccessSetter.SetValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Target: TGocciaValue;
  NewValue: TGocciaValue;
begin
  if AArgs.Length >= 2 then
  begin
    Target := AArgs.GetElement(0);
    NewValue := AArgs.GetElement(1);
  end
  else
  begin
    Target := AThisValue;
    if AArgs.Length > 0 then
      NewValue := AArgs.GetElement(0)
    else
      NewValue := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;

  if Target is TGocciaObjectValue then
  begin
    if FPropertyKey is TGocciaSymbolValue then
    begin
      if Target is TGocciaClassValue then
        TGocciaClassValue(Target).AssignSymbolProperty(
          TGocciaSymbolValue(FPropertyKey), NewValue)
      else
        TGocciaObjectValue(Target).AssignSymbolProperty(
          TGocciaSymbolValue(FPropertyKey), NewValue);
    end
    else if Target is TGocciaClassValue then
      TGocciaClassValue(Target).SetProperty(FPropertyName, NewValue)
    else
      TGocciaObjectValue(Target).AssignProperty(FPropertyName, NewValue);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.

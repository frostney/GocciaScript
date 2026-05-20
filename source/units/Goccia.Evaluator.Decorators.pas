unit Goccia.Evaluator.Decorators;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.Primitives;

type
  TGocciaInitializerCollector = class
  private
    FInitializers: array of TGocciaValue;
  public
    function AddInitializer(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetInitializers: TArray<TGocciaValue>;
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
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

{ TGocciaInitializerCollector }

function TGocciaInitializerCollector.AddInitializer(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if (AArgs.Length > 0) and AArgs.GetElement(0).IsCallable then
  begin
    SetLength(FInitializers, Length(FInitializers) + 1);
    FInitializers[High(FInitializers)] := AArgs.GetElement(0);
  end;
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

function TGocciaInitializerCollector.GetInitializers: TArray<TGocciaValue>;
var
  I: Integer;
begin
  SetLength(Result, Length(FInitializers));
  for I := 0 to High(FInitializers) do
    Result[I] := FInitializers[I];
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

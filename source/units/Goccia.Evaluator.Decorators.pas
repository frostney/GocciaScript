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
  public
    constructor Create(const ATarget: TGocciaValue; const APropertyName: string);
    function Get(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAccessSetter = class
  private
    FPropertyName: string;
  public
    constructor Create(const APropertyName: string);
    function SetValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.ObjectValue;

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
end;

function TGocciaAccessGetter.Get(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(AThisValue) and (AThisValue is TGocciaObjectValue) then
    Result := AThisValue.GetProperty(FPropertyName)
  else if Assigned(FTarget) then
    Result := FTarget.GetProperty(FPropertyName)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ TGocciaAccessSetter }

constructor TGocciaAccessSetter.Create(const APropertyName: string);
begin
  FPropertyName := APropertyName;
end;

function TGocciaAccessSetter.SetValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(AThisValue) and (AThisValue is TGocciaObjectValue) then
    TGocciaObjectValue(AThisValue).AssignProperty(FPropertyName, AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.

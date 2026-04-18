unit Goccia.Values.AutoAccessor;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  TGocciaAutoAccessorGetter = class
  private
    FBackingName: string;
  public
    constructor Create(const ABackingName: string);
    function Get(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

  TGocciaAutoAccessorSetter = class
  private
    FBackingName: string;
  public
    constructor Create(const ABackingName: string);
    function SetValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.ObjectValue;

{ TGocciaAutoAccessorGetter }

constructor TGocciaAutoAccessorGetter.Create(const ABackingName: string);
begin
  FBackingName := ABackingName;
end;

function TGocciaAutoAccessorGetter.Get(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(AThisValue) and (AThisValue is TGocciaObjectValue) then
    Result := TGocciaObjectValue(AThisValue).GetProperty(FBackingName)
  else
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

{ TGocciaAutoAccessorSetter }

constructor TGocciaAutoAccessorSetter.Create(const ABackingName: string);
begin
  FBackingName := ABackingName;
end;

function TGocciaAutoAccessorSetter.SetValue(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  if Assigned(AThisValue) and (AThisValue is TGocciaObjectValue) then
    TGocciaObjectValue(AThisValue).AssignProperty(FBackingName, AArgs.GetElement(0));
  Result := TGocciaUndefinedLiteralValue.UndefinedValue;
end;

end.

unit Goccia.Values.Base;

{$I Goccia.inc}

interface

uses
  Classes;

type
  TGocciaValue = class(TInterfacedObject)
  public
    function ToString: string; virtual; abstract;
    function ToBoolean: Boolean; virtual; abstract;
    function ToNumber: Double; virtual; abstract;
    function TypeName: string; virtual; abstract;
  end;

  TGocciaOptionalValue<T: TGocciaValue> = class(TGocciaValue)
  private
    function GetIsUndefined: Boolean;
  protected
    FValue: T;
  public
    constructor Create(AValue: T);

    function ToString: string; override;
    function ToBoolean: Boolean; override;
    function ToNumber: Double; override;
    function TypeName: string; override;

    property Value: T read FValue;
    property IsUndefined: Boolean read GetIsUndefined;
  end;

implementation

uses
  Goccia.Values.Undefined;

constructor TGocciaOptionalValue<T>.Create(AValue: T);
begin
  inherited Create;
  FValue := AValue;
end;

function TGocciaOptionalValue<T>.GetIsUndefined: Boolean;
begin
  Result := FValue = nil;
end;

function TGocciaOptionalValue<T>.ToBoolean: Boolean;
begin
  if GetIsUndefined then
    Result := TGocciaUndefinedValue.Create.ToBoolean
  else
    Result := FValue.ToBoolean;
end;

function TGocciaOptionalValue<T>.ToNumber: Double;
begin
  if GetIsUndefined then
    Result := TGocciaUndefinedValue.Create.ToNumber
  else
    Result := FValue.ToNumber;
end;

function TGocciaOptionalValue<T>.TypeName: string;
begin
  if GetIsUndefined then
    Result := TGocciaUndefinedValue.Create.TypeName
  else
    Result := FValue.TypeName;
end;

function TGocciaOptionalValue<T>.ToString: string;
begin
  if GetIsUndefined then
    Result := TGocciaUndefinedValue.Create.ToString
  else
    Result := FValue.ToString;
end;

end.

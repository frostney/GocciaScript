unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.Values.Base, Goccia.Values.ObjectValue, Generics.Collections, SysUtils, Math, Goccia.Logger;

type
  TGocciaNativeFunction = function(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue of object;

  TGocciaNativeFunctionValue = class(TGocciaObjectValue, IGocciaCallable)
  private
    FFunction: TGocciaNativeFunction;
    FName: string;
    FArity: Integer;
  public
    constructor Create(AFunction: TGocciaNativeFunction; const AName: string;
      AArity: Integer);
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function ToString: string; override;
    function ToNumber: Double; override;
    function TypeName: string; override;
    property NativeFunction: TGocciaNativeFunction read FFunction;
    property Name: string read FName;
    property Arity: Integer read FArity;
  end;


implementation

constructor TGocciaNativeFunctionValue.Create(AFunction: TGocciaNativeFunction;
  const AName: string; AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create;
end;

function TGocciaNativeFunctionValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FFunction(Arguments, ThisValue);
end;

function TGocciaNativeFunctionValue.ToString: string;
begin
  Result := Format('[NativeFunction: %s]', [FName]);
end;

function TGocciaNativeFunctionValue.ToNumber: Double;
begin
  Result := NaN;
end;

function TGocciaNativeFunctionValue.TypeName: string;
begin
  Result := 'function';
end;

end.

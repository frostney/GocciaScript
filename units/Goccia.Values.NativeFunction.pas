unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Interfaces, Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.FunctionBase, Generics.Collections, SysUtils, Math, Goccia.Logger;

type
  TGocciaNativeFunction = function(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue of object;

  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunction;
    FName: string;
    FArity: Integer;
  public
    constructor Create(AFunction: TGocciaNativeFunction; const AName: string;
      AArity: Integer);
    constructor CreateWithoutPrototype(AFunction: TGocciaNativeFunction; const AName: string;
      AArity: Integer);
    function Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
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

constructor TGocciaNativeFunctionValue.CreateWithoutPrototype(AFunction: TGocciaNativeFunction;
  const AName: string; AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create; // No prototype for methods that are part of the prototype
end;

function TGocciaNativeFunctionValue.Call(Arguments: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FFunction(Arguments, ThisValue);
end;


end.

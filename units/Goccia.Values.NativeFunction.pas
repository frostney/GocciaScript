unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives, Goccia.Values.ObjectValue, Goccia.Values.FunctionBase,
  Goccia.Arguments.Collection, Goccia.Values.NativeFunctionCallback, Generics.Collections, SysUtils, Math, Goccia.Logger;

type
  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunctionCallback;
    FName: string;
    FArity: Integer;
  public
    constructor Create(AFunction: TGocciaNativeFunctionCallback; const AName: string;
      AArity: Integer);
    constructor CreateWithoutPrototype(AFunction: TGocciaNativeFunctionCallback; const AName: string;
      AArity: Integer);
    function Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    property NativeFunction: TGocciaNativeFunctionCallback read FFunction;
    property Name: string read FName;
    property Arity: Integer read FArity;
  end;


implementation

constructor TGocciaNativeFunctionValue.Create(AFunction: TGocciaNativeFunctionCallback;
  const AName: string; AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create;
end;

constructor TGocciaNativeFunctionValue.CreateWithoutPrototype(AFunction: TGocciaNativeFunctionCallback;
  const AName: string; AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create; // No prototype for methods that are part of the prototype
end;

function TGocciaNativeFunctionValue.Call(Arguments: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FFunction(Arguments, ThisValue);
end;


end.

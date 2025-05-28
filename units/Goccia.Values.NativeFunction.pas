unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Generics.Collections, SysUtils, Math;

type
  // TODO: Add reference to thisArg
  TGocciaNativeFunction = function(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue of object;

  // TODO: Make this a subclass of TGocciaFunctionValue
  TGocciaNativeFunctionValue = class(TGocciaValue)
  private
    FFunction: TGocciaNativeFunction;
    FName: string;
    FArity: Integer;
  public
    constructor Create(AFunction: TGocciaNativeFunction; const AName: string;
      AArity: Integer);
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
unit Goccia.Values.NativeFunction;

{$I Goccia.inc}

interface

uses
  Generics.Collections,
  Math,
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Logger,
  Goccia.Values.FunctionBase,
  Goccia.Values.NativeFunctionCallback,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaNativeFunctionValue = class(TGocciaFunctionBase)
  private
    FFunction: TGocciaNativeFunctionCallback;
    FName: string;
    FArity: Integer;
  protected
    function GetFunctionLength: Integer; override;
    function GetFunctionName: string; override;
  public
    constructor Create(const AFunction: TGocciaNativeFunctionCallback; const AName: string;
      const AArity: Integer);
    constructor CreateWithoutPrototype(const AFunction: TGocciaNativeFunctionCallback; const AName: string;
      const AArity: Integer);
    function Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue; override;
    property NativeFunction: TGocciaNativeFunctionCallback read FFunction;
    property Name: string read FName;
    property Arity: Integer read FArity;
  end;


implementation

constructor TGocciaNativeFunctionValue.Create(const AFunction: TGocciaNativeFunctionCallback;
  const AName: string; const AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create;
end;

constructor TGocciaNativeFunctionValue.CreateWithoutPrototype(const AFunction: TGocciaNativeFunctionCallback;
  const AName: string; const AArity: Integer);
begin
  FFunction := AFunction;
  FName := AName;
  FArity := AArity;

  inherited Create; // No prototype for methods that are part of the prototype
end;

function TGocciaNativeFunctionValue.Call(const AArguments: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := FFunction(AArguments, AThisValue);
end;

function TGocciaNativeFunctionValue.GetFunctionLength: Integer;
begin
  // -1 means variadic, report 0 for length per ECMAScript spec
  if FArity < 0 then
    Result := 0
  else
    Result := FArity;
end;

function TGocciaNativeFunctionValue.GetFunctionName: string;
begin
  Result := FName;
end;

end.

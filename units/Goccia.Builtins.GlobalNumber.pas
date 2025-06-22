unit Goccia.Builtins.GlobalNumber;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error, Goccia.Values.Base, Goccia.Values.Error, Goccia.Values.ObjectValue, Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue, SysUtils, Math, Generics.Collections;

type
  TGocciaGlobalNumber = class(TGocciaBuiltin)
  private
    FBuiltinNumber: TGocciaObjectValue;
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);

    function NumberParseInt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function NumberParseFloat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsFinite(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsInteger(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

constructor TGocciaGlobalNumber.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowError);
begin
  inherited Create(AName, AScope, AThrowError);

  FBuiltinNumber := TGocciaObjectValue.Create;

  FBuiltinNumber.DefineProperty('parseInt', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(NumberParseInt, 'parseInt', 1), [pfConfigurable, pfWritable]));
  FBuiltinNumber.DefineProperty('parseFloat', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(NumberParseFloat, 'parseFloat', 1), [pfConfigurable, pfWritable]));
  FBuiltinNumber.DefineProperty('isFinite', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(NumberIsFinite, 'isFinite', 0), [pfConfigurable, pfWritable]));
  FBuiltinNumber.DefineProperty('isNaN', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(NumberIsNaN, 'isNaN', 0), [pfConfigurable, pfWritable]));
  FBuiltinNumber.DefineProperty('isInteger', TGocciaPropertyDescriptorData.Create(
    TGocciaNativeFunctionValue.Create(NumberIsInteger, 'isInteger', 0), [pfConfigurable, pfWritable]));

  AScope.DefineBuiltin(AName, FBuiltinNumber);
end;

function TGocciaGlobalNumber.NumberParseInt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // TODO: Use parseInt
  Result := TGocciaNumberValue.Create(StrToInt(Args[0].ToString));
end;

function TGocciaGlobalNumber.NumberParseFloat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // TODO: Use parseFloat
  Result := TGocciaNumberValue.Create(StrToFloat(Args[0].ToString));
end;

function TGocciaGlobalNumber.NumberIsFinite(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // TODO: Use isfinite
  Result := TGocciaBooleanValue.Create(not IsInfinite(Args[0].ToNumber));
end;

function TGocciaGlobalNumber.NumberIsNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
begin
  // TODO: Use isnan
  Result := TGocciaBooleanValue.Create(IsNaN(Args[0].ToNumber));
end;

function TGocciaGlobalNumber.NumberIsInteger(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  Value: Integer;
begin
  if TryStrToInt(Args[0].ToString, Value) then
    Result := TGocciaBooleanValue.Create(True)
  else
    Result := TGocciaBooleanValue.Create(False);
end;


end.

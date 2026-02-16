unit Goccia.Builtins.GlobalNumber;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error, Goccia.Error.ThrowErrorCallback, Goccia.Values.Error, Goccia.Values.ObjectValue, 
  Goccia.Values.Primitives, Goccia.Arguments.Collection, Goccia.Arguments.Validator, SysUtils, Math, Generics.Collections;

type
  TGocciaGlobalNumber = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);

    function NumberParseInt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NumberParseFloat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsFinite(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsNaN(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsInteger(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
    function NumberIsSafeInteger(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.ClassHelper;

constructor TGocciaGlobalNumber.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
begin
  inherited Create(AName, AScope, AThrowError);

  // Number constants (non-writable, non-enumerable, non-configurable per ECMAScript spec)
  FBuiltinObject.DefineProperty('NaN', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.NaNValue, []));
  FBuiltinObject.DefineProperty('POSITIVE_INFINITY', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.InfinityValue, []));
  FBuiltinObject.DefineProperty('NEGATIVE_INFINITY', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.NegativeInfinityValue, []));
  FBuiltinObject.DefineProperty('MAX_SAFE_INTEGER', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(9007199254740991), []));
  FBuiltinObject.DefineProperty('MIN_SAFE_INTEGER', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(-9007199254740991), []));
  FBuiltinObject.DefineProperty('MAX_VALUE', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(1.7976931348623157e+308), []));
  FBuiltinObject.DefineProperty('MIN_VALUE', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(5e-324), []));
  FBuiltinObject.DefineProperty('EPSILON', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteralValue.Create(2.2204460492503131e-16), []));

  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberParseInt, 'parseInt', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberParseFloat, 'parseFloat', 1));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsFinite, 'isFinite', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsNaN, 'isNaN', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsInteger, 'isInteger', 0));
  FBuiltinObject.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsSafeInteger, 'isSafeInteger', 0));

  AScope.DefineLexicalBinding(AName, FBuiltinObject, dtLet);
end;

function TGocciaGlobalNumber.NumberParseInt(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  InputStr: string;
  Radix: Integer;
  I, StartPos: Integer;
  C: Char;
  Sign: Integer;
  ResultValue: Int64;
  ValidChars: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(Args, 1, 'Number.parseInt', ThrowError);
  TGocciaArgumentValidator.RequireAtMost(Args, 2, 'Number.parseInt', ThrowError);

  InputStr := Trim(Args.GetElement(0).ToStringLiteral.Value);
  if InputStr = '' then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Get radix
  if Args.Length > 1 then
    Radix := Trunc(Args.GetElement(1).ToNumberLiteral.Value)
  else
    Radix := 0; // 0 means auto-detect

  I := 1;
  Sign := 1;

  // Check for sign
  if (I <= Length(InputStr)) and (InputStr[I] = '-') then
  begin
    Sign := -1;
    Inc(I);
  end
  else if (I <= Length(InputStr)) and (InputStr[I] = '+') then
  begin
    Inc(I);
  end;

  // Auto-detect radix when radix is 0 or unspecified
  if (Radix = 0) then
  begin
    if (I + 1 <= Length(InputStr)) and (InputStr[I] = '0') and
       ((InputStr[I + 1] = 'x') or (InputStr[I + 1] = 'X')) then
    begin
      Radix := 16;
      Inc(I, 2); // Skip "0x" or "0X"
    end
    else
      Radix := 10;
  end
  // Special case for radix 16 with 0x prefix (even when radix explicitly specified)
  else if (Radix = 16) and (I + 1 <= Length(InputStr)) and (InputStr[I] = '0') and
          ((InputStr[I + 1] = 'x') or (InputStr[I + 1] = 'X')) then
  begin
    Inc(I, 2); // Skip "0x" or "0X"
  end;

  // Check for invalid radix
  if (Radix < 2) or (Radix > 36) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  StartPos := I; // Remember where digit parsing starts

  // Build valid characters for this radix
  ValidChars := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ValidChars := Copy(ValidChars, 1, Radix);

  ResultValue := 0;

  // Parse digits
  while I <= Length(InputStr) do
  begin
    C := UpCase(InputStr[I]);
    if Pos(C, ValidChars) = 0 then
      Break; // Stop at first invalid character

    ResultValue := ResultValue * Radix + (Pos(C, ValidChars) - 1);
    Inc(I);
  end;

  // If no digits were parsed, return NaN
  if I = StartPos then // No digits parsed
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(Sign * ResultValue);
end;

function TGocciaGlobalNumber.NumberParseFloat(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  InputStr: string;
  I: Integer;
  C: Char;
  Sign: Integer;
  IntegerPart, FractionalPart: Double;
  FractionDivisor: Double;
  HasDigits: Boolean;
begin
  if Args.Length = 0 then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  InputStr := Trim(Args.GetElement(0).ToStringLiteral.Value);
  if InputStr = '' then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  I := 1;
  Sign := 1;
  HasDigits := False;

  // Check for sign
  if (I <= Length(InputStr)) and (InputStr[I] = '-') then
  begin
    Sign := -1;
    Inc(I);
  end
  else if (I <= Length(InputStr)) and (InputStr[I] = '+') then
  begin
    Inc(I);
  end;

  IntegerPart := 0;

  // Parse integer part
  while I <= Length(InputStr) do
  begin
    C := InputStr[I];
    if (C >= '0') and (C <= '9') then
    begin
      IntegerPart := IntegerPart * 10 + (Ord(C) - Ord('0'));
      HasDigits := True;
      Inc(I);
    end
    else
      Break;
  end;

  FractionalPart := 0;
  FractionDivisor := 1;

  // Check for decimal point
  if (I <= Length(InputStr)) and (InputStr[I] = '.') then
  begin
    Inc(I);

    // Parse fractional part
    while I <= Length(InputStr) do
    begin
      C := InputStr[I];
      if (C >= '0') and (C <= '9') then
      begin
        FractionDivisor := FractionDivisor * 10;
        FractionalPart := FractionalPart * 10 + (Ord(C) - Ord('0'));
        HasDigits := True;
        Inc(I);
      end
      else
        Break;
    end;
  end;

  // If no digits were parsed, return NaN
  if not HasDigits then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  Result := TGocciaNumberLiteralValue.Create(Sign * (IntegerPart + (FractionalPart / FractionDivisor)));
end;

function TGocciaGlobalNumber.NumberIsFinite(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  NumberArg := TGocciaNumberLiteralValue(Args.GetElement(0));

  if not (NumberArg is TGocciaNumberLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaGlobalNumber.NumberIsNaN(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  if Args.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Number.isNaN only returns true for actual NaN numbers, not for other types
  if Args.GetElement(0) is TGocciaNumberLiteralValue then
  begin
    NumberArg := TGocciaNumberLiteralValue(Args.GetElement(0));
    if NumberArg.IsNaN then
      Result := TGocciaBooleanLiteralValue.TrueValue
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalNumber.NumberIsInteger(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  Value: Double;
begin
  if Args.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  NumberArg := TGocciaNumberLiteralValue(Args.GetElement(0));

  if not (NumberArg is TGocciaNumberLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  Value := NumberArg.Value;

  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if Value = Trunc(Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaGlobalNumber.NumberIsSafeInteger(Args: TGocciaArgumentsCollection; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  Value: Double;
begin
  if Args.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if not (Args.GetElement(0) is TGocciaNumberLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  NumberArg := TGocciaNumberLiteralValue(Args.GetElement(0));

  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  Value := NumberArg.Value;

  // Must be an integer and within safe integer range
  if (Value = Trunc(Value)) and (Abs(Value) <= 9007199254740991) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

end.

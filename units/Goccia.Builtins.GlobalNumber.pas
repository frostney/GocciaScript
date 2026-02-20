unit Goccia.Builtins.GlobalNumber;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.Scope,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGlobalNumber = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);

    function NumberParseInt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberParseFloat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberIsFinite(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberIsNaN(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberIsInteger(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function NumberIsSafeInteger(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Arguments.Validator,
  Goccia.Values.ClassHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor;

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
end;

// ES2026 §21.1.2.13 Number.parseInt(string, radix)
function TGocciaGlobalNumber.NumberParseInt(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  InputStr: string;
  Radix: Integer;
  I, StartPos: Integer;
  C: Char;
  Sign: Integer;
  ResultValue: Int64;
  ValidChars: string;
begin
  TGocciaArgumentValidator.RequireAtLeast(AArgs, 1, 'Number.parseInt', ThrowError);
  TGocciaArgumentValidator.RequireAtMost(AArgs, 2, 'Number.parseInt', ThrowError);

  // Step 1: Let inputString be ? ToString(string)
  // Step 2: Let S be ! TrimString(inputString, start)
  InputStr := Trim(AArgs.GetElement(0).ToStringLiteral.Value);
  if InputStr = '' then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Step 5: Let R be ? ToInt32(radix)
  if AArgs.Length > 1 then
    Radix := Trunc(AArgs.GetElement(1).ToNumberLiteral.Value)
  else
    Radix := 0;

  I := 1;
  // Step 3: Let sign be 1
  Sign := 1;

  // Step 4: If S is not empty and the first code unit is U+002D (HYPHEN-MINUS),
  //         set sign to -1; remove the prefix code unit
  if (I <= Length(InputStr)) and (InputStr[I] = '-') then
  begin
    Sign := -1;
    Inc(I);
  end
  else if (I <= Length(InputStr)) and (InputStr[I] = '+') then
  begin
    Inc(I);
  end;

  // Step 6: If R = 0, let R be 10
  // Step 8: If S starts with "0x" or "0X" and R is 0 or 16, set R to 16 and skip prefix
  if (Radix = 0) then
  begin
    if (I + 1 <= Length(InputStr)) and (InputStr[I] = '0') and
       ((InputStr[I + 1] = 'x') or (InputStr[I + 1] = 'X')) then
    begin
      Radix := 16;
      Inc(I, 2);
    end
    else
      Radix := 10;
  end
  else if (Radix = 16) and (I + 1 <= Length(InputStr)) and (InputStr[I] = '0') and
          ((InputStr[I + 1] = 'x') or (InputStr[I + 1] = 'X')) then
  begin
    Inc(I, 2);
  end;

  // Step 7: If R < 2 or R > 36, return NaN
  if (Radix < 2) or (Radix > 36) then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  StartPos := I;

  // Step 9: Build the set of valid digit characters for the given radix
  ValidChars := '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ValidChars := Copy(ValidChars, 1, Radix);

  ResultValue := 0;

  // Step 10: Parse digits — let Z be the longest prefix of S that consists
  //          of characters in the valid digit set for radix R
  while I <= Length(InputStr) do
  begin
    C := UpCase(InputStr[I]);
    if Pos(C, ValidChars) = 0 then
      Break;

    ResultValue := ResultValue * Radix + (Pos(C, ValidChars) - 1);
    Inc(I);
  end;

  // Step 11: If Z is empty, return NaN
  if I = StartPos then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Step 12: Return sign × MV of Z interpreted in radix R
  Result := TGocciaNumberLiteralValue.Create(Sign * ResultValue);
end;

// ES2026 §21.1.2.12 Number.parseFloat(string)
function TGocciaGlobalNumber.NumberParseFloat(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  InputStr: string;
  I: Integer;
  C: Char;
  Sign: Integer;
  IntegerPart, FractionalPart: Double;
  FractionDivisor: Double;
  HasDigits: Boolean;
begin
  if AArgs.Length = 0 then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Step 1: Let inputString be ? ToString(string)
  // Step 2: Let trimmedString be ! TrimString(inputString, start)
  InputStr := Trim(AArgs.GetElement(0).ToStringLiteral.Value);
  if InputStr = '' then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Step 3: Parse trimmedString as a StrDecimalLiteral
  I := 1;
  Sign := 1;
  HasDigits := False;

  // Step 3a: Handle optional sign prefix
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

  // Step 3b: Parse DecimalDigits (integer part)
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

  // Step 3c: Parse optional '.' DecimalDigits (fractional part)
  if (I <= Length(InputStr)) and (InputStr[I] = '.') then
  begin
    Inc(I);

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

  // Step 4: If no valid StrDecimalLiteral prefix was found, return NaN
  if not HasDigits then
  begin
    Result := TGocciaNumberLiteralValue.NaNValue;
    Exit;
  end;

  // Step 5: Return the MV of the longest valid prefix of trimmedString
  Result := TGocciaNumberLiteralValue.Create(Sign * (IntegerPart + (FractionalPart / FractionDivisor)));
end;

// ES2026 §21.1.2.2 Number.isFinite(number)
function TGocciaGlobalNumber.NumberIsFinite(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: If Type(number) is not Number, return false
  if AArgs.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if not (AArgs.GetElement(0) is TGocciaNumberLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  NumberArg := TGocciaNumberLiteralValue(AArgs.GetElement(0));

  // Step 2: If number is NaN, +∞𝔽, or -∞𝔽, return false
  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Step 3: Otherwise, return true
  Result := TGocciaBooleanLiteralValue.TrueValue;
end;

// ES2026 §21.1.2.4 Number.isNaN(number)
function TGocciaGlobalNumber.NumberIsNaN(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
begin
  // Step 1: If Type(number) is not Number, return false
  if AArgs.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if AArgs.GetElement(0) is TGocciaNumberLiteralValue then
  begin
    NumberArg := TGocciaNumberLiteralValue(AArgs.GetElement(0));
    // Step 2: If number is NaN, return true
    if NumberArg.IsNaN then
      Result := TGocciaBooleanLiteralValue.TrueValue
    // Step 3: Otherwise, return false
    else
      Result := TGocciaBooleanLiteralValue.FalseValue;
  end
  else
    // Step 1 (cont.): Non-Number type → false
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §21.1.2.3 Number.isInteger(number)
function TGocciaGlobalNumber.NumberIsInteger(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  Value: Double;
begin
  // Step 1: If Type(number) is not Number, return false
  if AArgs.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if not (AArgs.GetElement(0) is TGocciaNumberLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  NumberArg := TGocciaNumberLiteralValue(AArgs.GetElement(0));

  Value := NumberArg.Value;

  // Step 2: If number is NaN, +∞𝔽, or -∞𝔽, return false
  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  // Step 3: If floor(abs(ℝ(number))) ≠ abs(ℝ(number)), return false
  // Step 4: Return true
  if Value = Trunc(Value) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// ES2026 §21.1.2.5 Number.isSafeInteger(number)
function TGocciaGlobalNumber.NumberIsSafeInteger(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteralValue;
  Value: Double;
begin
  // Step 1: If IsInteger(number) is false, return false
  // (IsInteger checks: Type is Number, not NaN/Infinity, and floor(abs) = abs)
  if AArgs.Length = 0 then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  if not (AArgs.GetElement(0) is TGocciaNumberLiteralValue) then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  NumberArg := TGocciaNumberLiteralValue(AArgs.GetElement(0));

  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanLiteralValue.FalseValue;
    Exit;
  end;

  Value := NumberArg.Value;

  // Step 2: If abs(ℝ(number)) > 2^53 − 1, return false
  // Step 3: Return true
  if (Value = Trunc(Value)) and (Abs(Value) <= 9007199254740991) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

end.

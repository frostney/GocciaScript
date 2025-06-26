unit Goccia.Builtins.GlobalNumber;

{$I Goccia.inc}

interface

uses
  Goccia.Builtins.Base, Goccia.Scope, Goccia.Error, Goccia.Values.Core, Goccia.Values.Error, Goccia.Values.ObjectValue, Goccia.Values.NumberValue, Goccia.Values.StringValue, Goccia.Values.NullValue, Goccia.Values.BooleanValue, SysUtils, Math, Generics.Collections;

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

  // Number constants
  FBuiltinNumber.DefineProperty('NaN', TGocciaPropertyDescriptorData.Create(TGocciaNumberLiteral.CreateNaN, [pfEnumerable]));

  FBuiltinNumber.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberParseInt, 'parseInt', 1));
  FBuiltinNumber.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberParseFloat, 'parseFloat', 1));
  FBuiltinNumber.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsFinite, 'isFinite', 0));
  FBuiltinNumber.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsNaN, 'isNaN', 0));
  FBuiltinNumber.RegisterNativeMethod(TGocciaNativeFunctionValue.Create(NumberIsInteger, 'isInteger', 0));

  AScope.DefineBuiltin(AName, FBuiltinNumber);
end;

function TGocciaGlobalNumber.NumberParseInt(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  InputStr: string;
  Radix: Integer;
  I, StartPos: Integer;
  C: Char;
  Sign: Integer;
  ResultValue: Int64;
  ValidChars: string;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaNumberLiteral.CreateNaN;
    Exit;
  end;

  InputStr := Trim(Args[0].ToString);
  if InputStr = '' then
  begin
    Result := TGocciaNumberLiteral.CreateNaN;
    Exit;
  end;

  // Get radix
  if Args.Count > 1 then
    Radix := Trunc(Args[1].ToNumber)
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
    Result := TGocciaNumberLiteral.CreateNaN;
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
    Result := TGocciaNumberLiteral.CreateNaN;
    Exit;
  end;

  Result := TGocciaNumberLiteral.Create(Sign * ResultValue);
end;

function TGocciaGlobalNumber.NumberParseFloat(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  InputStr: string;
  I: Integer;
  C: Char;
  Sign: Integer;
  IntegerPart, FractionalPart: Double;
  FractionDivisor: Double;
  HasDigits: Boolean;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaNumberLiteral.CreateNaN;
    Exit;
  end;

  InputStr := Trim(Args[0].ToString);
  if InputStr = '' then
  begin
    Result := TGocciaNumberLiteral.CreateNaN;
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
    Result := TGocciaNumberLiteral.CreateNaN;
    Exit;
  end;

  Result := TGocciaNumberLiteral.Create(Sign * (IntegerPart + (FractionalPart / FractionDivisor)));
end;

function TGocciaGlobalNumber.NumberIsFinite(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteral;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  NumberArg := TGocciaNumberLiteral(Args[0]);

  if not (NumberArg is TGocciaNumberLiteral) then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  Result := TGocciaBooleanValue.Create(True);
end;

function TGocciaGlobalNumber.NumberIsNaN(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteral;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  // Number.isNaN only returns true for actual NaN numbers, not for other types
  if Args[0] is TGocciaNumberLiteral then
  begin
    NumberArg := TGocciaNumberLiteral(Args[0]);
    Result := TGocciaBooleanValue.Create(NumberArg.IsNaN);
  end
  else
  begin
    // If argument is not a NumberValue, Number.isNaN should return false
    Result := TGocciaBooleanValue.Create(False);
  end;
end;

function TGocciaGlobalNumber.NumberIsInteger(Args: TObjectList<TGocciaValue>; ThisValue: TGocciaValue): TGocciaValue;
var
  NumberArg: TGocciaNumberLiteral;
  Value: Double;
begin
  if Args.Count = 0 then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  NumberArg := TGocciaNumberLiteral(Args[0]);

  if not (NumberArg is TGocciaNumberLiteral) then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  Value := NumberArg.Value;

  if NumberArg.IsNaN or NumberArg.IsInfinity or NumberArg.IsNegativeInfinity then
  begin
    Result := TGocciaBooleanValue.Create(False);
    Exit;
  end;

  Result := TGocciaBooleanValue.Create(Value = Trunc(Value));
end;


end.

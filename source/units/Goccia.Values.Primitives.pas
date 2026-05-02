unit Goccia.Values.Primitives;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  OrderedStringMap,

  Goccia.GarbageCollector;

type
  TGocciaBooleanLiteralValue = class;
  TGocciaNumberLiteralValue = class;
  TGocciaStringLiteralValue = class;

  TGocciaValue = class(TGCManagedObject)
  public
    procedure AfterConstruction; override;
    function RuntimeCopy: TGocciaValue; virtual;

    function TypeName: string; virtual; abstract;
    function TypeOf: string; virtual; abstract;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; virtual; abstract;
    function ToNumberLiteral: TGocciaNumberLiteralValue; virtual; abstract;
    function ToStringLiteral: TGocciaStringLiteralValue; virtual; abstract;

    function IsPrimitive: Boolean; virtual;
    function IsCallable: Boolean; virtual;
    function IsConstructable: Boolean; virtual;
    function GetProperty(const AName: string): TGocciaValue; virtual;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); virtual;
  end;

  TGocciaValueList = TObjectList<TGocciaValue>;
  TGocciaValueMap = TOrderedStringMap<TGocciaValue>;

  TGocciaNullLiteralValue = class(TGocciaValue)
  private
    class var FNullValue: TGocciaNullLiteralValue;
  public
    class function NullValue: TGocciaNullLiteralValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
  end;

  TGocciaUndefinedLiteralValue = class(TGocciaValue)
  private
    class var FUndefinedValue: TGocciaUndefinedLiteralValue;
  public
    class function UndefinedValue: TGocciaUndefinedLiteralValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
  end;

  TGocciaBooleanLiteralValue = class(TGocciaValue)
  private
    FValue: Boolean;

    class var FTrueValue: TGocciaBooleanLiteralValue;
    class var FFalseValue: TGocciaBooleanLiteralValue;
  public
    constructor Create(const AValue: Boolean);

    class function FromBoolean(const AValue: Boolean): TGocciaBooleanLiteralValue; static; inline;
    class function TrueValue: TGocciaBooleanLiteralValue;
    class function FalseValue: TGocciaBooleanLiteralValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: Boolean read FValue;
  end;

  TGocciaNumberLiteralValue = class(TGocciaValue)
  private
    FValue: Double;

    function GetIsNaN: Boolean; inline;
    function GetIsNegativeZero: Boolean; inline;
    function GetIsInfinity: Boolean; inline;
    function GetIsNegativeInfinity: Boolean; inline;
    function GetIsInfinite: Boolean; inline;

    class var FZeroValue: TGocciaNumberLiteralValue;
    class var FOneValue: TGocciaNumberLiteralValue;
    class var FNaNValue: TGocciaNumberLiteralValue;
    class var FNegativeZeroValue: TGocciaNumberLiteralValue;
    class var FInfinityValue: TGocciaNumberLiteralValue;
    class var FNegativeInfinityValue: TGocciaNumberLiteralValue;
  public
    constructor Create(const AValue: Double);

    class function NaNValue: TGocciaNumberLiteralValue;
    class function NegativeZeroValue: TGocciaNumberLiteralValue;
    class function InfinityValue: TGocciaNumberLiteralValue;
    class function NegativeInfinityValue: TGocciaNumberLiteralValue;

    class function ZeroValue: TGocciaNumberLiteralValue;
    class function OneValue: TGocciaNumberLiteralValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: Double read FValue;

    property IsNegativeZero: Boolean read GetIsNegativeZero;
    property IsNaN: Boolean read GetIsNaN;
    property IsInfinity: Boolean read GetIsInfinity;
    property IsNegativeInfinity: Boolean read GetIsNegativeInfinity;
    property IsInfinite: Boolean read GetIsInfinite;
  end;

  TGocciaStringLiteralValue = class(TGocciaValue)
  private
    FValue: string;
  public
    constructor Create(const AValue: string);
    class function FromUTF8(const AValue: UTF8String): TGocciaStringLiteralValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: string read FValue;
  end;

  procedure PinPrimitiveSingletons;

  // ES2026 §6.1.6.1.13 Number::toString(x)
  function DoubleToESString(AValue: Double): string;

implementation

uses
  Math,
  SysUtils,

  TextSemantics,

  Goccia.Constants,
  Goccia.Constants.TypeNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.InstructionLimit,
  Goccia.Profiler,
  Goccia.Threading,
  Goccia.Timeout,
  Goccia.Values.ErrorHelper;

const
  MAX_SAFE_INTEGER_VALUE = 9007199254740991.0;

// ES2026 §6.1.6.1.13 Number::toString(x)
function DoubleToESString(AValue: Double): string;

  procedure FormatES(const AMantissa: string; AK, AN: Integer; ANeg: Boolean;
    out AResult: string);
  begin
    // Step 7: k <= n <= 21 — integer with trailing zeros
    if (AK <= AN) and (AN <= 21) then
      AResult := AMantissa + StringOfChar('0', AN - AK)
    // Step 8: 0 < n <= 21 (and n < k) — decimal within digits
    else if (0 < AN) and (AN <= 21) then
      AResult := Copy(AMantissa, 1, AN) + '.' + Copy(AMantissa, AN + 1, AK - AN)
    // Step 9: -6 < n <= 0 — leading zeros after "0."
    else if (AN > -6) and (AN <= 0) then
      AResult := '0.' + StringOfChar('0', -AN) + AMantissa
    // Steps 10-11: scientific notation
    else if AK = 1 then
    begin
      if AN - 1 > 0 then
        AResult := AMantissa + 'e+' + IntToStr(AN - 1)
      else
        AResult := AMantissa + 'e-' + IntToStr(1 - AN);
    end
    else
    begin
      if AN - 1 > 0 then
        AResult := AMantissa[1] + '.' + Copy(AMantissa, 2, AK - 1) + 'e+' + IntToStr(AN - 1)
      else
        AResult := AMantissa[1] + '.' + Copy(AMantissa, 2, AK - 1) + 'e-' + IntToStr(1 - AN);
    end;

    if ANeg then
      AResult := '-' + AResult;
  end;

var
  IsNeg: Boolean;
  SciStr, Mantissa, TestStr: string;
  Exp, N, K, I, W, EPos, D: Integer;
  Parsed: Double;
  FS: TFormatSettings;
begin
  if AValue = 0 then
    Exit('0');

  IsNeg := AValue < 0;
  if IsNeg then
    AValue := -AValue;

  // Fast path: safe integers (all digits exact, n <= 16 <= 21)
  if (Frac(AValue) = 0) and (AValue <= MAX_SAFE_INTEGER_VALUE) then
  begin
    Str(AValue:0:0, Result);
    if IsNeg then
      Result := '-' + Result;
    Exit;
  end;

  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';

  // Probe k=1 (single significant digit). Str(V:9) always outputs at
  // least 2 significant digits ("d.dE+ddd"), so we round the 2-digit
  // mantissa to 1 digit and check round-trip. This handles cases like
  // Number.MIN_VALUE whose shortest representation is "5e-324".
  Str(AValue:9, SciStr);
  SciStr := Trim(SciStr);
  EPos := Pos('E', SciStr);
  Exp := StrToInt(Copy(SciStr, EPos + 1, Length(SciStr) - EPos));

  D := Ord(SciStr[1]) - Ord('0');
  if SciStr[3] >= '5' then
    Inc(D);
  if D >= 10 then
  begin
    D := 1;
    Inc(Exp);
  end;

  TestStr := IntToStr(D) + 'E' + IntToStr(Exp);
  if TryStrToFloat(TestStr, Parsed, FS) and (Parsed = AValue) then
  begin
    N := Exp + 1;
    FormatES(IntToStr(D), 1, N, IsNeg, Result);
    Exit;
  end;

  // General case: find the shortest round-tripping representation.
  // Str(V:W) outputs scientific notation with (W - 7) significant digits
  // (for 3-digit exponents) and correctly rounds at each precision level.
  // W=9 gives the minimum (2 sig digits), W=24 gives the maximum (17).
  for W := 9 to 24 do
  begin
    Str(AValue:W, SciStr);
    SciStr := Trim(SciStr);

    if TryStrToFloat(SciStr, Parsed, FS) and (Parsed = AValue) then
    begin
      EPos := Pos('E', SciStr);
      Mantissa := Copy(SciStr, 1, EPos - 1);
      Exp := StrToInt(Copy(SciStr, EPos + 1, Length(SciStr) - EPos));
      N := Exp + 1;

      // Remove decimal point from mantissa
      I := Pos('.', Mantissa);
      if I > 0 then
        Delete(Mantissa, I, 1);

      // Strip trailing zeros
      I := Length(Mantissa);
      while (I > 1) and (Mantissa[I] = '0') do
        Dec(I);
      SetLength(Mantissa, I);
      K := Length(Mantissa);

      FormatES(Mantissa, K, N, IsNeg, Result);
      Exit;
    end;
  end;

  // Fallback (unreachable for valid finite doubles — 17 sig digits always
  // suffice). Use FloatToStr as a safety net.
  Result := FloatToStr(AValue, FS);
  if IsNeg then
    Result := '-' + Result;
end;

{ TGocciaValue }

procedure TGocciaValue.AfterConstruction;
var
  GC: TGarbageCollector;
begin
  inherited;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
  begin
    GC.RegisterObject(Self);
    if (GC.MaxBytes > 0) and (GC.BytesAllocated > GC.MaxBytes) and
       not GC.MemoryLimitFiring then
    begin
      // Unregister before throwing: AfterConstruction exceptions trigger
      // automatic destruction, so the GC must not hold a dangling pointer.
      GC.UnregisterObject(Self);
      GC.MemoryLimitFiring := True;
      try
        ThrowRangeError(SErrorMemoryLimitExceeded, SSuggestMemoryLimitExceeded);
      finally
        GC.MemoryLimitFiring := False;
      end;
    end;
  end;
  if GProfilingAllocations and Assigned(TGocciaProfiler.Instance) then
    TGocciaProfiler.Instance.RecordAllocation;
  CheckExecutionTimeout;
  CheckInstructionLimit;
end;

function TGocciaValue.RuntimeCopy: TGocciaValue;
begin
  // Default: return self (for singletons and complex values like objects/functions)
  Result := Self;
end;

function TGocciaValue.IsPrimitive: Boolean;
begin
  Result := False;
end;

function TGocciaValue.IsCallable: Boolean;
begin
  Result := False;
end;

function TGocciaValue.IsConstructable: Boolean;
begin
  Result := False;
end;

function TGocciaValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := nil;
end;

procedure TGocciaValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  // No-op for primitives
end;


{ Utility functions }

procedure PinPrimitiveSingletons;
begin
  if not Assigned(TGarbageCollector.Instance) then
    Exit;

  TGarbageCollector.Instance.PinObject(TGocciaUndefinedLiteralValue.UndefinedValue);
  TGarbageCollector.Instance.PinObject(TGocciaNullLiteralValue.NullValue);
  TGarbageCollector.Instance.PinObject(TGocciaBooleanLiteralValue.TrueValue);
  TGarbageCollector.Instance.PinObject(TGocciaBooleanLiteralValue.FalseValue);
  TGarbageCollector.Instance.PinObject(TGocciaNumberLiteralValue.NaNValue);
  TGarbageCollector.Instance.PinObject(TGocciaNumberLiteralValue.ZeroValue);
  TGarbageCollector.Instance.PinObject(TGocciaNumberLiteralValue.OneValue);
  TGarbageCollector.Instance.PinObject(TGocciaNumberLiteralValue.NegativeZeroValue);
  TGarbageCollector.Instance.PinObject(TGocciaNumberLiteralValue.InfinityValue);
  TGarbageCollector.Instance.PinObject(TGocciaNumberLiteralValue.NegativeInfinityValue);
  // BigInt singletons are pinned in TGocciaBigIntValue.BigIntZero/BigIntOne
end;

{ TGocciaNullLiteralValue }

class function TGocciaNullLiteralValue.NullValue: TGocciaNullLiteralValue;
begin
  if not Assigned(FNullValue) then
  begin
    Assert(not GIsWorkerThread, 'NullValue: must be initialised on main thread');
    FNullValue := TGocciaNullLiteralValue.Create;
  end;
  Result := FNullValue;
end;

function TGocciaNullLiteralValue.IsPrimitive: Boolean;
begin
  Result := True;
end;

function TGocciaNullLiteralValue.TypeName: string;
begin
  Result := NULL_TYPE_NAME;
end;

function TGocciaNullLiteralValue.TypeOf: string;
begin
  // Per JavaScript spec, typeof null === 'object' (a historical quirk preserved for compatibility)
  Result := OBJECT_TYPE_NAME;
end;

function TGocciaNullLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaNullLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.ZeroValue;
end;

function TGocciaNullLiteralValue.RuntimeCopy: TGocciaValue;
begin
  Result := NullValue;
end;

function TGocciaNullLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('null');
end;


{ TGocciaUndefinedLiteralValue }

function TGocciaUndefinedLiteralValue.IsPrimitive: Boolean;
begin
  Result := True;
end;

class function TGocciaUndefinedLiteralValue.UndefinedValue: TGocciaUndefinedLiteralValue;
begin
  if not Assigned(FUndefinedValue) then
  begin
    Assert(not GIsWorkerThread, 'UndefinedValue: must be initialised on main thread');
    FUndefinedValue := TGocciaUndefinedLiteralValue.Create;
  end;
  Result := FUndefinedValue;
end;

function TGocciaUndefinedLiteralValue.TypeName: string;
begin
  Result := UNDEFINED_TYPE_NAME;
end;

function TGocciaUndefinedLiteralValue.TypeOf: string;
begin
  Result := UNDEFINED_TYPE_NAME;
end;

function TGocciaUndefinedLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaUndefinedLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaUndefinedLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('undefined');
end;



{ TGocciaBooleanLiteralValue }

function TGocciaBooleanLiteralValue.IsPrimitive: Boolean;
begin
  Result := True;
end;

constructor TGocciaBooleanLiteralValue.Create(const AValue: Boolean);
begin
  FValue := AValue;
end;

class function TGocciaBooleanLiteralValue.TrueValue: TGocciaBooleanLiteralValue;
begin
  if not Assigned(FTrueValue) then
  begin
    Assert(not GIsWorkerThread, 'TrueValue: must be initialised on main thread');
    FTrueValue := TGocciaBooleanLiteralValue.Create(True);
  end;
  Result := FTrueValue;
end;

class function TGocciaBooleanLiteralValue.FalseValue: TGocciaBooleanLiteralValue;
begin
  if not Assigned(FFalseValue) then
  begin
    Assert(not GIsWorkerThread, 'FalseValue: must be initialised on main thread');
    FFalseValue := TGocciaBooleanLiteralValue.Create(False);
  end;
  Result := FFalseValue;
end;

class function TGocciaBooleanLiteralValue.FromBoolean(
  const AValue: Boolean): TGocciaBooleanLiteralValue; inline;
begin
  if AValue then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function TGocciaBooleanLiteralValue.TypeName: string;
begin
  Result := BOOLEAN_TYPE_NAME;
end;

function TGocciaBooleanLiteralValue.TypeOf: string;
begin
  Result := BOOLEAN_TYPE_NAME;
end;

function TGocciaBooleanLiteralValue.RuntimeCopy: TGocciaValue;
begin
  // Return the shared singleton -- no allocation
  if FValue then
    Result := TrueValue
  else
    Result := FalseValue;
end;

function TGocciaBooleanLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := Self;
end;

function TGocciaBooleanLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  if FValue then
    Result := TGocciaNumberLiteralValue.OneValue
  else
    Result := TGocciaNumberLiteralValue.ZeroValue;
end;

function TGocciaBooleanLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if FValue then
    Result := TGocciaStringLiteralValue.Create('true')
  else
    Result := TGocciaStringLiteralValue.Create('false');
end;

{ TGocciaNumberLiteralValue }

function TGocciaNumberLiteralValue.IsPrimitive: Boolean;
begin
  Result := True;
end;

constructor TGocciaNumberLiteralValue.Create(const AValue: Double);
begin
  FValue := AValue;
end;

function TGocciaNumberLiteralValue.GetIsNaN: Boolean;
begin
  Result := Math.IsNaN(FValue);
end;

function TGocciaNumberLiteralValue.GetIsNegativeZero: Boolean;
var
  V: Double;
  Bits: Int64 absolute V;
begin
  V := FValue;
  Result := (V = ZERO_VALUE) and (Bits < 0);
end;

function TGocciaNumberLiteralValue.GetIsInfinity: Boolean;
begin
  Result := Math.IsInfinite(FValue) and (FValue > 0);
end;

function TGocciaNumberLiteralValue.GetIsNegativeInfinity: Boolean;
begin
  Result := Math.IsInfinite(FValue) and (FValue < 0);
end;

function TGocciaNumberLiteralValue.GetIsInfinite: Boolean;
begin
  Result := Math.IsInfinite(FValue);
end;

class function TGocciaNumberLiteralValue.NaNValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FNanValue) then
  begin
    Assert(not GIsWorkerThread, 'NaNValue: must be initialised on main thread');
    FNaNValue := TGocciaNumberLiteralValue.Create(Math.NaN);
  end;
  Result := FNaNValue;
end;

class function TGocciaNumberLiteralValue.NegativeZeroValue: TGocciaNumberLiteralValue;
var
  NZ: Double;
begin
  if not Assigned(FNegativeZeroValue) then
  begin
    Assert(not GIsWorkerThread, 'NegativeZeroValue: must be initialised on main thread');
    NZ := ZERO_VALUE;
    NZ := NZ * (-1.0);
    FNegativeZeroValue := TGocciaNumberLiteralValue.Create(NZ);
  end;
  Result := FNegativeZeroValue;
end;

class function TGocciaNumberLiteralValue.InfinityValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FInfinityValue) then
  begin
    Assert(not GIsWorkerThread, 'InfinityValue: must be initialised on main thread');
    FInfinityValue := TGocciaNumberLiteralValue.Create(Math.Infinity);
  end;
  Result := FInfinityValue;
end;

class function TGocciaNumberLiteralValue.NegativeInfinityValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FNegativeInfinityValue) then
  begin
    Assert(not GIsWorkerThread, 'NegativeInfinityValue: must be initialised on main thread');
    FNegativeInfinityValue := TGocciaNumberLiteralValue.Create(Math.NegInfinity);
  end;
  Result := FNegativeInfinityValue;
end;

class function TGocciaNumberLiteralValue.ZeroValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FZeroValue) then
  begin
    Assert(not GIsWorkerThread, 'ZeroValue: must be initialised on main thread');
    FZeroValue := TGocciaNumberLiteralValue.Create(ZERO_VALUE);
  end;
  Result := FZeroValue;
end;

class function TGocciaNumberLiteralValue.OneValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FOneValue) then
  begin
    Assert(not GIsWorkerThread, 'OneValue: must be initialised on main thread');
    FOneValue := TGocciaNumberLiteralValue.Create(ONE_VALUE);
  end;
  Result := FOneValue;
end;

function TGocciaNumberLiteralValue.TypeName: string;
begin
  Result := NUMBER_TYPE_NAME;
end;

function TGocciaNumberLiteralValue.TypeOf: string;
begin
  Result := NUMBER_TYPE_NAME;
end;

function TGocciaNumberLiteralValue.RuntimeCopy: TGocciaValue;
begin
  if Math.IsNaN(FValue) then
    Exit(NaNValue);
  if Math.IsInfinite(FValue) then
  begin
    if FValue > 0 then
      Exit(InfinityValue);
    Exit(NegativeInfinityValue);
  end;
  if IsNegativeZero then
    Exit(NegativeZeroValue);
  if FValue = ZERO_VALUE then
    Exit(ZeroValue);
  if FValue = ONE_VALUE then
    Exit(OneValue);
  Result := TGocciaNumberLiteralValue.Create(FValue);
end;

function TGocciaNumberLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  if Math.IsNaN(FValue) or (FValue = ZERO_VALUE) then
    Result := TGocciaBooleanLiteralValue.FalseValue
  else
    Result := TGocciaBooleanLiteralValue.TrueValue;
end;

function TGocciaNumberLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := Self;
end;

function TGocciaNumberLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if Math.IsNaN(FValue) then
    Result := TGocciaStringLiteralValue.Create(NAN_LITERAL)
  else if Math.IsInfinite(FValue) then
  begin
    if FValue > 0 then
      Result := TGocciaStringLiteralValue.Create(INFINITY_LITERAL)
    else
      Result := TGocciaStringLiteralValue.Create(NEGATIVE_INFINITY_LITERAL);
  end
  else if IsNegativeZero then
    Result := TGocciaStringLiteralValue.Create('0')
  else
    Result := TGocciaStringLiteralValue.Create(DoubleToESString(FValue));
end;


{ TGocciaStringLiteralValue }

function TGocciaStringLiteralValue.IsPrimitive: Boolean;
begin
  Result := True;
end;

constructor TGocciaStringLiteralValue.Create(const AValue: string);
begin
  FValue := AValue;
end;

class function TGocciaStringLiteralValue.FromUTF8(
  const AValue: UTF8String): TGocciaStringLiteralValue;
var
  UTF8Bytes: RawByteString;
begin
  UTF8Bytes := AValue;
  Result := TGocciaStringLiteralValue.Create(RetagUTF8Text(UTF8Bytes));
end;

function TGocciaStringLiteralValue.TypeName: string;
begin
  Result := STRING_TYPE_NAME;
end;

function TGocciaStringLiteralValue.TypeOf: string;
begin
  Result := STRING_TYPE_NAME;
end;

function TGocciaStringLiteralValue.RuntimeCopy: TGocciaValue;
begin
  Result := TGocciaStringLiteralValue.Create(FValue);
end;

function TGocciaStringLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  if FValue <> EMPTY_STRING then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaStringLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
var
  TempValue: Double;
  Trimmed: string;
begin
  Trimmed := Trim(FValue);

  // Empty string (after trim) converts to 0
  if Trimmed = '' then
  begin
    Result := TGocciaNumberLiteralValue.ZeroValue;
    Exit;
  end;

  // Handle Infinity / -Infinity
  if Trimmed = 'Infinity' then
  begin
    Result := TGocciaNumberLiteralValue.Create(Infinity);
    Exit;
  end;
  if Trimmed = '-Infinity' then
  begin
    Result := TGocciaNumberLiteralValue.Create(NegInfinity);
    Exit;
  end;

  // Handle hex strings 0x / 0X
  if (Length(Trimmed) > 2) and (Trimmed[1] = '0') and ((Trimmed[2] = 'x') or (Trimmed[2] = 'X')) then
  begin
    try
      TempValue := StrToInt(Trimmed);
      Result := TGocciaNumberLiteralValue.Create(TempValue);
    except
      Result := TGocciaNumberLiteralValue.NaNValue;
    end;
    Exit;
  end;

  if TryStrToFloat(Trimmed, TempValue) then
    Result := TGocciaNumberLiteralValue.Create(TempValue)
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaStringLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := Self;
end;

end.

unit Goccia.Values.Primitives;

{$I Goccia.inc}

interface

uses
  SysUtils;

type
  TGocciaBooleanLiteralValue = class;
  TGocciaNumberLiteralValue = class;
  TGocciaStringLiteralValue = class;

  TGocciaValue = class(TInterfacedObject)
  private
    FGCMarked: Boolean;
  public
    procedure AfterConstruction; override;
    procedure GCMarkReferences; virtual;
    function RuntimeCopy: TGocciaValue; virtual;

    function TypeName: string; virtual; abstract;
    function TypeOf: string; virtual; abstract;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; virtual; abstract;
    function ToNumberLiteral: TGocciaNumberLiteralValue; virtual; abstract;
    function ToStringLiteral: TGocciaStringLiteralValue; virtual; abstract;

    // Virtual property access — overridden by ObjectValue, ClassValue, etc.
    function GetProperty(const AName: string): TGocciaValue; virtual;
    procedure SetProperty(const AName: string; AValue: TGocciaValue); virtual;

    property GCMarked: Boolean read FGCMarked write FGCMarked;
  end;

  TGocciaNullLiteralValue = class(TGocciaValue)
  public
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
    constructor Create(AValue: Boolean);

    class function TrueValue: TGocciaBooleanLiteralValue;
    class function FalseValue: TGocciaBooleanLiteralValue;

    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: Boolean read FValue;
  end;

  TGocciaNumberSpecialValue = (nsvNone, nsvNaN, nsvNegativeZero, nsvInfinity, nsvNegativeInfinity);

  TGocciaNumberLiteralValue = class(TGocciaValue)
  private
    FValue: Double;
    FSpecialValue: TGocciaNumberSpecialValue;

    function GetIsNaN: Boolean; inline;
    function GetIsNegativeZero: Boolean; inline;
    function GetIsInfinity: Boolean; inline;
    function GetIsNegativeInfinity: Boolean; inline;

    class var FZeroValue: TGocciaNumberLiteralValue;
    class var FOneValue: TGocciaNumberLiteralValue;
    class var FNaNValue: TGocciaNumberLiteralValue;
    class var FNegativeZeroValue: TGocciaNumberLiteralValue;
    class var FInfinityValue: TGocciaNumberLiteralValue;
    class var FNegativeInfinityValue: TGocciaNumberLiteralValue;
    class var FSmallIntCache: array[0..255] of TGocciaNumberLiteralValue;
    class var FSmallIntCacheInitialized: Boolean;
  public
    constructor Create(AValue: Double; ASpecialValue: TGocciaNumberSpecialValue = nsvNone);

    class function NaNValue: TGocciaNumberLiteralValue;
    class function NegativeZeroValue: TGocciaNumberLiteralValue;
    class function InfinityValue: TGocciaNumberLiteralValue;
    class function NegativeInfinityValue: TGocciaNumberLiteralValue;

    class function ZeroValue: TGocciaNumberLiteralValue;
    class function OneValue: TGocciaNumberLiteralValue;
    class function SmallInt(AValue: Integer): TGocciaNumberLiteralValue;

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
  end;

  TGocciaStringLiteralValue = class(TGocciaValue)
  private
    FValue: string;
  public
    constructor Create(AValue: string);

    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: string read FValue;
  end;

  function IsPrimitive(Value: TGocciaValue): Boolean;

implementation

uses
  Goccia.Values.Constants, Goccia.Values.ClassHelper, Math, Goccia.GarbageCollector;

{ TGocciaValue }

procedure TGocciaValue.AfterConstruction;
begin
  inherited;
  if Assigned(TGocciaGC.Instance) then
    TGocciaGC.Instance.RegisterValue(Self);
end;

procedure TGocciaValue.GCMarkReferences;
begin
  FGCMarked := True;
end;

function TGocciaValue.RuntimeCopy: TGocciaValue;
begin
  // Default: return self (for singletons and complex values like objects/functions)
  Result := Self;
end;

function TGocciaValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := nil;
end;

procedure TGocciaValue.SetProperty(const AName: string; AValue: TGocciaValue);
begin
  // No-op for primitives
end;

{ Utility functions }

function IsPrimitive(Value: TGocciaValue): Boolean;
begin
  Result := (Value is TGocciaBooleanLiteralValue) or
            (Value is TGocciaNumberLiteralValue) or
            (Value is TGocciaStringLiteralValue);
end;

{ TGocciaNullLiteralValue }

function TGocciaNullLiteralValue.TypeName: string;
begin
  Result := NULL_TYPE_NAME;
end;

function TGocciaNullLiteralValue.TypeOf: string;
begin
  // TODO: JavaScript compatibility: typeof null is object
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
  Result := TGocciaNullLiteralValue.Create;
end;

function TGocciaNullLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('null');
end;


{ TGocciaUndefinedLiteralValue }

class function TGocciaUndefinedLiteralValue.UndefinedValue: TGocciaUndefinedLiteralValue;
begin
  if not Assigned(FUndefinedValue) then
    FUndefinedValue := TGocciaUndefinedLiteralValue.Create;
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

constructor TGocciaBooleanLiteralValue.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

class function TGocciaBooleanLiteralValue.TrueValue: TGocciaBooleanLiteralValue;
begin
  if not Assigned(FTrueValue) then
    FTrueValue := TGocciaBooleanLiteralValue.Create(True);
  Result := FTrueValue;
end;

class function TGocciaBooleanLiteralValue.FalseValue: TGocciaBooleanLiteralValue;
begin
  if not Assigned(FFalseValue) then
    FFalseValue := TGocciaBooleanLiteralValue.Create(False);
  Result := FFalseValue;
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
  Result := TGocciaNumberLiteralValue.Create(Ord(FValue));
end;

function TGocciaBooleanLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  if FValue then
    Result := TGocciaStringLiteralValue.Create('true')
  else
    Result := TGocciaStringLiteralValue.Create('false');
end;

{ TGocciaNumberLiteralValue }

constructor TGocciaNumberLiteralValue.Create(AValue: Double; ASpecialValue: TGocciaNumberSpecialValue = nsvNone);
begin
  // Check if the input is NaN without storing it
  if Math.IsNaN(AValue) then
  begin
    FSpecialValue := nsvNaN;
    FValue := ZERO_VALUE; // Store a safe value instead of NaN
  end
  else if Math.IsInfinite(AValue) then
  begin
    if AValue > 0 then
      FSpecialValue := nsvInfinity
    else
      FSpecialValue := nsvNegativeInfinity;
    FValue := ZERO_VALUE; // Store a safe value instead of infinity
  end
  else if (AValue = ZERO_VALUE) and (ASpecialValue = nsvNegativeZero) then
  begin
    FSpecialValue := nsvNegativeZero;
    FValue := ZERO_VALUE; // Store a safe value for negative zero
  end
  else
  begin
    FSpecialValue := ASpecialValue;
    FValue := AValue;
  end;
end;

function TGocciaNumberLiteralValue.GetIsNaN: Boolean;
begin
  Result := FSpecialValue = nsvNaN;
end;

function TGocciaNumberLiteralValue.GetIsNegativeZero: Boolean;
begin
  Result := FSpecialValue = nsvNegativeZero;
end;

function TGocciaNumberLiteralValue.GetIsInfinity: Boolean;
begin
  Result := FSpecialValue = nsvInfinity;
end;

function TGocciaNumberLiteralValue.GetIsNegativeInfinity: Boolean;
begin
  Result := FSpecialValue = nsvNegativeInfinity;
end;

class function TGocciaNumberLiteralValue.NaNValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FNanValue) then
  begin
    FNaNValue := TGocciaNumberLiteralValue.Create(ZERO_VALUE, nsvNaN);
  end;

  // Return the cached value
  Result := FNaNValue;
end;

class function TGocciaNumberLiteralValue.NegativeZeroValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FNegativeZeroValue) then
  begin
    FNegativeZeroValue := TGocciaNumberLiteralValue.Create(ZERO_VALUE, nsvNegativeZero);
  end;

  // Return the cached value
  Result := FNegativeZeroValue;
end;

class function TGocciaNumberLiteralValue.InfinityValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FInfinityValue) then
  begin
    FInfinityValue := TGocciaNumberLiteralValue.Create(ZERO_VALUE, nsvInfinity);
  end;

  // Return the cached value
  Result := FInfinityValue;
end;

class function TGocciaNumberLiteralValue.NegativeInfinityValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FNegativeInfinityValue) then
  begin
    FNegativeInfinityValue := TGocciaNumberLiteralValue.Create(ZERO_VALUE, nsvNegativeInfinity);
  end;

  // Return the cached value
  Result := FNegativeInfinityValue;
end;

class function TGocciaNumberLiteralValue.ZeroValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FZeroValue) then
    FZeroValue := TGocciaNumberLiteralValue.Create(ZERO_VALUE);
  Result := FZeroValue;
end;

class function TGocciaNumberLiteralValue.OneValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FOneValue) then
    FOneValue := TGocciaNumberLiteralValue.Create(ONE_VALUE);
  Result := FOneValue;
end;

class function TGocciaNumberLiteralValue.SmallInt(AValue: Integer): TGocciaNumberLiteralValue;
var
  I: Integer;
begin
  if (AValue >= 0) and (AValue <= 255) then
  begin
    if not FSmallIntCacheInitialized then
    begin
      for I := 0 to 255 do
        FSmallIntCache[I] := TGocciaNumberLiteralValue.Create(I);
      FSmallIntCacheInitialized := True;
    end;
    Result := FSmallIntCache[AValue];
  end
  else
    Result := TGocciaNumberLiteralValue.Create(AValue);
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
  // Use SmallInt cache for common integer values (already GC-pinned, zero allocation)
  if (FSpecialValue = nsvNone) and (FValue >= 0) and (FValue <= 255)
     and (Frac(FValue) = 0) then
    Result := SmallInt(Round(FValue))
  else
    Result := TGocciaNumberLiteralValue.Create(FValue, FSpecialValue);
end;

function TGocciaNumberLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  // JavaScript spec: NaN and 0 (including -0) convert to false
  if (FSpecialValue = nsvNaN) or (FValue = ZERO_VALUE) then
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
  case FSpecialValue of
    nsvNaN:
      Result := TGocciaStringLiteralValue.Create('NaN');
    nsvInfinity:
      Result := TGocciaStringLiteralValue.Create('Infinity');
    nsvNegativeInfinity:
      Result := TGocciaStringLiteralValue.Create('-Infinity');
    nsvNegativeZero:
      Result := TGocciaStringLiteralValue.Create('0');  // Convert -0 to string as "0"
  else
    Result := TGocciaStringLiteralValue.Create(FloatToStr(FValue));
  end;
end;


{ TGocciaStringLiteralValue }

constructor TGocciaStringLiteralValue.Create(AValue: string);
begin
  FValue := AValue;
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
    Result := TGocciaNumberLiteralValue.Create(0);
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

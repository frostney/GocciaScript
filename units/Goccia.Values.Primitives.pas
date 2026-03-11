unit Goccia.Values.Primitives;

{$I Goccia.inc}

interface

uses
  Generics.Collections,

  GarbageCollector.Managed,
  OrderedStringMap;

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
    function GetProperty(const AName: string): TGocciaValue; virtual;
    procedure SetProperty(const AName: string; const AValue: TGocciaValue); virtual;
  end;

  TGocciaValueList = TObjectList<TGocciaValue>;
  TGocciaValueMap = TOrderedStringMap<TGocciaValue>;

  TGocciaNullLiteralValue = class(TGocciaValue)
  public
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
    class var FSmallIntCache: array[0..255] of TGocciaNumberLiteralValue;
    class var FSmallIntCacheInitialized: Boolean;
  public
    constructor Create(const AValue: Double);

    class function NaNValue: TGocciaNumberLiteralValue;
    class function NegativeZeroValue: TGocciaNumberLiteralValue;
    class function InfinityValue: TGocciaNumberLiteralValue;
    class function NegativeInfinityValue: TGocciaNumberLiteralValue;

    class function ZeroValue: TGocciaNumberLiteralValue;
    class function OneValue: TGocciaNumberLiteralValue;
    class function SmallInt(const AValue: Integer): TGocciaNumberLiteralValue;

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

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: string read FValue;
  end;

  function IsPrimitive(const AValue: TGocciaValue): Boolean;

implementation

uses
  Math,
  SysUtils,

  GarbageCollector.Generic,

  Goccia.Constants,
  Goccia.Constants.TypeNames,
  Goccia.Values.ClassHelper;

{ TGocciaValue }

procedure TGocciaValue.AfterConstruction;
begin
  inherited;
  if Assigned(TGarbageCollector.Instance) then
    TGarbageCollector.Instance.RegisterObject(Self);
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


function TGocciaValue.GetProperty(const AName: string): TGocciaValue;
begin
  Result := nil;
end;

procedure TGocciaValue.SetProperty(const AName: string; const AValue: TGocciaValue);
begin
  // No-op for primitives
end;

{ Utility functions }

function IsPrimitive(const AValue: TGocciaValue): Boolean;
begin
  Result := AValue.IsPrimitive;
end;

{ TGocciaNullLiteralValue }

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
  Result := TGocciaNullLiteralValue.Create;
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
    FNaNValue := TGocciaNumberLiteralValue.Create(Math.NaN);
  Result := FNaNValue;
end;

class function TGocciaNumberLiteralValue.NegativeZeroValue: TGocciaNumberLiteralValue;
var
  NZ: Double;
begin
  if not Assigned(FNegativeZeroValue) then
  begin
    NZ := ZERO_VALUE;
    NZ := NZ * (-1.0);
    FNegativeZeroValue := TGocciaNumberLiteralValue.Create(NZ);
  end;
  Result := FNegativeZeroValue;
end;

class function TGocciaNumberLiteralValue.InfinityValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FInfinityValue) then
    FInfinityValue := TGocciaNumberLiteralValue.Create(Math.Infinity);
  Result := FInfinityValue;
end;

class function TGocciaNumberLiteralValue.NegativeInfinityValue: TGocciaNumberLiteralValue;
begin
  if not Assigned(FNegativeInfinityValue) then
    FNegativeInfinityValue := TGocciaNumberLiteralValue.Create(Math.NegInfinity);
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

class function TGocciaNumberLiteralValue.SmallInt(const AValue: Integer): TGocciaNumberLiteralValue;
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
  if (not IsNegativeZero) and (FValue >= 0) and (FValue <= 255)
     and (Frac(FValue) = 0) then
    Result := SmallInt(Round(FValue))
  else
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
    Result := TGocciaStringLiteralValue.Create(FloatToStr(FValue));
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

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
  public
    function TypeName: string; virtual; abstract;
    function TypeOf: string; virtual; abstract;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; virtual; abstract;
    function ToNumberLiteral: TGocciaNumberLiteralValue; virtual; abstract;
    function ToStringLiteral: TGocciaStringLiteralValue; virtual; abstract;
  end;

  TGocciaNullLiteralValue = class(TGocciaValue)
  public
    function TypeName: string; override;
    function TypeOf: string; override;

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
  public
    constructor Create(AValue: Double; ASpecialValue: TGocciaNumberSpecialValue = nsvNone);

    class function NaNValue: TGocciaNumberLiteralValue;
    class function NegativeZeroValue: TGocciaNumberLiteralValue;
    class function InfinityValue: TGocciaNumberLiteralValue;
    class function NegativeInfinityValue: TGocciaNumberLiteralValue;

    class function ZeroValue: TGocciaNumberLiteralValue;
    class function OneValue: TGocciaNumberLiteralValue;

    function TypeName: string; override;
    function TypeOf: string; override;

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

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;

    property Value: string read FValue;
  end;

  function IsPrimitive(Value: TGocciaValue): Boolean;

implementation

uses
  Goccia.Values.Constants, Goccia.Values.ClassHelper, Math;

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
  Result := TGocciaStringLiteralValue.Create(BoolToStr(FValue, True));
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
    FSpecialValue := nsvInfinity;
    FValue := ZERO_VALUE; // Store a safe value instead of infinity
  end
  else if AValue = ZERO_VALUE then
  begin
    FSpecialValue := nsvNegativeZero;
    FValue := ZERO_VALUE; // Store a safe value instead of negative zero
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

function TGocciaNumberLiteralValue.TypeName: string;
begin
  Result := NUMBER_TYPE_NAME;
end;

function TGocciaNumberLiteralValue.TypeOf: string;
begin
  Result := NUMBER_TYPE_NAME;
end;

function TGocciaNumberLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(FValue <> ZERO_VALUE);
end;

function TGocciaNumberLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := Self;
end;

function TGocciaNumberLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create(FloatToStr(FValue));
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

function TGocciaStringLiteralValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.Create(FValue <> EMPTY_STRING);
end;

function TGocciaStringLiteralValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  if TryStrToFloat(FValue, Result.FValue) then
    Result := TGocciaNumberLiteralValue.Create(Result.FValue)
  else
    Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaStringLiteralValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := Self;
end;

end.

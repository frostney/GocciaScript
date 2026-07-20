unit Goccia.Compiler.ConstantValue;

{$I Goccia.inc}

interface

uses
  BigInteger,

  Goccia.Bytecode.Chunk,
  Goccia.Values.Primitives;

type
  TGocciaCompileTimeValueKind = (
    ctvkUnknown,
    ctvkUndefined,
    ctvkNull,
    ctvkBoolean,
    ctvkNumber,
    ctvkString,
    ctvkBigInt
  );

  TGocciaCompileTimeValue = record
    Kind: TGocciaCompileTimeValueKind;
    BooleanValue: Boolean;
    NumberValue: Double;
    StringValue: string;
    BigIntValue: TBigInteger;
  end;

  TGocciaCompilerOptimizationOptions = record
    EnableConstantFolding: Boolean;
    EnableConstPropagation: Boolean;
    EnableDeadBranchElimination: Boolean;
    PreserveCoverageShape: Boolean;
  end;

function DefaultCompilerOptimizationOptions: TGocciaCompilerOptimizationOptions; {$IFDEF FPC}inline;{$ENDIF}

function UnknownCompileTimeValue: TGocciaCompileTimeValue;
function UndefinedCompileTimeValue: TGocciaCompileTimeValue;
function NullCompileTimeValue: TGocciaCompileTimeValue;
function BooleanCompileTimeValue(const AValue: Boolean): TGocciaCompileTimeValue;
function NumberCompileTimeValue(const AValue: Double): TGocciaCompileTimeValue;
function StringCompileTimeValue(const AValue: string): TGocciaCompileTimeValue;
function BigIntCompileTimeValue(const AValue: TBigInteger): TGocciaCompileTimeValue;

function TryCompileTimeValueFromLiteral(const AValue: TGocciaValue;
  out AConstant: TGocciaCompileTimeValue): Boolean;
function CompileTimeValueToBoolean(const AValue: TGocciaCompileTimeValue): Boolean;
function CompileTimeValueIsNullish(const AValue: TGocciaCompileTimeValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
function CompileTimeValueToLocalType(
  const AValue: TGocciaCompileTimeValue): TGocciaLocalType; {$IFDEF FPC}inline;{$ENDIF}
function CompileTimeValueToString(const AValue: TGocciaCompileTimeValue): string;
function TryCompileTimeValueToNumber(const AValue: TGocciaCompileTimeValue;
  out ANumber: Double): Boolean;
function CompileTimeValueIsNumber(const AValue: TGocciaCompileTimeValue;
  const ANumber: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
function CompileTimeNumberIsNegativeZero(const AValue: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}

implementation

uses
  Math,

  NumberBits,
  NumericText,

  Goccia.Constants,
  Goccia.Values.BigIntValue;

function DefaultCompilerOptimizationOptions: TGocciaCompilerOptimizationOptions; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result.EnableConstantFolding := True;
  Result.EnableConstPropagation := True;
  Result.EnableDeadBranchElimination := True;
  Result.PreserveCoverageShape := False;
end;

function UnknownCompileTimeValue: TGocciaCompileTimeValue;
begin
  Result.Kind := ctvkUnknown;
  Result.BooleanValue := False;
  Result.NumberValue := 0.0;
  Result.StringValue := '';
  Result.BigIntValue := TBigInteger.Zero;
end;

function UndefinedCompileTimeValue: TGocciaCompileTimeValue;
begin
  Result := UnknownCompileTimeValue;
  Result.Kind := ctvkUndefined;
end;

function NullCompileTimeValue: TGocciaCompileTimeValue;
begin
  Result := UnknownCompileTimeValue;
  Result.Kind := ctvkNull;
end;

function BooleanCompileTimeValue(
  const AValue: Boolean): TGocciaCompileTimeValue;
begin
  Result := UnknownCompileTimeValue;
  Result.Kind := ctvkBoolean;
  Result.BooleanValue := AValue;
end;

function NumberCompileTimeValue(const AValue: Double): TGocciaCompileTimeValue;
begin
  Result := UnknownCompileTimeValue;
  Result.Kind := ctvkNumber;
  Result.NumberValue := AValue;
end;

function StringCompileTimeValue(
  const AValue: string): TGocciaCompileTimeValue;
begin
  Result := UnknownCompileTimeValue;
  Result.Kind := ctvkString;
  Result.StringValue := AValue;
end;

function BigIntCompileTimeValue(
  const AValue: TBigInteger): TGocciaCompileTimeValue;
begin
  Result := UnknownCompileTimeValue;
  Result.Kind := ctvkBigInt;
  Result.BigIntValue := AValue;
end;

function TryCompileTimeValueFromLiteral(const AValue: TGocciaValue;
  out AConstant: TGocciaCompileTimeValue): Boolean;
begin
  Result := True;

  if AValue is TGocciaUndefinedLiteralValue then
    AConstant := UndefinedCompileTimeValue
  else if AValue is TGocciaNullLiteralValue then
    AConstant := NullCompileTimeValue
  else if AValue is TGocciaBooleanLiteralValue then
    AConstant := BooleanCompileTimeValue(
      TGocciaBooleanLiteralValue(AValue).Value)
  else if AValue is TGocciaNumberLiteralValue then
    AConstant := NumberCompileTimeValue(
      TGocciaNumberLiteralValue(AValue).Value)
  else if AValue is TGocciaStringLiteralValue then
    AConstant := StringCompileTimeValue(
      TGocciaStringLiteralValue(AValue).Value)
  else if AValue is TGocciaBigIntValue then
    AConstant := BigIntCompileTimeValue(TGocciaBigIntValue(AValue).Value)
  else
  begin
    AConstant := UnknownCompileTimeValue;
    Result := False;
  end;
end;

function CompileTimeNumberIsNegativeZero(const AValue: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := NumberBits.IsNegativeZero(AValue);
end;

function CompileTimeValueToBoolean(
  const AValue: TGocciaCompileTimeValue): Boolean;
begin
  case AValue.Kind of
    ctvkUndefined, ctvkNull:
      Result := False;
    ctvkBoolean:
      Result := AValue.BooleanValue;
    ctvkNumber:
      Result := (AValue.NumberValue <> 0.0) and
        (not IsNaN(AValue.NumberValue));
    ctvkString:
      Result := AValue.StringValue <> '';
    ctvkBigInt:
      Result := not AValue.BigIntValue.IsZero;
  else
    Result := False;
  end;
end;

function CompileTimeValueIsNullish(
  const AValue: TGocciaCompileTimeValue): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := AValue.Kind in [ctvkUndefined, ctvkNull];
end;

function CompileTimeValueToLocalType(
  const AValue: TGocciaCompileTimeValue): TGocciaLocalType; {$IFDEF FPC}inline;{$ENDIF}
begin
  case AValue.Kind of
    ctvkBoolean:
      Result := sltBoolean;
    ctvkNumber:
      Result := sltFloat;
    ctvkString:
      Result := sltString;
  else
    Result := sltUntyped;
  end;
end;

function CompileTimeValueToString(
  const AValue: TGocciaCompileTimeValue): string;
begin
  case AValue.Kind of
    ctvkUndefined:
      Result := 'undefined';
    ctvkNull:
      Result := 'null';
    ctvkBoolean:
      if AValue.BooleanValue then
        Result := 'true'
      else
        Result := 'false';
    ctvkNumber:
      if IsNaN(AValue.NumberValue) then
        Result := NAN_LITERAL
      else if IsInfinite(AValue.NumberValue) then
      begin
        if AValue.NumberValue > 0 then
          Result := INFINITY_LITERAL
        else
          Result := NEGATIVE_INFINITY_LITERAL;
      end
      else if CompileTimeNumberIsNegativeZero(AValue.NumberValue) then
        Result := '0'
      else
        Result := FormatDouble(AValue.NumberValue);
    ctvkString:
      Result := AValue.StringValue;
    ctvkBigInt:
      Result := AValue.BigIntValue.ToString;
  else
    Result := '';
  end;
end;

function TryCompileTimeValueToNumber(const AValue: TGocciaCompileTimeValue;
  out ANumber: Double): Boolean;
begin
  Result := True;
  case AValue.Kind of
    ctvkUndefined:
      ANumber := NaN;
    ctvkNull:
      ANumber := 0.0;
    ctvkBoolean:
      if AValue.BooleanValue then
        ANumber := 1.0
      else
        ANumber := 0.0;
    ctvkNumber:
      ANumber := AValue.NumberValue;
    ctvkString:
      // ES2026 §7.1.4.1.1 StringToNumber: shared with runtime coercion
      // (Goccia.Values.Primitives) so folding cannot diverge from execution.
      ANumber := StringToNumber(AValue.StringValue);
  else
  begin
    ANumber := 0.0;
    Result := False;
  end;
  end;
end;

function CompileTimeValueIsNumber(const AValue: TGocciaCompileTimeValue;
  const ANumber: Double): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AValue.Kind = ctvkNumber) and (not IsNaN(AValue.NumberValue)) and
    (AValue.NumberValue = ANumber);
end;

end.

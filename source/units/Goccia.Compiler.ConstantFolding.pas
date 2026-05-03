unit Goccia.Compiler.ConstantFolding;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.AST.Node,
  Goccia.Compiler.ConstantValue,
  Goccia.Compiler.Context;

function TryEvaluateConstantExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; out AValue: TGocciaCompileTimeValue): Boolean;
function TryEmitConstantExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt8): Boolean;
procedure EmitCompileTimeValue(const ACtx: TGocciaCompilationContext;
  const AValue: TGocciaCompileTimeValue; const ADest: UInt8);
function CompileTimeValueToBoolean(
  const AValue: TGocciaCompileTimeValue): Boolean; inline;
function CompileTimeValueIsNullish(
  const AValue: TGocciaCompileTimeValue): Boolean; inline;

function TryFoldBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8): Boolean;
function TryFoldUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8): Boolean;
function IsNegativeZeroFloat(const AValue: Double): Boolean; inline;

implementation

uses
  Math,
  SysUtils,

  BigInteger,

  Goccia.Bytecode,
  Goccia.Bytecode.Chunk,
  Goccia.Compiler.Statements,
  Goccia.Compiler.TypeRules,
  Goccia.Constants,
  Goccia.Constants.TypeNames,
  Goccia.Token,
  Goccia.Values.BigIntValue,
  Goccia.Values.Primitives;

function CompileTimeValueToBoolean(
  const AValue: TGocciaCompileTimeValue): Boolean; inline;
begin
  Result := Goccia.Compiler.ConstantValue.CompileTimeValueToBoolean(AValue);
end;

function CompileTimeValueIsNullish(
  const AValue: TGocciaCompileTimeValue): Boolean; inline;
begin
  Result := Goccia.Compiler.ConstantValue.CompileTimeValueIsNullish(AValue);
end;

function IsNegativeZeroFloat(const AValue: Double): Boolean; inline;
begin
  Result := CompileTimeNumberIsNegativeZero(AValue);
end;

function NegativeZeroFloat: Double; inline;
begin
  Result := 0.0;
  Result := Result * -1.0;
end;

function NumberWithFloatingPointMask(const AValue: Double): Double; inline;
begin
  Result := AValue;
end;

procedure EmitFoldedNumber(const ACtx: TGocciaCompilationContext;
  const AValue: Double; const ADest: UInt8);
var
  Idx: UInt16;
begin
  if not IsNaN(AValue) and not IsInfinite(AValue)
     and not IsNegativeZeroFloat(AValue)
     and (Frac(AValue) = 0.0) and (AValue >= MIN_SBX) and (AValue <= MAX_SBX) then
    EmitInstruction(ACtx, EncodeAsBx(OP_LOAD_INT, ADest, Int16(Trunc(AValue))))
  else
  begin
    Idx := ACtx.Template.AddConstantFloat(AValue);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
  end;
end;

procedure EmitCompileTimeValue(const ACtx: TGocciaCompilationContext;
  const AValue: TGocciaCompileTimeValue; const ADest: UInt8);
var
  Idx: UInt16;
begin
  case AValue.Kind of
    ctvkUndefined:
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
    ctvkNull:
      EmitInstruction(ACtx, EncodeABC(OP_LOAD_NULL, ADest, 0, 0));
    ctvkBoolean:
      if AValue.BooleanValue then
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
      else
        EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
    ctvkNumber:
      EmitFoldedNumber(ACtx, AValue.NumberValue, ADest);
    ctvkString:
    begin
      Idx := ACtx.Template.AddConstantString(AValue.StringValue);
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
    end;
    ctvkBigInt:
    begin
      Idx := ACtx.Template.AddConstantBigInt(AValue.BigIntValue.ToString);
      EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
    end;
  else
    EmitInstruction(ACtx, EncodeABC(OP_LOAD_UNDEFINED, ADest, 0, 0));
  end;
end;

function ToUint32Number(const ANumber: Double): Cardinal;
const
  UINT32_MODULUS = QWord(High(Cardinal)) + 1;
var
  IntegerPart: Double;
begin
  if IsNaN(ANumber) or IsInfinite(ANumber) or (ANumber = 0.0) then
    Exit(0);

  IntegerPart := Int(ANumber);
  IntegerPart := IntegerPart - Floor(IntegerPart / UINT32_MODULUS) *
    UINT32_MODULUS;

  if IntegerPart >= UINT32_MODULUS then
    Exit(0);

  Result := Cardinal(Trunc(IntegerPart));
end;

function ToInt32Number(const ANumber: Double): Int32;
begin
  Result := Int32(ToUint32Number(ANumber));
end;

function FoldedModuloValue(const ALeft, ARight: Double): Double;
var
  Quotient: Double;
begin
  if IsNaN(ALeft) or IsNaN(ARight) or IsInfinite(ALeft) or
     (ARight = 0.0) then
    Exit(NaN);

  if IsInfinite(ARight) then
    Exit(ALeft);

  Quotient := ALeft / ARight;
  Result := ALeft - ARight * Trunc(Quotient);

  if (Result = 0.0) and ((ALeft < 0.0) or IsNegativeZeroFloat(ALeft)) then
    Result := NegativeZeroFloat;
end;

function FoldedPowerValue(const ALeft, ARight: Double): Double;
var
  OldMask: TFPUExceptionMask;
begin
  if (ARight = 0.0) and not IsNaN(ARight) then
    Exit(1.0);

  if IsNaN(ALeft) or IsNaN(ARight) then
    Exit(NaN);

  if IsInfinite(ARight) then
  begin
    if IsInfinite(ALeft) or (Abs(ALeft) > 1.0) then
    begin
      if ARight > 0.0 then
        Exit(Infinity);
      Exit(0.0);
    end;
    if Abs(ALeft) = 1.0 then
      Exit(NaN);
    if ARight > 0.0 then
      Exit(0.0);
    Exit(Infinity);
  end;

  if IsInfinite(ALeft) then
  begin
    if ARight > 0.0 then
    begin
      if ALeft > 0.0 then
        Exit(Infinity);
      if Frac(ARight) <> 0.0 then
        Exit(Infinity);
      if Frac(ARight / 2.0) = 0.0 then
        Exit(Infinity);
      Exit(NegInfinity);
    end;

    if (ALeft < 0.0) and (Frac(ARight) = 0.0) and
       (Frac(ARight / 2.0) <> 0.0) then
      Exit(NegativeZeroFloat);
    Exit(0.0);
  end;

  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  try
    Result := NumberWithFloatingPointMask(Power(ALeft, ARight));
  finally
    SetExceptionMask(OldMask);
  end;
end;

function FoldedDivisionValue(const ALeft, ARight: Double): Double;
var
  OldMask: TFPUExceptionMask;
begin
  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  try
    Result := NumberWithFloatingPointMask(ALeft / ARight);
  finally
    SetExceptionMask(OldMask);
  end;
end;

function FoldedNumericBinary(const AOp: TGocciaTokenType;
  const ALeft, ARight: Double; out AValue: TGocciaCompileTimeValue): Boolean;
var
  OldMask: TFPUExceptionMask;
begin
  OldMask := GetExceptionMask;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow,
    exUnderflow, exPrecision]);
  try
    Result := True;
    case AOp of
      gttPlus:
        AValue := NumberCompileTimeValue(ALeft + ARight);
      gttMinus:
        AValue := NumberCompileTimeValue(ALeft - ARight);
      gttStar:
        AValue := NumberCompileTimeValue(ALeft * ARight);
      gttSlash:
        AValue := NumberCompileTimeValue(FoldedDivisionValue(ALeft, ARight));
      gttPercent:
        AValue := NumberCompileTimeValue(FoldedModuloValue(ALeft, ARight));
      gttPower:
        AValue := NumberCompileTimeValue(FoldedPowerValue(ALeft, ARight));
      gttBitwiseAnd:
        AValue := NumberCompileTimeValue(ToInt32Number(ALeft) and
          ToInt32Number(ARight));
      gttBitwiseOr:
        AValue := NumberCompileTimeValue(ToInt32Number(ALeft) or
          ToInt32Number(ARight));
      gttBitwiseXor:
        AValue := NumberCompileTimeValue(ToInt32Number(ALeft) xor
          ToInt32Number(ARight));
      gttLeftShift:
        AValue := NumberCompileTimeValue(ToInt32Number(ALeft) shl
          (ToUint32Number(ARight) and 31));
      gttRightShift:
        AValue := NumberCompileTimeValue(SarLongint(ToInt32Number(ALeft),
          ToUint32Number(ARight) and 31));
      gttUnsignedRightShift:
        AValue := NumberCompileTimeValue(ToUint32Number(ALeft) shr
          (ToUint32Number(ARight) and 31));
    else
      AValue := UnknownCompileTimeValue;
      Result := False;
    end;
  finally
    SetExceptionMask(OldMask);
  end;
end;

function FoldedBigIntBinary(const AOp: TGocciaTokenType;
  const ALeft, ARight: TBigInteger;
  out AValue: TGocciaCompileTimeValue): Boolean;
begin
  Result := True;
  case AOp of
    gttPlus:
      AValue := BigIntCompileTimeValue(ALeft.Add(ARight));
    gttMinus:
      AValue := BigIntCompileTimeValue(ALeft.Subtract(ARight));
    gttStar:
      AValue := BigIntCompileTimeValue(ALeft.Multiply(ARight));
    gttSlash:
      begin
        if ARight.IsZero then
          Exit(False);
        AValue := BigIntCompileTimeValue(ALeft.Divide(ARight));
      end;
    gttPercent:
      begin
        if ARight.IsZero then
          Exit(False);
        AValue := BigIntCompileTimeValue(ALeft.Modulo(ARight));
      end;
    gttPower:
      begin
        if ARight.IsNegative then
          Exit(False);
        AValue := BigIntCompileTimeValue(ALeft.Power(ARight));
      end;
    gttBitwiseAnd:
      AValue := BigIntCompileTimeValue(ALeft.BitwiseAnd(ARight));
    gttBitwiseOr:
      AValue := BigIntCompileTimeValue(ALeft.BitwiseOr(ARight));
    gttBitwiseXor:
      AValue := BigIntCompileTimeValue(ALeft.BitwiseXor(ARight));
  else
    AValue := UnknownCompileTimeValue;
    Result := False;
  end;
end;

function StrictEqualConstants(const ALeft,
  ARight: TGocciaCompileTimeValue): Boolean;
begin
  if ALeft.Kind <> ARight.Kind then
    Exit(False);

  case ALeft.Kind of
    ctvkUndefined, ctvkNull:
      Result := True;
    ctvkBoolean:
      Result := ALeft.BooleanValue = ARight.BooleanValue;
    ctvkNumber:
      if IsNaN(ALeft.NumberValue) or IsNaN(ARight.NumberValue) then
        Result := False
      else
        Result := ALeft.NumberValue = ARight.NumberValue;
    ctvkString:
      Result := ALeft.StringValue = ARight.StringValue;
    ctvkBigInt:
      Result := ALeft.BigIntValue.Equal(ARight.BigIntValue);
  else
    Result := False;
  end;
end;

function TryCompareConstants(const ALeft, ARight: TGocciaCompileTimeValue;
  out ACompare: Integer): Boolean;
var
  LeftNum, RightNum: Double;
begin
  Result := True;
  ACompare := 0;

  if (ALeft.Kind = ctvkString) and (ARight.Kind = ctvkString) then
  begin
    ACompare := CompareStr(ALeft.StringValue, ARight.StringValue);
    Exit;
  end;

  if (ALeft.Kind = ctvkBigInt) and (ARight.Kind = ctvkBigInt) then
  begin
    ACompare := ALeft.BigIntValue.Compare(ARight.BigIntValue);
    Exit;
  end;

  if (ALeft.Kind = ctvkBigInt) or (ARight.Kind = ctvkBigInt) then
    Exit(False);

  if not TryCompileTimeValueToNumber(ALeft, LeftNum) or
     not TryCompileTimeValueToNumber(ARight, RightNum) then
    Exit(False);

  if IsNaN(LeftNum) or IsNaN(RightNum) then
  begin
    ACompare := MaxInt;
    Exit(True);
  end;

  if LeftNum < RightNum then
    ACompare := -1
  else if LeftNum > RightNum then
    ACompare := 1
  else
    ACompare := 0;
end;

function TryFoldComparison(const AOp: TGocciaTokenType;
  const ALeft, ARight: TGocciaCompileTimeValue;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  Compare: Integer;
begin
  Result := True;
  case AOp of
    gttEqual:
      AValue := BooleanCompileTimeValue(StrictEqualConstants(ALeft, ARight));
    gttNotEqual:
      AValue := BooleanCompileTimeValue(not StrictEqualConstants(ALeft, ARight));
    gttLess, gttGreater, gttLessEqual, gttGreaterEqual:
      begin
        if not TryCompareConstants(ALeft, ARight, Compare) then
          Exit(False);

        if Compare = MaxInt then
          AValue := BooleanCompileTimeValue(False)
        else
          case AOp of
            gttLess:
              AValue := BooleanCompileTimeValue(Compare < 0);
            gttGreater:
              AValue := BooleanCompileTimeValue(Compare > 0);
            gttLessEqual:
              AValue := BooleanCompileTimeValue(Compare <= 0);
            gttGreaterEqual:
              AValue := BooleanCompileTimeValue(Compare >= 0);
          else
            AValue := BooleanCompileTimeValue(False);
          end;
      end;
  else
    AValue := UnknownCompileTimeValue;
    Result := False;
  end;
end;

function TryFoldBinaryConstants(const AOp: TGocciaTokenType;
  const ALeft, ARight: TGocciaCompileTimeValue;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  LeftNum, RightNum: Double;
begin
  Result := False;
  AValue := UnknownCompileTimeValue;

  if AOp in [gttEqual, gttNotEqual, gttLess, gttGreater, gttLessEqual,
    gttGreaterEqual] then
    Exit(TryFoldComparison(AOp, ALeft, ARight, AValue));

  if AOp = gttPlus then
  begin
    if (ALeft.Kind = ctvkString) or (ARight.Kind = ctvkString) then
    begin
      AValue := StringCompileTimeValue(
        Goccia.Compiler.ConstantValue.CompileTimeValueToString(ALeft) +
        Goccia.Compiler.ConstantValue.CompileTimeValueToString(ARight));
      Exit(True);
    end;
  end;

  if (ALeft.Kind = ctvkBigInt) or (ARight.Kind = ctvkBigInt) then
  begin
    if (ALeft.Kind = ctvkBigInt) and (ARight.Kind = ctvkBigInt) then
      Exit(FoldedBigIntBinary(AOp, ALeft.BigIntValue, ARight.BigIntValue, AValue));
    Exit(False);
  end;

  if not TryCompileTimeValueToNumber(ALeft, LeftNum) or
     not TryCompileTimeValueToNumber(ARight, RightNum) then
    Exit(False);

  Result := FoldedNumericBinary(AOp, LeftNum, RightNum, AValue);
end;

function TryEvaluateIdentifier(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaIdentifierExpression;
  out AValue: TGocciaCompileTimeValue): Boolean;
begin
  if ACtx.OptimizationOptions.EnableConstPropagation and
     ACtx.Scope.TryGetVisibleConstantValue(AExpr.Name, AValue) then
    Exit(True);

  if ACtx.Scope.HasVisibleLocal(AExpr.Name) then
  begin
    AValue := UnknownCompileTimeValue;
    Exit(False);
  end;

  if AExpr.Name = UNDEFINED_LITERAL then
    AValue := UndefinedCompileTimeValue
  else if AExpr.Name = NAN_LITERAL then
    AValue := NumberCompileTimeValue(NaN)
  else if AExpr.Name = INFINITY_LITERAL then
    AValue := NumberCompileTimeValue(Infinity)
  else
  begin
    AValue := UnknownCompileTimeValue;
    Exit(False);
  end;

  Result := True;
end;

function TryEvaluateUnaryExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  Operand: TGocciaCompileTimeValue;
  NumberValue: Double;
begin
  AValue := UnknownCompileTimeValue;

  if AExpr.Operator = gttDelete then
    Exit(False);

  if not TryEvaluateConstantExpression(ACtx, AExpr.Operand, Operand) then
    Exit(False);

  Result := True;
  case AExpr.Operator of
    gttNot:
      AValue := BooleanCompileTimeValue(not
        Goccia.Compiler.ConstantValue.CompileTimeValueToBoolean(Operand));
    gttVoid:
      AValue := UndefinedCompileTimeValue;
    gttTypeof:
      case Operand.Kind of
        ctvkUndefined:
          AValue := StringCompileTimeValue(UNDEFINED_TYPE_NAME);
        ctvkNull:
          AValue := StringCompileTimeValue(OBJECT_TYPE_NAME);
        ctvkBoolean:
          AValue := StringCompileTimeValue(BOOLEAN_TYPE_NAME);
        ctvkNumber:
          AValue := StringCompileTimeValue(NUMBER_TYPE_NAME);
        ctvkString:
          AValue := StringCompileTimeValue(STRING_TYPE_NAME);
        ctvkBigInt:
          AValue := StringCompileTimeValue(BIGINT_TYPE_NAME);
      else
        Result := False;
      end;
    gttPlus:
      begin
        if not TryCompileTimeValueToNumber(Operand, NumberValue) then
          Exit(False);
        AValue := NumberCompileTimeValue(NumberValue);
      end;
    gttMinus:
      if Operand.Kind = ctvkBigInt then
        AValue := BigIntCompileTimeValue(Operand.BigIntValue.Negate)
      else
      begin
        if not TryCompileTimeValueToNumber(Operand, NumberValue) then
          Exit(False);
        AValue := NumberCompileTimeValue(-NumberValue);
      end;
    gttBitwiseNot:
      if Operand.Kind = ctvkBigInt then
        AValue := BigIntCompileTimeValue(Operand.BigIntValue.BitwiseNot)
      else
      begin
        if not TryCompileTimeValueToNumber(Operand, NumberValue) then
          Exit(False);
        AValue := NumberCompileTimeValue(not ToInt32Number(NumberValue));
      end;
  else
    Result := False;
  end;
end;

function TryEvaluateBinaryExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  LeftValue, RightValue: TGocciaCompileTimeValue;
begin
  AValue := UnknownCompileTimeValue;

  if not TryEvaluateConstantExpression(ACtx, AExpr.Left, LeftValue) then
    Exit(False);

  case AExpr.Operator of
    gttAnd:
      begin
        if not Goccia.Compiler.ConstantValue.CompileTimeValueToBoolean(LeftValue) then
        begin
          AValue := LeftValue;
          Exit(True);
        end;
        if TryEvaluateConstantExpression(ACtx, AExpr.Right, RightValue) then
        begin
          AValue := RightValue;
          Exit(True);
        end;
        Exit(False);
      end;
    gttOr:
      begin
        if Goccia.Compiler.ConstantValue.CompileTimeValueToBoolean(LeftValue) then
        begin
          AValue := LeftValue;
          Exit(True);
        end;
        if TryEvaluateConstantExpression(ACtx, AExpr.Right, RightValue) then
        begin
          AValue := RightValue;
          Exit(True);
        end;
        Exit(False);
      end;
    gttNullishCoalescing:
      begin
        if not Goccia.Compiler.ConstantValue.CompileTimeValueIsNullish(LeftValue) then
        begin
          AValue := LeftValue;
          Exit(True);
        end;
        if TryEvaluateConstantExpression(ACtx, AExpr.Right, RightValue) then
        begin
          AValue := RightValue;
          Exit(True);
        end;
        Exit(False);
      end;
  end;

  if not TryEvaluateConstantExpression(ACtx, AExpr.Right, RightValue) then
    Exit(False);

  Result := TryFoldBinaryConstants(AExpr.Operator, LeftValue, RightValue, AValue);
end;

function TryEvaluateSequenceExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaSequenceExpression;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  I: Integer;
begin
  AValue := UndefinedCompileTimeValue;
  for I := 0 to AExpr.Expressions.Count - 1 do
    if not TryEvaluateConstantExpression(ACtx, AExpr.Expressions[I], AValue) then
    begin
      AValue := UnknownCompileTimeValue;
      Exit(False);
    end;
  Result := True;
end;

function TryEvaluateConditionalExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaConditionalExpression;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  Condition: TGocciaCompileTimeValue;
begin
  if not TryEvaluateConstantExpression(ACtx, AExpr.Condition, Condition) then
  begin
    AValue := UnknownCompileTimeValue;
    Exit(False);
  end;

  if Goccia.Compiler.ConstantValue.CompileTimeValueToBoolean(Condition) then
    Result := TryEvaluateConstantExpression(ACtx, AExpr.Consequent, AValue)
  else
    Result := TryEvaluateConstantExpression(ACtx, AExpr.Alternate, AValue);
end;

function TryEvaluateTemplateWithInterpolation(
  const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaTemplateWithInterpolationExpression;
  out AValue: TGocciaCompileTimeValue): Boolean;
var
  I: Integer;
  PartValue: TGocciaCompileTimeValue;
  Text: string;
begin
  Text := '';
  for I := 0 to AExpr.Parts.Count - 1 do
  begin
    if not TryEvaluateConstantExpression(ACtx, AExpr.Parts[I], PartValue) then
    begin
      AValue := UnknownCompileTimeValue;
      Exit(False);
    end;

    Text := Text +
      Goccia.Compiler.ConstantValue.CompileTimeValueToString(PartValue);
  end;

  AValue := StringCompileTimeValue(Text);
  Result := True;
end;

function TryEvaluateConstantExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; out AValue: TGocciaCompileTimeValue): Boolean;
begin
  AValue := UnknownCompileTimeValue;
  if not ACtx.OptimizationOptions.EnableConstantFolding then
    Exit(False);

  if AExpr is TGocciaLiteralExpression then
    Result := TryCompileTimeValueFromLiteral(
      TGocciaLiteralExpression(AExpr).Value, AValue)
  else if AExpr is TGocciaIdentifierExpression then
    Result := TryEvaluateIdentifier(ACtx, TGocciaIdentifierExpression(AExpr), AValue)
  else if AExpr is TGocciaUnaryExpression then
    Result := TryEvaluateUnaryExpression(ACtx, TGocciaUnaryExpression(AExpr), AValue)
  else if AExpr is TGocciaBinaryExpression then
    Result := TryEvaluateBinaryExpression(ACtx, TGocciaBinaryExpression(AExpr), AValue)
  else if AExpr is TGocciaSequenceExpression then
    Result := TryEvaluateSequenceExpression(ACtx, TGocciaSequenceExpression(AExpr), AValue)
  else if AExpr is TGocciaConditionalExpression then
    Result := TryEvaluateConditionalExpression(ACtx, TGocciaConditionalExpression(AExpr), AValue)
  else if AExpr is TGocciaTemplateLiteralExpression then
  begin
    AValue := StringCompileTimeValue(
      TGocciaTemplateLiteralExpression(AExpr).Value);
    Result := True;
  end
  else if AExpr is TGocciaTemplateWithInterpolationExpression then
    Result := TryEvaluateTemplateWithInterpolation(ACtx,
      TGocciaTemplateWithInterpolationExpression(AExpr), AValue)
  else
    Result := False;
end;

function IsEmptyStringConstant(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression): Boolean;
var
  Value: TGocciaCompileTimeValue;
begin
  Result := TryEvaluateConstantExpression(ACtx, AExpr, Value) and
    (Value.Kind = ctvkString) and (Value.StringValue = '');
end;

function IsBooleanConstant(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const AExpected: Boolean): Boolean;
var
  Value: TGocciaCompileTimeValue;
begin
  Result := TryEvaluateConstantExpression(ACtx, AExpr, Value) and
    (Value.Kind = ctvkBoolean) and (Value.BooleanValue = AExpected);
end;

function IsNumberConstant(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const AExpected: Double): Boolean;
var
  Value: TGocciaCompileTimeValue;
begin
  Result := TryEvaluateConstantExpression(ACtx, AExpr, Value) and
    CompileTimeValueIsNumber(Value, AExpected);
end;

function TryEmitStrictTypeSimplification(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt8): Boolean;
var
  Unary: TGocciaUnaryExpression;
  InnerUnary: TGocciaUnaryExpression;
  Binary: TGocciaBinaryExpression;
  LeftType, RightType: TGocciaLocalType;
begin
  Result := False;
  if not ACtx.StrictTypes then
    Exit;

  if AExpr is TGocciaUnaryExpression then
  begin
    Unary := TGocciaUnaryExpression(AExpr);
    if (Unary.Operator = gttNot) and
       (Unary.Operand is TGocciaUnaryExpression) then
    begin
      InnerUnary := TGocciaUnaryExpression(Unary.Operand);
      if (InnerUnary.Operator = gttNot) and
         (ExpressionType(ACtx.Scope, InnerUnary.Operand) = sltBoolean) then
      begin
        ACtx.CompileExpression(InnerUnary.Operand, ADest);
        Exit(True);
      end;
    end;
  end
  else if AExpr is TGocciaBinaryExpression then
  begin
    Binary := TGocciaBinaryExpression(AExpr);
    LeftType := ExpressionType(ACtx.Scope, Binary.Left);
    RightType := ExpressionType(ACtx.Scope, Binary.Right);

    if (Binary.Operator = gttAnd) and (LeftType = sltBoolean) and
       IsBooleanConstant(ACtx, Binary.Right, True) then
    begin
      ACtx.CompileExpression(Binary.Left, ADest);
      Exit(True);
    end;

    if (Binary.Operator = gttOr) and (LeftType = sltBoolean) and
       IsBooleanConstant(ACtx, Binary.Right, False) then
    begin
      ACtx.CompileExpression(Binary.Left, ADest);
      Exit(True);
    end;

    if (Binary.Operator = gttPlus) and (LeftType = sltString) and
       IsEmptyStringConstant(ACtx, Binary.Right) then
    begin
      ACtx.CompileExpression(Binary.Left, ADest);
      Exit(True);
    end;

    if (Binary.Operator = gttPlus) and (RightType = sltString) and
       IsEmptyStringConstant(ACtx, Binary.Left) then
    begin
      ACtx.CompileExpression(Binary.Right, ADest);
      Exit(True);
    end;

    if (Binary.Operator = gttPower) and IsKnownNumeric(LeftType) and
       IsNumberConstant(ACtx, Binary.Right, 1.0) then
    begin
      ACtx.CompileExpression(Binary.Left, ADest);
      Exit(True);
    end;

    if (Binary.Operator = gttPower) and IsKnownNumeric(LeftType) and
       IsNumberConstant(ACtx, Binary.Right, 0.0) then
    begin
      ACtx.CompileExpression(Binary.Left, ADest);
      EmitCompileTimeValue(ACtx, NumberCompileTimeValue(1.0), ADest);
      Exit(True);
    end;
  end;
end;

function TryEmitConstantExpression(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaExpression; const ADest: UInt8): Boolean;
var
  Value: TGocciaCompileTimeValue;
begin
  if TryEvaluateConstantExpression(ACtx, AExpr, Value) then
  begin
    EmitCompileTimeValue(ACtx, Value, ADest);
    Exit(True);
  end;

  Result := TryEmitStrictTypeSimplification(ACtx, AExpr, ADest);
end;

function TryFoldBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8): Boolean;
begin
  Result := TryEmitConstantExpression(ACtx, AExpr, ADest);
end;

function TryFoldUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8): Boolean;
begin
  Result := TryEmitConstantExpression(ACtx, AExpr, ADest);
end;

end.

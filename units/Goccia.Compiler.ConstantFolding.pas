unit Goccia.Compiler.ConstantFolding;

{$I Goccia.inc}

interface

uses
  Goccia.AST.Expressions,
  Goccia.Compiler.Context;

function TryFoldBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8): Boolean;
function TryFoldUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8): Boolean;
function IsNegativeZeroFloat(const AValue: Double): Boolean; inline;

implementation

uses
  Math,

  Souffle.Bytecode,

  Goccia.Token,
  Goccia.Values.Primitives;

function IsNegativeZeroFloat(const AValue: Double): Boolean; inline;
var
  V: Double;
  Bits: Int64 absolute V;
begin
  V := AValue;
  Result := (V = 0.0) and (Bits < 0);
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

function TryFoldBinary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaBinaryExpression; const ADest: UInt8): Boolean;
var
  LeftLit, RightLit: TGocciaLiteralExpression;
  LeftNum, RightNum: TGocciaNumberLiteralValue;
  LeftVal, RightVal, FoldedVal: Double;
  LeftStr, RightStr: TGocciaStringLiteralValue;
  Idx: UInt16;
begin
  Result := False;

  if not (AExpr.Left is TGocciaLiteralExpression) or
     not (AExpr.Right is TGocciaLiteralExpression) then
    Exit;

  LeftLit := TGocciaLiteralExpression(AExpr.Left);
  RightLit := TGocciaLiteralExpression(AExpr.Right);

  if (LeftLit.Value is TGocciaNumberLiteralValue) and
     (RightLit.Value is TGocciaNumberLiteralValue) then
  begin
    LeftNum := TGocciaNumberLiteralValue(LeftLit.Value);
    RightNum := TGocciaNumberLiteralValue(RightLit.Value);

    if LeftNum.IsNaN or LeftNum.IsInfinite or LeftNum.IsNegativeZero or
       RightNum.IsNaN or RightNum.IsInfinite or RightNum.IsNegativeZero then
      Exit;

    LeftVal := LeftNum.Value;
    RightVal := RightNum.Value;

    case AExpr.Operator of
      gttPlus:
      begin
        EmitFoldedNumber(ACtx, LeftVal + RightVal, ADest);
        Result := True;
      end;
      gttMinus:
      begin
        EmitFoldedNumber(ACtx, LeftVal - RightVal, ADest);
        Result := True;
      end;
      gttStar:
      begin
        EmitFoldedNumber(ACtx, LeftVal * RightVal, ADest);
        Result := True;
      end;
      gttSlash:
      begin
        if RightVal = 0.0 then
          Exit;
        FoldedVal := LeftVal / RightVal;
        if not IsNaN(FoldedVal) and not IsInfinite(FoldedVal) then
        begin
          EmitFoldedNumber(ACtx, FoldedVal, ADest);
          Result := True;
        end;
      end;
      gttPercent:
      begin
        if RightVal = 0.0 then
          Exit;
        if (Frac(LeftVal) = 0.0) and (Frac(RightVal) = 0.0) then
          EmitFoldedNumber(ACtx, Trunc(LeftVal) mod Trunc(RightVal), ADest)
        else
          EmitFoldedNumber(ACtx, FMod(LeftVal, RightVal), ADest);
        Result := True;
      end;
      gttPower:
      begin
        FoldedVal := Math.Power(LeftVal, RightVal);
        if not IsNaN(FoldedVal) and not IsInfinite(FoldedVal) then
        begin
          EmitFoldedNumber(ACtx, FoldedVal, ADest);
          Result := True;
        end;
      end;
      gttEqual:
      begin
        if LeftVal = RightVal then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
        else
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
        Result := True;
      end;
      gttNotEqual:
      begin
        if LeftVal <> RightVal then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
        else
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
        Result := True;
      end;
      gttLess:
      begin
        if LeftVal < RightVal then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
        else
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
        Result := True;
      end;
      gttGreater:
      begin
        if LeftVal > RightVal then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
        else
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
        Result := True;
      end;
      gttLessEqual:
      begin
        if LeftVal <= RightVal then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
        else
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
        Result := True;
      end;
      gttGreaterEqual:
      begin
        if LeftVal >= RightVal then
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_TRUE, ADest, 0, 0))
        else
          EmitInstruction(ACtx, EncodeABC(OP_LOAD_FALSE, ADest, 0, 0));
        Result := True;
      end;
      gttBitwiseAnd:
      begin
        EmitFoldedNumber(ACtx, Int32(Trunc(LeftVal)) and Int32(Trunc(RightVal)), ADest);
        Result := True;
      end;
      gttBitwiseOr:
      begin
        EmitFoldedNumber(ACtx, Int32(Trunc(LeftVal)) or Int32(Trunc(RightVal)), ADest);
        Result := True;
      end;
      gttBitwiseXor:
      begin
        EmitFoldedNumber(ACtx, Int32(Trunc(LeftVal)) xor Int32(Trunc(RightVal)), ADest);
        Result := True;
      end;
      gttLeftShift:
      begin
        EmitFoldedNumber(ACtx, Int32(Trunc(LeftVal)) shl (Int32(Trunc(RightVal)) and $1F), ADest);
        Result := True;
      end;
      gttRightShift:
      begin
        EmitFoldedNumber(ACtx, SarLongint(Int32(Trunc(LeftVal)), Int32(Trunc(RightVal)) and $1F), ADest);
        Result := True;
      end;
      gttUnsignedRightShift:
      begin
        EmitFoldedNumber(ACtx, Int64(UInt32(Trunc(LeftVal)) shr
          (Int32(Trunc(RightVal)) and $1F)), ADest);
        Result := True;
      end;
    end;
    Exit;
  end;

  if (AExpr.Operator = gttPlus) and
     (LeftLit.Value is TGocciaStringLiteralValue) and
     (RightLit.Value is TGocciaStringLiteralValue) then
  begin
    LeftStr := TGocciaStringLiteralValue(LeftLit.Value);
    RightStr := TGocciaStringLiteralValue(RightLit.Value);
    Idx := ACtx.Template.AddConstantString(LeftStr.Value + RightStr.Value);
    EmitInstruction(ACtx, EncodeABx(OP_LOAD_CONST, ADest, Idx));
    Result := True;
  end;
end;

function TryFoldUnary(const ACtx: TGocciaCompilationContext;
  const AExpr: TGocciaUnaryExpression; const ADest: UInt8): Boolean;
var
  Lit: TGocciaLiteralExpression;
  Num: TGocciaNumberLiteralValue;
  NumVal: Double;
begin
  Result := False;

  if not (AExpr.Operand is TGocciaLiteralExpression) then
    Exit;

  Lit := TGocciaLiteralExpression(AExpr.Operand);

  if not (Lit.Value is TGocciaNumberLiteralValue) then
    Exit;

  Num := TGocciaNumberLiteralValue(Lit.Value);

  if Num.IsNaN or Num.IsInfinite or Num.IsNegativeZero then
    Exit;

  NumVal := Num.Value;

  case AExpr.Operator of
    gttMinus:
    begin
      EmitFoldedNumber(ACtx, -NumVal, ADest);
      Result := True;
    end;
    gttPlus:
    begin
      EmitFoldedNumber(ACtx, NumVal, ADest);
      Result := True;
    end;
    gttBitwiseNot:
    begin
      EmitFoldedNumber(ACtx, not Int32(Trunc(NumVal)), ADest);
      Result := True;
    end;
  end;
end;

end.

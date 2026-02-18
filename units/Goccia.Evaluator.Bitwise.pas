unit Goccia.Evaluator.Bitwise;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue; inline;

implementation

uses
  Goccia.Values.ClassHelper;

function EvaluateBitwiseAnd(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) and Trunc(ARight.ToNumberLiteral.Value));
end;

function EvaluateBitwiseOr(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) or Trunc(ARight.ToNumberLiteral.Value));
end;

function EvaluateBitwiseXor(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) xor Trunc(ARight.ToNumberLiteral.Value));
end;

function EvaluateLeftShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) shl (Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

function EvaluateRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(ALeft.ToNumberLiteral.Value) shr (Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

function EvaluateUnsignedRightShift(const ALeft, ARight: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Cardinal(Trunc(ALeft.ToNumberLiteral.Value)) shr (Trunc(ARight.ToNumberLiteral.Value) and 31));
end;

function EvaluateBitwiseNot(const AOperand: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(not Trunc(AOperand.ToNumberLiteral.Value));
end;

end.

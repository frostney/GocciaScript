unit Goccia.Evaluator.Bitwise;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Core, Goccia.Values.Primitives;

function EvaluateBitwiseAnd(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseOr(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseXor(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateLeftShift(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateRightShift(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateUnsignedRightShift(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseNot(Operand: TGocciaValue): TGocciaValue; inline;

implementation

uses Goccia.Values.ClassHelper;

function EvaluateBitwiseAnd(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(Left.ToNumberLiteral.Value) and Trunc(Right.ToNumberLiteral.Value));
end;

function EvaluateBitwiseOr(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(Left.ToNumberLiteral.Value) or Trunc(Right.ToNumberLiteral.Value));
end;

function EvaluateBitwiseXor(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(Left.ToNumberLiteral.Value) xor Trunc(Right.ToNumberLiteral.Value));
end;

function EvaluateLeftShift(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(Left.ToNumberLiteral.Value) shl (Trunc(Right.ToNumberLiteral.Value) and 31));
end;

function EvaluateRightShift(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Trunc(Left.ToNumberLiteral.Value) shr (Trunc(Right.ToNumberLiteral.Value) and 31));
end;

function EvaluateUnsignedRightShift(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(Cardinal(Trunc(Left.ToNumberLiteral.Value)) shr (Trunc(Right.ToNumberLiteral.Value) and 31));
end;

function EvaluateBitwiseNot(Operand: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(not Trunc(Operand.ToNumberLiteral.Value));
end;

end.

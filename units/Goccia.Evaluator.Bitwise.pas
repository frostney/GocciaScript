unit Goccia.Evaluator.Bitwise;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Base, Goccia.Values.NumberValue;

function EvaluateBitwiseAnd(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseOr(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseXor(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateLeftShift(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateRightShift(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateUnsignedRightShift(Left, Right: TGocciaValue): TGocciaValue; inline;
function EvaluateBitwiseNot(Operand: TGocciaValue): TGocciaValue; inline;

implementation

function EvaluateBitwiseAnd(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Trunc(Left.ToNumber) and Trunc(Right.ToNumber));
end;

function EvaluateBitwiseOr(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Trunc(Left.ToNumber) or Trunc(Right.ToNumber));
end;

function EvaluateBitwiseXor(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Trunc(Left.ToNumber) xor Trunc(Right.ToNumber));
end;

function EvaluateLeftShift(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Trunc(Left.ToNumber) shl (Trunc(Right.ToNumber) and 31));
end;

function EvaluateRightShift(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Trunc(Left.ToNumber) shr (Trunc(Right.ToNumber) and 31));
end;

function EvaluateUnsignedRightShift(Left, Right: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(Cardinal(Trunc(Left.ToNumber)) shr (Trunc(Right.ToNumber) and 31));
end;

function EvaluateBitwiseNot(Operand: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberValue.Create(not Trunc(Operand.ToNumber));
end;

end.

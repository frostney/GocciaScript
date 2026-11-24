unit Goccia.AST.LiteralValues;

{$I Goccia.inc}

// The old runtime's view of a literal expression: its value as a runtime
// value. The parser keeps a literal engine-free (Goccia.AST.Expressions); the
// old runtime's compiler, constant folding and type checks read it through
// Value, which makes the runtime value of a number, string or BigInt once and
// leaves it with the expression. Goes with the old runtime.

interface

uses
  Goccia.AST.Expressions,
  Goccia.Values.Primitives;

type
  TGocciaLiteralExpressionValue = class helper for TGocciaLiteralExpression
  public
    // The literal as a runtime value: a primitive singleton for undefined,
    // null and the booleans, else a value the expression owns.
    function Value: TGocciaValue;
  end;

implementation

uses
  Goccia.GarbageCollector,
  Goccia.Values.BigIntValue;

function TGocciaLiteralExpressionValue.Value: TGocciaValue;
var
  Made: TGocciaValue;
begin
  case Kind of
    lkUndefined:
      Exit(TGocciaUndefinedLiteralValue.UndefinedValue);
    lkNull:
      Exit(TGocciaNullLiteralValue.NullValue);
    lkBoolean:
      if BooleanValue then
        Exit(TGocciaBooleanLiteralValue.TrueValue)
      else
        Exit(TGocciaBooleanLiteralValue.FalseValue);
  end;
  if RuntimeValue = nil then
  begin
    case Kind of
      lkNumber:
        Made := TGocciaNumberLiteralValue.Create(NumberValue);
      lkString:
        Made := TGocciaStringLiteralValue.Create(StringValue);
    else
      Made := TGocciaBigIntValue.Create(BigIntValue);
    end;
    // The expression owns it, not the garbage collector.
    if TGarbageCollector.Instance <> nil then
      TGarbageCollector.Instance.UnregisterObject(Made);
    RuntimeValue := Made;
  end;
  Result := TGocciaValue(RuntimeValue);
end;

end.

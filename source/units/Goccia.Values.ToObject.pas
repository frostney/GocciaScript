unit Goccia.Values.ToObject;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// ES2026 §7.1.18 ToObject(argument)
// Throws TypeError for undefined/null. Returns objects as-is. Boxes primitives.
function ToObject(const AValue: TGocciaValue): TGocciaObjectValue;

// ES2026 §7.3.3 LengthOfArrayLike(obj)
// Returns ToLength(? Get(obj, "length")).
function LengthOfArrayLike(const AObj: TGocciaObjectValue): Integer;

// ES2026 §7.3.3 LengthOfArrayLike(obj), returning both the raw post-ToLength
// value (capped at 2^53-1 but not truncated to Integer) and the Integer-clamped
// length used by legacy callers.  Callers that need to enforce ArrayCreate
// bounds (2^32-1) or safe-integer length bounds (2^53-1) must use AOutRawLen.
function LengthOfArrayLikeEx(const AObj: TGocciaObjectValue; out AOutRawLen: Double): Integer;

implementation

uses
  Math,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ClassHelper,
  Goccia.Values.ErrorHelper;

// ES2026 §7.1.18 ToObject(argument)
function ToObject(const AValue: TGocciaValue): TGocciaObjectValue;
var
  Boxed: TGocciaObjectValue;
begin
  if (AValue is TGocciaUndefinedLiteralValue) or (AValue is TGocciaNullLiteralValue) then
    ThrowTypeError(SErrorCannotConvertNullOrUndefined, SSuggestCheckNullBeforeAccess);

  if AValue is TGocciaObjectValue then
  begin
    Result := TGocciaObjectValue(AValue);
    Exit;
  end;

  // Box primitives (boolean, number, string, bigint)
  Boxed := AValue.Box;
  if Assigned(Boxed) then
  begin
    Result := Boxed;
    Exit;
  end;

  // Symbol and other non-coercible types
  ThrowTypeError(SErrorCannotConvertValueToObject, SSuggestObjectArgType);
  Result := nil;
end;

// ES2026 §7.3.3 LengthOfArrayLike(obj)
function LengthOfArrayLike(const AObj: TGocciaObjectValue): Integer;
var
  RawLen: Double;
begin
  Result := LengthOfArrayLikeEx(AObj, RawLen);
end;

// Shared implementation that also returns the raw post-ToLength value.
function LengthOfArrayLikeEx(const AObj: TGocciaObjectValue; out AOutRawLen: Double): Integer;
const
  MAX_SAFE_INTEGER = 9007199254740991.0; // 2^53 - 1
var
  LengthProp: TGocciaValue;
  LengthValue: Double;
begin
  // Step 1: Let len be ? Get(obj, "length")
  LengthProp := AObj.GetProperty(PROP_LENGTH);

  // Step 2: Return ? ToLength(len)
  // ES2026 §7.1.22 ToLength: undefined/null → 0, NaN → 0, negative → 0, cap at 2^53−1
  if not Assigned(LengthProp) or
     (LengthProp is TGocciaUndefinedLiteralValue) or
     (LengthProp is TGocciaNullLiteralValue) then
  begin
    AOutRawLen := 0;
    Result := 0;
    Exit;
  end;

  LengthValue := LengthProp.ToNumberLiteral.Value;

  if IsNan(LengthValue) or (LengthValue <= 0) then
  begin
    AOutRawLen := 0;
    Result := 0;
    Exit;
  end;

  // Cap raw length at 2^53 - 1 per ToLength step 3.
  if LengthValue > MAX_SAFE_INTEGER then
    AOutRawLen := MAX_SAFE_INTEGER
  else
    AOutRawLen := Trunc(LengthValue);

  if LengthValue >= MaxInt then
    Result := MaxInt
  else
    Result := Trunc(LengthValue);
end;

end.

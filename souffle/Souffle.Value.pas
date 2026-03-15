unit Souffle.Value;

{$I Souffle.inc}

interface

uses
  Souffle.Heap;

const
  SOUFFLE_INLINE_STRING_MAX = 13;
  SOUFFLE_NIL_DEFAULT = 0;
  SOUFFLE_NIL_MATCH_ANY = 255;

type
  TSouffleValueKind = (
    svkNil,
    svkBoolean,
    svkInteger,
    svkFloat,
    svkString,
    svkReference
  );

  TSouffleInlineString = string[SOUFFLE_INLINE_STRING_MAX];

  PSouffleValue = ^TSouffleValue;

  TSouffleValue = packed record
    Kind: TSouffleValueKind;
    Flags: Byte;
    case TSouffleValueKind of
      svkNil:       ();
      svkBoolean:   (AsBoolean: Boolean);
      svkInteger:   (AsInteger: Int64);
      svkFloat:     (AsFloat: Double);
      svkString:    (AsInlineString: TSouffleInlineString);
      svkReference: (AsReference: TSouffleHeapObject);
  end;

  TSouffleValueArray = array of TSouffleValue;
  PSouffleValueArray = ^TSouffleValueArray;

function SouffleNil: TSouffleValue; inline;
function SouffleNilWithFlags(const AFlags: Byte): TSouffleValue; inline;
function SouffleBoolean(const AValue: Boolean): TSouffleValue; inline;
function SouffleInteger(const AValue: Int64): TSouffleValue; inline;
function SouffleFloat(const AValue: Double): TSouffleValue; inline;
function SouffleString(const AValue: string): TSouffleValue;
function SouffleReference(const AObject: TSouffleHeapObject): TSouffleValue; inline;

function SouffleIsNil(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsBoolean(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsInteger(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsFloat(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsInlineString(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsReference(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsNumeric(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsStringValue(const AValue: TSouffleValue): Boolean; inline;

function SouffleIsTrue(const AValue: TSouffleValue): Boolean; inline;
function SouffleAsNumber(const AValue: TSouffleValue): Double; inline;
function SouffleToDouble(const AValue: TSouffleValue): Double; inline;
function SouffleGetString(const AValue: TSouffleValue): string;

function SouffleValuesEqual(const A, B: TSouffleValue): Boolean;
function SouffleValueToString(const AValue: TSouffleValue): string;

implementation

uses
  Math,
  SysUtils,

  GarbageCollector.Generic;

{ Value constructors }

function SouffleNil: TSouffleValue;
begin
  Result.Kind := svkNil;
  Result.Flags := SOUFFLE_NIL_DEFAULT;
end;

function SouffleNilWithFlags(const AFlags: Byte): TSouffleValue;
begin
  Result.Kind := svkNil;
  Result.Flags := AFlags;
end;

function SouffleBoolean(const AValue: Boolean): TSouffleValue;
begin
  Result.Kind := svkBoolean;
  Result.Flags := 0;
  Result.AsBoolean := AValue;
end;

function SouffleInteger(const AValue: Int64): TSouffleValue;
begin
  Result.Kind := svkInteger;
  Result.Flags := 0;
  Result.AsInteger := AValue;
end;

function SouffleFloat(const AValue: Double): TSouffleValue;
begin
  Result.Kind := svkFloat;
  Result.Flags := 0;
  Result.AsFloat := AValue;
end;

function SouffleString(const AValue: string): TSouffleValue;
var
  HeapStr: TSouffleHeapString;
  GC: TGarbageCollector;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(AValue) <= SOUFFLE_INLINE_STRING_MAX then
  begin
    Result.Kind := svkString;
    Result.AsInlineString := AValue;
  end
  else
  begin
    HeapStr := TSouffleHeapString.Create(AValue);
    GC := TGarbageCollector.Instance;
    if Assigned(GC) then
      GC.AllocateObject(HeapStr);
    Result.Kind := svkReference;
    Result.AsReference := HeapStr;
  end;
end;

function SouffleReference(const AObject: TSouffleHeapObject): TSouffleValue;
begin
  Result.Kind := svkReference;
  Result.Flags := 0;
  Result.AsReference := AObject;
end;

{ Type checks }

function SouffleIsNil(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkNil;
end;

function SouffleIsBoolean(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkBoolean;
end;

function SouffleIsInteger(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkInteger;
end;

function SouffleIsFloat(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkFloat;
end;

function SouffleIsInlineString(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkString;
end;

function SouffleIsReference(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkReference;
end;

function SouffleIsNumeric(const AValue: TSouffleValue): Boolean;
begin
  Result := (AValue.Kind = svkInteger) or (AValue.Kind = svkFloat);
end;

function SouffleIsStringValue(const AValue: TSouffleValue): Boolean;
begin
  Result := (AValue.Kind = svkString) or
    ((AValue.Kind = svkReference) and (AValue.AsReference is TSouffleHeapString));
end;

{ Truthiness -- universal semantics used by JUMP_IF_TRUE/JUMP_IF_FALSE }

function SouffleIsTrue(const AValue: TSouffleValue): Boolean;
begin
  case AValue.Kind of
    svkNil:
      Result := False;
    svkBoolean:
      Result := AValue.AsBoolean;
    svkInteger:
      Result := AValue.AsInteger <> 0;
    svkFloat:
      Result := (AValue.AsFloat <> 0.0) and not IsNaN(AValue.AsFloat);
    svkString:
      Result := AValue.AsInlineString <> '';
    svkReference:
      Result := Assigned(AValue.AsReference);
  else
    Result := False;
  end;
end;

{ Numeric coercion for VM-level arithmetic fast paths }

function SouffleAsNumber(const AValue: TSouffleValue): Double;
begin
  case AValue.Kind of
    svkInteger:
      Result := AValue.AsInteger;
    svkFloat:
      Result := AValue.AsFloat;
  else
    Result := NaN;
  end;
end;

function SouffleToDouble(const AValue: TSouffleValue): Double;
begin
  if AValue.Kind = svkFloat then
    Result := AValue.AsFloat
  else if AValue.Kind = svkInteger then
    Result := AValue.AsInteger * 1.0
  else
  begin
    {$IFDEF DEBUG}
    Assert(False, 'SouffleToDouble: expected svkFloat or svkInteger, got kind ' +
      IntToStr(Ord(AValue.Kind)));
    {$ENDIF}
    Result := NaN;
  end;
end;

{ String access -- handles both inline (svkString) and heap (TSouffleHeapString) }

function SouffleGetString(const AValue: TSouffleValue): string;
begin
  if AValue.Kind = svkString then
    Result := AValue.AsInlineString
  else if (AValue.Kind = svkReference) and (AValue.AsReference is TSouffleHeapString) then
    Result := TSouffleHeapString(AValue.AsReference).Value
  else
    Result := '';
end;

{ Identity equality for core operations }

function SouffleValuesEqual(const A, B: TSouffleValue): Boolean;
begin
  if A.Kind <> B.Kind then
  begin
    if SouffleIsStringValue(A) and SouffleIsStringValue(B) then
      Exit(SouffleGetString(A) = SouffleGetString(B));
    Exit(False);
  end;

  case A.Kind of
    svkNil:
      Result := A.Flags = B.Flags;
    svkBoolean:
      Result := A.AsBoolean = B.AsBoolean;
    svkInteger:
      Result := A.AsInteger = B.AsInteger;
    svkFloat:
      Result := A.AsFloat = B.AsFloat;
    svkString:
      Result := A.AsInlineString = B.AsInlineString;
    svkReference:
      Result := A.AsReference = B.AsReference;
  else
    Result := False;
  end;
end;

{ Debug string representation }

function SouffleValueToString(const AValue: TSouffleValue): string;
begin
  case AValue.Kind of
    svkNil:
      Result := 'nil';
    svkBoolean:
      if AValue.AsBoolean then
        Result := 'true'
      else
        Result := 'false';
    svkInteger:
      Result := IntToStr(AValue.AsInteger);
    svkFloat:
      if IsNaN(AValue.AsFloat) then
        Result := 'NaN'
      else if IsInfinite(AValue.AsFloat) then
      begin
        if AValue.AsFloat > 0 then
          Result := 'Infinity'
        else
          Result := '-Infinity';
      end
      else if (Frac(AValue.AsFloat) = 0.0) and (Abs(AValue.AsFloat) < 1e20) then
        Result := FloatToStrF(AValue.AsFloat, ffFixed, 20, 0)
      else
        Result := FloatToStr(AValue.AsFloat);
    svkString:
      Result := AValue.AsInlineString;
    svkReference:
      if AValue.AsReference is TSouffleHeapString then
        Result := TSouffleHeapString(AValue.AsReference).Value
      else if Assigned(AValue.AsReference) then
        Result := AValue.AsReference.DebugString
      else
        Result := 'nil';
  else
    Result := '<unknown>';
  end;
end;

end.

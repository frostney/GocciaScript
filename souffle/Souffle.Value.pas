unit Souffle.Value;

{$I Souffle.inc}

interface

uses
  Souffle.Heap;

type
  TSouffleValueKind = (
    svkNil,
    svkBoolean,
    svkInteger,
    svkFloat,
    svkReference
  );

  PSouffleValue = ^TSouffleValue;

  TSouffleValue = record
    Kind: TSouffleValueKind;
    case TSouffleValueKind of
      svkNil:       ();
      svkBoolean:   (AsBoolean: Boolean);
      svkInteger:   (AsInteger: Int64);
      svkFloat:     (AsFloat: Double);
      svkReference: (AsReference: TSouffleHeapObject);
  end;

  TSouffleValueArray = array of TSouffleValue;
  PSouffleValueArray = ^TSouffleValueArray;

function SouffleNil: TSouffleValue; inline;
function SouffleBoolean(const AValue: Boolean): TSouffleValue; inline;
function SouffleInteger(const AValue: Int64): TSouffleValue; inline;
function SouffleFloat(const AValue: Double): TSouffleValue; inline;
function SouffleReference(const AObject: TSouffleHeapObject): TSouffleValue; inline;

function SouffleIsNil(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsBoolean(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsInteger(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsFloat(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsReference(const AValue: TSouffleValue): Boolean; inline;
function SouffleIsNumeric(const AValue: TSouffleValue): Boolean; inline;

function SouffleIsTrue(const AValue: TSouffleValue): Boolean; inline;
function SouffleAsNumber(const AValue: TSouffleValue): Double; inline;

function SouffleValuesEqual(const A, B: TSouffleValue): Boolean;
function SouffleValueToString(const AValue: TSouffleValue): string;

implementation

uses
  Math,
  SysUtils;

{ Value constructors }

function SouffleNil: TSouffleValue;
begin
  Result.Kind := svkNil;
  Result.AsInteger := 0;
end;

function SouffleBoolean(const AValue: Boolean): TSouffleValue;
begin
  Result.Kind := svkBoolean;
  Result.AsInteger := 0;
  Result.AsBoolean := AValue;
end;

function SouffleInteger(const AValue: Int64): TSouffleValue;
begin
  Result.Kind := svkInteger;
  Result.AsInteger := AValue;
end;

function SouffleFloat(const AValue: Double): TSouffleValue;
begin
  Result.Kind := svkFloat;
  Result.AsFloat := AValue;
end;

function SouffleReference(const AObject: TSouffleHeapObject): TSouffleValue;
begin
  Result.Kind := svkReference;
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

function SouffleIsReference(const AValue: TSouffleValue): Boolean;
begin
  Result := AValue.Kind = svkReference;
end;

function SouffleIsNumeric(const AValue: TSouffleValue): Boolean;
begin
  Result := (AValue.Kind = svkInteger) or (AValue.Kind = svkFloat);
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
      Result := AValue.AsInteger * 1.0;
    svkFloat:
      Result := AValue.AsFloat;
  else
    Result := NaN;
  end;
end;

{ Identity equality for Tier 1 operations }

function SouffleValuesEqual(const A, B: TSouffleValue): Boolean;
begin
  if A.Kind <> B.Kind then
    Exit(False);

  case A.Kind of
    svkNil:
      Result := True;
    svkBoolean:
      Result := A.AsBoolean = B.AsBoolean;
    svkInteger:
      Result := A.AsInteger = B.AsInteger;
    svkFloat:
      Result := A.AsFloat = B.AsFloat;
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
      Result := FloatToStr(AValue.AsFloat);
    svkReference:
      if Assigned(AValue.AsReference) then
      begin
        if AValue.AsReference is TSouffleString then
          Result := TSouffleString(AValue.AsReference).Value
        else
          Result := AValue.AsReference.DebugString;
      end
      else
        Result := 'nil';
  else
    Result := '<unknown>';
  end;
end;

end.

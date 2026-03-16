unit Souffle.Value;

{$I Souffle.inc}

interface

uses
  Souffle.Heap;

const
  SOUFFLE_INLINE_STRING_MAX = 5;
  SOUFFLE_NIL_DEFAULT = 0;
  SOUFFLE_NIL_MATCH_ANY = 255;
  SOUFFLE_POOL_INITIAL_CAPACITY = 4096;

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
      svkInteger:   (AsIntSlot: UInt32);
      svkFloat:     (AsFloatSlot: UInt32);
      svkString:    (AsInlineString: TSouffleInlineString);
      svkReference: (AsRefSlot: UInt32);
  end;

  TSouffleValueArray = array of TSouffleValue;
  PSouffleValueArray = ^TSouffleValueArray;

procedure SouffleInitPools;
procedure SouffleResetPools;

function SouffleGetInt(const AValue: TSouffleValue): Int64; inline;
function SouffleGetFloat(const AValue: TSouffleValue): Double; inline;
function SouffleGetRef(const AValue: TSouffleValue): TSouffleHeapObject; inline;

function SouffleAllocInt(const AValue: Int64): UInt32; inline;
function SouffleAllocFloat(const AValue: Double): UInt32; inline;
function SouffleAllocRef(const AObject: TSouffleHeapObject): UInt32; inline;

function SouffleRefPoolCount: UInt32; inline;
function SouffleRefPoolEntry(const AIndex: UInt32): TSouffleHeapObject; inline;

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

var
  GIntPool: array of Int64;
  GIntPoolCount: UInt32;
  GFloatPool: array of Double;
  GFloatPoolCount: UInt32;
  GRefPool: array of TSouffleHeapObject;
  GRefPoolCount: UInt32;

{ Pool lifecycle }

procedure GrowIntPool;
begin
  if Length(GIntPool) = 0 then
    SetLength(GIntPool, SOUFFLE_POOL_INITIAL_CAPACITY)
  else
    SetLength(GIntPool, Length(GIntPool) * 2);
end;

procedure GrowFloatPool;
begin
  if Length(GFloatPool) = 0 then
    SetLength(GFloatPool, SOUFFLE_POOL_INITIAL_CAPACITY)
  else
    SetLength(GFloatPool, Length(GFloatPool) * 2);
end;

procedure GrowRefPool;
begin
  if Length(GRefPool) = 0 then
    SetLength(GRefPool, SOUFFLE_POOL_INITIAL_CAPACITY)
  else
    SetLength(GRefPool, Length(GRefPool) * 2);
end;

procedure SouffleInitPools;
begin
  SetLength(GIntPool, SOUFFLE_POOL_INITIAL_CAPACITY);
  GIntPoolCount := 0;
  SetLength(GFloatPool, SOUFFLE_POOL_INITIAL_CAPACITY);
  GFloatPoolCount := 0;
  SetLength(GRefPool, SOUFFLE_POOL_INITIAL_CAPACITY);
  GRefPoolCount := 0;
end;

procedure SouffleResetPools;
begin
  GIntPoolCount := 0;
  GFloatPoolCount := 0;
  GRefPoolCount := 0;
end;

{ Pool allocators }

function SouffleAllocInt(const AValue: Int64): UInt32;
begin
  Result := GIntPoolCount;
  if Result >= UInt32(Length(GIntPool)) then
    GrowIntPool;
  GIntPool[Result] := AValue;
  Inc(GIntPoolCount);
end;

function SouffleAllocFloat(const AValue: Double): UInt32;
begin
  Result := GFloatPoolCount;
  if Result >= UInt32(Length(GFloatPool)) then
    GrowFloatPool;
  GFloatPool[Result] := AValue;
  Inc(GFloatPoolCount);
end;

function SouffleAllocRef(const AObject: TSouffleHeapObject): UInt32;
begin
  Result := GRefPoolCount;
  if Result >= UInt32(Length(GRefPool)) then
    GrowRefPool;
  GRefPool[Result] := AObject;
  Inc(GRefPoolCount);
end;

{ Pool accessors }

function SouffleGetInt(const AValue: TSouffleValue): Int64;
begin
  Result := GIntPool[AValue.AsIntSlot];
end;

function SouffleGetFloat(const AValue: TSouffleValue): Double;
begin
  Result := GFloatPool[AValue.AsFloatSlot];
end;

function SouffleGetRef(const AValue: TSouffleValue): TSouffleHeapObject;
begin
  Result := GRefPool[AValue.AsRefSlot];
end;

function SouffleRefPoolCount: UInt32;
begin
  Result := GRefPoolCount;
end;

function SouffleRefPoolEntry(const AIndex: UInt32): TSouffleHeapObject;
begin
  Result := GRefPool[AIndex];
end;

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
  Result.AsIntSlot := SouffleAllocInt(AValue);
end;

function SouffleFloat(const AValue: Double): TSouffleValue;
begin
  Result.Kind := svkFloat;
  Result.Flags := 0;
  Result.AsFloatSlot := SouffleAllocFloat(AValue);
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
    Result.AsRefSlot := SouffleAllocRef(HeapStr);
  end;
end;

function SouffleReference(const AObject: TSouffleHeapObject): TSouffleValue;
begin
  Result.Kind := svkReference;
  Result.Flags := 0;
  Result.AsRefSlot := SouffleAllocRef(AObject);
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
    ((AValue.Kind = svkReference) and
     (GRefPool[AValue.AsRefSlot] is TSouffleHeapString));
end;

{ Truthiness -- universal semantics used by JUMP_IF_TRUE/JUMP_IF_FALSE }

function SouffleIsTrue(const AValue: TSouffleValue): Boolean;
var
  F: Double;
begin
  case AValue.Kind of
    svkNil:
      Result := False;
    svkBoolean:
      Result := AValue.AsBoolean;
    svkInteger:
      Result := GIntPool[AValue.AsIntSlot] <> 0;
    svkFloat:
      begin
        F := GFloatPool[AValue.AsFloatSlot];
        Result := (F <> 0.0) and not IsNaN(F);
      end;
    svkString:
      Result := AValue.AsInlineString <> '';
    svkReference:
      Result := Assigned(GRefPool[AValue.AsRefSlot]);
  else
    Result := False;
  end;
end;

{ Numeric coercion for VM-level arithmetic fast paths }

function SouffleAsNumber(const AValue: TSouffleValue): Double;
begin
  case AValue.Kind of
    svkInteger:
      Result := GIntPool[AValue.AsIntSlot];
    svkFloat:
      Result := GFloatPool[AValue.AsFloatSlot];
  else
    Result := NaN;
  end;
end;

function SouffleToDouble(const AValue: TSouffleValue): Double;
begin
  if AValue.Kind = svkFloat then
    Result := GFloatPool[AValue.AsFloatSlot]
  else if AValue.Kind = svkInteger then
    Result := GIntPool[AValue.AsIntSlot] * 1.0
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
var
  Ref: TSouffleHeapObject;
begin
  if AValue.Kind = svkString then
    Result := AValue.AsInlineString
  else if AValue.Kind = svkReference then
  begin
    Ref := GRefPool[AValue.AsRefSlot];
    if Ref is TSouffleHeapString then
      Result := TSouffleHeapString(Ref).Value
    else
      Result := '';
  end
  else
    Result := '';
end;

{ Identity equality for core operations }

function SouffleValuesEqual(const A, B: TSouffleValue): Boolean;
var
  RefA, RefB: TSouffleHeapObject;
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
      Result := GIntPool[A.AsIntSlot] = GIntPool[B.AsIntSlot];
    svkFloat:
      Result := GFloatPool[A.AsFloatSlot] = GFloatPool[B.AsFloatSlot];
    svkString:
      Result := A.AsInlineString = B.AsInlineString;
    svkReference:
      begin
        RefA := GRefPool[A.AsRefSlot];
        RefB := GRefPool[B.AsRefSlot];
        if (RefA is TSouffleHeapString) and (RefB is TSouffleHeapString) then
          Result := TSouffleHeapString(RefA).Value =
            TSouffleHeapString(RefB).Value
        else
          Result := RefA = RefB;
      end;
  else
    Result := False;
  end;
end;

{ Debug string representation }

function SouffleValueToString(const AValue: TSouffleValue): string;
var
  F: Double;
  Ref: TSouffleHeapObject;
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
      Result := IntToStr(GIntPool[AValue.AsIntSlot]);
    svkFloat:
      begin
        F := GFloatPool[AValue.AsFloatSlot];
        if IsNaN(F) then
          Result := 'NaN'
        else if IsInfinite(F) then
        begin
          if F > 0 then
            Result := 'Infinity'
          else
            Result := '-Infinity';
        end
        else if (Frac(F) = 0.0) and (Abs(F) < 1e20) then
          Result := FloatToStrF(F, ffFixed, 20, 0)
        else
          Result := FloatToStr(F);
      end;
    svkString:
      Result := AValue.AsInlineString;
    svkReference:
      begin
        Ref := GRefPool[AValue.AsRefSlot];
        if Ref is TSouffleHeapString then
          Result := TSouffleHeapString(Ref).Value
        else if Assigned(Ref) then
          Result := Ref.DebugString
        else
          Result := 'nil';
      end;
  else
    Result := '<unknown>';
  end;
end;

initialization
  SouffleInitPools;

end.

unit Souffle.VM.RuntimeOperations;

{$I Souffle.inc}

interface

uses
  Souffle.Value;

type
  TSouffleRuntimeOperations = class abstract
  public
    // Arithmetic
    function Add(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Subtract(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Multiply(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Divide(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Modulo(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Power(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function Negate(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Bitwise
    function BitwiseAnd(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseOr(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseXor(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftLeft(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function ShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function UnsignedShiftRight(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function BitwiseNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Comparison
    function Equal(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function NotEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThan(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function LessThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function GreaterThanOrEqual(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;

    // Logical / Type
    function LogicalNot(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function TypeOf(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function IsInstance(const A, B: TSouffleValue): TSouffleValue; virtual; abstract;
    function HasProperty(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToBoolean(const A: TSouffleValue): TSouffleValue; virtual; abstract;
    function ToPrimitive(const A: TSouffleValue): TSouffleValue; virtual; abstract;

    // Property access
    function GetProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; virtual; abstract;
    procedure SetProperty(const AObject: TSouffleValue; const AKey: string;
      const AValue: TSouffleValue); virtual; abstract;
    function GetIndex(const AObject, AKey: TSouffleValue): TSouffleValue; virtual; abstract;
    procedure SetIndex(const AObject: TSouffleValue;
      const AKey, AValue: TSouffleValue); virtual; abstract;
    function DeleteProperty(const AObject: TSouffleValue;
      const AKey: string): TSouffleValue; virtual; abstract;
    function DeleteIndex(const AObject, AKey: TSouffleValue): TSouffleValue; virtual;

    // Invocation
    function Invoke(const ACallee: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer; const AReceiver: TSouffleValue): TSouffleValue; virtual; abstract;
    function Construct(const AConstructor: TSouffleValue; const AArgs: PSouffleValue;
      const AArgCount: Integer): TSouffleValue; virtual; abstract;

    // Iteration
    function GetIterator(const AIterable: TSouffleValue;
      const ATryAsync: Boolean = False): TSouffleValue; virtual; abstract;
    function IteratorNext(const AIterator: TSouffleValue;
      out ADone: Boolean): TSouffleValue; virtual; abstract;
    procedure SpreadInto(const ATarget, ASource: TSouffleValue); virtual; abstract;
    procedure SpreadObjectInto(const ATarget, ASource: TSouffleValue); virtual;

    function ArrayRest(const ASource: TSouffleValue;
      const AStartIndex: Integer): TSouffleValue; virtual;
    function ObjectRest(const ASource, AExclusionKeys: TSouffleValue): TSouffleValue; virtual;
    procedure RequireObjectCoercible(const AValue: TSouffleValue); virtual;
    procedure RequireIterable(const AValue: TSouffleValue); virtual;
    function CoerceValueToString(const A: TSouffleValue): TSouffleValue; virtual;

    // Modules
    function ImportModule(const APath: string): TSouffleValue; virtual; abstract;
    procedure ExportBinding(const AValue: TSouffleValue;
      const AName: string); virtual; abstract;

    // Async
    function AwaitValue(const AValue: TSouffleValue): TSouffleValue; virtual; abstract;
    function WrapInPromise(const AValue: TSouffleValue;
      const AIsRejected: Boolean): TSouffleValue; virtual;

    // Globals
    function GetGlobal(const AName: string): TSouffleValue; virtual; abstract;
    procedure SetGlobal(const AName: string;
      const AValue: TSouffleValue); virtual; abstract;
    function HasGlobal(const AName: string): Boolean; virtual; abstract;

    // Accessor properties
    procedure DefineGetter(const AObject: TSouffleValue; const AKey: string;
      const AGetter: TSouffleValue); virtual;
    procedure DefineSetter(const AObject: TSouffleValue; const AKey: string;
      const ASetter: TSouffleValue); virtual;

  end;

implementation

uses
  Souffle.Compound;

procedure TSouffleRuntimeOperations.DefineGetter(const AObject: TSouffleValue;
  const AKey: string; const AGetter: TSouffleValue);
begin
end;

procedure TSouffleRuntimeOperations.DefineSetter(const AObject: TSouffleValue;
  const AKey: string; const ASetter: TSouffleValue);
begin
end;

function TSouffleRuntimeOperations.DeleteIndex(
  const AObject, AKey: TSouffleValue): TSouffleValue;
begin
  Result := DeleteProperty(AObject, SouffleValueToString(AKey));
end;

procedure TSouffleRuntimeOperations.SpreadObjectInto(
  const ATarget, ASource: TSouffleValue);
var
  SrcRec, TgtRec: TSouffleRecord;
  I: Integer;
begin
  if not (SouffleIsReference(ATarget) and (ATarget.AsReference is TSouffleRecord)) then
    Exit;
  if not (SouffleIsReference(ASource) and (ASource.AsReference is TSouffleRecord)) then
    Exit;
  TgtRec := TSouffleRecord(ATarget.AsReference);
  SrcRec := TSouffleRecord(ASource.AsReference);
  for I := 0 to SrcRec.Count - 1 do
    TgtRec.Put(SrcRec.GetOrderedKey(I), SrcRec.GetOrderedValue(I));
end;

function TSouffleRuntimeOperations.ArrayRest(const ASource: TSouffleValue;
  const AStartIndex: Integer): TSouffleValue;
var
  SrcArr, RestArr: TSouffleArray;
  I: Integer;
begin
  if SouffleIsReference(ASource) and (ASource.AsReference is TSouffleArray) then
  begin
    SrcArr := TSouffleArray(ASource.AsReference);
    RestArr := TSouffleArray.Create(SrcArr.Count - AStartIndex);
    for I := AStartIndex to SrcArr.Count - 1 do
      RestArr.Push(SrcArr.Get(I));
    Result := SouffleReference(RestArr);
  end
  else
    Result := SouffleNil;
end;

function TSouffleRuntimeOperations.ObjectRest(
  const ASource, AExclusionKeys: TSouffleValue): TSouffleValue;
var
  SrcRec, RestRec: TSouffleRecord;
  ExclArr: TSouffleArray;
  I, J: Integer;
  Key: string;
  Excluded: Boolean;
begin
  if not (SouffleIsReference(ASource) and (ASource.AsReference is TSouffleRecord)) then
    Exit(SouffleNil);
  SrcRec := TSouffleRecord(ASource.AsReference);
  RestRec := TSouffleRecord.Create;
  if SouffleIsReference(AExclusionKeys) and
     (AExclusionKeys.AsReference is TSouffleArray) then
  begin
    ExclArr := TSouffleArray(AExclusionKeys.AsReference);
    for I := 0 to SrcRec.Count - 1 do
    begin
      Key := SrcRec.GetOrderedKey(I);
      Excluded := False;
      for J := 0 to ExclArr.Count - 1 do
        if SouffleValueToString(ExclArr.Get(J)) = Key then
        begin
          Excluded := True;
          Break;
        end;
      if not Excluded then
        RestRec.Put(Key, SrcRec.GetOrderedValue(I));
    end;
  end;
  Result := SouffleReference(RestRec);
end;

procedure TSouffleRuntimeOperations.RequireObjectCoercible(
  const AValue: TSouffleValue);
begin
end;

procedure TSouffleRuntimeOperations.RequireIterable(
  const AValue: TSouffleValue);
begin
end;

function TSouffleRuntimeOperations.CoerceValueToString(
  const A: TSouffleValue): TSouffleValue;
begin
  Result := SouffleNil;
end;

function TSouffleRuntimeOperations.WrapInPromise(const AValue: TSouffleValue;
  const AIsRejected: Boolean): TSouffleValue;
begin
  Result := AValue;
end;

end.

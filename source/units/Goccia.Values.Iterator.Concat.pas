unit Goccia.Values.Iterator.Concat;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaConcatIterableRecord = record
    Iterable: TGocciaValue;
    IteratorMethod: TGocciaValue; // nil for string iterables (handled specially)
  end;

  TGocciaConcatIteratorValue = class(TGocciaIteratorHelperValue)
  private
    FIterables: array of TGocciaConcatIterableRecord;
    FCurrentIndex: Integer;
    FCurrentIterator: TGocciaIteratorValue;
    function OpenNextIterator: Boolean;
  protected
    function DoAdvanceNext: TGocciaObjectValue; override;
    function DoDirectNext(out ADone: Boolean): TGocciaValue; override;
  public
    constructor Create(const AIterables: array of TGocciaConcatIterableRecord);
    procedure Close; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic;

{ TGocciaConcatIteratorValue }

constructor TGocciaConcatIteratorValue.Create(const AIterables: array of TGocciaConcatIterableRecord);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FIterables, Length(AIterables));
  for I := 0 to High(AIterables) do
    FIterables[I] := AIterables[I];
  FCurrentIndex := 0;
  FCurrentIterator := nil;
end;

// TC39 Iterator Sequencing §1 Iterator.concat ( ...items ) — steps 3.a.i-iii
function TGocciaConcatIteratorValue.OpenNextIterator: Boolean;
var
  CallArgs: TGocciaArgumentsCollection;
  IteratorObj: TGocciaValue;
begin
  Result := False;
  if FCurrentIndex >= Length(FIterables) then
    Exit;

  if not Assigned(FIterables[FCurrentIndex].IteratorMethod) then
  begin
    // String iterable — create string iterator directly
    FCurrentIterator := TGocciaStringIteratorValue.Create(FIterables[FCurrentIndex].Iterable);
  end
  else
  begin
    // TC39 Iterator Sequencing §1 step 3.a.i: Call(method, iterable)
    CallArgs := TGocciaArgumentsCollection.Create;
    try
      IteratorObj := TGocciaFunctionBase(FIterables[FCurrentIndex].IteratorMethod).Call(
        CallArgs, FIterables[FCurrentIndex].Iterable);
    finally
      CallArgs.Free;
    end;

    // TC39 Iterator Sequencing §1 step 3.a.ii: If iter is not an Object, throw TypeError
    if not (IteratorObj is TGocciaObjectValue) then
      ThrowTypeError(SErrorIteratorConcatMustReturnObject, SSuggestIteratorProtocol);

    if IteratorObj is TGocciaIteratorValue then
      FCurrentIterator := TGocciaIteratorValue(IteratorObj)
    else
      FCurrentIterator := CreateRootedGenericIterator(IteratorObj);
  end;

  Inc(FCurrentIndex);
  Result := True;
end;

// TC39 Iterator Sequencing §1 Iterator.concat ( ...items ) — step 3.a.iv-v
function TGocciaConcatIteratorValue.DoAdvanceNext: TGocciaObjectValue;
var
  InnerResult: TGocciaObjectValue;
begin
  repeat
    if Assigned(FCurrentIterator) then
    begin
      try
        InnerResult := FCurrentIterator.AdvanceNext;
      except
        AcquireExceptionObject;
        CloseIteratorPreservingError(FCurrentIterator);
        FCurrentIterator := nil;
        FDone := True;
        raise;
      end;
      if not TGocciaBooleanLiteralValue(InnerResult.GetProperty(PROP_DONE)).Value then
      begin
        Result := CreateIteratorResult(InnerResult.GetProperty(PROP_VALUE), False);
        Exit;
      end;
      FCurrentIterator := nil;
    end;

    try
      if not OpenNextIterator then
      begin
        FDone := True;
        Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
        Exit;
      end;
    except
      FDone := True;
      raise;
    end;
  until False;
end;

// TC39 Iterator Sequencing §1 Iterator.concat ( ...items ) — step 3.a.iv-v (DoDirectNext)
function TGocciaConcatIteratorValue.DoDirectNext(out ADone: Boolean): TGocciaValue;
var
  InnerDone: Boolean;
  InnerValue: TGocciaValue;
begin
  repeat
    if Assigned(FCurrentIterator) then
    begin
      try
        InnerValue := FCurrentIterator.DirectNext(InnerDone);
      except
        AcquireExceptionObject;
        CloseIteratorPreservingError(FCurrentIterator);
        FCurrentIterator := nil;
        FDone := True;
        raise;
      end;
      if not InnerDone then
      begin
        ADone := False;
        Result := InnerValue;
        Exit;
      end;
      FCurrentIterator := nil;
    end;

    try
      if not OpenNextIterator then
      begin
        FDone := True;
        ADone := True;
        Result := TGocciaUndefinedLiteralValue.UndefinedValue;
        Exit;
      end;
    except
      FDone := True;
      raise;
    end;
  until False;
end;

procedure TGocciaConcatIteratorValue.Close;
begin
  inherited;
  if Assigned(FCurrentIterator) then
  begin
    FCurrentIterator.Close;
    FCurrentIterator := nil;
  end;
end;

procedure TGocciaConcatIteratorValue.MarkReferences;
var
  I: Integer;
begin
  if GCMarked then Exit;
  inherited;
  for I := 0 to High(FIterables) do
  begin
    if Assigned(FIterables[I].Iterable) then
      FIterables[I].Iterable.MarkReferences;
    if Assigned(FIterables[I].IteratorMethod) then
      FIterables[I].IteratorMethod.MarkReferences;
  end;
  if Assigned(FCurrentIterator) then
    FCurrentIterator.MarkReferences;
end;

end.

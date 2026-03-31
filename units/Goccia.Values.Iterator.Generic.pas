unit Goccia.Values.Iterator.Generic;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaGenericIteratorValue = class(TGocciaIteratorValue)
  private
    FSource: TGocciaValue;
  public
    constructor Create(const AIteratorObject: TGocciaValue);
    function AdvanceNext: TGocciaObjectValue; override;
    function DirectNext(out ADone: Boolean): TGocciaValue; override;
    procedure Close; override;
    procedure MarkReferences; override;
  end;

implementation

uses
  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Values.FunctionBase;

{ TGocciaGenericIteratorValue }

constructor TGocciaGenericIteratorValue.Create(const AIteratorObject: TGocciaValue);
begin
  inherited Create;
  FSource := AIteratorObject;
end;

function TGocciaGenericIteratorValue.AdvanceNext: TGocciaObjectValue;
var
  NextMethod, NextResult, DoneVal, ValueVal: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if FDone then
  begin
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  NextMethod := FSource.GetProperty(PROP_NEXT);
  if not Assigned(NextMethod) or not NextMethod.IsCallable then
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
    Exit;
  end;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    NextResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, FSource);
  finally
    CallArgs.Free;
  end;

  if NextResult is TGocciaObjectValue then
  begin
    DoneVal := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
    if Assigned(DoneVal) and DoneVal.ToBooleanLiteral.Value then
    begin
      FDone := True;
      ValueVal := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
      if not Assigned(ValueVal) then
        ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue;
      Result := CreateIteratorResult(ValueVal, True);
    end
    else
    begin
      ValueVal := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
      if not Assigned(ValueVal) then
        ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue;
      Result := CreateIteratorResult(ValueVal, False);
    end;
  end
  else
  begin
    FDone := True;
    Result := CreateIteratorResult(TGocciaUndefinedLiteralValue.UndefinedValue, True);
  end;
end;

function TGocciaGenericIteratorValue.DirectNext(out ADone: Boolean): TGocciaValue;
var
  NextMethod, NextResult, DoneVal, ValueVal: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if FDone then
  begin
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  NextMethod := FSource.GetProperty(PROP_NEXT);
  if not Assigned(NextMethod) or not NextMethod.IsCallable then
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
    Exit;
  end;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    NextResult := TGocciaFunctionBase(NextMethod).Call(CallArgs, FSource);
  finally
    CallArgs.Free;
  end;

  if NextResult is TGocciaObjectValue then
  begin
    DoneVal := TGocciaObjectValue(NextResult).GetProperty(PROP_DONE);
    if Assigned(DoneVal) and DoneVal.ToBooleanLiteral.Value then
    begin
      FDone := True;
      ADone := True;
      ValueVal := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
      if not Assigned(ValueVal) then
        ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue;
      Result := ValueVal;
    end
    else
    begin
      ADone := False;
      ValueVal := TGocciaObjectValue(NextResult).GetProperty(PROP_VALUE);
      if not Assigned(ValueVal) then
        ValueVal := TGocciaUndefinedLiteralValue.UndefinedValue;
      Result := ValueVal;
    end;
  end
  else
  begin
    FDone := True;
    ADone := True;
    Result := TGocciaUndefinedLiteralValue.UndefinedValue;
  end;
end;

procedure TGocciaGenericIteratorValue.Close;
var
  ReturnMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
begin
  if FDone then Exit;

  FDone := True;
  ReturnMethod := FSource.GetProperty(PROP_RETURN);
  if not Assigned(ReturnMethod) or (ReturnMethod is TGocciaUndefinedLiteralValue) or not ReturnMethod.IsCallable then
    Exit;

  CallArgs := TGocciaArgumentsCollection.Create;
  try
    TGocciaFunctionBase(ReturnMethod).Call(CallArgs, FSource);
  finally
    CallArgs.Free;
  end;
end;

procedure TGocciaGenericIteratorValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  if Assigned(FSource) then
    FSource.MarkReferences;
end;


end.

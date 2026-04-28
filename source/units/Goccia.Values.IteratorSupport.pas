unit Goccia.Values.IteratorSupport;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.Primitives;

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorMethod, IteratorObj, NextMethod: TGocciaValue;
  CallArgs: TGocciaArgumentsCollection;
  WasSourceRooted, WasMethodRooted, WasIteratorRooted: Boolean;
  GC: TGarbageCollector;

  function AddRootIfNeeded(const AValue: TGocciaValue): Boolean;
  begin
    Result := Assigned(GC) and Assigned(AValue) and not GC.IsTempRoot(AValue);
    if Result then
      GC.AddTempRoot(AValue);
  end;

  procedure RemoveRootIfNeeded(const AValue: TGocciaValue; const AWasAdded: Boolean);
  begin
    if AWasAdded then
      GC.RemoveTempRoot(AValue);
  end;

begin
  GC := TGarbageCollector.Instance;

  if AValue is TGocciaIteratorValue then
  begin
    Result := TGocciaIteratorValue(AValue);
    Exit;
  end;

  if AValue is TGocciaObjectValue then
  begin
    IteratorMethod := TGocciaObjectValue(AValue).GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
    if Assigned(IteratorMethod) and not (IteratorMethod is TGocciaUndefinedLiteralValue) then
    begin
      if not IteratorMethod.IsCallable then
        ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']), SSuggestIteratorProtocol);

      WasSourceRooted := AddRootIfNeeded(AValue);
      WasMethodRooted := AddRootIfNeeded(IteratorMethod);
      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, AValue);
        finally
          CallArgs.Free;
        end;
      finally
        RemoveRootIfNeeded(IteratorMethod, WasMethodRooted);
        RemoveRootIfNeeded(AValue, WasSourceRooted);
      end;

      if IteratorObj is TGocciaIteratorValue then
      begin
        Result := TGocciaIteratorValue(IteratorObj);
        Exit;
      end;

      if not (IteratorObj is TGocciaObjectValue) then
        ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);

      WasIteratorRooted := AddRootIfNeeded(IteratorObj);
      try
        NextMethod := TGocciaObjectValue(IteratorObj).GetProperty(PROP_NEXT);
        if not Assigned(NextMethod) or (NextMethod is TGocciaUndefinedLiteralValue) or
           not NextMethod.IsCallable then
          ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);

        Result := TGocciaGenericIteratorValue.Create(IteratorObj);
      finally
        RemoveRootIfNeeded(IteratorObj, WasIteratorRooted);
      end;
      Exit;
    end;
  end;

  if AValue is TGocciaStringLiteralValue then
  begin
    WasSourceRooted := AddRootIfNeeded(AValue);
    try
      Result := TGocciaStringIteratorValue.Create(AValue);
    finally
      RemoveRootIfNeeded(AValue, WasSourceRooted);
    end;
    Exit;
  end;

  Result := nil;
end;

procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);
begin
  if not Assigned(AIterator) then
    Exit;
  try
    AIterator.Close;
  except
    // Iterator close errors must not replace the original abrupt completion.
  end;
end;

end.

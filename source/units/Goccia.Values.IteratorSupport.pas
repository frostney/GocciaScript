unit Goccia.Values.IteratorSupport;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.Primitives;

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
// CloseIterator is the normal-completion variant per ES2024 §7.4.10
// IteratorClose: errors from iter.return() propagate to the caller.  Use
// this on success paths.  CloseIteratorPreservingError below swallows
// errors so an existing abrupt completion is not replaced — use that one
// only on the abrupt-completion cleanup path.
procedure CloseIterator(const AIterator: TGocciaIteratorValue);
procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);

implementation

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.FunctionBase,
  Goccia.Values.Iterator.Concrete,
  Goccia.Values.Iterator.Generic,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.ToObject;

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
var
  IteratorHost: TGocciaObjectValue;
  IteratorReceiver: TGocciaValue;
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

  IteratorHost := nil;
  IteratorReceiver := AValue;
  if AValue is TGocciaObjectValue then
    IteratorHost := TGocciaObjectValue(AValue)
  else if not (AValue is TGocciaNullLiteralValue) and
          not (AValue is TGocciaUndefinedLiteralValue) then
    IteratorHost := ToObject(AValue);

  if Assigned(IteratorHost) then
  begin
    WasSourceRooted := AddRootIfNeeded(IteratorHost);
    try
      IteratorMethod := IteratorHost.GetSymbolPropertyWithReceiver(
        TGocciaSymbolValue.WellKnownIterator, IteratorReceiver);
      if Assigned(IteratorMethod) and
         not (IteratorMethod is TGocciaUndefinedLiteralValue) and
         not (IteratorMethod is TGocciaNullLiteralValue) then
      begin
        if not IteratorMethod.IsCallable then
          ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']), SSuggestIteratorProtocol);

        WasMethodRooted := AddRootIfNeeded(IteratorMethod);
        try
          CallArgs := TGocciaArgumentsCollection.Create;
          try
            IteratorObj := InvokeCallable(IteratorMethod, CallArgs, IteratorReceiver);
          finally
            CallArgs.Free;
          end;
        finally
          RemoveRootIfNeeded(IteratorMethod, WasMethodRooted);
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
          // ES2026 §7.4.2 GetIteratorDirect captures "next" without
          // validating callability; IteratorStep/IteratorNext reports that
          // error later, after destructuring assignment targets run.
          Result := CreateRootedGenericIterator(IteratorObj, NextMethod);
        finally
          RemoveRootIfNeeded(IteratorObj, WasIteratorRooted);
        end;
        Exit;
      end;
    finally
      RemoveRootIfNeeded(IteratorHost, WasSourceRooted);
    end;
  end;

  Result := nil;
end;

procedure CloseIterator(const AIterator: TGocciaIteratorValue);
begin
  if not Assigned(AIterator) then
    Exit;
  AIterator.Close;
end;

procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);
begin
  Goccia.Values.IteratorValue.CloseIteratorPreservingError(AIterator);
end;

end.

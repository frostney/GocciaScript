unit Goccia.Values.IteratorSupport;

{$I Goccia.inc}

interface

uses
  Goccia.Values.IteratorValue,
  Goccia.Values.Primitives;

type
  TGocciaIteratorPrimitiveHandling = (
    iphRejectPrimitives,
    iphIterateStringPrimitives
  );

function GetIteratorFromValue(const AValue: TGocciaValue): TGocciaIteratorValue;
function GetIteratorFlattenable(
  const AValue: TGocciaValue;
  const APrimitiveHandling: TGocciaIteratorPrimitiveHandling): TGocciaIteratorValue;
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

function GetIteratorFlattenable(
  const AValue: TGocciaValue;
  const APrimitiveHandling: TGocciaIteratorPrimitiveHandling): TGocciaIteratorValue;
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

  if AValue is TGocciaIteratorValue then
    Exit(TGocciaIteratorValue(AValue));

  if AValue is TGocciaObjectValue then
  begin
    IteratorHost := TGocciaObjectValue(AValue);
    IteratorReceiver := AValue;
  end
  else if (APrimitiveHandling = iphIterateStringPrimitives) and
          (AValue is TGocciaStringLiteralValue) then
  begin
    IteratorHost := ToObject(AValue);
    IteratorReceiver := AValue;
  end
  else
    ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);

  WasSourceRooted := AddRootIfNeeded(IteratorHost);
  try
    IteratorMethod := IteratorHost.GetSymbolPropertyWithReceiver(
      TGocciaSymbolValue.WellKnownIterator, IteratorReceiver);
    if Assigned(IteratorMethod) and
       not (IteratorMethod is TGocciaUndefinedLiteralValue) and
       not (IteratorMethod is TGocciaNullLiteralValue) then
    begin
      if not IteratorMethod.IsCallable then
        ThrowTypeError(Format(SErrorValueNotFunction, ['[Symbol.iterator]']),
          SSuggestIteratorProtocol);

      WasMethodRooted := AddRootIfNeeded(IteratorMethod);
      try
        CallArgs := TGocciaArgumentsCollection.Create;
        try
          IteratorObj := InvokeCallable(IteratorMethod, CallArgs,
            IteratorReceiver);
        finally
          CallArgs.Free;
        end;
      finally
        RemoveRootIfNeeded(IteratorMethod, WasMethodRooted);
      end;

      if not (IteratorObj is TGocciaObjectValue) then
        ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);

      if IteratorObj is TGocciaIteratorValue then
        Exit(TGocciaIteratorValue(IteratorObj));

      WasIteratorRooted := AddRootIfNeeded(IteratorObj);
      try
        NextMethod := TGocciaObjectValue(IteratorObj).GetProperty(PROP_NEXT);
        Result := CreateRootedGenericIterator(IteratorObj, NextMethod);
      finally
        RemoveRootIfNeeded(IteratorObj, WasIteratorRooted);
      end;
      Exit;
    end;

    NextMethod := IteratorHost.GetProperty(PROP_NEXT);
    Exit(CreateRootedGenericIterator(IteratorHost, NextMethod));
  finally
    RemoveRootIfNeeded(IteratorHost, WasSourceRooted);
  end;

  ThrowTypeError(SErrorIteratorInvalid, SSuggestIteratorProtocol);
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

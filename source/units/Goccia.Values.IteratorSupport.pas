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

  IteratorHost := nil;
  if AValue is TGocciaObjectValue then
    IteratorHost := TGocciaObjectValue(AValue)
  else if not (AValue is TGocciaNullLiteralValue) and
          not (AValue is TGocciaUndefinedLiteralValue) then
    IteratorHost := ToObject(AValue);

  if Assigned(IteratorHost) then
  begin
    WasSourceRooted := AddRootIfNeeded(IteratorHost);
    try
      IteratorMethod := IteratorHost.GetSymbolProperty(TGocciaSymbolValue.WellKnownIterator);
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
            IteratorObj := TGocciaFunctionBase(IteratorMethod).Call(CallArgs, IteratorHost);
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
          Result := TGocciaGenericIteratorValue.Create(IteratorObj, NextMethod);
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
var
  Root: TGocciaTempRoot;
begin
  if not Assigned(AIterator) then
    Exit;
  InitializeTempRoot(Root);
  AddTempRootIfNeeded(Root, AIterator);
  try
    AIterator.Close;
  finally
    RemoveTempRootIfNeeded(Root);
  end;
end;

procedure CloseIteratorPreservingError(const AIterator: TGocciaIteratorValue);
begin
  Goccia.Values.IteratorValue.CloseIteratorPreservingError(AIterator);
end;

end.

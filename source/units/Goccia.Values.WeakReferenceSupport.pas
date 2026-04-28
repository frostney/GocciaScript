unit Goccia.Values.WeakReferenceSupport;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

function CanBeHeldWeakly(const AValue: TGocciaValue): Boolean;
procedure RequireCanBeHeldWeakly(const AValue: TGocciaValue;
  const AMethodName: string);

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.SymbolValue;

function CanBeHeldWeakly(const AValue: TGocciaValue): Boolean;
begin
  Result :=
    (AValue is TGocciaObjectValue) or
    ((AValue is TGocciaSymbolValue) and
      not TGocciaSymbolValue(AValue).Registered);
end;

procedure RequireCanBeHeldWeakly(const AValue: TGocciaValue;
  const AMethodName: string);
begin
  if not CanBeHeldWeakly(AValue) then
    ThrowTypeError(Format(SErrorWeakCollectionInvalidKey, [AMethodName]),
      SSuggestWeakCollectionKey);
end;

end.

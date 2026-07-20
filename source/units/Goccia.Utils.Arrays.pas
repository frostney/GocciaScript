unit Goccia.Utils.Arrays;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// ES2026 §7.3.6 CreateDataPropertyOrThrow(O, P, V) for array indices.
procedure ArrayCreateDataProperty(const AObject: TGocciaObjectValue; const AIndex: Integer; const AValue: TGocciaValue); {$IFDEF FPC}inline;{$ENDIF}

implementation

uses
  SysUtils,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ErrorHelper;

procedure ArrayCreateDataProperty(const AObject: TGocciaObjectValue; const AIndex: Integer; const AValue: TGocciaValue);
begin
  if AIndex < 0 then
    ThrowRangeError(Format(SErrorInvalidArrayIndexFmt, [AIndex]), SSuggestArrayLengthRange);
  AObject.CreateDataPropertyOrThrow(IntToStr(AIndex), AValue);
end;

end.

unit Goccia.Utils.Array;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ArrayValue,
  Goccia.Values.Primitives;

// ES2026 §7.3.5 CreateDataPropertyOrThrow(A, k, value) for arrays.
// Handles both pre-allocated arrays (from Construct) and empty arrays.
procedure ArrayCreateDataProperty(const AArray: TGocciaArrayValue; const AIndex: Integer; const AValue: TGocciaValue); inline;

implementation

procedure ArrayCreateDataProperty(const AArray: TGocciaArrayValue; const AIndex: Integer; const AValue: TGocciaValue);
begin
  if AIndex < AArray.Elements.Count then
    AArray.Elements[AIndex] := AValue
  else
  begin
    while AArray.Elements.Count < AIndex do
      AArray.Elements.Add(nil);
    AArray.Elements.Add(AValue);
  end;
end;

end.

unit Goccia.Values.HoleValue;

{$I Goccia.inc}

interface

uses
  Goccia.Values.Primitives;

type
  TGocciaHoleValue = class(TGocciaValue)
  private
    class var FHoleValue: TGocciaHoleValue;
  public
    class function HoleValue: TGocciaHoleValue;

    function IsPrimitive: Boolean; override;
    function TypeName: string; override;
    function TypeOf: string; override;
    function RuntimeCopy: TGocciaValue; override;

    function ToBooleanLiteral: TGocciaBooleanLiteralValue; override;
    function ToNumberLiteral: TGocciaNumberLiteralValue; override;
    function ToStringLiteral: TGocciaStringLiteralValue; override;
  end;

// Dense hole-fill: extend AElements with holes until it holds ACount
// entries.  A single huge index (e.g. `x[2 ** 24] = v`) fills millions of
// slots in one native call, so the loop mask-polls the cooperative timeout
// to keep the engine deadline reachable.  If the deadline (or any other
// exception) fires mid-fill, the list is truncated back to its pre-fill
// count before re-raising, so callers never observe a partially extended
// list — extension stays atomic with respect to JS-visible state.
procedure ExtendElementsWithHoles(const AElements: TGocciaValueList;
  const ACount: Int64);

implementation

uses
  Goccia.Constants.TypeNames,
  Goccia.Timeout;

procedure ExtendElementsWithHoles(const AElements: TGocciaValueList;
  const ACount: Int64);
var
  StartCount: Integer;
begin
  StartCount := AElements.Count;
  try
    while AElements.Count < ACount do
    begin
      AElements.Add(TGocciaHoleValue.HoleValue);
      if (AElements.Count and 1023) = 0 then
        CheckExecutionTimeout;
    end;
  except
    AElements.Count := StartCount;
    raise;
  end;
end;

class function TGocciaHoleValue.HoleValue: TGocciaHoleValue;
begin
  if not Assigned(FHoleValue) then
    FHoleValue := TGocciaHoleValue.Create;
  Result := FHoleValue;
end;

function TGocciaHoleValue.IsPrimitive: Boolean;
begin
  Result := False;
end;

function TGocciaHoleValue.TypeName: string;
begin
  Result := 'hole';
end;

function TGocciaHoleValue.TypeOf: string;
begin
  Result := UNDEFINED_TYPE_NAME;
end;

function TGocciaHoleValue.RuntimeCopy: TGocciaValue;
begin
  Result := HoleValue;
end;

function TGocciaHoleValue.ToBooleanLiteral: TGocciaBooleanLiteralValue;
begin
  Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaHoleValue.ToNumberLiteral: TGocciaNumberLiteralValue;
begin
  Result := TGocciaNumberLiteralValue.NaNValue;
end;

function TGocciaHoleValue.ToStringLiteral: TGocciaStringLiteralValue;
begin
  Result := TGocciaStringLiteralValue.Create('');
end;

end.

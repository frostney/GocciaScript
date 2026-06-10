unit Goccia.Values.RawJSON;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  // ES2026 §25.5.2.2 step 4a: Represents a raw JSON value created by JSON.rawJSON().
  // The [[IsRawJSON]] internal slot is modeled by the class identity (is TGocciaRawJSONValue).
  // The object has a null prototype, a single "rawJSON" property, and is frozen.
  TGocciaRawJSONValue = class(TGocciaObjectValue)
  private
    FRawText: string;
  public
    constructor Create(const ARawText: string);

    property RawText: string read FRawText;
  end;

implementation

uses
  Goccia.Constants.PropertyNames;

constructor TGocciaRawJSONValue.Create(const ARawText: string);
begin
  // ES2026 §25.5.2.4 step 5: OrdinaryObjectCreate(null, << [[IsRawJSON]] >>)
  inherited Create(nil, 1);
  FRawText := ARawText;

  // ES2026 §25.5.2.4 step 6: CreateDataPropertyOrThrow(obj, "rawJSON", jsonString)
  CreateDataPropertyOrThrow(PROP_RAW_JSON,
    TGocciaStringLiteralValue.Create(ARawText));

  // ES2026 §25.5.2.4 step 7: SetIntegrityLevel(obj, frozen)
  Freeze;
end;

end.

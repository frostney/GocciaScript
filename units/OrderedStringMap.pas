{
  TOrderedStringMap<TValue> - String-keyed ordered map.

  Thin subclass of TOrderedMap<string, TValue> that overrides
  HashKey and KeysEqual with proper string-aware implementations:
  DJB2 hash on characters and native string equality.

  Use case: JS object string properties, class methods, module exports.
}

unit OrderedStringMap;

{$I Goccia.inc}

interface

uses
  SysUtils,

  BaseMap,
  OrderedMap;

type
  TOrderedStringMap<TValue> = class(TOrderedMap<string, TValue>)
  protected
    function HashKey(const AKey: string): Cardinal; override;
    function KeysEqual(const A, B: string): Boolean; override;
  end;

  TStringStringMap = TOrderedStringMap<string>;

implementation

{$PUSH}{$R-}{$Q-}
function TOrderedStringMap<TValue>.HashKey(const AKey: string): Cardinal;
var
  I: Integer;
begin
  Result := 5381;
  for I := 1 to Length(AKey) do
    Result := Result * 33 + Ord(AKey[I]);
end;
{$POP}

function TOrderedStringMap<TValue>.KeysEqual(const A, B: string): Boolean;
begin
  Result := A = B;
end;

end.

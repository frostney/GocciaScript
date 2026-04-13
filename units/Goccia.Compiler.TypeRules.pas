unit Goccia.Compiler.TypeRules;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk;

function TypesAreCompatible(const AProduced, AExpected: TGocciaLocalType): Boolean;
function IsKnownNumeric(const AType: TGocciaLocalType): Boolean; inline;

implementation

function TypesAreCompatible(const AProduced, AExpected: TGocciaLocalType): Boolean;
begin
  if AProduced = sltUntyped then
    Exit(False);
  if AProduced = AExpected then
    Exit(True);
  if (AExpected = sltFloat) and (AProduced = sltInteger) then
    Exit(True);
  Result := False;
end;

function IsKnownNumeric(const AType: TGocciaLocalType): Boolean; inline;
begin
  Result := AType in [sltInteger, sltFloat];
end;

end.

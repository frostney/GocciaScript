unit Goccia.Compiler.TypeRules;

{$I Goccia.inc}

interface

uses
  Goccia.Bytecode.Chunk;

function TypesAreCompatible(const AProduced, AExpected: TGocciaLocalType): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
function IsKnownNumeric(const AType: TGocciaLocalType): Boolean;
{$IFDEF FPC}inline;{$ENDIF}

implementation

function TypesAreCompatible(const AProduced, AExpected: TGocciaLocalType): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  if AProduced = sltUntyped then
    Exit(False);
  if AProduced = AExpected then
    Exit(True);
  if (AExpected = sltFloat) and (AProduced = sltInteger) then
    Exit(True);
  Result := False;
end;

function IsKnownNumeric(const AType: TGocciaLocalType): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := AType in [sltInteger, sltFloat];
end;

end.

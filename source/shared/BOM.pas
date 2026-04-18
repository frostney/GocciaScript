unit BOM;

{$I Shared.inc}

interface

uses
  SysUtils;

const
  UTF8_BOM = #$EF#$BB#$BF;
  UTF8_BOM_LEN = 3;

  UTF8_BOM_BYTE_1 = $EF;
  UTF8_BOM_BYTE_2 = $BB;
  UTF8_BOM_BYTE_3 = $BF;

function HasUTF8BOMString(const AText: string): Boolean; inline;
function HasUTF8BOMBytes(const ABytes: TBytes;
  const AStart, AEnd: Integer): Boolean; inline;
function SkipUTF8BOM(const AText: string;
  const AStart: Integer): Integer; inline;

implementation

function HasUTF8BOMString(const AText: string): Boolean;
begin
  Result := (Length(AText) >= UTF8_BOM_LEN) and
    (Copy(AText, 1, UTF8_BOM_LEN) = UTF8_BOM);
end;

function HasUTF8BOMBytes(const ABytes: TBytes;
  const AStart, AEnd: Integer): Boolean;
begin
  Result := (AEnd - AStart >= UTF8_BOM_LEN) and
    (ABytes[AStart] = UTF8_BOM_BYTE_1) and
    (ABytes[AStart + 1] = UTF8_BOM_BYTE_2) and
    (ABytes[AStart + 2] = UTF8_BOM_BYTE_3);
end;

function SkipUTF8BOM(const AText: string;
  const AStart: Integer): Integer;
begin
  if (AStart = 0) and HasUTF8BOMString(AText) then
    Result := UTF8_BOM_LEN
  else
    Result := AStart;
end;

end.

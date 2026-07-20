unit BOM;

{$I Shared.inc}

interface

uses
  SysUtils;

const
  BYTE_ORDER_MARK = #$FEFF;
  BYTE_ORDER_MARK_CODE_UNIT_LENGTH = 1;

  UTF8_BOM_BYTE_LENGTH = 3;
  UTF8_BOM_BYTE_1 = $EF;
  UTF8_BOM_BYTE_2 = $BB;
  UTF8_BOM_BYTE_3 = $BF;

function HasByteOrderMark(const AText: string): Boolean; {$IFDEF FPC}inline;{$ENDIF}
function HasUTF8BOMBytes(const ABytes: TBytes;
  const AStart, AEnd: Integer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
function SkipByteOrderMark(const AText: string;
  const AStart: Integer): Integer; {$IFDEF FPC}inline;{$ENDIF}

implementation

function HasByteOrderMark(const AText: string): Boolean;
begin
  Result := (Length(AText) >= BYTE_ORDER_MARK_CODE_UNIT_LENGTH) and
    (AText[1] = BYTE_ORDER_MARK);
end;

function HasUTF8BOMBytes(const ABytes: TBytes;
  const AStart, AEnd: Integer): Boolean;
begin
  Result := (AEnd - AStart >= UTF8_BOM_BYTE_LENGTH) and
    (ABytes[AStart] = UTF8_BOM_BYTE_1) and
    (ABytes[AStart + 1] = UTF8_BOM_BYTE_2) and
    (ABytes[AStart + 2] = UTF8_BOM_BYTE_3);
end;

function SkipByteOrderMark(const AText: string;
  const AStart: Integer): Integer;
begin
  if (AStart = 0) and HasByteOrderMark(AText) then
    Result := BYTE_ORDER_MARK_CODE_UNIT_LENGTH
  else
    Result := AStart;
end;

end.

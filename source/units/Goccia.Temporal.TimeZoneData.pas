unit Goccia.Temporal.TimeZoneData;

{$I Goccia.inc}

interface

uses
  SysUtils;

function TryGetGeneratedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;

implementation

uses
  Generated.TimeZoneData;

const
  BITS_PER_BYTE = 8;

function TryFindGeneratedTimeZoneEntry(const ATimeZone: string;
  out AEntry: TGeneratedTimeZoneDataEntry): Boolean;
var
  LowIndex, HighIndex, MiddleIndex, CompareResult: Integer;
begin
  Result := False;
  LowIndex := 0;
  HighIndex := GeneratedTimeZoneDataEntryCount - 1;

  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    CompareResult := CompareStr(ATimeZone, GeneratedTimeZoneDataEntries[MiddleIndex].Name);
    if CompareResult = 0 then
    begin
      AEntry := GeneratedTimeZoneDataEntries[MiddleIndex];
      Result := True;
      Exit;
    end;

    if CompareResult < 0 then
      HighIndex := MiddleIndex - 1
    else
      LowIndex := MiddleIndex + 1;
  end;
end;

function ReadGeneratedTimeZoneByte(const AOffset: Integer): Byte;
var
  WordValue: QWord;
  ShiftAmount: Integer;
begin
  WordValue := QWord(GeneratedTimeZoneDataWords[AOffset div GeneratedTimeZoneDataWordSize]);
  ShiftAmount := (AOffset mod GeneratedTimeZoneDataWordSize) * BITS_PER_BYTE;
  Result := Byte((WordValue shr ShiftAmount) and $FF);
end;

procedure CopyGeneratedTimeZoneBytes(const AOffset, ALength: Integer; out ABytes: TBytes);
var
  Index: Integer;
begin
  SetLength(ABytes, ALength);
  for Index := 0 to ALength - 1 do
    ABytes[Index] := ReadGeneratedTimeZoneByte(AOffset + Index);
end;

function TryGetGeneratedTimeZoneFile(const ATimeZone: string; out ABytes: TBytes): Boolean;
var
  Entry: TGeneratedTimeZoneDataEntry;
begin
  Result := False;
  SetLength(ABytes, 0);

  if not TryFindGeneratedTimeZoneEntry(ATimeZone, Entry) then
    Exit;

  if Entry.Offset + Entry.Length > GeneratedTimeZoneDataBlobByteCount then
    Exit;

  CopyGeneratedTimeZoneBytes(Entry.Offset, Entry.Length, ABytes);
  Result := True;
end;

end.

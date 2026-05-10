unit EmbeddedResourceReader;

{$I Shared.inc}

interface

uses
  SysUtils;

function HasBytesAvailable(const ABuffer: TBytes; const AOffset, ALength: Integer): Boolean;
function TryUInt32ToInteger(const AValue: UInt32; out AInteger: Integer): Boolean;
function ReadUInt32LE(const ABuffer: TBytes; const AOffset: Integer): UInt32;
function CopyStringFromBytes(const ABuffer: TBytes; const AOffset,
  ALength: Integer): string;

implementation

function HasBytesAvailable(const ABuffer: TBytes; const AOffset, ALength: Integer): Boolean;
begin
  Result := False;

  if (AOffset < 0) or (ALength < 0) then
    Exit;

  if AOffset > Length(ABuffer) then
    Exit;

  Result := ALength <= Length(ABuffer) - AOffset;
end;

function TryUInt32ToInteger(const AValue: UInt32; out AInteger: Integer): Boolean;
begin
  Result := AValue <= UInt32(High(Integer));
  if Result then
    AInteger := Integer(AValue)
  else
    AInteger := 0;
end;

function ReadUInt32LE(const ABuffer: TBytes; const AOffset: Integer): UInt32;
begin
  Result := UInt32(ABuffer[AOffset]) or
            (UInt32(ABuffer[AOffset + 1]) shl 8) or
            (UInt32(ABuffer[AOffset + 2]) shl 16) or
            (UInt32(ABuffer[AOffset + 3]) shl 24);
end;

function CopyStringFromBytes(const ABuffer: TBytes; const AOffset,
  ALength: Integer): string;
begin
  SetLength(Result, ALength);
  if ALength > 0 then
    Move(ABuffer[AOffset], Result[1], ALength);
end;

end.

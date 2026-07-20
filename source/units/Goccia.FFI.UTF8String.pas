unit Goccia.FFI.UTF8String;

{$I Goccia.inc}

interface

uses
  SysUtils;

function TryEncodeFFIUTF8String(const AText: string;
  out ABytes: TBytes): Boolean;
function TryDecodeFFIUTF8String(const AValue: PAnsiChar;
  out AText: string): Boolean;

implementation

uses
  TextEncoding;

function TryEncodeFFIUTF8String(const AText: string;
  out ABytes: TBytes): Boolean;
var
  ErrorOffset: Integer;
begin
  SetLength(ABytes, 0);
  if Pos(#0, AText) > 0 then
    Exit(False);
  if not TryEncodeUTF8NullTerminated(AText, ABytes, ErrorOffset) then
    Exit(False);
  Result := True;
end;

function TryDecodeFFIUTF8String(const AValue: PAnsiChar;
  out AText: string): Boolean;
var
  Bytes: TBytes;
  ByteLength: NativeInt;
  ErrorOffset: Integer;
begin
  AText := '';
  if not Assigned(AValue) then
    Exit(False);
  ByteLength := StrLen(AValue);
  SetLength(Bytes, ByteLength);
  if ByteLength > 0 then
    Move(AValue^, Bytes[0], ByteLength);
  Result := TryDecodeUTF8(Bytes, AText, ErrorOffset);
end;

end.

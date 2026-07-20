unit TextEncoding;

{$I Shared.inc}

interface

uses
  SysUtils;

function TryDecodeUTF8(const ABytes: TBytes; out AText: string;
  out AErrorOffset: Integer): Boolean;
function DecodeUTF8WithReplacement(const ABytes: TBytes): string;
function TryEncodeUTF8(const AText: string; out ABytes: TBytes;
  out AErrorOffset: Integer): Boolean;
function TryEncodeUTF8NullTerminated(const AText: string; out ABytes: TBytes;
  out AErrorOffset: Integer): Boolean;
function TryEncodeASCIINullTerminated(const AText: string; out ABytes: TBytes;
  out AErrorOffset: Integer): Boolean;
function EncodeUTF8WithReplacement(const AText: string): TBytes;
function CodePointToUTF16(const ACodePoint: Cardinal): string;

implementation

uses
  StringBuffer;

const
  REPLACEMENT_CHARACTER = Char($FFFD);

function CodePointToUTF16(const ACodePoint: Cardinal): string;
var
  CodePoint, Supplementary: Cardinal;
begin
  if ACodePoint > $10FFFF then
    CodePoint := $FFFD
  else
    CodePoint := ACodePoint;
  if CodePoint <= $FFFF then
    Exit(Char(CodePoint));
  Supplementary := CodePoint - $10000;
  Result := Char($D800 + (Supplementary shr 10)) +
    Char($DC00 + (Supplementary and $3FF));
end;

function IsContinuationByte(const AByte: Byte): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (AByte and $C0) = $80;
end;

procedure AppendCodePoint(var ABuffer: TStringBuffer;
  const ACodePoint: Cardinal);
var
  Supplementary: Cardinal;
begin
  if ACodePoint <= $FFFF then
    ABuffer.AppendChar(Char(ACodePoint))
  else
  begin
    Supplementary := ACodePoint - $10000;
    ABuffer.AppendChar(Char($D800 + (Supplementary shr 10)));
    ABuffer.AppendChar(Char($DC00 + (Supplementary and $3FF)));
  end;
end;

function TryReadUTF8CodePoint(const ABytes: TBytes; const AIndex: Integer;
  out ACodePoint: Cardinal; out ALength: Integer): Boolean;
var
  B0, B1, B2, B3: Byte;
begin
  Result := False;
  ACodePoint := 0;
  ALength := 1;
  if (AIndex < 0) or (AIndex >= Length(ABytes)) then
    Exit;

  B0 := ABytes[AIndex];
  if B0 < $80 then
  begin
    ACodePoint := B0;
    Exit(True);
  end;

  if (B0 >= $C2) and (B0 <= $DF) then
  begin
    ALength := 2;
    if AIndex + 1 >= Length(ABytes) then
      Exit;
    B1 := ABytes[AIndex + 1];
    if not IsContinuationByte(B1) then
      Exit;
    ACodePoint := Cardinal(B0 and $1F) shl 6 or Cardinal(B1 and $3F);
    Exit(True);
  end;

  if (B0 >= $E0) and (B0 <= $EF) then
  begin
    ALength := 3;
    if AIndex + 2 >= Length(ABytes) then
      Exit;
    B1 := ABytes[AIndex + 1];
    B2 := ABytes[AIndex + 2];
    if not IsContinuationByte(B1) or not IsContinuationByte(B2) or
       ((B0 = $E0) and (B1 < $A0)) or
       ((B0 = $ED) and (B1 >= $A0)) then
      Exit;
    ACodePoint := Cardinal(B0 and $0F) shl 12 or
      Cardinal(B1 and $3F) shl 6 or Cardinal(B2 and $3F);
    Exit(True);
  end;

  if (B0 >= $F0) and (B0 <= $F4) then
  begin
    ALength := 4;
    if AIndex + 3 >= Length(ABytes) then
      Exit;
    B1 := ABytes[AIndex + 1];
    B2 := ABytes[AIndex + 2];
    B3 := ABytes[AIndex + 3];
    if not IsContinuationByte(B1) or not IsContinuationByte(B2) or
       not IsContinuationByte(B3) or
       ((B0 = $F0) and (B1 < $90)) or
       ((B0 = $F4) and (B1 > $8F)) then
      Exit;
    ACodePoint := Cardinal(B0 and $07) shl 18 or
      Cardinal(B1 and $3F) shl 12 or Cardinal(B2 and $3F) shl 6 or
      Cardinal(B3 and $3F);
    Exit(True);
  end;
end;

function TryDecodeUTF8(const ABytes: TBytes; out AText: string;
  out AErrorOffset: Integer): Boolean;
var
  Buffer: TStringBuffer;
  CodePoint: Cardinal;
  Index, SequenceLength: Integer;
begin
  AErrorOffset := -1;
  Buffer := TStringBuffer.Create(Length(ABytes));
  Index := 0;
  while Index < Length(ABytes) do
  begin
    if not TryReadUTF8CodePoint(ABytes, Index, CodePoint,
      SequenceLength) then
    begin
      AText := '';
      AErrorOffset := Index;
      Exit(False);
    end;
    AppendCodePoint(Buffer, CodePoint);
    Inc(Index, SequenceLength);
  end;
  AText := Buffer.ToString;
  Result := True;
end;

function InvalidSequenceLength(const ABytes: TBytes;
  const AIndex, AExpectedLength: Integer): Integer;
var
  FirstByte, SecondByte: Byte;
begin
  Result := 1;
  if (AExpectedLength <= 1) or
     (AIndex + 1 >= Length(ABytes)) then
    Exit;

  FirstByte := ABytes[AIndex];
  SecondByte := ABytes[AIndex + 1];
  if not IsContinuationByte(SecondByte) or
     ((FirstByte = $E0) and (SecondByte < $A0)) or
     ((FirstByte = $ED) and (SecondByte >= $A0)) or
     ((FirstByte = $F0) and (SecondByte < $90)) or
     ((FirstByte = $F4) and (SecondByte > $8F)) then
    Exit;

  while (Result < AExpectedLength) and
        (AIndex + Result < Length(ABytes)) and
        IsContinuationByte(ABytes[AIndex + Result]) do
    Inc(Result);
end;

function DecodeUTF8WithReplacement(const ABytes: TBytes): string;
var
  Buffer: TStringBuffer;
  CodePoint: Cardinal;
  Index, SequenceLength: Integer;
begin
  Buffer := TStringBuffer.Create(Length(ABytes));
  Index := 0;
  while Index < Length(ABytes) do
  begin
    if TryReadUTF8CodePoint(ABytes, Index, CodePoint, SequenceLength) then
    begin
      AppendCodePoint(Buffer, CodePoint);
      Inc(Index, SequenceLength);
    end
    else
    begin
      Buffer.AppendChar(REPLACEMENT_CHARACTER);
      Inc(Index, InvalidSequenceLength(ABytes, Index, SequenceLength));
    end;
  end;
  Result := Buffer.ToString;
end;

function IsHighSurrogate(const AChar: Char): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (Ord(AChar) >= $D800) and (Ord(AChar) <= $DBFF);
end;

function IsLowSurrogate(const AChar: Char): Boolean;
{$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (Ord(AChar) >= $DC00) and (Ord(AChar) <= $DFFF);
end;

procedure AppendUTF8CodePoint(var ABytes: TBytes; var ALength: Integer;
  const ACodePoint: Cardinal);
begin
  if ACodePoint <= $7F then
  begin
    SetLength(ABytes, ALength + 1);
    ABytes[ALength] := Byte(ACodePoint);
    Inc(ALength);
  end
  else if ACodePoint <= $7FF then
  begin
    SetLength(ABytes, ALength + 2);
    ABytes[ALength] := $C0 or Byte(ACodePoint shr 6);
    ABytes[ALength + 1] := $80 or Byte(ACodePoint and $3F);
    Inc(ALength, 2);
  end
  else if ACodePoint <= $FFFF then
  begin
    SetLength(ABytes, ALength + 3);
    ABytes[ALength] := $E0 or Byte(ACodePoint shr 12);
    ABytes[ALength + 1] := $80 or Byte((ACodePoint shr 6) and $3F);
    ABytes[ALength + 2] := $80 or Byte(ACodePoint and $3F);
    Inc(ALength, 3);
  end
  else
  begin
    SetLength(ABytes, ALength + 4);
    ABytes[ALength] := $F0 or Byte(ACodePoint shr 18);
    ABytes[ALength + 1] := $80 or Byte((ACodePoint shr 12) and $3F);
    ABytes[ALength + 2] := $80 or Byte((ACodePoint shr 6) and $3F);
    ABytes[ALength + 3] := $80 or Byte(ACodePoint and $3F);
    Inc(ALength, 4);
  end;
end;

function EncodeUTF8(const AText: string; const AReplaceInvalid: Boolean;
  out ABytes: TBytes; out AErrorOffset: Integer): Boolean;
var
  CodePoint: Cardinal;
  Index, OutputLength: Integer;
begin
  AErrorOffset := -1;
  SetLength(ABytes, 0);
  Index := 1;
  OutputLength := 0;
  while Index <= Length(AText) do
  begin
    if IsHighSurrogate(AText[Index]) then
    begin
      if (Index < Length(AText)) and IsLowSurrogate(AText[Index + 1]) then
      begin
        CodePoint := $10000 + (Cardinal(Ord(AText[Index]) - $D800) shl 10) +
          Cardinal(Ord(AText[Index + 1]) - $DC00);
        Inc(Index, 2);
      end
      else if AReplaceInvalid then
      begin
        CodePoint := $FFFD;
        Inc(Index);
      end
      else
      begin
        AErrorOffset := Index - 1;
        Exit(False);
      end;
    end
    else if IsLowSurrogate(AText[Index]) then
    begin
      if not AReplaceInvalid then
      begin
        AErrorOffset := Index - 1;
        Exit(False);
      end;
      CodePoint := $FFFD;
      Inc(Index);
    end
    else
    begin
      CodePoint := Ord(AText[Index]);
      Inc(Index);
    end;
    AppendUTF8CodePoint(ABytes, OutputLength, CodePoint);
  end;
  Result := True;
end;

function TryEncodeUTF8(const AText: string; out ABytes: TBytes;
  out AErrorOffset: Integer): Boolean;
begin
  Result := EncodeUTF8(AText, False, ABytes, AErrorOffset);
end;

function TryEncodeUTF8NullTerminated(const AText: string;
  out ABytes: TBytes; out AErrorOffset: Integer): Boolean;
var
  ByteLength: Integer;
begin
  Result := TryEncodeUTF8(AText, ABytes, AErrorOffset);
  if not Result then
    Exit;
  ByteLength := Length(ABytes);
  SetLength(ABytes, ByteLength + 1);
  ABytes[ByteLength] := 0;
end;

function TryEncodeASCIINullTerminated(const AText: string;
  out ABytes: TBytes; out AErrorOffset: Integer): Boolean;
var
  I: Integer;
begin
  AErrorOffset := -1;
  SetLength(ABytes, Length(AText) + 1);
  for I := 1 to Length(AText) do
  begin
    if Ord(AText[I]) > $7F then
    begin
      AErrorOffset := I - 1;
      SetLength(ABytes, 0);
      Exit(False);
    end;
    ABytes[I - 1] := Byte(Ord(AText[I]));
  end;
  ABytes[Length(AText)] := 0;
  Result := True;
end;

function EncodeUTF8WithReplacement(const AText: string): TBytes;
var
  ErrorOffset: Integer;
begin
  EncodeUTF8(AText, True, Result, ErrorOffset);
end;

end.

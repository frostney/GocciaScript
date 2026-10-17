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

function IsContinuationByte(const AByte: Byte): Boolean; {$IFDEF FPC}inline;{$ENDIF}
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

function IsHighSurrogate(const AChar: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (Ord(AChar) >= $D800) and (Ord(AChar) <= $DBFF);
end;

function IsLowSurrogate(const AChar: Char): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := (Ord(AChar) >= $DC00) and (Ord(AChar) <= $DFFF);
end;

function IsPairAt(const AText: string; const AIndex: Integer): Boolean; {$IFDEF FPC}inline;{$ENDIF}
begin
  Result := IsHighSurrogate(AText[AIndex]) and (AIndex < Length(AText)) and
    IsLowSurrogate(AText[AIndex + 1]);
end;

{ The number of UTF-8 bytes AText encodes to, and in AStop the index where
  encoding stops: Length(AText) + 1, or the first unpaired surrogate when
  AReplaceInvalid is False. A surrogate pair takes 4 bytes; an unpaired
  surrogate is replaced by U+FFFD, 3 bytes. }
function UTF8EncodedLength(const AText: string; const AReplaceInvalid: Boolean;
  out AStop: Integer): SizeInt;
var
  CodeUnit: Cardinal;
  Index: Integer;
begin
  Result := 0;
  Index := 1;
  while Index <= Length(AText) do
  begin
    CodeUnit := Ord(AText[Index]);
    if CodeUnit <= $7F then
      Inc(Result)
    else if CodeUnit <= $7FF then
      Inc(Result, 2)
    else if IsPairAt(AText, Index) then
    begin
      Inc(Result, 4);
      Inc(Index);
    end
    else if (CodeUnit >= $D800) and (CodeUnit <= $DFFF) and
      not AReplaceInvalid then
      Break
    else
      Inc(Result, 3);
    Inc(Index);
  end;
  AStop := Index;
end;

{ Writes AText[1..AStop - 1] as UTF-8 into ABytes, which holds exactly the
  length UTF8EncodedLength counted for that range. }
procedure WriteUTF8(const AText: string; const AStop: Integer;
  const ABytes: TBytes);
var
  CodePoint: Cardinal;
  Index: Integer;
  Output: SizeInt;
begin
  Output := 0;
  Index := 1;
  while Index < AStop do
  begin
    CodePoint := Ord(AText[Index]);
    if CodePoint <= $7F then
    begin
      ABytes[Output] := Byte(CodePoint);
      Inc(Output);
      Inc(Index);
      Continue;
    end;
    if IsPairAt(AText, Index) then
    begin
      CodePoint := $10000 + ((CodePoint - $D800) shl 10) +
        Cardinal(Ord(AText[Index + 1]) - $DC00);
      Inc(Index);
    end
    else if (CodePoint >= $D800) and (CodePoint <= $DFFF) then
      CodePoint := $FFFD;
    Inc(Index);
    if CodePoint <= $7FF then
    begin
      ABytes[Output] := $C0 or Byte(CodePoint shr 6);
      ABytes[Output + 1] := $80 or Byte(CodePoint and $3F);
      Inc(Output, 2);
    end
    else if CodePoint <= $FFFF then
    begin
      ABytes[Output] := $E0 or Byte(CodePoint shr 12);
      ABytes[Output + 1] := $80 or Byte((CodePoint shr 6) and $3F);
      ABytes[Output + 2] := $80 or Byte(CodePoint and $3F);
      Inc(Output, 3);
    end
    else
    begin
      ABytes[Output] := $F0 or Byte(CodePoint shr 18);
      ABytes[Output + 1] := $80 or Byte((CodePoint shr 12) and $3F);
      ABytes[Output + 2] := $80 or Byte((CodePoint shr 6) and $3F);
      ABytes[Output + 3] := $80 or Byte(CodePoint and $3F);
      Inc(Output, 4);
    end;
  end;
end;

{ Counts the output first and allocates it once. Growing the array per code
  point copied it ever more often as it grew, so the cost per character rose
  with the input's size (#1544). On an unpaired surrogate without
  AReplaceInvalid, ABytes holds the bytes before it. }
function EncodeUTF8(const AText: string; const AReplaceInvalid: Boolean;
  out ABytes: TBytes; out AErrorOffset: Integer): Boolean;
var
  Stop: Integer;
begin
  ABytes := nil;
  SetLength(ABytes, UTF8EncodedLength(AText, AReplaceInvalid, Stop));
  WriteUTF8(AText, Stop, ABytes);
  Result := Stop > Length(AText);
  if Result then
    AErrorOffset := -1
  else
    AErrorOffset := Stop - 1;
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

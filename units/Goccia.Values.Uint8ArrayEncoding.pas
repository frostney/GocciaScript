unit Goccia.Values.Uint8ArrayEncoding;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Values.Primitives;

type
  TGocciaUint8ArrayEncoding = class
  public
    { Prototype methods — called with this = Uint8Array instance }
    function ToBase64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function ToHex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetFromBase64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function SetFromHex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;

    { Static methods — called on Uint8Array constructor }
    function FromBase64(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function FromHex(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.ObjectValue,
  Goccia.Values.TypedArrayValue;

type
  TBase64Alphabet = array[0..63] of Char;
  TBase64DecodeTable = array[0..127] of Byte;

const
  ALPHABET_BASE64    = 'base64';
  ALPHABET_BASE64URL = 'base64url';

  LAST_CHUNK_LOOSE            = 'loose';
  LAST_CHUNK_STRICT           = 'strict';
  LAST_CHUNK_STOP_BEFORE_PARTIAL = 'stop-before-partial';

  HEX_CHARS: array[0..15] of Char = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
  );

  BASE64_STANDARD: TBase64Alphabet = (
    'A','B','C','D','E','F','G','H','I','J','K','L','M',
    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    'a','b','c','d','e','f','g','h','i','j','k','l','m',
    'n','o','p','q','r','s','t','u','v','w','x','y','z',
    '0','1','2','3','4','5','6','7','8','9','+','/'
  );

  BASE64_URL_SAFE: TBase64Alphabet = (
    'A','B','C','D','E','F','G','H','I','J','K','L','M',
    'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    'a','b','c','d','e','f','g','h','i','j','k','l','m',
    'n','o','p','q','r','s','t','u','v','w','x','y','z',
    '0','1','2','3','4','5','6','7','8','9','-','_'
  );

  INVALID_BASE64_VALUE = 255;
  BASE64_PAD_CHAR      = '=';

{ Decoding lookup tables initialized once }

var
  Base64StandardDecode: TBase64DecodeTable;
  Base64UrlDecode: TBase64DecodeTable;
  TablesInitialized: Boolean = False;

procedure InitDecodeTables;
var
  I: Integer;
begin
  if TablesInitialized then Exit;
  for I := 0 to 127 do
  begin
    Base64StandardDecode[I] := INVALID_BASE64_VALUE;
    Base64UrlDecode[I] := INVALID_BASE64_VALUE;
  end;
  for I := 0 to 63 do
  begin
    Base64StandardDecode[Ord(BASE64_STANDARD[I])] := Byte(I);
    Base64UrlDecode[Ord(BASE64_URL_SAFE[I])] := Byte(I);
  end;
  TablesInitialized := True;
end;

function IsAsciiWhitespace(const ACh: Char): Boolean; inline;
begin
  Result := (ACh = #9) or (ACh = #10) or (ACh = #12) or (ACh = #13) or (ACh = ' ');
end;

function RequireUint8Array(const AThisValue: TGocciaValue; const AMethod: string): TGocciaTypedArrayValue;
begin
  if not (AThisValue is TGocciaTypedArrayValue) or
     (TGocciaTypedArrayValue(AThisValue).Kind <> takUint8) then
    ThrowTypeError(Format(SErrorRequiresUint8Array, [AMethod]),
      SSuggestUint8ArrayThisType);
  Result := TGocciaTypedArrayValue(AThisValue);
end;

function ReadAlphabetOption(const AOptions: TGocciaValue): string;
var
  AlphabetVal: TGocciaValue;
begin
  Result := ALPHABET_BASE64;
  if (AOptions is TGocciaObjectValue) then
  begin
    AlphabetVal := TGocciaObjectValue(AOptions).GetProperty(PROP_ALPHABET);
    if Assigned(AlphabetVal) and not (AlphabetVal is TGocciaUndefinedLiteralValue) then
    begin
      Result := AlphabetVal.ToStringLiteral.Value;
      if (Result <> ALPHABET_BASE64) and (Result <> ALPHABET_BASE64URL) then
        ThrowTypeError(SErrorInvalidAlphabet, SSuggestBase64Format);
    end;
  end;
end;

function ReadOmitPaddingOption(const AOptions: TGocciaValue): Boolean;
var
  Val: TGocciaValue;
begin
  Result := False;
  if (AOptions is TGocciaObjectValue) then
  begin
    Val := TGocciaObjectValue(AOptions).GetProperty(PROP_OMIT_PADDING);
    if Assigned(Val) and not (Val is TGocciaUndefinedLiteralValue) then
      Result := Val.ToBooleanLiteral.Value;
  end;
end;

function ReadLastChunkHandlingOption(const AOptions: TGocciaValue): string;
var
  Val: TGocciaValue;
begin
  Result := LAST_CHUNK_LOOSE;
  if (AOptions is TGocciaObjectValue) then
  begin
    Val := TGocciaObjectValue(AOptions).GetProperty(PROP_LAST_CHUNK_HANDLING);
    if Assigned(Val) and not (Val is TGocciaUndefinedLiteralValue) then
    begin
      Result := Val.ToStringLiteral.Value;
      if (Result <> LAST_CHUNK_LOOSE) and (Result <> LAST_CHUNK_STRICT) and
         (Result <> LAST_CHUNK_STOP_BEFORE_PARTIAL) then
        ThrowTypeError(SErrorInvalidLastChunkHandling, SSuggestBase64Format);
    end;
  end;
end;

{ ---- Hex helpers ---- }

function HexCharToNibble(const ACh: Char): Integer; inline;
begin
  case ACh of
    '0'..'9': Result := Ord(ACh) - Ord('0');
    'a'..'f': Result := 10 + Ord(ACh) - Ord('a');
    'A'..'F': Result := 10 + Ord(ACh) - Ord('A');
  else
    Result := -1;
  end;
end;

{ ---- Base64 encoding ---- }

function EncodeBase64(const AData: TBytes; const AOffset, ALength: Integer;
  const AAlphabet: string; const AOmitPadding: Boolean): string;
var
  Table: TBase64Alphabet;
  I, Groups, Remainder, OutLen, Pos: Integer;
  B0, B1, B2: Byte;
begin
  if AAlphabet = ALPHABET_BASE64URL then
    Table := BASE64_URL_SAFE
  else
    Table := BASE64_STANDARD;

  Groups := ALength div 3;
  Remainder := ALength mod 3;
  if AOmitPadding then
  begin
    case Remainder of
      0: OutLen := Groups * 4;
      1: OutLen := Groups * 4 + 2;
      2: OutLen := Groups * 4 + 3;
    else
      OutLen := Groups * 4;
    end;
  end
  else
  begin
    if Remainder > 0 then
      OutLen := (Groups + 1) * 4
    else
      OutLen := Groups * 4;
  end;

  SetLength(Result, OutLen);
  Pos := 1;

  for I := 0 to Groups - 1 do
  begin
    B0 := AData[AOffset + I * 3];
    B1 := AData[AOffset + I * 3 + 1];
    B2 := AData[AOffset + I * 3 + 2];
    Result[Pos]     := Table[(B0 shr 2) and $3F];
    Result[Pos + 1] := Table[((B0 and $03) shl 4) or ((B1 shr 4) and $0F)];
    Result[Pos + 2] := Table[((B1 and $0F) shl 2) or ((B2 shr 6) and $03)];
    Result[Pos + 3] := Table[B2 and $3F];
    Inc(Pos, 4);
  end;

  case Remainder of
    1:
    begin
      B0 := AData[AOffset + Groups * 3];
      Result[Pos]     := Table[(B0 shr 2) and $3F];
      Result[Pos + 1] := Table[(B0 and $03) shl 4];
      if not AOmitPadding then
      begin
        Result[Pos + 2] := BASE64_PAD_CHAR;
        Result[Pos + 3] := BASE64_PAD_CHAR;
      end;
    end;
    2:
    begin
      B0 := AData[AOffset + Groups * 3];
      B1 := AData[AOffset + Groups * 3 + 1];
      Result[Pos]     := Table[(B0 shr 2) and $3F];
      Result[Pos + 1] := Table[((B0 and $03) shl 4) or ((B1 shr 4) and $0F)];
      Result[Pos + 2] := Table[(B1 and $0F) shl 2];
      if not AOmitPadding then
        Result[Pos + 3] := BASE64_PAD_CHAR;
    end;
  end;
end;

{ ---- Base64 decoding ---- }

type
  TBase64DecodeResult = record
    Bytes: TBytes;
    ByteLength: Integer;
    CharsRead: Integer;
  end;

function DecodeBase64(const AInput: string; const AAlphabet: string;
  const ALastChunkHandling: string; const AMaxBytes: Integer): TBase64DecodeResult;
var
  DecodeTable: TBase64DecodeTable;
  Cleaned: string;
  CleanedLen, I, ChunkLen: Integer;
  Ch: Char;
  HasPadding: Boolean;
  PadStart, FullChunks, Remainder: Integer;
  Pos, OutPos: Integer;
  V0, V1, V2, V3: Byte;
  CharsConsumed: Integer;

  { Map cleaned index back to original input index }
  CleanedToOriginal: array of Integer;
begin
  InitDecodeTables;
  if AAlphabet = ALPHABET_BASE64URL then
    DecodeTable := Base64UrlDecode
  else
    DecodeTable := Base64StandardDecode;

  // Step 1: Strip ASCII whitespace and track original positions
  SetLength(Cleaned, Length(AInput));
  SetLength(CleanedToOriginal, Length(AInput) + 1);
  CleanedLen := 0;
  for I := 1 to Length(AInput) do
  begin
    Ch := AInput[I];
    if not IsAsciiWhitespace(Ch) then
    begin
      Inc(CleanedLen);
      Cleaned[CleanedLen] := Ch;
      CleanedToOriginal[CleanedLen] := I;
    end;
  end;
  SetLength(Cleaned, CleanedLen);

  // Step 2: Handle padding — remove trailing '=' and record
  HasPadding := False;
  PadStart := CleanedLen;
  if (CleanedLen >= 1) and (Cleaned[CleanedLen] = BASE64_PAD_CHAR) then
  begin
    HasPadding := True;
    Dec(PadStart);
    if (PadStart >= 1) and (Cleaned[PadStart] = BASE64_PAD_CHAR) then
      Dec(PadStart);
  end;

  if HasPadding then
    ChunkLen := PadStart
  else
    ChunkLen := CleanedLen;

  // Step 3: Validate all non-padding characters
  for I := 1 to ChunkLen do
  begin
    Ch := Cleaned[I];
    if (Ord(Ch) > 127) or (DecodeTable[Ord(Ch)] = INVALID_BASE64_VALUE) then
      ThrowSyntaxError(SErrorInvalidBase64Character, SSuggestBase64Format);
  end;

  // Step 4: Compute full chunks and remainder
  FullChunks := ChunkLen div 4;
  Remainder := ChunkLen mod 4;

  // Step 5: Handle last chunk based on mode
  if Remainder = 1 then
  begin
    // A single trailing character is never valid
    if ALastChunkHandling = LAST_CHUNK_STOP_BEFORE_PARTIAL then
    begin
      // Just ignore the last char, treat only full chunks
      Dec(ChunkLen, 1);
      Remainder := 0;
    end
    else
      ThrowSyntaxError(SErrorBase64IncompleteChunk, SSuggestBase64Format);
  end;

  if Remainder > 0 then
  begin
    if ALastChunkHandling = LAST_CHUNK_STRICT then
    begin
      if HasPadding then
      begin
        // In strict mode with padding, the padded chunk must be exactly 4 chars
        // Check that the padding makes it a complete 4-char chunk
        if (Remainder = 2) and (CleanedLen - PadStart = 2) then
        begin
          // 2 data + 2 pad = 4: OK, check overflow bits
          V0 := DecodeTable[Ord(Cleaned[PadStart - 1])];
          V1 := DecodeTable[Ord(Cleaned[PadStart])];
          if (V1 and $0F) <> 0 then
            ThrowSyntaxError(SErrorBase64NonZeroPaddingBits, SSuggestBase64Format);
        end
        else if (Remainder = 3) and (CleanedLen - PadStart = 1) then
        begin
          // 3 data + 1 pad = 4: OK, check overflow bits
          V0 := DecodeTable[Ord(Cleaned[PadStart])];
          if (V0 and $03) <> 0 then
            ThrowSyntaxError(SErrorBase64NonZeroPaddingBits, SSuggestBase64Format);
        end
        else
          ThrowSyntaxError(SErrorBase64MalformedPadding, SSuggestBase64Format);
      end
      else
      begin
        // In strict mode without padding, partial chunks are invalid
        ThrowSyntaxError(SErrorBase64IncompleteChunkStrict, SSuggestBase64Format);
      end;
    end
    else if ALastChunkHandling = LAST_CHUNK_STOP_BEFORE_PARTIAL then
    begin
      // Don't decode the partial last chunk
      Dec(ChunkLen, Remainder);
      Remainder := 0;
      FullChunks := ChunkLen div 4;
    end;
    // 'loose' mode: decode the partial chunk normally
  end
  else if HasPadding and (ALastChunkHandling = LAST_CHUNK_STRICT) then
  begin
    // Padding present but no remainder means the padding was part of a complete chunk
    // Validate the padding makes sense
    // (This case occurs when e.g., "AAAA==" — the padding overflows a complete chunk)
    // Actually if Remainder = 0 and HasPadding, the padding was stripped from a complete set
    // which means padding was erroneous
    if PadStart <> CleanedLen then
      ThrowSyntaxError(SErrorBase64UnexpectedPadding, SSuggestBase64Format);
  end;

  // Step 6: Compute output size
  Result.ByteLength := FullChunks * 3;
  case Remainder of
    2: Inc(Result.ByteLength, 1);
    3: Inc(Result.ByteLength, 2);
  end;

  if (AMaxBytes >= 0) and (Result.ByteLength > AMaxBytes) then
    Result.ByteLength := AMaxBytes;

  SetLength(Result.Bytes, Result.ByteLength);

  // Step 7: Decode full 4-char chunks — only consume when all 3 output bytes fit
  Pos := 1;
  OutPos := 0;
  for I := 0 to FullChunks - 1 do
  begin
    if OutPos + 3 > Result.ByteLength then Break;
    V0 := DecodeTable[Ord(Cleaned[Pos])];
    V1 := DecodeTable[Ord(Cleaned[Pos + 1])];
    V2 := DecodeTable[Ord(Cleaned[Pos + 2])];
    V3 := DecodeTable[Ord(Cleaned[Pos + 3])];
    Result.Bytes[OutPos]     := (V0 shl 2) or (V1 shr 4);
    Result.Bytes[OutPos + 1] := ((V1 and $0F) shl 4) or (V2 shr 2);
    Result.Bytes[OutPos + 2] := ((V2 and $03) shl 6) or V3;
    Inc(OutPos, 3);
    Inc(Pos, 4);
  end;

  // Step 8: Decode remainder — only consume when all output bytes fit
  // 2 trailing chars → 1 byte; 3 trailing chars → 2 bytes
  if (Remainder >= 2) and (OutPos + (Remainder - 1) <= Result.ByteLength) then
  begin
    V0 := DecodeTable[Ord(Cleaned[Pos])];
    V1 := DecodeTable[Ord(Cleaned[Pos + 1])];
    Result.Bytes[OutPos] := (V0 shl 2) or (V1 shr 4);
    Inc(OutPos);
    if Remainder >= 3 then
    begin
      V2 := DecodeTable[Ord(Cleaned[Pos + 2])];
      Result.Bytes[OutPos] := ((V1 and $0F) shl 4) or (V2 shr 2);
      Inc(OutPos);
    end;
    Inc(Pos, Remainder);
  end;

  Result.ByteLength := OutPos;
  SetLength(Result.Bytes, Result.ByteLength);

  // Compute characters read from the original input
  // After successful decode, all characters (including padding) were consumed
  if (Pos > ChunkLen) then
  begin
    // All data chars were decoded — consumed everything including padding
    if CleanedLen > 0 then
      CharsConsumed := CleanedToOriginal[CleanedLen]
    else
      CharsConsumed := 0;
  end
  else if Pos > 1 then
    CharsConsumed := CleanedToOriginal[Pos - 1]
  else
    CharsConsumed := 0;

  // For stop-before-partial, report only up to the chars that were actually decoded
  if (ALastChunkHandling = LAST_CHUNK_STOP_BEFORE_PARTIAL) and (ChunkLen < PadStart) then
  begin
    if ChunkLen > 0 then
      CharsConsumed := CleanedToOriginal[ChunkLen]
    else
      CharsConsumed := 0;
  end;

  Result.CharsRead := CharsConsumed;
end;

{ ---- Public methods ---- }

// ES2026 §5.1.1 Uint8Array.prototype.toBase64([options])
function TGocciaUint8ArrayEncoding.ToBase64(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Alphabet: string;
  OmitPadding: Boolean;
  Options: TGocciaValue;
  Encoded: string;
begin
  TA := RequireUint8Array(AThisValue, 'Uint8Array.prototype.toBase64');

  Alphabet := ALPHABET_BASE64;
  OmitPadding := False;
  if AArgs.Length > 0 then
  begin
    Options := AArgs.GetElement(0);
    if not (Options is TGocciaUndefinedLiteralValue) then
    begin
      if not (Options is TGocciaObjectValue) then
        ThrowTypeError(SErrorToBase64OptionsNotObject, SSuggestBase64Format);
      Alphabet := ReadAlphabetOption(Options);
      OmitPadding := ReadOmitPaddingOption(Options);
    end;
  end;

  Encoded := EncodeBase64(TA.BufferData, TA.ByteOffset, TA.Length, Alphabet, OmitPadding);
  Result := TGocciaStringLiteralValue.Create(Encoded);
end;

// ES2026 §5.1.2 Uint8Array.prototype.toHex()
function TGocciaUint8ArrayEncoding.ToHex(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  I, Offset: Integer;
  Hex: string;
  B: Byte;
begin
  TA := RequireUint8Array(AThisValue, 'Uint8Array.prototype.toHex');

  SetLength(Hex, TA.Length * 2);
  Offset := TA.ByteOffset;
  for I := 0 to TA.Length - 1 do
  begin
    B := TA.BufferData[Offset + I];
    Hex[I * 2 + 1] := HEX_CHARS[B shr 4];
    Hex[I * 2 + 2] := HEX_CHARS[B and $0F];
  end;
  Result := TGocciaStringLiteralValue.Create(Hex);
end;

// ES2026 §5.1.3 Uint8Array.prototype.setFromBase64(string [, options])
function TGocciaUint8ArrayEncoding.SetFromBase64(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Input, Alphabet, LastChunkHandling: string;
  Options: TGocciaValue;
  DecResult: TBase64DecodeResult;
  Written, I: Integer;
  ResultObj: TGocciaObjectValue;
begin
  TA := RequireUint8Array(AThisValue, 'Uint8Array.prototype.setFromBase64');

  if AArgs.Length = 0 then
    ThrowTypeError(SErrorSetFromBase64RequiresString, SSuggestStringArgRequired);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorSetFromBase64FirstArgString, SSuggestStringArgRequired);

  Input := AArgs.GetElement(0).ToStringLiteral.Value;

  Alphabet := ALPHABET_BASE64;
  LastChunkHandling := LAST_CHUNK_LOOSE;
  if AArgs.Length > 1 then
  begin
    Options := AArgs.GetElement(1);
    if not (Options is TGocciaUndefinedLiteralValue) then
    begin
      if not (Options is TGocciaObjectValue) then
        ThrowTypeError(SErrorSetFromBase64OptionsNotObject, SSuggestBase64Format);
      Alphabet := ReadAlphabetOption(Options);
      LastChunkHandling := ReadLastChunkHandlingOption(Options);
    end;
  end;

  DecResult := DecodeBase64(Input, Alphabet, LastChunkHandling, TA.Length);

  Written := DecResult.ByteLength;
  if Written > TA.Length then
    Written := TA.Length;

  for I := 0 to Written - 1 do
    TA.BufferData[TA.ByteOffset + I] := DecResult.Bytes[I];

  ResultObj := TGocciaObjectValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultObj);
  try
    ResultObj.AssignProperty(PROP_READ, TGocciaNumberLiteralValue.Create(DecResult.CharsRead));
    ResultObj.AssignProperty(PROP_WRITTEN, TGocciaNumberLiteralValue.Create(Written));
    Result := ResultObj;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultObj);
  end;
end;

// ES2026 §5.1.4 Uint8Array.prototype.setFromHex(string)
function TGocciaUint8ArrayEncoding.SetFromHex(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  TA: TGocciaTypedArrayValue;
  Input: string;
  I, Written, Hi, Lo: Integer;
  ResultObj: TGocciaObjectValue;
  CharsRead: Integer;
begin
  TA := RequireUint8Array(AThisValue, 'Uint8Array.prototype.setFromHex');

  if AArgs.Length = 0 then
    ThrowTypeError(SErrorSetFromHexRequiresString, SSuggestStringArgRequired);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorSetFromHexFirstArgString, SSuggestStringArgRequired);

  Input := AArgs.GetElement(0).ToStringLiteral.Value;

  if (Length(Input) mod 2) <> 0 then
    ThrowSyntaxError(SErrorInvalidHexOddLength, SSuggestHexFormat);

  Written := 0;
  CharsRead := 0;
  I := 1;
  while (I + 1 <= Length(Input)) and (Written < TA.Length) do
  begin
    Hi := HexCharToNibble(Input[I]);
    Lo := HexCharToNibble(Input[I + 1]);
    if (Hi < 0) or (Lo < 0) then
      ThrowSyntaxError(SErrorInvalidHexCharacter, SSuggestHexFormat);
    TA.BufferData[TA.ByteOffset + Written] := Byte((Hi shl 4) or Lo);
    Inc(Written);
    Inc(I, 2);
    CharsRead := I - 1;
  end;

  if Written = 0 then
    CharsRead := 0;

  ResultObj := TGocciaObjectValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultObj);
  try
    ResultObj.AssignProperty(PROP_READ, TGocciaNumberLiteralValue.Create(CharsRead));
    ResultObj.AssignProperty(PROP_WRITTEN, TGocciaNumberLiteralValue.Create(Written));
    Result := ResultObj;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultObj);
  end;
end;

// ES2026 §5.2.1 Uint8Array.fromBase64(string [, options])
function TGocciaUint8ArrayEncoding.FromBase64(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Input, Alphabet, LastChunkHandling: string;
  Options: TGocciaValue;
  DecResult: TBase64DecodeResult;
  NewTA: TGocciaTypedArrayValue;
  I: Integer;
begin
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorFromBase64RequiresString, SSuggestStringArgRequired);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorFromBase64FirstArgString, SSuggestStringArgRequired);

  Input := AArgs.GetElement(0).ToStringLiteral.Value;

  Alphabet := ALPHABET_BASE64;
  LastChunkHandling := LAST_CHUNK_LOOSE;
  if AArgs.Length > 1 then
  begin
    Options := AArgs.GetElement(1);
    if not (Options is TGocciaUndefinedLiteralValue) then
    begin
      if not (Options is TGocciaObjectValue) then
        ThrowTypeError(SErrorFromBase64OptionsNotObject, SSuggestBase64Format);
      Alphabet := ReadAlphabetOption(Options);
      LastChunkHandling := ReadLastChunkHandlingOption(Options);
    end;
  end;

  DecResult := DecodeBase64(Input, Alphabet, LastChunkHandling, -1);

  NewTA := TGocciaTypedArrayValue.Create(takUint8, DecResult.ByteLength);
  for I := 0 to DecResult.ByteLength - 1 do
    NewTA.BufferData[NewTA.ByteOffset + I] := DecResult.Bytes[I];

  Result := NewTA;
end;

// ES2026 §5.2.2 Uint8Array.fromHex(string)
function TGocciaUint8ArrayEncoding.FromHex(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  I, ByteLen, Hi, Lo: Integer;
  NewTA: TGocciaTypedArrayValue;
begin
  if AArgs.Length = 0 then
    ThrowTypeError(SErrorFromHexRequiresString, SSuggestStringArgRequired);
  if not (AArgs.GetElement(0) is TGocciaStringLiteralValue) then
    ThrowTypeError(SErrorFromHexFirstArgString, SSuggestStringArgRequired);

  Input := AArgs.GetElement(0).ToStringLiteral.Value;

  if (Length(Input) mod 2) <> 0 then
    ThrowSyntaxError(SErrorInvalidHexOddLength, SSuggestHexFormat);

  ByteLen := Length(Input) div 2;
  NewTA := TGocciaTypedArrayValue.Create(takUint8, ByteLen);

  for I := 0 to ByteLen - 1 do
  begin
    Hi := HexCharToNibble(Input[I * 2 + 1]);
    Lo := HexCharToNibble(Input[I * 2 + 2]);
    if (Hi < 0) or (Lo < 0) then
      ThrowSyntaxError(SErrorInvalidHexCharacter, SSuggestHexFormat);
    NewTA.BufferData[NewTA.ByteOffset + I] := Byte((Hi shl 4) or Lo);
  end;

  Result := NewTA;
end;

end.

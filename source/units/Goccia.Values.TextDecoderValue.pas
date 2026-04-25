unit Goccia.Values.TextDecoderValue;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTextDecoderValue = class(TGocciaInstanceValue)
  private
    FEncoding: string;
    FFatal: Boolean;
    FIgnoreBOM: Boolean;

    // Getters
    function EncodingGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function FatalGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function IgnoreBOMGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    // Methods
    function Decode(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;
    function ToStringTag: string; override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    property Encoding: string read FEncoding;
    property Fatal: Boolean read FFatal;
    property IgnoreBOM: Boolean read FIgnoreBOM;
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TypedArrayValue;

var
  GTextDecoderSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

function GetTextDecoderShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTextDecoderSharedSlot))
  else
    Result := nil;
end;

const
  ENCODING_UTF8 = 'utf-8';

  // UTF-8 BOM bytes (EF BB BF).
  UTF8_BOM_BYTE0 = $EF;
  UTF8_BOM_BYTE1 = $BB;
  UTF8_BOM_BYTE2 = $BF;

  MIN_BOM_LENGTH = 3;

// Map a label string to the WHATWG canonical encoding name.
// Returns an empty string when the label is not supported.
// Per WHATWG Encoding §4.2 — UTF-8 labels only for now; everything else is a
// RangeError at construction time.
function NormalizeEncodingLabel(const ALabel: string): string;
var
  Lower: string;
begin
  Lower := LowerCase(Trim(ALabel));
  if (Lower = 'utf-8') or (Lower = 'utf8') or
     (Lower = 'unicode-1-1-utf-8') or (Lower = 'unicode11utf8') or
     (Lower = 'unicode20utf8') or (Lower = 'x-unicode20utf8') then
    Result := ENCODING_UTF8
  else
    Result := '';
end;

// Validate that AData[AOffset .. AOffset+ALength-1] is well-formed UTF-8.
// Checks leading byte range, continuation bytes, and overlong/surrogate
// sequences per RFC 3629.
function IsValidUTF8(const AData: TBytes; const AOffset, ALength: Integer): Boolean;
var
  I, SeqLen, J: Integer;
  B: Byte;
begin
  Result := True;
  I := AOffset;
  while I < AOffset + ALength do
  begin
    B := AData[I];
    if B and $80 = 0 then
      SeqLen := 1
    else if (B and $E0 = $C0) and (B >= $C2) then
      // $C0/$C1 are overlong encodings of U+0000-U+007F — reject them.
      SeqLen := 2
    else if B and $F0 = $E0 then
      SeqLen := 3
    else if (B and $F8 = $F0) and (B <= $F4) then
      // Limit to U+10FFFF ($F4 80 80 80); $F5-$FF are out of range.
      SeqLen := 4
    else
    begin
      Result := False;
      Exit;
    end;

    // Make sure all continuation bytes are present.
    if I + SeqLen > AOffset + ALength then
    begin
      Result := False;
      Exit;
    end;

    // Second-byte range checks required by RFC 3629 to reject overlong
    // sequences, UTF-16 surrogate pairs, and code points above U+10FFFF.
    if SeqLen = 3 then
    begin
      case B of
        $E0: // Overlong: second byte must be A0-BF (U+0800 minimum).
          if AData[I + 1] < $A0 then begin Result := False; Exit; end;
        $ED: // Surrogate range U+D800-U+DFFF: second byte must be 80-9F.
          if AData[I + 1] > $9F then begin Result := False; Exit; end;
      end;
    end
    else if SeqLen = 4 then
    begin
      case B of
        $F0: // Overlong: second byte must be 90-BF (U+10000 minimum).
          if AData[I + 1] < $90 then begin Result := False; Exit; end;
        $F4: // Limit to U+10FFFF: second byte must be 80-8F.
          if AData[I + 1] > $8F then begin Result := False; Exit; end;
      end;
    end;

    // Validate each continuation byte (must be 10xxxxxx).
    for J := 1 to SeqLen - 1 do
      if (AData[I + J] and $C0) <> $80 then
      begin
        Result := False;
        Exit;
      end;

    Inc(I, SeqLen);
  end;
end;

// WHATWG Encoding §4.1: decode UTF-8 bytes with replacement semantics.
// Each ill-formed byte (invalid leading byte, truncated sequence, overlong,
// surrogate, or out-of-range code point) emits U+FFFD (EF BF BD) and advances
// the cursor by one byte.
function DecodeUTF8WithReplacement(const AData: TBytes;
  const AOffset, ALength: Integer): string;
const
  FFFD_0 = $EF;
  FFFD_1 = $BF;
  FFFD_2 = $BD;
var
  OutBytes: array of Byte;
  OutPos, I, SeqLen, K: Integer;
  B, NextByte: Byte;
  Valid: Boolean;
  U8: UTF8String;
begin
  if ALength = 0 then
  begin
    Result := '';
    Exit;
  end;
  // Worst case: every input byte becomes 3 bytes of U+FFFD.
  SetLength(OutBytes, ALength * 3);
  OutPos := 0;
  I := AOffset;
  while I < AOffset + ALength do
  begin
    B := AData[I];
    // Determine the expected sequence length from the leading byte.
    if B and $80 = 0 then
      SeqLen := 1
    else if (B and $E0 = $C0) and (B >= $C2) then
      SeqLen := 2
    else if B and $F0 = $E0 then
      SeqLen := 3
    else if (B and $F8 = $F0) and (B <= $F4) then
      SeqLen := 4
    else
      SeqLen := 0;

    if SeqLen = 0 then
    begin
      // Invalid leading byte ($80-$BF, $C0, $C1, $F5-$FF).
      OutBytes[OutPos]     := FFFD_0;
      OutBytes[OutPos + 1] := FFFD_1;
      OutBytes[OutPos + 2] := FFFD_2;
      Inc(OutPos, 3);
      Inc(I);
    end
    else if I + SeqLen > AOffset + ALength then
    begin
      // Truncated sequence — not enough bytes remain.
      OutBytes[OutPos]     := FFFD_0;
      OutBytes[OutPos + 1] := FFFD_1;
      OutBytes[OutPos + 2] := FFFD_2;
      Inc(OutPos, 3);
      Inc(I);
    end
    else if SeqLen = 1 then
    begin
      // ASCII — always valid, pass through directly.
      OutBytes[OutPos] := B;
      Inc(OutPos);
      Inc(I);
    end
    else
    begin
      // Multi-byte: validate all continuation bytes (must be 10xxxxxx).
      Valid := True;
      for K := 1 to SeqLen - 1 do
        if (AData[I + K] and $C0) <> $80 then
        begin
          Valid := False;
          Break;
        end;

      // Second-byte range guards (same constraints as IsValidUTF8).
      if Valid then
      begin
        NextByte := AData[I + 1];
        case SeqLen of
          3:
          begin
            if (B = $E0) and (NextByte < $A0) then Valid := False; // overlong
            if (B = $ED) and (NextByte > $9F) then Valid := False; // surrogate
          end;
          4:
          begin
            if (B = $F0) and (NextByte < $90) then Valid := False; // overlong
            if (B = $F4) and (NextByte > $8F) then Valid := False; // > U+10FFFF
          end;
        end;
      end;

      if Valid then
      begin
        for K := 0 to SeqLen - 1 do
          OutBytes[OutPos + K] := AData[I + K];
        Inc(OutPos, SeqLen);
        Inc(I, SeqLen);
      end
      else
      begin
        // Invalid sequence — emit U+FFFD and retreat to next byte.
        OutBytes[OutPos]     := FFFD_0;
        OutBytes[OutPos + 1] := FFFD_1;
        OutBytes[OutPos + 2] := FFFD_2;
        Inc(OutPos, 3);
        Inc(I);
      end;
    end;
  end;

  SetLength(U8, OutPos);
  if OutPos > 0 then
    Move(OutBytes[0], U8[1], OutPos);
  Result := string(U8);
end;

{ TGocciaTextDecoderValue }

constructor TGocciaTextDecoderValue.Create(const AClass: TGocciaClassValue = nil);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FEncoding := ENCODING_UTF8;
  FFatal := False;
  FIgnoreBOM := False;
  InitializePrototype;
  Shared := GetTextDecoderShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

// WHATWG Encoding §8.1 new TextDecoder([label [, options]])
procedure TGocciaTextDecoderValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  LabelArg, OptionsArg, FatalOpt, IgnoreBOMOpt: TGocciaValue;
  Normalized: string;
begin
  // Step 1: If label is provided and not undefined, normalise it.
  if AArguments.Length > 0 then
  begin
    LabelArg := AArguments.GetElement(0);
    if not (LabelArg is TGocciaUndefinedLiteralValue) then
    begin
      Normalized := NormalizeEncodingLabel(LabelArg.ToStringLiteral.Value);
      if Normalized = '' then
        ThrowRangeError(Format(SErrorTextDecoderInvalidEncoding,
          [LabelArg.ToStringLiteral.Value]),
          SSuggestTextDecoderThisType);
      FEncoding := Normalized;
    end;
  end;

  // Step 2: If options are provided, extract fatal and ignoreBOM flags.
  if AArguments.Length > 1 then
  begin
    OptionsArg := AArguments.GetElement(1);
    if not (OptionsArg is TGocciaUndefinedLiteralValue) then
    begin
      if not (OptionsArg is TGocciaObjectValue) then
        ThrowTypeError(SErrorTextDecoderOptionsMustBeObject, SSuggestTextDecoderThisType);

      FatalOpt := TGocciaObjectValue(OptionsArg).GetProperty(PROP_FATAL);
      if Assigned(FatalOpt) and not (FatalOpt is TGocciaUndefinedLiteralValue) then
        FFatal := FatalOpt.ToBooleanLiteral.Value;

      IgnoreBOMOpt := TGocciaObjectValue(OptionsArg).GetProperty(PROP_IGNORE_BOM);
      if Assigned(IgnoreBOMOpt) and
         not (IgnoreBOMOpt is TGocciaUndefinedLiteralValue) then
        FIgnoreBOM := IgnoreBOMOpt.ToBooleanLiteral.Value;
    end;
  end;
end;

procedure TGocciaTextDecoderValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTextDecoderShared) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTextDecoderSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_ENCODING, EncodingGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_FATAL, FatalGetter, nil, [pfConfigurable]);
      Members.AddAccessor(PROP_IGNORE_BOM, IgnoreBOMGetter, nil, [pfConfigurable]);
      Members.AddNamedMethod(PROP_DECODE, Decode, 1, gmkPrototypeMethod,
        [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTextDecoderValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTextDecoderShared;
  if not Assigned(Shared) then
  begin
    TGocciaTextDecoderValue.Create;
    Shared := GetTextDecoderShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTextDecoderValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_TEXT_DECODER;
end;

procedure TGocciaTextDecoderValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
  // No additional TGocciaValue fields — FEncoding is a plain Pascal string.
end;

// WHATWG Encoding §8.2.1 get TextDecoder.prototype.encoding
function TGocciaTextDecoderValue.EncodingGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaTextDecoderValue;
begin
  if not (AThisValue is TGocciaTextDecoderValue) then
    ThrowTypeError(SErrorTextDecoderEncodingIllegalInvocation, SSuggestTextDecoderThisType);
  Obj := TGocciaTextDecoderValue(AThisValue);
  Result := TGocciaStringLiteralValue.Create(Obj.FEncoding);
end;

// WHATWG Encoding §8.2.2 get TextDecoder.prototype.fatal
function TGocciaTextDecoderValue.FatalGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaTextDecoderValue;
begin
  if not (AThisValue is TGocciaTextDecoderValue) then
    ThrowTypeError(SErrorTextDecoderFatalIllegalInvocation, SSuggestTextDecoderThisType);
  Obj := TGocciaTextDecoderValue(AThisValue);
  if Obj.FFatal then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG Encoding §8.2.3 get TextDecoder.prototype.ignoreBOM
function TGocciaTextDecoderValue.IgnoreBOMGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaTextDecoderValue;
begin
  if not (AThisValue is TGocciaTextDecoderValue) then
    ThrowTypeError(SErrorTextDecoderIgnoreBOMIllegalInvocation, SSuggestTextDecoderThisType);
  Obj := TGocciaTextDecoderValue(AThisValue);
  if Obj.FIgnoreBOM then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

// WHATWG Encoding §8.2.4 TextDecoder.prototype.decode([input [, options]])
function TGocciaTextDecoderValue.Decode(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Obj: TGocciaTextDecoderValue;
  InputArg: TGocciaValue;
  Data: TBytes;
  Offset, DataLen: Integer;
  U8: UTF8String;
begin
  if not (AThisValue is TGocciaTextDecoderValue) then
    ThrowTypeError(SErrorTextDecoderDecodeIllegalInvocation, SSuggestTextDecoderThisType);
  Obj := TGocciaTextDecoderValue(AThisValue);

  // No input or undefined input — return empty string.
  if (AArgs.Length = 0) or (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  InputArg := AArgs.GetElement(0);
  Offset := 0;
  DataLen := 0;

  // Extract raw bytes and byte-range from the buffer source.
  if InputArg is TGocciaTypedArrayValue then
  begin
    Data := TGocciaTypedArrayValue(InputArg).BufferData;
    Offset := TGocciaTypedArrayValue(InputArg).ByteOffset;
    DataLen := TGocciaTypedArrayValue(InputArg).Length *
      TGocciaTypedArrayValue.BytesPerElement(
        TGocciaTypedArrayValue(InputArg).Kind);
  end
  else if InputArg is TGocciaArrayBufferValue then
  begin
    Data := TGocciaArrayBufferValue(InputArg).Data;
    Offset := 0;
    DataLen := Length(Data);
  end
  else
    ThrowTypeError(SErrorTextDecoderDecodeInputType, SSuggestTextDecoderThisType);

  // Strip the UTF-8 BOM (EF BB BF) unless ignoreBOM is true.
  if not Obj.FIgnoreBOM and (DataLen >= MIN_BOM_LENGTH) and
     (Data[Offset] = UTF8_BOM_BYTE0) and
     (Data[Offset + 1] = UTF8_BOM_BYTE1) and
     (Data[Offset + 2] = UTF8_BOM_BYTE2) then
  begin
    Inc(Offset, MIN_BOM_LENGTH);
    Dec(DataLen, MIN_BOM_LENGTH);
  end;

  // WHATWG Encoding §8.2.4 step 5: fatal vs. replacement decoding.
  if Obj.FFatal then
  begin
    // Fatal mode: reject any ill-formed byte sequence with a TypeError.
    if not IsValidUTF8(Data, Offset, DataLen) then
      ThrowTypeError(SErrorTextDecoderDecodeInvalidUTF8, SSuggestTextDecoderThisType);
    // All bytes are well-formed — raw copy is safe.
    SetLength(U8, DataLen);
    if DataLen > 0 then
      Move(Data[Offset], U8[1], DataLen);
    Result := TGocciaStringLiteralValue.Create(string(U8));
  end
  else
    // Non-fatal mode: replace each ill-formed sequence with U+FFFD.
    Result := TGocciaStringLiteralValue.Create(
      DecodeUTF8WithReplacement(Data, Offset, DataLen));
end;

initialization
  GTextDecoderSharedSlot := RegisterRealmOwnedSlot('TextDecoder.shared');

end.

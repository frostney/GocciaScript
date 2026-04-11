unit Goccia.Values.TextEncoderValue;

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
  TGocciaTextEncoderValue = class(TGocciaInstanceValue)
  private
    class var FShared: TGocciaSharedPrototype;
    class var FPrototypeMembers: array of TGocciaMemberDefinition;
  private
    // Getter
    function EncodingGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    // Methods
    function Encode(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function EncodeInto(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
  public
    constructor Create(const AClass: TGocciaClassValue = nil);

    function ToStringTag: string; override;
    procedure MarkReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);
  end;

implementation

uses
  SysUtils,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.GarbageCollector,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.TypedArrayValue;

const
  ENCODING_UTF8 = 'utf-8';

  // UTF-8 encoding of U+FFFD REPLACEMENT CHARACTER (EF BF BD).
  REPLACEMENT_BYTE0 = $EF;
  REPLACEMENT_BYTE1 = $BF;
  REPLACEMENT_BYTE2 = $BD;

// Convert an FPC string (stored as UTF-8) to a raw byte array of UTF-8 octets,
// normalising lone surrogate code points (U+D800-U+DFFF) to U+FFFD per the
// WHATWG USVString requirement (Encoding §8.3.2 encode).
//
// Surrogates appear as the three-byte sequence ED A0-BF 80-BF in CESU-8/WTF-8
// style storage.  Since U+FFFD also encodes to exactly 3 bytes (EF BF BD),
// the replacement can be done in-place without resizing the output buffer.
function StringToUTF8Bytes(const AStr: string): TBytes;
var
  U8: UTF8String;
  Len, I: Integer;
  B: Byte;
begin
  U8 := UTF8String(AStr);
  Len := Length(U8);
  SetLength(Result, Len);
  if Len > 0 then
    Move(U8[1], Result[0], Len);

  // Scan for 3-byte sequences in the surrogate range (ED A0 xx .. ED BF xx)
  // and replace each with the U+FFFD replacement character (EF BF BD).
  I := 0;
  while I < Len do
  begin
    B := Result[I];
    if B and $80 = 0 then
      // ASCII — 1 byte
      Inc(I)
    else if (B and $E0 = $C0) and (I + 1 < Len) then
      // 2-byte sequence
      Inc(I, 2)
    else if (B and $F0 = $E0) and (I + 2 < Len) then
    begin
      // 3-byte sequence — check for surrogate range (ED A0-BF continuation)
      if (B = $ED) and (Result[I + 1] >= $A0) then
      begin
        // Lone surrogate: replace in-place with U+FFFD.
        Result[I]     := REPLACEMENT_BYTE0;
        Result[I + 1] := REPLACEMENT_BYTE1;
        Result[I + 2] := REPLACEMENT_BYTE2;
      end;
      Inc(I, 3);
    end
    else if (B and $F8 = $F0) and (I + 3 < Len) then
      // 4-byte sequence
      Inc(I, 4)
    else
      // Invalid leading byte — step one byte to avoid infinite loop.
      Inc(I);
  end;
end;

// Return the byte-length of the UTF-8 sequence that starts with AByte.
// Returns 1 for invalid leading bytes so the encoder can skip them gracefully.
function UTF8SeqLen(const AByte: Byte): Integer;
begin
  if AByte and $80 = 0 then
    Result := 1
  else if AByte and $E0 = $C0 then
    Result := 2
  else if AByte and $F0 = $E0 then
    Result := 3
  else if AByte and $F8 = $F0 then
    Result := 4
  else
    Result := 1;
end;

{ TGocciaTextEncoderValue }

constructor TGocciaTextEncoderValue.Create(const AClass: TGocciaClassValue = nil);
begin
  inherited Create(AClass);
  InitializePrototype;
  if not Assigned(AClass) and Assigned(FShared) then
    FPrototype := FShared.Prototype;
end;

procedure TGocciaTextEncoderValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
begin
  if Assigned(FShared) then Exit;

  FShared := TGocciaSharedPrototype.Create(Self);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor(PROP_ENCODING, EncodingGetter, nil, [pfConfigurable]);
      Members.AddNamedMethod(PROP_ENCODE, Encode, 1, gmkPrototypeMethod,
        [gmfNoFunctionPrototype]);
      Members.AddNamedMethod(PROP_ENCODE_INTO, EncodeInto, 2, gmkPrototypeMethod,
        [gmfNoFunctionPrototype]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(FShared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTextEncoderValue.ExposePrototype(const AConstructor: TGocciaValue);
begin
  if not Assigned(FShared) then
    TGocciaTextEncoderValue.Create;
  ExposeSharedPrototypeOnConstructor(FShared, AConstructor);
end;

function TGocciaTextEncoderValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_TEXT_ENCODER;
end;

procedure TGocciaTextEncoderValue.MarkReferences;
begin
  if GCMarked then Exit;
  inherited;
end;

// WHATWG Encoding §8.3.1 get TextEncoder.prototype.encoding
function TGocciaTextEncoderValue.EncodingGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  // TextEncoder always uses UTF-8 — no instance state required.
  Result := TGocciaStringLiteralValue.Create(ENCODING_UTF8);
end;

// WHATWG Encoding §8.3.2 TextEncoder.prototype.encode(input = '')
function TGocciaTextEncoderValue.Encode(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Input: string;
  Bytes: TBytes;
  NewTA: TGocciaTypedArrayValue;
  I: Integer;
begin
  // Step 1: Let input be the empty string.
  Input := '';
  // Step 2: If input is given, convert to a scalar-value string.
  if (AArgs.Length > 0) and not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    Input := AArgs.GetElement(0).ToStringLiteral.Value;

  // Step 3: Run UTF-8 encode on the USVString (lone surrogates → U+FFFD)
  // and return the resulting Uint8Array.
  Bytes := StringToUTF8Bytes(Input);
  NewTA := TGocciaTypedArrayValue.Create(takUint8, Length(Bytes));
  for I := 0 to Length(Bytes) - 1 do
    NewTA.BufferData[NewTA.ByteOffset + I] := Bytes[I];
  Result := NewTA;
end;

// WHATWG Encoding §8.3.3 TextEncoder.prototype.encodeInto(source, destination)
function TGocciaTextEncoderValue.EncodeInto(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Source: string;
  Dest: TGocciaTypedArrayValue;
  Bytes: TBytes;
  I, J, SeqLen, Written, Read, DestLen: Integer;
  ResultObj: TGocciaObjectValue;
begin
  if AArgs.Length < 2 then
    ThrowTypeError(
      'TextEncoder.prototype.encodeInto requires source and destination arguments');

  if AArgs.GetElement(0) is TGocciaUndefinedLiteralValue then
    Source := ''
  else
    Source := AArgs.GetElement(0).ToStringLiteral.Value;

  if not (AArgs.GetElement(1) is TGocciaTypedArrayValue) then
    ThrowTypeError(
      'TextEncoder.prototype.encodeInto: destination must be a Uint8Array');
  Dest := TGocciaTypedArrayValue(AArgs.GetElement(1));
  if Dest.Kind <> takUint8 then
    ThrowTypeError(
      'TextEncoder.prototype.encodeInto: destination must be a Uint8Array');

  // Convert source to USVString bytes (lone surrogates normalized to U+FFFD).
  Bytes := StringToUTF8Bytes(Source);
  DestLen := Dest.Length;
  Written := 0;
  Read := 0;
  I := 0;

  while I < Length(Bytes) do
  begin
    SeqLen := UTF8SeqLen(Bytes[I]);
    // Stop before writing a sequence that would overflow the destination.
    if Written + SeqLen > DestLen then Break;

    for J := 0 to SeqLen - 1 do
      Dest.BufferData[Dest.ByteOffset + Written + J] := Bytes[I + J];

    Inc(Written, SeqLen);
    Inc(I, SeqLen);
    // Supplementary-plane code points (4-byte sequences) count as 2 UTF-16
    // code units; everything else counts as 1.
    if SeqLen = 4 then
      Inc(Read, 2)
    else
      Inc(Read);
  end;

  // Return { read, written }.
  ResultObj := TGocciaObjectValue.Create;
  TGarbageCollector.Instance.AddTempRoot(ResultObj);
  try
    ResultObj.AssignProperty(PROP_READ, TGocciaNumberLiteralValue.Create(Read));
    ResultObj.AssignProperty(PROP_WRITTEN, TGocciaNumberLiteralValue.Create(Written));
    Result := ResultObj;
  finally
    TGarbageCollector.Instance.RemoveTempRoot(ResultObj);
  end;
end;

end.

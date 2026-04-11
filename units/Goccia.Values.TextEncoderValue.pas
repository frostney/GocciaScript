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

// Convert an FPC string (stored as UTF-8) to a raw byte array of UTF-8 octets.
function StringToUTF8Bytes(const AStr: string): TBytes;
var
  U8: UTF8String;
begin
  U8 := UTF8String(AStr);
  SetLength(Result, Length(U8));
  if Length(U8) > 0 then
    Move(U8[1], Result[0], Length(U8));
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
      Members.AddNamedMethod('encode', Encode, 1, gmkPrototypeMethod,
        [gmfNoFunctionPrototype]);
      Members.AddNamedMethod('encodeInto', EncodeInto, 2, gmkPrototypeMethod,
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

  // Step 3: Run UTF-8 encode on input and return the resulting Uint8Array.
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

  // Convert source to UTF-8 bytes in a temporary buffer.
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

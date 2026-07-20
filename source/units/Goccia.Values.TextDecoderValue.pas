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

  TextEncoding,

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

function GetTextDecoderShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
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
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetTextDecoderShared <> nil) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTextDecoderSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    Members.AddAccessor(PROP_ENCODING, EncodingGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_FATAL, FatalGetter, nil, [pfConfigurable]);
    Members.AddAccessor(PROP_IGNORE_BOM, IgnoreBOMGetter, nil, [pfConfigurable]);
    Members.AddNamedMethod(PROP_DECODE, Decode, 1, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
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
  Data, Bytes: TBytes;
  Offset, DataLen, ErrorOffset: Integer;
  Decoded: string;
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

  SetLength(Bytes, DataLen);
  if DataLen > 0 then
    Move(Data[Offset], Bytes[0], DataLen);

  // WHATWG Encoding §8.2.4 step 5: fatal vs. replacement decoding.
  if Obj.FFatal then
  begin
    if not TryDecodeUTF8(Bytes, Decoded, ErrorOffset) then
      ThrowTypeError(SErrorTextDecoderDecodeInvalidUTF8, SSuggestTextDecoderThisType);
    Result := TGocciaStringLiteralValue.Create(Decoded);
  end
  else
    Result := TGocciaStringLiteralValue.Create(
      TextEncoding.DecodeUTF8WithReplacement(Bytes));
end;

initialization
  GTextDecoderSharedSlot := RegisterRealmOwnedSlot('TextDecoder.shared');

end.

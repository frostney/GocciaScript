unit Goccia.Values.BlobValue;

// File API Blob
// https://w3c.github.io/FileAPI/#blob-section

{$I Goccia.inc}

interface

uses
  SysUtils,

  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ClassValue,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  { A Blob's byte sequence is immutable once made, so its bytes are held as
    one TBytes, reserved against the memory limit on this thread's
    collector for the Blob's life (as a Response body is). }
  TGocciaBlobValue = class(TGocciaInstanceValue)
  private
    FBytes: TBytes;
    FType: string;
    { How many of FBytes' bytes are reserved on this thread's collector
      (TGarbageCollector.TryReserveExternalBytes); released with the Blob. }
    FChargedBytes: Int64;

    function SizeGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function TypeGetter(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BlobSlice(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BlobText(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BlobArrayBuffer(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;
    function BlobBytes(const AArgs: TGocciaArgumentsCollection;
      const AThisValue: TGocciaValue): TGocciaValue;

    procedure InitializePrototype;
    procedure ReleaseBytes;
    { Reserves ALength bytes on this thread's collector, or throws the
      engine's memory-limit RangeError when they do not fit. }
    procedure ReserveBytes(const ALength: Int64);
    { Takes ABytes as the byte sequence; the caller has reserved
      Length(ABytes) on this Blob's behalf (ReserveBytes). }
    procedure AdoptBytes(const ABytes: TBytes; const AType: string);
  public
    constructor Create(const AClass: TGocciaClassValue = nil);
    destructor Destroy; override;

    { For embedders: a new Blob holding a copy of ABytes with the media type
      AType, normalised as the constructor's `type` option is (lower case,
      or the empty string when it has a character outside U+0020-U+007E).
      The bytes are reserved against the memory limit; when they do not fit
      it throws the engine's catchable RangeError, as `new Blob` does. The
      caller roots the result like any new value. }
    class function CreateFromBytes(const ABytes: TBytes;
      const AType: string = ''): TGocciaBlobValue;

    procedure InitializeNativeFromArguments(
      const AArguments: TGocciaArgumentsCollection); override;

    function ToStringTag: string; override;
    procedure TraceReferences; override;

    class procedure ExposePrototype(const AConstructor: TGocciaValue);

    { The byte sequence. Treat it as read-only: a Blob's bytes never
      change, and the array may be shared with copies of it. }
    property Bytes: TBytes read FBytes;
    { The normalised media type, `blob.type`. }
    property BlobType: string read FType;
  end;

{ File API §3.1 constructor steps 3, and §2's slice blob step for
  contentType: the empty string when AType has a character outside
  U+0020-U+007E, else AType in ASCII lower case. }
function NormalizeBlobType(const AType: string): string;

implementation

uses
  Math,

  TextEncoding,

  Goccia.Constants.ConstructorNames,
  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.GarbageCollector,
  Goccia.Realm,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.DataViewValue,
  Goccia.Values.Error,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IteratorSupport,
  Goccia.Values.IteratorValue,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.PromiseValue,
  Goccia.Values.SharedArrayBufferValue,
  Goccia.Values.SymbolValue,
  Goccia.Values.TypedArrayValue,
  Goccia.WebIDL.BufferSource;

const
  ENDINGS_TRANSPARENT = 'transparent';
  ENDINGS_NATIVE = 'native';
  { A Blob's size is held as one TBytes and read back through ArrayBuffer
    and Uint8Array, whose lengths are Integer. }
  MAX_BLOB_SIZE = High(Integer);
  { Web IDL §3.2.4.9 ConvertToInt: the bounds of a 64-bit integer type. }
  MAX_SAFE_LONG_LONG = 9007199254740991.0;

var
  GBlobSharedSlot: TGocciaRealmOwnedSlotId;

function GetBlobShared: TGocciaSharedPrototype; {$IFDEF FPC}inline;{$ENDIF}
begin
  if (CurrentRealm <> nil) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GBlobSharedSlot))
  else
    Result := nil;
end;

function NormalizeBlobType(const AType: string): string;
var
  I: Integer;
  C: Char;
begin
  Result := AType;
  for I := 1 to Length(Result) do
  begin
    C := Result[I];
    if (Ord(C) < $20) or (Ord(C) > $7E) then
      Exit('');
    if (C >= 'A') and (C <= 'Z') then
      Result[I] := Char(Ord(C) + 32);
  end;
end;

// ---------------------------------------------------------------------------
// USVString parts: UTF-8 encoding with lone surrogates as U+FFFD (Web IDL
// USVString conversion), and File API §3.1.1 "convert line endings to
// native" applied on the way when endings is "native". Measured first and
// written second, so a part repeated many times is never encoded into a
// buffer of its own.
// ---------------------------------------------------------------------------

{ The code point at AIndex (1-based) of AText and how many code units it
  takes; a lone surrogate reads as U+FFFD. }
function CodePointAt(const AText: string; const AIndex: Integer;
  out AUnits: Integer): Cardinal;
var
  High_, Low_: Cardinal;
begin
  High_ := Ord(AText[AIndex]);
  AUnits := 1;
  if (High_ >= $D800) and (High_ <= $DBFF) and (AIndex < Length(AText)) then
  begin
    Low_ := Ord(AText[AIndex + 1]);
    if (Low_ >= $DC00) and (Low_ <= $DFFF) then
    begin
      AUnits := 2;
      Exit($10000 + ((High_ - $D800) shl 10) + (Low_ - $DC00));
    end;
  end;
  if (High_ >= $D800) and (High_ <= $DFFF) then
    Result := $FFFD
  else
    Result := High_;
end;

function UTF8Length(const ACodePoint: Cardinal): Integer; {$IFDEF FPC}inline;{$ENDIF}
begin
  if ACodePoint < $80 then
    Result := 1
  else if ACodePoint < $800 then
    Result := 2
  else if ACodePoint < $10000 then
    Result := 3
  else
    Result := 4;
end;

procedure PutUTF8(const ACodePoint: Cardinal; var ADest: TBytes;
  var AOffset: Int64);
begin
  if ACodePoint < $80 then
  begin
    ADest[AOffset] := Byte(ACodePoint);
    Inc(AOffset);
  end
  else if ACodePoint < $800 then
  begin
    ADest[AOffset] := Byte($C0 or (ACodePoint shr 6));
    ADest[AOffset + 1] := Byte($80 or (ACodePoint and $3F));
    Inc(AOffset, 2);
  end
  else if ACodePoint < $10000 then
  begin
    ADest[AOffset] := Byte($E0 or (ACodePoint shr 12));
    ADest[AOffset + 1] := Byte($80 or ((ACodePoint shr 6) and $3F));
    ADest[AOffset + 2] := Byte($80 or (ACodePoint and $3F));
    Inc(AOffset, 3);
  end
  else
  begin
    ADest[AOffset] := Byte($F0 or (ACodePoint shr 18));
    ADest[AOffset + 1] := Byte($80 or ((ACodePoint shr 12) and $3F));
    ADest[AOffset + 2] := Byte($80 or ((ACodePoint shr 6) and $3F));
    ADest[AOffset + 3] := Byte($80 or (ACodePoint and $3F));
    Inc(AOffset, 4);
  end;
end;

{ File API §3.1.1 "convert line endings to native": the native line ending
  is CR LF where the platform's convention is CR LF, else LF; a CR LF pair,
  a lone CR and a lone LF each become one native line ending. }
const
{$IFDEF MSWINDOWS}
  NATIVE_LINE_ENDING_BYTES = 2;
{$ELSE}
  NATIVE_LINE_ENDING_BYTES = 1;
{$ENDIF}

procedure PutNativeLineEnding(var ADest: TBytes; var AOffset: Int64);
begin
{$IFDEF MSWINDOWS}
  ADest[AOffset] := 13;
  Inc(AOffset);
{$ENDIF}
  ADest[AOffset] := 10;
  Inc(AOffset);
end;

{ Walks AText as process blob parts encodes it. With ADest nil it only
  counts; otherwise it writes at AOffset, which ADest has room for. Returns
  the number of bytes. }
function EncodeBlobString(const AText: string; const ANative: Boolean;
  const AWrite: Boolean; var ADest: TBytes; var AOffset: Int64): Int64;
var
  I, Units: Integer;
  CodePoint: Cardinal;
begin
  Result := 0;
  I := 1;
  while I <= Length(AText) do
  begin
    if ANative and ((AText[I] = #13) or (AText[I] = #10)) then
    begin
      // A CR followed by an LF is one line ending.
      if (AText[I] = #13) and (I < Length(AText)) and (AText[I + 1] = #10) then
        Inc(I, 2)
      else
        Inc(I);
      Inc(Result, NATIVE_LINE_ENDING_BYTES);
      if AWrite then
        PutNativeLineEnding(ADest, AOffset);
      Continue;
    end;
    CodePoint := CodePointAt(AText, I, Units);
    Inc(I, Units);
    Inc(Result, UTF8Length(CodePoint));
    if AWrite then
      PutUTF8(CodePoint, ADest, AOffset);
  end;
end;

{ The number of bytes "get a copy of the bytes held by the buffer source"
  returns for ASource, without copying them. }
function HeldByteLength(const ASource: TGocciaValue): Int64;
var
  Buffer: TGocciaValue;
begin
  if ASource is TGocciaTypedArrayValue then
  begin
    Buffer := TGocciaTypedArrayValue(ASource).BufferValue;
    Result := Int64(TGocciaTypedArrayValue(ASource).Length) *
      TGocciaTypedArrayValue.BytesPerElement(TGocciaTypedArrayValue(ASource).Kind);
  end
  else if ASource is TGocciaDataViewValue then
  begin
    Buffer := TGocciaDataViewValue(ASource).BufferValue;
    Result := TGocciaDataViewValue(ASource).ByteLengthSlot;
  end
  else
  begin
    Buffer := ASource;
    Result := Length(TGocciaArrayBufferValue(ASource).Data);
  end;
  if (Buffer is TGocciaArrayBufferValue) and
     TGocciaArrayBufferValue(Buffer).Detached then
    Result := 0;
end;

// ---------------------------------------------------------------------------
// Web IDL conversions
// ---------------------------------------------------------------------------

{ Web IDL §3.2.25 union conversion for BlobPart (BufferSource or Blob or
  USVString): a Blob is itself, an object with [[ArrayBufferData]],
  [[DataView]] or [[TypedArrayName]] converts as a BufferSource (no
  [AllowShared], no [AllowResizable]: a TypeError otherwise), anything else
  as a USVString. Returns the part to keep: the Blob, the buffer source or
  a string value. }
function ConvertBlobPart(const AValue: TGocciaValue): TGocciaValue;
begin
  if AValue is TGocciaBlobValue then
    Result := AValue
  else if (AValue is TGocciaArrayBufferValue) or
          (AValue is TGocciaSharedArrayBufferValue) or
          (AValue is TGocciaTypedArrayValue) or
          (AValue is TGocciaDataViewValue) then
  begin
    ConvertToBufferSource(AValue, SErrorBlobPartContext, SSuggestBlobParts);
    Result := AValue;
  end
  else
    Result := TGocciaStringLiteralValue.Create(
      AValue.ToStringLiteral.Value);
end;

{ Web IDL §3.2.4.9 ConvertToInt for [Clamp] long long, as a Double. }
function ClampLongLong(const AValue: TGocciaValue): Double;
var
  X, Floored, Fraction: Double;
  Num: TGocciaNumberLiteralValue;
begin
  Num := AValue.ToNumberLiteral;
  if Num.IsNaN then
    Exit(0);
  X := Num.Value;
  if X > MAX_SAFE_LONG_LONG then
    X := MAX_SAFE_LONG_LONG
  else if X < -MAX_SAFE_LONG_LONG then
    X := -MAX_SAFE_LONG_LONG;
  // Round to the nearest integer, ties to even; -0 becomes +0.
  if (X >= 0) or (Int(X) = X) then
    Floored := Int(X)
  else
    Floored := Int(X) - 1;
  Fraction := X - Floored;
  if Fraction > 0.5 then
    Result := Floored + 1
  else if Fraction < 0.5 then
    Result := Floored
  else if Frac(Floored / 2) = 0 then
    Result := Floored
  else
    Result := Floored + 1;
  if Result = 0 then
    Result := 0;
end;

// ---------------------------------------------------------------------------
// TGocciaBlobValue
// ---------------------------------------------------------------------------

constructor TGocciaBlobValue.Create(const AClass: TGocciaClassValue);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(AClass);
  FBytes := nil;
  FType := '';
  FChargedBytes := 0;
  InitializePrototype;
  Shared := GetBlobShared;
  if not Assigned(AClass) and Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

destructor TGocciaBlobValue.Destroy;
begin
  ReleaseBytes;
  inherited;
end;

procedure TGocciaBlobValue.ReleaseBytes;
var
  GC: TGarbageCollector;
begin
  FBytes := nil;
  if FChargedBytes <= 0 then
    Exit;
  GC := TGarbageCollector.Instance;
  if Assigned(GC) then
    GC.ReleaseExternalBytes(FChargedBytes);
  FChargedBytes := 0;
end;

procedure TGocciaBlobValue.ReserveBytes(const ALength: Int64);
var
  GC: TGarbageCollector;
begin
  if ALength > MAX_BLOB_SIZE then
    ThrowRangeError(Format(SErrorBlobTooLarge, [MAX_BLOB_SIZE]),
      SSuggestMemoryLimitExceeded);
  GC := TGarbageCollector.Instance;
  if Assigned(GC) and (ALength > 0) then
  begin
    if not GC.TryReserveExternalBytes(ALength, Self) then
      ThrowRangeError(SErrorMemoryLimitExceeded, SSuggestMemoryLimitExceeded);
  end;
end;

procedure TGocciaBlobValue.AdoptBytes(const ABytes: TBytes;
  const AType: string);
begin
  ReleaseBytes;
  FBytes := ABytes;
  if Assigned(TGarbageCollector.Instance) then
    FChargedBytes := Length(ABytes)
  else
    FChargedBytes := 0;
  FType := AType;
end;

class function TGocciaBlobValue.CreateFromBytes(const ABytes: TBytes;
  const AType: string): TGocciaBlobValue;
var
  Roots: TGocciaActiveRootFrame;
  Copied: TBytes;
begin
  Result := TGocciaBlobValue.Create;
  Roots.Initialize;
  try
    Roots.Add(Result);
    Result.ReserveBytes(Length(ABytes));
    try
      Copied := Copy(ABytes);
    except
      if Assigned(TGarbageCollector.Instance) and (Length(ABytes) > 0) then
        TGarbageCollector.Instance.ReleaseExternalBytes(Length(ABytes));
      raise;
    end;
    Result.AdoptBytes(Copied, NormalizeBlobType(AType));
  finally
    Roots.Clear;
  end;
end;

procedure TGocciaBlobValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
  PrototypeMembers: TArray<TGocciaMemberDefinition>;
begin
  if (CurrentRealm = nil) then Exit;
  if (GetBlobShared <> nil) then Exit;

  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GBlobSharedSlot, Shared);
  Members := TGocciaMemberCollection.Create;
  try
    // File API §3: the interface's members, in IDL order.
    Members.AddAccessor(PROP_SIZE, SizeGetter, nil,
      [pfConfigurable, pfEnumerable]);
    Members.AddAccessor(PROP_TYPE, TypeGetter, nil,
      [pfConfigurable, pfEnumerable]);
    Members.AddNamedMethod(PROP_SLICE, BlobSlice, 0, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_TEXT, BlobText, 0, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_ARRAY_BUFFER_METHOD, BlobArrayBuffer, 0,
      gmkPrototypeMethod, [gmfNoFunctionPrototype]);
    Members.AddNamedMethod(PROP_BYTES, BlobBytes, 0, gmkPrototypeMethod,
      [gmfNoFunctionPrototype]);
    // WebIDL: the class string lives on the interface prototype.
    Members.AddSymbolDataProperty(
      TGocciaSymbolValue.WellKnownToStringTag,
      TGocciaStringLiteralValue.Create(CONSTRUCTOR_BLOB),
      [pfConfigurable]);
    PrototypeMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(Shared.Prototype, PrototypeMembers);
end;

class procedure TGocciaBlobValue.ExposePrototype(const AConstructor: TGocciaValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetBlobShared;
  if not Assigned(Shared) then
  begin
    TGocciaBlobValue.Create;
    Shared := GetBlobShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

// File API §3.1: new Blob(blobParts?, options?)
procedure TGocciaBlobValue.InitializeNativeFromArguments(
  const AArguments: TGocciaArgumentsCollection);
var
  Roots: TGocciaActiveRootFrame;
  PartsArg, OptionsArg, Item, Part, Field: TGocciaValue;
  Iterator: TGocciaIteratorValue;
  Parts: array of TGocciaValue;
  Count, I: Integer;
  Done, Native: Boolean;
  BlobType, Endings: string;
  Total, Offset, PartLength: Int64;
  Bytes, Held: TBytes;
  Reserved: Boolean;
begin
  // Step 1: no parameters is an empty Blob with an empty type.
  if AArguments.Length = 0 then
    Exit;

  Roots.Initialize;
  try
    Roots.Add(Self);
    Count := 0;
    SetLength(Parts, 0);

    // Web IDL converts the arguments in order: blobParts, an optional
    // sequence<BlobPart>, first. undefined is "not given"; anything else
    // must be an object with a @@iterator method, and each element converts
    // to BlobPart as the iterator yields it (Web IDL §3.2.21.1, which does
    // not close the iterator when a conversion throws).
    PartsArg := AArguments.GetElement(0);
    if not (PartsArg is TGocciaUndefinedLiteralValue) then
    begin
      if not (PartsArg is TGocciaObjectValue) then
        ThrowTypeError(SErrorBlobPartsNotSequence, SSuggestBlobParts);
      Iterator := GetIteratorFromValue(PartsArg);
      if not Assigned(Iterator) then
        ThrowTypeError(SErrorBlobPartsNotSequence, SSuggestBlobParts);
      Roots.Add(Iterator);
      Item := Iterator.DirectNext(Done);
      while not Done do
      begin
        Roots.Add(Item);
        Part := ConvertBlobPart(Item);
        Roots.Add(Part);
        if Count = Length(Parts) then
          SetLength(Parts, Max(8, Count * 2));
        Parts[Count] := Part;
        Inc(Count);
        Item := Iterator.DirectNext(Done);
      end;
    end;

    // options, a BlobPropertyBag: undefined and null are the defaults, any
    // other non-object a TypeError; members convert in lexicographic order,
    // endings before type.
    Native := False;
    BlobType := '';
    if AArguments.Length > 1 then
    begin
      OptionsArg := AArguments.GetElement(1);
      if not ((OptionsArg is TGocciaUndefinedLiteralValue) or
              (OptionsArg is TGocciaNullLiteralValue)) then
      begin
        if not (OptionsArg is TGocciaObjectValue) then
          ThrowTypeError(SErrorBlobOptionsNotObject, SSuggestBlobOptions);
        Field := TGocciaObjectValue(OptionsArg).GetProperty(PROP_ENDINGS);
        if Assigned(Field) and not (Field is TGocciaUndefinedLiteralValue) then
        begin
          Endings := Field.ToStringLiteral.Value;
          if Endings = ENDINGS_NATIVE then
            Native := True
          else if Endings <> ENDINGS_TRANSPARENT then
            ThrowTypeError(Format(SErrorBlobEndings, [Endings]),
              SSuggestBlobOptions);
        end;
        Field := TGocciaObjectValue(OptionsArg).GetProperty(PROP_TYPE);
        if Assigned(Field) and not (Field is TGocciaUndefinedLiteralValue) then
          BlobType := Field.ToStringLiteral.Value;
      end;
    end;

    // Step 2, process blob parts. No script code runs from here on, so the
    // buffers' bytes are read as they are now (a later element's toString
    // may have changed or detached an earlier one, which the copy sees).
    // The whole size is reserved before anything is allocated.
    Total := 0;
    Offset := 0;
    for I := 0 to Count - 1 do
    begin
      Part := Parts[I];
      if Part is TGocciaBlobValue then
        PartLength := Length(TGocciaBlobValue(Part).FBytes)
      else if Part is TGocciaStringLiteralValue then
        PartLength := EncodeBlobString(TGocciaStringLiteralValue(Part).Value,
          Native, False, Bytes, Offset)
      else
        PartLength := HeldByteLength(Part);
      Inc(Total, PartLength);
      if Total > MAX_BLOB_SIZE then
        ThrowRangeError(Format(SErrorBlobTooLarge, [MAX_BLOB_SIZE]),
          SSuggestMemoryLimitExceeded);
    end;

    ReserveBytes(Total);
    Reserved := Assigned(TGarbageCollector.Instance) and (Total > 0);
    try
      SetLength(Bytes, Total);
      Offset := 0;
      for I := 0 to Count - 1 do
      begin
        Part := Parts[I];
        if Part is TGocciaBlobValue then
        begin
          PartLength := Length(TGocciaBlobValue(Part).FBytes);
          if PartLength > 0 then
            Move(TGocciaBlobValue(Part).FBytes[0], Bytes[Offset], PartLength);
          Inc(Offset, PartLength);
        end
        else if Part is TGocciaStringLiteralValue then
          EncodeBlobString(TGocciaStringLiteralValue(Part).Value, Native,
            True, Bytes, Offset)
        else
        begin
          Held := GetCopyOfBytesHeldByBufferSource(Part);
          if Length(Held) > 0 then
            Move(Held[0], Bytes[Offset], Length(Held));
          Inc(Offset, Length(Held));
          Held := nil;
        end;
      end;
    except
      if Reserved then
        TGarbageCollector.Instance.ReleaseExternalBytes(Total);
      raise;
    end;

    // Step 3: the type, normalised.
    AdoptBytes(Bytes, NormalizeBlobType(BlobType));
  finally
    Roots.Clear;
  end;
end;

function TGocciaBlobValue.ToStringTag: string;
begin
  Result := CONSTRUCTOR_BLOB;
end;

procedure TGocciaBlobValue.TraceReferences;
begin
  inherited;
end;

// ---------------------------------------------------------------------------
// Attributes
// ---------------------------------------------------------------------------

// File API §3.2 size
function TGocciaBlobValue.SizeGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBlobValue) then
    ThrowTypeError(Format(SErrorBlobIllegalInvocation, ['Blob.prototype.size']),
      SSuggestBlobThisType);
  Result := TGocciaNumberLiteralValue.Create(
    Length(TGocciaBlobValue(AThisValue).FBytes));
end;

// File API §3.2 type
function TGocciaBlobValue.TypeGetter(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
begin
  if not (AThisValue is TGocciaBlobValue) then
    ThrowTypeError(Format(SErrorBlobIllegalInvocation, ['Blob.prototype.type']),
      SSuggestBlobThisType);
  Result := TGocciaStringLiteralValue.Create(TGocciaBlobValue(AThisValue).FType);
end;

// ---------------------------------------------------------------------------
// Methods
// ---------------------------------------------------------------------------

// File API §3.3.1 slice(start?, end?, contentType?) and §2 slice blob
function TGocciaBlobValue.BlobSlice(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  Source, Sliced: TGocciaBlobValue;
  OriginalSize, RelativeStart, RelativeEnd, X: Double;
  ContentType: string;
  Span: Int64;
  Bytes: TBytes;
  Roots: TGocciaActiveRootFrame;
begin
  // Web IDL: the receiver is checked before the arguments convert.
  if not (AThisValue is TGocciaBlobValue) then
    ThrowTypeError(Format(SErrorBlobIllegalInvocation, ['Blob.prototype.slice']),
      SSuggestBlobThisType);
  Source := TGocciaBlobValue(AThisValue);

  Roots.Initialize;
  try
    Roots.Add(Source);
    // The arguments convert in order; a valueOf or toString may run.
    OriginalSize := Length(Source.FBytes);
    RelativeStart := 0;
    if (AArgs.Length > 0) and
       not (AArgs.GetElement(0) is TGocciaUndefinedLiteralValue) then
    begin
      X := ClampLongLong(AArgs.GetElement(0));
      if X < 0 then
        RelativeStart := Max(OriginalSize + X, 0)
      else
        RelativeStart := Min(X, OriginalSize);
    end;
    RelativeEnd := OriginalSize;
    if (AArgs.Length > 1) and
       not (AArgs.GetElement(1) is TGocciaUndefinedLiteralValue) then
    begin
      X := ClampLongLong(AArgs.GetElement(1));
      if X < 0 then
        RelativeEnd := Max(OriginalSize + X, 0)
      else
        RelativeEnd := Min(X, OriginalSize);
    end;
    ContentType := '';
    if (AArgs.Length > 2) and
       not (AArgs.GetElement(2) is TGocciaUndefinedLiteralValue) then
      ContentType := NormalizeBlobType(AArgs.GetElement(2).ToStringLiteral.Value);

    // The span is copied: a Blob owns its bytes and their reservation.
    Span := Trunc(Max(RelativeEnd - RelativeStart, 0));
    Sliced := TGocciaBlobValue.Create;
    Roots.Add(Sliced);
    Sliced.ReserveBytes(Span);
    try
      SetLength(Bytes, Span);
      if Span > 0 then
        Move(Source.FBytes[Trunc(RelativeStart)], Bytes[0], Span);
    except
      if Assigned(TGarbageCollector.Instance) and (Span > 0) then
        TGarbageCollector.Instance.ReleaseExternalBytes(Span);
      raise;
    end;
    Sliced.AdoptBytes(Bytes, ContentType);
    Result := Sliced;
  finally
    Roots.Clear;
  end;
end;

{ A promise-returning operation turns every exception, the receiver check
  included, into a rejected promise (Web IDL §3.7.6 operations, step 4). }
function RejectedForReceiver(const AMethod: string): TGocciaValue;
var
  P: TGocciaPromiseValue;
  Roots: TGocciaActiveRootFrame;
begin
  Roots.Initialize;
  try
    P := TGocciaPromiseValue.Create;
    Roots.Add(P);
    P.Reject(CreateErrorObject('TypeError',
      Format(SErrorBlobIllegalInvocation, [AMethod])));
    Result := P;
  finally
    Roots.Clear;
  end;
end;

// File API §3.3.3 text(): UTF-8 decode, which drops a leading BOM and
// replaces each invalid sequence with U+FFFD.
function TGocciaBlobValue.BlobText(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  B: TGocciaBlobValue;
  P: TGocciaPromiseValue;
  Roots: TGocciaActiveRootFrame;
  Bytes: TBytes;
  Text: string;
begin
  if not (AThisValue is TGocciaBlobValue) then
    Exit(RejectedForReceiver('Blob.prototype.text'));
  B := TGocciaBlobValue(AThisValue);
  Roots.Initialize;
  try
    Roots.Add(B);
    P := TGocciaPromiseValue.Create;
    Roots.Add(P);
    Result := P;
    try
      Bytes := B.FBytes;
      if (Length(Bytes) >= 3) and (Bytes[0] = $EF) and (Bytes[1] = $BB) and
         (Bytes[2] = $BF) then
        Bytes := Copy(B.FBytes, 3, Length(B.FBytes) - 3);
      Text := DecodeUTF8WithReplacement(Bytes);
      Bytes := nil;
      P.Resolve(TGocciaStringLiteralValue.Create(Text));
    except
      // Text over the memory limit rejects the read, as Response.text does.
      on E: TGocciaThrowValue do
        P.Reject(E.Value);
    end;
  finally
    Roots.Clear;
  end;
end;

{ A new ArrayBuffer holding a copy of the Blob's bytes, reserved as the
  buffer's own; the engine's RangeError when they do not fit. }
function CopyToArrayBuffer(const ABlob: TGocciaBlobValue): TGocciaArrayBufferValue;
begin
  Result := TGocciaArrayBufferValue.Create(Length(ABlob.FBytes));
  if Length(ABlob.FBytes) > 0 then
    Move(ABlob.FBytes[0], Result.Data[0], Length(ABlob.FBytes));
end;

// File API §3.3.4 arrayBuffer()
function TGocciaBlobValue.BlobArrayBuffer(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  B: TGocciaBlobValue;
  P: TGocciaPromiseValue;
  AB: TGocciaArrayBufferValue;
  Roots: TGocciaActiveRootFrame;
begin
  if not (AThisValue is TGocciaBlobValue) then
    Exit(RejectedForReceiver('Blob.prototype.arrayBuffer'));
  B := TGocciaBlobValue(AThisValue);
  Roots.Initialize;
  try
    Roots.Add(B);
    P := TGocciaPromiseValue.Create;
    Roots.Add(P);
    Result := P;
    try
      AB := CopyToArrayBuffer(B);
      Roots.Add(AB);
    except
      // A buffer over the memory limit rejects the read.
      on E: TGocciaThrowValue do
      begin
        P.Reject(E.Value);
        Exit;
      end;
    end;
    P.Resolve(AB);
  finally
    Roots.Clear;
  end;
end;

// File API §3.3.5 bytes(): a Uint8Array over a new ArrayBuffer
function TGocciaBlobValue.BlobBytes(const AArgs: TGocciaArgumentsCollection;
  const AThisValue: TGocciaValue): TGocciaValue;
var
  B: TGocciaBlobValue;
  P: TGocciaPromiseValue;
  AB: TGocciaArrayBufferValue;
  View: TGocciaTypedArrayValue;
  Roots: TGocciaActiveRootFrame;
begin
  if not (AThisValue is TGocciaBlobValue) then
    Exit(RejectedForReceiver('Blob.prototype.bytes'));
  B := TGocciaBlobValue(AThisValue);
  Roots.Initialize;
  try
    Roots.Add(B);
    P := TGocciaPromiseValue.Create;
    Roots.Add(P);
    Result := P;
    try
      AB := CopyToArrayBuffer(B);
      Roots.Add(AB);
      View := TGocciaTypedArrayValue.Create(takUint8, AB);
      Roots.Add(View);
    except
      on E: TGocciaThrowValue do
      begin
        P.Reject(E.Value);
        Exit;
      end;
    end;
    P.Resolve(View);
  finally
    Roots.Clear;
  end;
end;

initialization
  GBlobSharedSlot := RegisterRealmOwnedSlot('Blob.shared');

end.

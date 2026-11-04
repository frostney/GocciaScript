unit Goccia.WebIDL.BufferSource;

{$I Goccia.inc}

{ Web IDL buffer source conversions (https://webidl.spec.whatwg.org/,
  §3.2.25 Union types and §3.2.26 Buffer source types) for the built-ins
  whose IDL signatures take a buffer source argument. None of those
  signatures carries [AllowResizable], so every conversion here rejects a
  resizable ArrayBuffer and a growable SharedArrayBuffer, and every view of
  one, whether or not the buffer has been resized or detached since. }

interface

uses
  SysUtils,

  Goccia.Values.Primitives,
  Goccia.Values.TypedArrayValue;

// ES2026 §25.1.3.9 IsFixedLengthArrayBuffer(arrayBuffer), for an ArrayBuffer
// or SharedArrayBuffer value.
function IsFixedLengthArrayBuffer(const ABuffer: TGocciaValue): Boolean;

{ Converts AValue to the IDL AllowSharedBufferSource union
  (ArrayBuffer or SharedArrayBuffer or [AllowShared] ArrayBufferView).
  Throws a TypeError, naming AContext, when AValue is none of these or is
  backed by a buffer that is not fixed-length. }
procedure ConvertToAllowSharedBufferSource(const AValue: TGocciaValue;
  const AContext, ASuggestion: string);

{ Converts AValue to the IDL BufferSource union (ArrayBuffer or
  ArrayBufferView), without [AllowShared]: throws a TypeError, naming
  AContext, when AValue is none of these, is or views a SharedArrayBuffer,
  or is backed by a buffer that is not fixed-length. }
procedure ConvertToBufferSource(const AValue: TGocciaValue;
  const AContext, ASuggestion: string);

{ Converts AValue to the IDL type [AllowShared] Uint8Array. Throws a
  TypeError, naming AContext, when AValue is not a Uint8Array or its buffer
  is not fixed-length. }
function ConvertToAllowSharedUint8Array(const AValue: TGocciaValue;
  const AContext, ASuggestion: string): TGocciaTypedArrayValue;

{ Web IDL §3.2.26 "get a copy of the bytes held by the buffer source", for a
  value ConvertToAllowSharedBufferSource accepted. }
function GetCopyOfBytesHeldByBufferSource(const ASource: TGocciaValue): TBytes;

{ How many bytes GetCopyOfBytesHeldByBufferSource would return for ASource,
  without copying them. }
function ByteLengthHeldByBufferSource(const ASource: TGocciaValue): Integer;

{ Copies the bytes GetCopyOfBytesHeldByBufferSource would return into ADest
  at AOffset, which has room for ByteLengthHeldByBufferSource(ASource), and
  returns how many it copied. }
function CopyBytesHeldByBufferSource(const ASource: TGocciaValue;
  const ADest: TBytes; const AOffset: SizeInt): Integer;

implementation

uses
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Values.ArrayBufferValue,
  Goccia.Values.DataViewValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.SharedArrayBufferValue;

function IsFixedLengthArrayBuffer(const ABuffer: TGocciaValue): Boolean;
begin
  // ES2026 §25.1.3.9 step 1: a buffer created with maxByteLength has the
  // [[ArrayBufferMaxByteLength]] slot for its whole life, detached or not.
  if ABuffer is TGocciaArrayBufferValue then
    Result := TGocciaArrayBufferValue(ABuffer).MaxByteLength < 0
  else if ABuffer is TGocciaSharedArrayBufferValue then
    Result := TGocciaSharedArrayBufferValue(ABuffer).MaxByteLength < 0
  else
    Result := True;
end;

function ViewedArrayBuffer(const AView: TGocciaValue): TGocciaValue;
begin
  if AView is TGocciaTypedArrayValue then
    Result := TGocciaTypedArrayValue(AView).BufferValue
  else if AView is TGocciaDataViewValue then
    Result := TGocciaDataViewValue(AView).BufferValue
  else
    Result := nil;
end;

procedure ThrowNotFixedLength(const AContext: string);
begin
  ThrowTypeError(Format(SErrorBufferSourceNotFixedLength, [AContext]),
    SSuggestBufferSourceNotFixedLength);
end;

// Web IDL §3.2.25 Union types, for AllowSharedBufferSource; each member's
// conversion is §3.2.26's.
procedure ConvertToAllowSharedBufferSource(const AValue: TGocciaValue;
  const AContext, ASuggestion: string);
begin
  // An object with [[ArrayBufferData]] converts to ArrayBuffer or
  // SharedArrayBuffer, one with [[DataView]] or [[TypedArrayName]] to that
  // view type, with [AllowShared].
  if (AValue is TGocciaArrayBufferValue) or
     (AValue is TGocciaSharedArrayBufferValue) then
  begin
    // §3.2.26 ArrayBuffer and SharedArrayBuffer, step 3: without
    // [AllowResizable], IsFixedLengthArrayBuffer(V) false throws.
    if not IsFixedLengthArrayBuffer(AValue) then
      ThrowNotFixedLength(AContext);
  end
  else if (AValue is TGocciaTypedArrayValue) or
          (AValue is TGocciaDataViewValue) then
  begin
    // §3.2.26 DataView and typed arrays, step 3: without [AllowResizable],
    // IsFixedLengthArrayBuffer(V.[[ViewedArrayBuffer]]) false throws.
    if not IsFixedLengthArrayBuffer(ViewedArrayBuffer(AValue)) then
      ThrowNotFixedLength(AContext);
  end
  else
    // No other union member matches: the union's last step throws.
    ThrowTypeError(Format(SErrorBufferSourceType, [AContext]), ASuggestion);
end;

// Web IDL §3.2.25 Union types, for BufferSource; each member's conversion is
// §3.2.26's, whose step 2 throws for a shared buffer without [AllowShared].
procedure ConvertToBufferSource(const AValue: TGocciaValue;
  const AContext, ASuggestion: string);
var
  Buffer: TGocciaValue;
begin
  if (AValue is TGocciaArrayBufferValue) or
     (AValue is TGocciaSharedArrayBufferValue) then
    Buffer := AValue
  else if (AValue is TGocciaTypedArrayValue) or
          (AValue is TGocciaDataViewValue) then
    Buffer := ViewedArrayBuffer(AValue)
  else
  begin
    ThrowTypeError(Format(SErrorBufferSourceNonSharedType, [AContext]),
      ASuggestion);
    Exit;
  end;
  if Buffer is TGocciaSharedArrayBufferValue then
    ThrowTypeError(Format(SErrorBufferSourceShared, [AContext]),
      SSuggestBufferSourceShared);
  if not IsFixedLengthArrayBuffer(Buffer) then
    ThrowNotFixedLength(AContext);
end;

// Web IDL §3.2.26 converting a JavaScript value to an IDL Uint8Array, with
// [AllowShared]
function ConvertToAllowSharedUint8Array(const AValue: TGocciaValue;
  const AContext, ASuggestion: string): TGocciaTypedArrayValue;
begin
  // Step 1: V must have [[TypedArrayName]] "Uint8Array".
  if not (AValue is TGocciaTypedArrayValue) or
     (TGocciaTypedArrayValue(AValue).Kind <> takUint8) then
    ThrowTypeError(Format(SErrorBufferSourceUint8ArrayType, [AContext]),
      ASuggestion);
  Result := TGocciaTypedArrayValue(AValue);
  // Step 2 does not apply: [AllowShared] admits a SharedArrayBuffer.
  // Step 3: without [AllowResizable], the viewed buffer must be fixed-length.
  if not IsFixedLengthArrayBuffer(Result.BufferValue) then
    ThrowNotFixedLength(AContext);
end;

function BufferData(const ABuffer: TGocciaValue): TBytes;
begin
  if ABuffer is TGocciaArrayBufferValue then
    Result := TGocciaArrayBufferValue(ABuffer).Data
  else if ABuffer is TGocciaSharedArrayBufferValue then
    Result := TGocciaSharedArrayBufferValue(ABuffer).Data
  else
    Result := nil;
end;

// ES2026 §25.1.3.2 ArrayBufferByteLength(arrayBuffer, order): measured from
// the buffer, not from its data block, which a SharedArrayBuffer may
// allocate past its byte length.
function ArrayBufferByteLength(const ABuffer: TGocciaValue): Integer;
begin
  if ABuffer is TGocciaArrayBufferValue then
    Result := System.Length(TGocciaArrayBufferValue(ABuffer).Data)
  else if ABuffer is TGocciaSharedArrayBufferValue then
    Result := TGocciaSharedArrayBufferValue(ABuffer).ByteLength
  else
    Result := 0;
end;

function IsDetachedBuffer(const ABuffer: TGocciaValue): Boolean;
begin
  Result := (ABuffer is TGocciaArrayBufferValue) and
    TGocciaArrayBufferValue(ABuffer).Detached;
end;

{ Web IDL §3.2.26 get a copy of the bytes held by the buffer source, steps
  2-9 without the copy: the data block and the range of it ASource holds, or
  False when it holds no bytes (a detached buffer, an empty range). }
function ResolveHeldBytes(const ASource: TGocciaValue; out AData: TBytes;
  out AOffset, AByteLength: Integer): Boolean;
var
  ArrayBuffer: TGocciaValue;
  View: TGocciaTypedArrayValue;
  DataView: TGocciaDataViewValue;
begin
  Result := False;
  AData := nil;
  // Steps 2-6: the viewed buffer, offset and [[ByteLength]] of a view, or
  // the whole buffer.
  if ASource is TGocciaTypedArrayValue then
  begin
    View := TGocciaTypedArrayValue(ASource);
    ArrayBuffer := View.BufferValue;
    AOffset := View.ByteOffset;
    AByteLength := View.Length *
      TGocciaTypedArrayValue.BytesPerElement(View.Kind);
  end
  else if ASource is TGocciaDataViewValue then
  begin
    DataView := TGocciaDataViewValue(ASource);
    ArrayBuffer := DataView.BufferValue;
    AOffset := DataView.ByteOffset;
    // Only a view of a resizable buffer tracks its length, and the
    // conversion refused those.
    AByteLength := DataView.ByteLengthSlot;
  end
  else
  begin
    ArrayBuffer := ASource;
    AOffset := 0;
    AByteLength := ArrayBufferByteLength(ASource);
  end;

  // Step 7: a detached buffer holds no bytes.
  if IsDetachedBuffer(ArrayBuffer) then
    Exit;

  // Steps 8-9: the range [offset, offset + length). A fixed-length buffer
  // never changes length while attached, so the range is in bounds; check
  // anyway rather than read past the block.
  AData := BufferData(ArrayBuffer);
  if (AByteLength <= 0) or (AOffset < 0) or
     (Int64(AOffset) + AByteLength > ArrayBufferByteLength(ArrayBuffer)) or
     (Int64(AOffset) + AByteLength > System.Length(AData)) then
    Exit;
  Result := True;
end;

// Web IDL §3.2.26 get a copy of the bytes held by the buffer source
function GetCopyOfBytesHeldByBufferSource(const ASource: TGocciaValue): TBytes;
var
  Data: TBytes;
  Offset, ByteLength: Integer;
begin
  Result := nil;
  if not ResolveHeldBytes(ASource, Data, Offset, ByteLength) then
    Exit;
  SetLength(Result, ByteLength);
  Move(Data[Offset], Result[0], ByteLength);
end;

function ByteLengthHeldByBufferSource(const ASource: TGocciaValue): Integer;
var
  Data: TBytes;
  Offset, ByteLength: Integer;
begin
  if ResolveHeldBytes(ASource, Data, Offset, ByteLength) then
    Result := ByteLength
  else
    Result := 0;
end;

function CopyBytesHeldByBufferSource(const ASource: TGocciaValue;
  const ADest: TBytes; const AOffset: SizeInt): Integer;
var
  Data: TBytes;
  Offset, ByteLength: Integer;
begin
  Result := 0;
  if not ResolveHeldBytes(ASource, Data, Offset, ByteLength) then
    Exit;
  Move(Data[Offset], ADest[AOffset], ByteLength);
  Result := ByteLength;
end;

end.

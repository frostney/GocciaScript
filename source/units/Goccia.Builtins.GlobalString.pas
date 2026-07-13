unit Goccia.Builtins.GlobalString;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.Builtins.Base,
  Goccia.Error.ThrowErrorCallback,
  Goccia.ObjectModel,
  Goccia.Scope,
  Goccia.Values.Primitives;

type
  TGocciaGlobalString = class(TGocciaBuiltin)
  public
    constructor Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
  published
    function StringFromCharCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringFromCodePoint(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function StringRaw(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

implementation

uses
  Math,
  SysUtils,

  StringBuffer,

  Goccia.Constants.PropertyNames,
  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.ThreadCleanupRegistry,
  Goccia.Utils,
  Goccia.Values.ArrayValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.NativeFunction,
  Goccia.Values.ObjectValue;

threadvar
  FStaticMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FStaticMembers, 0);
end;

constructor TGocciaGlobalString.Create(const AName: string; const AScope: TGocciaScope; const AThrowError: TGocciaThrowErrorCallback);
var
  Members: TGocciaMemberCollection;
begin
  inherited Create(AName, AScope, AThrowError);

  Members := TGocciaMemberCollection.Create;
  try
    Members.AddMethod(StringFromCharCode, 1, gmkStaticMethod);
    Members.AddMethod(StringFromCodePoint, 1, gmkStaticMethod);
    Members.AddMethod(StringRaw, 1, gmkStaticMethod);
    FStaticMembers := Members.ToDefinitions;
  finally
    Members.Free;
  end;
  RegisterMemberDefinitions(FBuiltinObject, FStaticMembers);
end;

// ES2026 §22.1.2.1 String.fromCharCode(...codeUnits)
function TGocciaGlobalString.StringFromCharCode(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Buffer: TStringBuffer;
  I, CodeUnit, SecondCodeUnit, PendingHighSurrogate, CodePoint: Integer;

  function EncodeCodeUnit(const ACodeUnit: Integer): string;
  begin
    if ACodeUnit < $80 then
      Exit(Chr(ACodeUnit));
    if ACodeUnit < $800 then
      Exit(Chr($C0 or (ACodeUnit shr 6)) +
        Chr($80 or (ACodeUnit and $3F)));
    Result := Chr($E0 or (ACodeUnit shr 12)) +
      Chr($80 or ((ACodeUnit shr 6) and $3F)) +
      Chr($80 or (ACodeUnit and $3F));
  end;

  procedure AppendCodeUnit(const ACodeUnit: Integer);
  begin
    Buffer.Append(EncodeCodeUnit(ACodeUnit));
  end;
begin
  // The overwhelmingly common one- and two-unit cases avoid allocating a
  // growable buffer. The two-unit path also canonicalizes complete surrogate
  // pairs so raw equality can take its constant-time fast path.
  if AArgs.Length = 0 then
    Exit(TGocciaStringLiteralValue.Create(''));
  if AArgs.Length <= 2 then
  begin
    CodeUnit := ToUint16Value(AArgs.GetElement(0));
    if AArgs.Length = 1 then
      Exit(TGocciaStringLiteralValue.Create(EncodeCodeUnit(CodeUnit)));
    SecondCodeUnit := ToUint16Value(AArgs.GetElement(1));
    if (CodeUnit >= $D800) and (CodeUnit <= $DBFF) and
       (SecondCodeUnit >= $DC00) and (SecondCodeUnit <= $DFFF) then
    begin
      CodePoint := $10000 + ((CodeUnit - $D800) shl 10) +
        (SecondCodeUnit - $DC00);
      Exit(TGocciaStringLiteralValue.Create(
        Chr($F0 or (CodePoint shr 18)) +
        Chr($80 or ((CodePoint shr 12) and $3F)) +
        Chr($80 or ((CodePoint shr 6) and $3F)) +
        Chr($80 or (CodePoint and $3F))));
    end;
    Exit(TGocciaStringLiteralValue.Create(EncodeCodeUnit(CodeUnit) +
      EncodeCodeUnit(SecondCodeUnit)));
  end;

  Buffer := TStringBuffer.Create(AArgs.Length * 3);
  PendingHighSurrogate := -1;
  I := 0;
  while I < AArgs.Length do
  begin
    // ES2026 §7.1.10 ToUint16: NaN/±0/±∞ → 0, otherwise truncate mod 2^16.
    CodeUnit := ToUint16Value(AArgs.GetElement(I));

    if PendingHighSurrogate >= 0 then
    begin
      if (CodeUnit >= $DC00) and (CodeUnit <= $DFFF) then
      begin
        // Canonicalize a complete UTF-16 surrogate pair to its UTF-8 code
        // point. Lone surrogates remain WTF-8 below, preserving JS indexing.
        CodePoint := $10000 + ((PendingHighSurrogate - $D800) shl 10) +
          (CodeUnit - $DC00);
        Buffer.Append(Chr($F0 or (CodePoint shr 18)) +
          Chr($80 or ((CodePoint shr 12) and $3F)) +
          Chr($80 or ((CodePoint shr 6) and $3F)) +
          Chr($80 or (CodePoint and $3F)));
        PendingHighSurrogate := -1;
        Inc(I);
        Continue;
      end;

      AppendCodeUnit(PendingHighSurrogate);
      PendingHighSurrogate := -1;
    end;

    if (CodeUnit >= $D800) and (CodeUnit <= $DBFF) then
      PendingHighSurrogate := CodeUnit
    else
      AppendCodeUnit(CodeUnit);
    Inc(I);
  end;

  if PendingHighSurrogate >= 0 then
    AppendCodeUnit(PendingHighSurrogate);
  Result := TGocciaStringLiteralValue.Create(Buffer.ToString);
end;

// ES2026 §22.1.2.2 String.fromCodePoint(...codePoints)
function TGocciaGlobalString.StringFromCodePoint(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  I: Integer;
  ArgValue: TGocciaValue;
  NumArg: TGocciaNumberLiteralValue;
  RawValue: Double;
  CodePoint: Cardinal;
  SB: TStringBuffer;
begin
  SB := TStringBuffer.Create(AArgs.Length * 4);
  I := 0;
  while I < AArgs.Length do
  begin
    ArgValue := AArgs.Items[I];
    if ArgValue is TGocciaNumberLiteralValue then
      NumArg := TGocciaNumberLiteralValue(ArgValue)
    else
      NumArg := ArgValue.ToNumberLiteral;
    RawValue := NumArg.Value;
    if IsNan(RawValue) or IsInfinite(RawValue) then
      ThrowRangeError(SErrorInvalidCodePoint, SSuggestCodePointRange);
    if (RawValue < 0) or (RawValue > $10FFFF) then
      ThrowRangeError(Format(SErrorNotValidCodePoint, [FormatDouble(RawValue)]), SSuggestCodePointRange);
    CodePoint := Trunc(RawValue);
    if RawValue <> CodePoint then
      ThrowRangeError(Format(SErrorNotValidCodePoint, [FormatDouble(RawValue)]), SSuggestCodePointRange);

    if CodePoint < $80 then
      SB.AppendChar(AnsiChar(CodePoint))
    else if CodePoint < $800 then
    begin
      SB.AppendChar(AnsiChar($C0 or (CodePoint shr 6)));
      SB.AppendChar(AnsiChar($80 or (CodePoint and $3F)));
    end
    else if CodePoint < $10000 then
    begin
      SB.AppendChar(AnsiChar($E0 or (CodePoint shr 12)));
      SB.AppendChar(AnsiChar($80 or ((CodePoint shr 6) and $3F)));
      SB.AppendChar(AnsiChar($80 or (CodePoint and $3F)));
    end
    else
    begin
      SB.AppendChar(AnsiChar($F0 or (CodePoint shr 18)));
      SB.AppendChar(AnsiChar($80 or ((CodePoint shr 12) and $3F)));
      SB.AppendChar(AnsiChar($80 or ((CodePoint shr 6) and $3F)));
      SB.AppendChar(AnsiChar($80 or (CodePoint and $3F)));
    end;
    Inc(I);
  end;
  Result := TGocciaStringLiteralValue.Create(SB.ToString);
end;

// ES2026 §22.1.2.4 String.raw(template, ...substitutions)
function TGocciaGlobalString.StringRaw(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  TemplateObj, RawValue, RawElement: TGocciaValue;
  RawObj: TGocciaValue;
  LiteralSegments: Integer;
  SB: TStringBuffer;
  I: Integer;
  NextSub: TGocciaValue;
begin
  if AArgs.Length = 0 then
    ThrowTypeError(Format(SErrorCannotConvertToObject, ['undefined']), SSuggestCheckNullBeforeAccess);

  // ES2026 §22.1.2.4 step 1-2: Let cooked be ToObject(template)
  TemplateObj := AArgs.GetElement(0);
  if (TemplateObj is TGocciaUndefinedLiteralValue) or (TemplateObj is TGocciaNullLiteralValue) then
    ThrowTypeError(Format(SErrorCannotConvertToObject, [TemplateObj.TypeName]), SSuggestCheckNullBeforeAccess);

  // ES2026 §22.1.2.4 step 3: Let raw be ToObject(Get(cooked, "raw"))
  RawValue := TemplateObj.GetProperty(PROP_RAW);
  if not Assigned(RawValue) or (RawValue is TGocciaUndefinedLiteralValue) then
    ThrowTypeError(Format(SErrorCannotConvertToObject, ['undefined']), SSuggestCheckNullBeforeAccess)
  else if RawValue is TGocciaNullLiteralValue then
    ThrowTypeError(Format(SErrorCannotConvertToObject, ['null']), SSuggestCheckNullBeforeAccess);
  RawObj := RawValue;

  // ES2026 §22.1.2.4 step 4: Let literalSegments be LengthOfArrayLike(raw)
  RawElement := RawObj.GetProperty(PROP_LENGTH);
  if not Assigned(RawElement) or (RawElement is TGocciaUndefinedLiteralValue) then
    LiteralSegments := 0
  else
    LiteralSegments := ToLengthValue(RawElement);

  // ES2026 §22.1.2.4 step 5: If literalSegments <= 0, return ""
  if LiteralSegments <= 0 then
  begin
    Result := TGocciaStringLiteralValue.Create('');
    Exit;
  end;

  // ES2026 §22.1.2.4 steps 6-9: Build result by interleaving raw segments with substitutions
  SB := TStringBuffer.Create;
  I := 0;
  while I < LiteralSegments do
  begin
    // ES2026 §22.1.2.4 step 8a: Let nextSeg be ToString(Get(raw, ToString(nextIndex)))
    RawElement := RawObj.GetProperty(IntToStr(I));
    if Assigned(RawElement) then
      SB.Append(RawElement.ToStringLiteral.Value);

    // ES2026 §22.1.2.4 step 8d: Append substitution if not the last segment
    if I + 1 < LiteralSegments then
    begin
      if I + 1 < AArgs.Length then
      begin
        NextSub := AArgs.GetElement(I + 1);
        SB.Append(NextSub.ToStringLiteral.Value);
      end;
    end;
    Inc(I);
  end;

  Result := TGocciaStringLiteralValue.Create(SB.ToString);
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);

end.

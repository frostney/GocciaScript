unit Goccia.Values.TemporalInstant;

{$I Goccia.inc}

interface

uses
  Goccia.Arguments.Collection,
  Goccia.ObjectModel,
  Goccia.SharedPrototype,
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

type
  TGocciaTemporalInstantValue = class(TGocciaObjectValue)
  private
    FEpochMilliseconds: Int64;
    FSubMillisecondNanoseconds: Integer;

    procedure InitializePrototype;
  public
    constructor Create(const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer); overload;

    function ToStringTag: string; override;
    class procedure ExposePrototype(const AConstructor: TGocciaObjectValue);

    property EpochMilliseconds: Int64 read FEpochMilliseconds;
    property SubMillisecondNanoseconds: Integer read FSubMillisecondNanoseconds;
  published
    function GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
    function InstantToZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
  end;

function CoerceTemporalInstant(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;

implementation

uses
  SysUtils,

  BigInteger,

  Goccia.Error.Messages,
  Goccia.Error.Suggestions,
  Goccia.Realm,
  Goccia.Temporal.DurationMath,
  Goccia.Temporal.Options,
  Goccia.Temporal.TimeZone,
  Goccia.Temporal.Utils,
  Goccia.ThreadCleanupRegistry,
  Goccia.Values.BigIntValue,
  Goccia.Values.ErrorHelper,
  Goccia.Values.IntlDateTimeFormat,
  Goccia.Values.ObjectPropertyDescriptor,
  Goccia.Values.SymbolValue,
  Goccia.Values.TemporalDuration,
  Goccia.Values.TemporalZonedDateTime;

var
  GTemporalInstantSharedSlot: TGocciaRealmOwnedSlotId;

threadvar
  FPrototypeMembers: TArray<TGocciaMemberDefinition>;

procedure ClearThreadvarMembers;
begin
  SetLength(FPrototypeMembers, 0);
end;

function InstantEpochSecond(const AEpochMilliseconds: Int64): Int64; inline;
begin
  Result := AEpochMilliseconds div 1000;
  if (AEpochMilliseconds < 0) and (AEpochMilliseconds mod 1000 <> 0) then
    Dec(Result);
end;

function FormatInstantOffsetString(const AOffsetSeconds: Integer): string;
var
  AbsSeconds, Hours, Minutes, Seconds: Integer;
  Sign: Char;
begin
  if AOffsetSeconds < 0 then
    Sign := '-'
  else
    Sign := '+';
  AbsSeconds := Abs(AOffsetSeconds);
  Hours := AbsSeconds div 3600;
  Minutes := (AbsSeconds mod 3600) div 60;
  Seconds := AbsSeconds mod 60;
  if Seconds = 0 then
    Result := Sign + Format('%.2d:%.2d', [Hours, Minutes])
  else
    Result := Sign + Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
end;

function GetTemporalInstantShared: TGocciaSharedPrototype; inline;
begin
  if Assigned(CurrentRealm) then
    Result := TGocciaSharedPrototype(CurrentRealm.GetOwnedSlot(GTemporalInstantSharedSlot))
  else
    Result := nil;
end;

function AsInstant(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;
begin
  if not (AValue is TGocciaTemporalInstantValue) then
    ThrowTypeError(AMethod + ' called on non-Instant', SSuggestTemporalThisType);
  Result := TGocciaTemporalInstantValue(AValue);
end;

procedure NormalizeInstantSubMillisecondNanoseconds(var AEpochMilliseconds: TBigInteger;
  const ASubNanosecondsTotal: TBigInteger; out ASubMillisecondNanoseconds: Integer);
var
  CarryMilliseconds, RemainderNanoseconds, Million: TBigInteger;
begin
  Million := TBigInteger.FromInt64(NANOSECONDS_PER_MILLISECOND);
  CarryMilliseconds := ASubNanosecondsTotal.Divide(Million);
  RemainderNanoseconds := ASubNanosecondsTotal.Modulo(Million);
  if RemainderNanoseconds.IsNegative then
  begin
    CarryMilliseconds := CarryMilliseconds.Subtract(TBigInteger.One);
    RemainderNanoseconds := RemainderNanoseconds.Add(Million);
  end;

  AEpochMilliseconds := AEpochMilliseconds.Add(CarryMilliseconds);
  ASubMillisecondNanoseconds := Integer(RemainderNanoseconds.ToInt64);
end;

function BigIntToInstantInt64(const AValue: TBigInteger; const AFieldName: string): Int64;
begin
  if (AValue.Compare(TBigInteger.FromInt64(Low(Int64))) < 0) or
     (AValue.Compare(TBigInteger.FromInt64(High(Int64))) > 0) then
    raise ETemporalDurationInt64Overflow.CreateFmt('Temporal.Instant.%s is outside Int64 range', [AFieldName]);
  Result := AValue.ToInt64;
end;

function InstantIsASCIIDigit(const AChar: Char): Boolean; inline;
begin
  Result := (AChar >= '0') and (AChar <= '9');
end;

function ReadInstantFixedDigits(const AValue: string; var APos: Integer;
  const ACount: Integer; out AResult: Integer): Boolean;
var
  I: Integer;
begin
  AResult := 0;
  if APos + ACount - 1 > Length(AValue) then
    Exit(False);
  for I := 0 to ACount - 1 do
  begin
    if not InstantIsASCIIDigit(AValue[APos + I]) then
      Exit(False);
    AResult := AResult * 10 + Ord(AValue[APos + I]) - Ord('0');
  end;
  Inc(APos, ACount);
  Result := True;
end;

function ReadInstantFractionalNanoseconds(const AValue: string; var APos: Integer;
  out AMillisecond, AMicrosecond, ANanosecond: Integer): Boolean;
var
  Digits: Integer;
  Fraction: Int64;
begin
  Result := False;
  AMillisecond := 0;
  AMicrosecond := 0;
  ANanosecond := 0;

  if (APos > Length(AValue)) or ((AValue[APos] <> '.') and (AValue[APos] <> ',')) then
    Exit(True);

  Inc(APos);
  Digits := 0;
  Fraction := 0;
  while (APos <= Length(AValue)) and InstantIsASCIIDigit(AValue[APos]) do
  begin
    if Digits >= 9 then
      Exit;
    Fraction := Fraction * 10 + Ord(AValue[APos]) - Ord('0');
    Inc(APos);
    Inc(Digits);
  end;
  if Digits = 0 then
    Exit;

  while Digits < 9 do
  begin
    Fraction := Fraction * 10;
    Inc(Digits);
  end;

  AMillisecond := Integer(Fraction div 1000000);
  AMicrosecond := Integer((Fraction div 1000) mod 1000);
  ANanosecond := Integer(Fraction mod 1000);
  Result := True;
end;

function InstantAnnotationKeyIsValid(const AValue: string): Boolean;
var
  I: Integer;
begin
  Result := AValue <> '';
  for I := 1 to Length(AValue) do
  begin
    if (AValue[I] >= 'A') and (AValue[I] <= 'Z') then
      Exit(False);
  end;
end;

function InstantAnnotationHasSubMinuteOffset(const AValue: string): Boolean;
var
  Body: string;
  ColonCount, I: Integer;
begin
  Result := False;
  if (AValue = '') or not ((AValue[1] = '+') or (AValue[1] = '-')) then
    Exit;
  Body := Copy(AValue, 2, Length(AValue) - 1);
  ColonCount := 0;
  for I := 1 to Length(Body) do
  begin
    if Body[I] = ':' then
      Inc(ColonCount)
    else if (Body[I] = '.') or (Body[I] = ',') then
      Exit(True);
  end;
  if ColonCount >= 2 then
    Exit(True);
  if (ColonCount = 0) and (Length(Body) > 4) then
    Exit(True);
end;

function StripInstantAnnotations(var AValue: string): Boolean;
var
  BracketStart, BracketEnd, EqualsPos: Integer;
  Annotation, Key: string;
  Critical, CalendarCriticalSeen: Boolean;
  CalendarCount, TimeZoneCount: Integer;
begin
  Result := False;
  CalendarCriticalSeen := False;
  CalendarCount := 0;
  TimeZoneCount := 0;

  while (Length(AValue) > 0) and (AValue[Length(AValue)] = ']') do
  begin
    BracketEnd := Length(AValue);
    BracketStart := BracketEnd - 1;
    while (BracketStart > 0) and (AValue[BracketStart] <> '[') do
      Dec(BracketStart);
    if BracketStart = 0 then
      Exit;

    Annotation := Copy(AValue, BracketStart + 1, BracketEnd - BracketStart - 1);
    Critical := (Length(Annotation) > 0) and (Annotation[1] = '!');
    if Critical then
      Annotation := Copy(Annotation, 2, Length(Annotation) - 1);
    if Annotation = '' then
      Exit;

    EqualsPos := System.Pos('=', Annotation);
    if EqualsPos > 0 then
    begin
      Key := Copy(Annotation, 1, EqualsPos - 1);
      if not InstantAnnotationKeyIsValid(Key) then
        Exit;
      if Key = 'u-ca' then
      begin
        Inc(CalendarCount);
        if Critical then
          CalendarCriticalSeen := True;
        if (CalendarCount > 1) and (CalendarCriticalSeen or Critical) then
          Exit;
      end
      else if Critical then
        Exit;
    end
    else
    begin
      if InstantAnnotationHasSubMinuteOffset(Annotation) then
        Exit;
      Inc(TimeZoneCount);
      if TimeZoneCount > 1 then
        Exit;
    end;

    AValue := Copy(AValue, 1, BracketStart - 1);
  end;

  Result := True;
end;

function ParseInstantDatePart(const AValue: string; out ADate: TTemporalDateRecord): Boolean;
var
  Pos, Sign, Year, Month, Day: Integer;
  Extended: Boolean;
begin
  Result := False;
  Pos := 1;
  Sign := 1;
  if Pos > Length(AValue) then
    Exit;

  if AValue[Pos] = '+' then
  begin
    Inc(Pos);
    if not ReadInstantFixedDigits(AValue, Pos, 6, Year) then
      Exit;
  end
  else if AValue[Pos] = '-' then
  begin
    Inc(Pos);
    Sign := -1;
    if not ReadInstantFixedDigits(AValue, Pos, 6, Year) then
      Exit;
    if Year = 0 then
      Exit;
  end
  else if not ReadInstantFixedDigits(AValue, Pos, 4, Year) then
    Exit;

  Year := Year * Sign;
  Extended := (Pos <= Length(AValue)) and (AValue[Pos] = '-');
  if Extended then
    Inc(Pos);
  if not ReadInstantFixedDigits(AValue, Pos, 2, Month) then
    Exit;
  if Extended then
  begin
    if (Pos > Length(AValue)) or (AValue[Pos] <> '-') then
      Exit;
    Inc(Pos);
  end;
  if not ReadInstantFixedDigits(AValue, Pos, 2, Day) then
    Exit;
  if Pos <= Length(AValue) then
    Exit;
  if not IsValidDate(Year, Month, Day) then
    Exit;

  ADate.Year := Year;
  ADate.Month := Month;
  ADate.Day := Day;
  Result := True;
end;

function ParseInstantTimePart(const AValue: string; out ATime: TTemporalTimeRecord): Boolean;
var
  Pos, Hour, Minute, Second: Integer;
  Extended: Boolean;
begin
  Result := False;
  FillChar(ATime, SizeOf(ATime), 0);
  Pos := 1;
  if not ReadInstantFixedDigits(AValue, Pos, 2, Hour) then
    Exit;
  Minute := 0;
  Second := 0;

  if Pos <= Length(AValue) then
  begin
    Extended := AValue[Pos] = ':';
    if Extended then
      Inc(Pos);

    if not ReadInstantFixedDigits(AValue, Pos, 2, Minute) then
      Exit;

    if Pos <= Length(AValue) then
    begin
      if Extended then
      begin
        if AValue[Pos] = ':' then
          Inc(Pos)
        else if (AValue[Pos] = '.') or (AValue[Pos] = ',') then
        begin
          if not ReadInstantFractionalNanoseconds(AValue, Pos,
            ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then
            Exit;
          if Pos <= Length(AValue) then
            Exit;
          if not IsValidTime(Hour, Minute, Second, ATime.Millisecond,
            ATime.Microsecond, ATime.Nanosecond) then
            Exit;
          ATime.Hour := Hour;
          ATime.Minute := Minute;
          ATime.Second := Second;
          Exit(True);
        end
        else
          Exit;
      end
      else if (AValue[Pos] = '.') or (AValue[Pos] = ',') then
      begin
        if not ReadInstantFractionalNanoseconds(AValue, Pos,
          ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then
          Exit;
        if Pos <= Length(AValue) then
          Exit;
        if not IsValidTime(Hour, Minute, Second, ATime.Millisecond,
          ATime.Microsecond, ATime.Nanosecond) then
          Exit;
        ATime.Hour := Hour;
        ATime.Minute := Minute;
        ATime.Second := Second;
        Exit(True);
      end;

      if not ReadInstantFixedDigits(AValue, Pos, 2, Second) then
        Exit;
      if not ReadInstantFractionalNanoseconds(AValue, Pos,
        ATime.Millisecond, ATime.Microsecond, ATime.Nanosecond) then
        Exit;
    end;
  end;

  if Pos <= Length(AValue) then
    Exit;
  if Second = 60 then
    Second := 59;
  if not IsValidTime(Hour, Minute, Second, ATime.Millisecond,
    ATime.Microsecond, ATime.Nanosecond) then
    Exit;

  ATime.Hour := Hour;
  ATime.Minute := Minute;
  ATime.Second := Second;
  Result := True;
end;

function SplitInstantOffset(const AValue: string; out ATimePart, AOffsetPart: string): Boolean;
var
  I: Integer;
begin
  ATimePart := AValue;
  AOffsetPart := '';
  Result := False;
  if AValue = '' then
    Exit;

  if (AValue[Length(AValue)] = 'Z') or (AValue[Length(AValue)] = 'z') then
  begin
    ATimePart := Copy(AValue, 1, Length(AValue) - 1);
    AOffsetPart := Copy(AValue, Length(AValue), 1);
    Exit(True);
  end;

  for I := 1 to Length(AValue) do
  begin
    if (AValue[I] = '+') or (AValue[I] = '-') then
    begin
      ATimePart := Copy(AValue, 1, I - 1);
      AOffsetPart := Copy(AValue, I, Length(AValue) - I + 1);
      Exit(True);
    end;
  end;
end;

function ParseInstantOffsetPart(const AValue: string; out AOffsetNanoseconds: TBigInteger): Boolean;
var
  Pos, Sign, Hour, Minute, Second, Ms, Us, Ns: Integer;
  Extended: Boolean;
begin
  Result := False;
  AOffsetNanoseconds := TBigInteger.Zero;
  if (AValue = 'Z') or (AValue = 'z') then
    Exit(True);
  if Length(AValue) = 0 then
    Exit;

  case AValue[1] of
    '+': Sign := 1;
    '-': Sign := -1;
  else
    Exit;
  end;

  Pos := 2;
  if not ReadInstantFixedDigits(AValue, Pos, 2, Hour) then
    Exit;
  Minute := 0;
  Second := 0;
  Ms := 0;
  Us := 0;
  Ns := 0;
  Extended := (Pos <= Length(AValue)) and (AValue[Pos] = ':');
  if Extended then
    Inc(Pos);

  if Pos <= Length(AValue) then
  begin
    if not ReadInstantFixedDigits(AValue, Pos, 2, Minute) then
      Exit;
    if Pos <= Length(AValue) then
    begin
      if Extended then
      begin
        if AValue[Pos] <> ':' then
          Exit;
        Inc(Pos);
      end;
      if not ReadInstantFixedDigits(AValue, Pos, 2, Second) then
        Exit;
      if not ReadInstantFractionalNanoseconds(AValue, Pos, Ms, Us, Ns) then
        Exit;
    end;
  end;

  if Pos <= Length(AValue) then
    Exit;
  if (Hour > 23) or (Minute > 59) or (Second > 59) then
    Exit;

  AOffsetNanoseconds := TBigInteger.FromInt64(Sign)
    .Multiply(TBigInteger.FromInt64(Int64(Hour) * NANOSECONDS_PER_HOUR))
    .Add(TBigInteger.FromInt64(Sign)
      .Multiply(TBigInteger.FromInt64(Int64(Minute) * NANOSECONDS_PER_MINUTE)))
    .Add(TBigInteger.FromInt64(Sign)
      .Multiply(TBigInteger.FromInt64(Int64(Second) * NANOSECONDS_PER_SECOND)))
    .Add(TBigInteger.FromInt64(Sign)
      .Multiply(TBigInteger.FromInt64(Int64(Ms) * NANOSECONDS_PER_MILLISECOND +
        Int64(Us) * NANOSECONDS_PER_MICROSECOND + Ns)));
  Result := True;
end;

procedure SplitInstantEpochNanoseconds(const AEpochNanoseconds: TBigInteger;
  out AEpochMilliseconds: Int64; out ASubMillisecondNanoseconds: Integer);
var
  UnitNs, MillisecondsBig, SubMillisecondBig: TBigInteger;
begin
  UnitNs := TBigInteger.FromInt64(NANOSECONDS_PER_MILLISECOND);
  MillisecondsBig := AEpochNanoseconds.Divide(UnitNs);
  SubMillisecondBig := AEpochNanoseconds.Modulo(UnitNs);
  if SubMillisecondBig.IsNegative then
  begin
    MillisecondsBig := MillisecondsBig.Subtract(TBigInteger.One);
    SubMillisecondBig := SubMillisecondBig.Add(UnitNs);
  end;

  AEpochMilliseconds := BigIntToInstantInt64(MillisecondsBig, 'epochMilliseconds');
  ASubMillisecondNanoseconds := Integer(SubMillisecondBig.ToInt64);
end;

function RoundInstantEpochNanosecondsToIncrement(const AValue, AIncrement: TBigInteger;
  const AMode: TTemporalRoundingMode): TBigInteger;
var
  Quotient, Remainder, Lower, Upper, DistanceToLower, DistanceToUpper: TBigInteger;
  Compare: Integer;
  IsNegative: Boolean;
begin
  IsNegative := AValue.IsNegative;
  Quotient := AValue.Divide(AIncrement);
  Remainder := AValue.Modulo(AIncrement);
  if Remainder.IsNegative then
  begin
    Quotient := Quotient.Subtract(TBigInteger.One);
    Remainder := Remainder.Add(AIncrement);
  end;
  Lower := Quotient.Multiply(AIncrement);
  if Remainder.IsZero then
    Exit(Lower);
  Upper := Lower.Add(AIncrement);

  case AMode of
    rmFloor:
      Exit(Lower);
    rmCeil:
      Exit(Upper);
    rmTrunc:
      if IsNegative then
        Exit(Upper)
      else
        Exit(Lower);
    rmExpand:
      if IsNegative then
        Exit(Lower)
      else
        Exit(Upper);
  else
    ;
  end;

  DistanceToLower := Remainder;
  DistanceToUpper := AIncrement.Subtract(Remainder);
  Compare := DistanceToLower.Compare(DistanceToUpper);
  if Compare < 0 then
    Exit(Lower);
  if Compare > 0 then
    Exit(Upper);

  case AMode of
    rmHalfFloor:
      Result := Lower;
    rmHalfTrunc:
      if IsNegative then
        Result := Upper
      else
        Result := Lower;
    rmHalfExpand:
      if IsNegative then
        Result := Lower
      else
        Result := Upper;
    rmHalfEven:
      if Quotient.GetBit(0) then
        Result := Upper
      else
        Result := Lower;
  else
    Result := Upper;
  end;
end;

procedure ValidateInstantRoundingIncrement(const AIncrement: Integer;
  const ASmallestUnit: TTemporalUnit);
var
  Maximum: Int64;
begin
  if AIncrement <= 1 then
    Exit;
  case ASmallestUnit of
    tuHour: Maximum := 24;
    tuMinute: Maximum := 24 * 60;
    tuSecond: Maximum := 24 * 60 * 60;
    tuMillisecond: Maximum := 24 * 60 * 60 * 1000;
    tuMicrosecond: Maximum := NANOSECONDS_PER_DAY div NANOSECONDS_PER_MICROSECOND;
    tuNanosecond: Maximum := NANOSECONDS_PER_DAY;
  else
    Exit;
  end;
  if (AIncrement > Maximum) or (Maximum mod AIncrement <> 0) then
    ThrowRangeError(Format(SErrorRoundingIncrementDivisor, [AIncrement, Maximum]),
      SSuggestTemporalRoundArg);
end;

function TryParseTemporalInstantString(const AValue: string;
  out AEpochMilliseconds: Int64; out ASubMillisecondNanoseconds: Integer): Boolean;
var
  DateRec: TTemporalDateRecord;
  TimeRec: TTemporalTimeRecord;
  Base, DatePart, TimeAndOffset, TimePart, OffsetPart: string;
  SepPos, I: Integer;
  OffsetNs, EpochNs: TBigInteger;
begin
  Result := False;
  Base := AValue;
  if Base = '' then
    Exit;
  if not StripInstantAnnotations(Base) then
    Exit;

  SepPos := 0;
  for I := 1 to Length(Base) do
  begin
    if (Base[I] = 'T') or (Base[I] = 't') or (Base[I] = ' ') then
    begin
      SepPos := I;
      Break;
    end;
  end;
  if SepPos = 0 then
    Exit;

  DatePart := Copy(Base, 1, SepPos - 1);
  TimeAndOffset := Copy(Base, SepPos + 1, Length(Base) - SepPos);
  if (DatePart = '') or (TimeAndOffset = '') then
    Exit;
  if not ParseInstantDatePart(DatePart, DateRec) then
    Exit;
  if not SplitInstantOffset(TimeAndOffset, TimePart, OffsetPart) then
    Exit;
  if (TimePart = '') or (OffsetPart = '') then
    Exit;
  if not ParseInstantTimePart(TimePart, TimeRec) then
    Exit;
  if not ParseInstantOffsetPart(OffsetPart, OffsetNs) then
    Exit;

  EpochNs := TBigInteger.FromInt64(DateToEpochDays(DateRec.Year, DateRec.Month, DateRec.Day))
    .Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_DAY))
    .Add(TBigInteger.FromInt64(Int64(TimeRec.Hour) * NANOSECONDS_PER_HOUR))
    .Add(TBigInteger.FromInt64(Int64(TimeRec.Minute) * NANOSECONDS_PER_MINUTE))
    .Add(TBigInteger.FromInt64(Int64(TimeRec.Second) * NANOSECONDS_PER_SECOND))
    .Add(TBigInteger.FromInt64(Int64(TimeRec.Millisecond) * NANOSECONDS_PER_MILLISECOND))
    .Add(TBigInteger.FromInt64(Int64(TimeRec.Microsecond) * NANOSECONDS_PER_MICROSECOND))
    .Add(TBigInteger.FromInt64(TimeRec.Nanosecond))
    .Subtract(OffsetNs);
  if not IsValidEpochNanoseconds(EpochNs) then
    Exit;
  SplitInstantEpochNanoseconds(EpochNs, AEpochMilliseconds,
    ASubMillisecondNanoseconds);
  Result := True;
end;

function DurationPow10(const ACount: Integer): TBigInteger;
var
  I: Integer;
begin
  Result := TBigInteger.One;
  for I := 1 to ACount do
    Result := Result.Multiply(TBigInteger.FromInt64(10));
end;

function FractionalDurationNanoseconds(const AFractionDigits: string;
  const AUnitNanoseconds: Int64): TBigInteger;
var
  Numerator, Scale, Product, Quotient, Remainder: TBigInteger;
begin
  if AFractionDigits = '' then
    Exit(TBigInteger.Zero);
  Numerator := TBigInteger.FromDecimalString(AFractionDigits);
  Scale := DurationPow10(Length(AFractionDigits));
  Product := Numerator.Multiply(TBigInteger.FromInt64(AUnitNanoseconds));
  Quotient := Product.Divide(Scale);
  Remainder := Product.Modulo(Scale);
  if Remainder.Multiply(TBigInteger.FromInt64(2)).Compare(Scale) >= 0 then
    Quotient := Quotient.Add(TBigInteger.One);
  Result := Quotient;
end;

function TryParseDurationStringAsBig(const AValue: string;
  out ADuration: TGocciaTemporalDurationValue): Boolean;
var
  Pos, NumberStart, FractionStart, Sign, LastUnitOrder, UnitOrder: Integer;
  InTimePart, HasAny, FractionSeen: Boolean;
  C: Char;
  NumberText, FractionText: string;
  Whole, FractionNs: TBigInteger;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: TBigInteger;

  procedure ApplySign(var AField: TBigInteger);
  begin
    if Sign < 0 then
      AField := AField.Negate;
  end;

begin
  Result := False;
  ADuration := nil;
  if Length(AValue) < 2 then
    Exit;
  Pos := 1;
  Sign := 1;
  if AValue[Pos] = '-' then
  begin
    Sign := -1;
    Inc(Pos);
  end
  else if AValue[Pos] = '+' then
    Inc(Pos);
  if (Pos > Length(AValue)) or (UpCase(AValue[Pos]) <> 'P') then
    Exit;
  Inc(Pos);

  InTimePart := False;
  HasAny := False;
  FractionSeen := False;
  LastUnitOrder := 0;
  Y := TBigInteger.Zero; Mo := TBigInteger.Zero; W := TBigInteger.Zero;
  Da := TBigInteger.Zero; H := TBigInteger.Zero; Mi := TBigInteger.Zero;
  S := TBigInteger.Zero; Ms := TBigInteger.Zero; Us := TBigInteger.Zero;
  Ns := TBigInteger.Zero;

  while Pos <= Length(AValue) do
  begin
    C := UpCase(AValue[Pos]);
    if C = 'T' then
    begin
      if InTimePart then
        Exit;
      InTimePart := True;
      Inc(Pos);
      if Pos > Length(AValue) then
        Exit;
      Continue;
    end;

    NumberStart := Pos;
    while (Pos <= Length(AValue)) and InstantIsASCIIDigit(AValue[Pos]) do
      Inc(Pos);
    if NumberStart = Pos then
      Exit;
    NumberText := Copy(AValue, NumberStart, Pos - NumberStart);
    FractionText := '';
    if (Pos <= Length(AValue)) and ((AValue[Pos] = '.') or (AValue[Pos] = ',')) then
    begin
      Inc(Pos);
      FractionStart := Pos;
      while (Pos <= Length(AValue)) and InstantIsASCIIDigit(AValue[Pos]) do
        Inc(Pos);
      if FractionStart = Pos then
        Exit;
      FractionText := Copy(AValue, FractionStart, Pos - FractionStart);
    end;
    if Pos > Length(AValue) then
      Exit;

    Whole := TBigInteger.FromDecimalString(NumberText);
    C := UpCase(AValue[Pos]);
    Inc(Pos);
    if FractionSeen then
      Exit;
    HasAny := True;

    if not InTimePart then
    begin
      if FractionText <> '' then
        Exit;
      case C of
        'Y': begin UnitOrder := 1; Y := Whole; end;
        'M': begin UnitOrder := 2; Mo := Whole; end;
        'W': begin UnitOrder := 3; W := Whole; end;
        'D': begin UnitOrder := 4; Da := Whole; end;
      else
        Exit;
      end;
    end
    else
    begin
      case C of
        'H':
        begin
          UnitOrder := 5;
          H := Whole;
          FractionNs := FractionalDurationNanoseconds(FractionText, NANOSECONDS_PER_HOUR);
          Ns := Ns.Add(FractionNs);
        end;
        'M':
        begin
          UnitOrder := 6;
          Mi := Whole;
          FractionNs := FractionalDurationNanoseconds(FractionText, NANOSECONDS_PER_MINUTE);
          Ns := Ns.Add(FractionNs);
        end;
        'S':
        begin
          UnitOrder := 7;
          S := Whole;
          FractionNs := FractionalDurationNanoseconds(FractionText, NANOSECONDS_PER_SECOND);
          Ns := Ns.Add(FractionNs);
        end;
      else
        Exit;
      end;
    end;

    if UnitOrder <= LastUnitOrder then
      Exit;
    LastUnitOrder := UnitOrder;
    if FractionText <> '' then
      FractionSeen := True;
  end;

  if not HasAny then
    Exit;
  ApplySign(Y); ApplySign(Mo); ApplySign(W); ApplySign(Da); ApplySign(H);
  ApplySign(Mi); ApplySign(S); ApplySign(Ms); ApplySign(Us); ApplySign(Ns);
  ADuration := TGocciaTemporalDurationValue.CreateFromBigIntegers(Y, Mo, W,
    Da, H, Mi, S, Ms, Us, Ns);
  Result := True;
end;

function CoerceInstantDuration(const AArg: TGocciaValue; const AMethod: string): TGocciaTemporalDurationValue;
var
  DurRec: TTemporalDurationRecord;
  Obj: TGocciaObjectValue;
  VF: TGocciaValue;
  Seen: Boolean;
  Y, Mo, W, Da, H, Mi, S, Ms, Us, Ns: TBigInteger;

  procedure ReadField(const AName: string; var ATarget: TBigInteger);
  begin
    VF := Obj.GetProperty(AName);
    if Assigned(VF) and not (VF is TGocciaUndefinedLiteralValue) then
    begin
      ATarget := DurationFieldToBigInteger(VF);
      Seen := True;
    end;
  end;

begin
  if AArg is TGocciaTemporalDurationValue then
    Result := TGocciaTemporalDurationValue(AArg)
  else if AArg is TGocciaStringLiteralValue then
  begin
    if TryParseDurationStringAsBig(TGocciaStringLiteralValue(AArg).Value, Result) then
      Exit;
    if not TryParseISODuration(TGocciaStringLiteralValue(AArg).Value, DurRec) then
      ThrowRangeError(SErrorInvalidDurationString, SSuggestTemporalDurationArg);
    Result := TGocciaTemporalDurationValue.Create(
      DurRec.Years, DurRec.Months, DurRec.Weeks, DurRec.Days,
      DurRec.Hours, DurRec.Minutes, DurRec.Seconds,
      DurRec.Milliseconds, DurRec.Microseconds, DurRec.Nanoseconds);
  end
  else if AArg is TGocciaObjectValue then
  begin
    Obj := TGocciaObjectValue(AArg);
    Seen := False;
    Y := TBigInteger.Zero;
    Mo := TBigInteger.Zero;
    W := TBigInteger.Zero;
    Da := TBigInteger.Zero;
    H := TBigInteger.Zero;
    Mi := TBigInteger.Zero;
    S := TBigInteger.Zero;
    Ms := TBigInteger.Zero;
    Us := TBigInteger.Zero;
    Ns := TBigInteger.Zero;

    ReadField('days', Da);
    ReadField('hours', H);
    ReadField('microseconds', Us);
    ReadField('milliseconds', Ms);
    ReadField('minutes', Mi);
    ReadField('months', Mo);
    ReadField('nanoseconds', Ns);
    ReadField('seconds', S);
    ReadField('weeks', W);
    ReadField('years', Y);

    if not Seen then
      ThrowTypeError(AMethod + ' requires at least one duration field',
        SSuggestTemporalDurationArg);
    Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(Y, Mo, W, Da,
      H, Mi, S, Ms, Us, Ns);
  end
  else
  begin
    ThrowTypeError(AMethod + ' requires a Duration, string, or object',
      SSuggestTemporalDurationArg);
    Result := nil;
  end;
end;

function GetInstantFractionalSecondDigits(const AOptions: TGocciaObjectValue): Integer;
var
  V: TGocciaValue;
  Num: TGocciaNumberLiteralValue;
  S: string;
begin
  Result := -1;
  if AOptions = nil then
    Exit;
  V := AOptions.GetProperty('fractionalSecondDigits');
  if (V = nil) or (V is TGocciaUndefinedLiteralValue) then
    Exit;
  if V is TGocciaNumberLiteralValue then
  begin
    Num := TGocciaNumberLiteralValue(V);
    if Num.IsNaN or Num.IsInfinity then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
    if (Num.Value < 0) or (Num.Value >= 10) then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
    if (Num.Value < 0) and (Frac(Num.Value) <> 0) then
      Result := Trunc(Num.Value) - 1
    else
      Result := Trunc(Num.Value);
    if (Result < 0) or (Result > 9) then
      ThrowRangeError(SErrorFractionalDigitsRange, SSuggestTemporalRoundArg);
  end
  else
  begin
    S := V.ToStringLiteral.Value;
    if S = 'auto' then
      Result := -1
    else
      ThrowRangeError(Format(SErrorInvalidFractionalDigits, [S]),
        SSuggestTemporalRoundArg);
  end;
end;

procedure ResolveInstantToStringOptions(const AOptions: TGocciaObjectValue;
  out AFractionalDigits: Integer; out ARoundingMode: TTemporalRoundingMode;
  out ASmallestUnit: string; out AHasSmallestUnit: Boolean);
var
  V: TGocciaValue;
begin
  AFractionalDigits := -1;
  ARoundingMode := rmTrunc;
  ASmallestUnit := '';
  AHasSmallestUnit := False;
  if AOptions = nil then
    Exit;

  AFractionalDigits := GetInstantFractionalSecondDigits(AOptions);
  ARoundingMode := GetRoundingMode(AOptions, rmTrunc);
  V := AOptions.GetProperty('smallestUnit');
  if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
  begin
    ASmallestUnit := V.ToStringLiteral.Value;
    AHasSmallestUnit := True;
  end;
end;

procedure ApplyInstantSmallestUnit(const ASmallestUnit: string;
  const AHasSmallestUnit: Boolean; var AFractionalDigits: Integer);
var
  SmallestUnit: TTemporalUnit;
begin
  if not AHasSmallestUnit then
    Exit;
  if not GetTemporalUnitFromString(ASmallestUnit, SmallestUnit) then
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [ASmallestUnit]),
      SSuggestTemporalValidUnits);
  case SmallestUnit of
    tuMinute: AFractionalDigits := -2;
    tuSecond: AFractionalDigits := 0;
    tuMillisecond: AFractionalDigits := 3;
    tuMicrosecond: AFractionalDigits := 6;
    tuNanosecond: AFractionalDigits := 9;
  else
    ThrowRangeError(Format(SErrorInvalidSmallestUnit, [ASmallestUnit]),
      SSuggestTemporalValidUnits);
  end;
end;

procedure RoundInstantTimeForToString(
  var AHour, AMinute, ASecond, AMs, AUs, ANs: Integer;
  var AExtraDays: Integer;
  const AFractionalDigits: Integer;
  const ARoundingMode: TTemporalRoundingMode);
var
  TotalNs, Divisor, Rounded, ExDays: Int64;
  BalancedTime: TTemporalTimeRecord;
begin
  AExtraDays := 0;
  if (AFractionalDigits < 0) and (AFractionalDigits <> -2) then
    Exit;
  if AFractionalDigits = 9 then
    Exit;

  TotalNs := Int64(AHour) * NANOSECONDS_PER_HOUR +
             Int64(AMinute) * NANOSECONDS_PER_MINUTE +
             Int64(ASecond) * NANOSECONDS_PER_SECOND +
             Int64(AMs) * NANOSECONDS_PER_MILLISECOND +
             Int64(AUs) * NANOSECONDS_PER_MICROSECOND +
             Int64(ANs);
  if AFractionalDigits = -2 then
    Divisor := NANOSECONDS_PER_MINUTE
  else if AFractionalDigits = 0 then
    Divisor := NANOSECONDS_PER_SECOND
  else
  begin
    Divisor := 1;
    while Divisor < NANOSECONDS_PER_SECOND do
    begin
      if Length(IntToStr(Divisor)) > AFractionalDigits then
        Break;
      Divisor := Divisor * 10;
    end;
    Divisor := NANOSECONDS_PER_SECOND div Divisor;
  end;

  Rounded := RoundWithMode(TotalNs, Divisor, ARoundingMode);
  BalancedTime := BalanceTime(0, 0, 0, 0, 0, Rounded, ExDays);
  AHour := BalancedTime.Hour;
  AMinute := BalancedTime.Minute;
  ASecond := BalancedTime.Second;
  AMs := BalancedTime.Millisecond;
  AUs := BalancedTime.Microsecond;
  ANs := BalancedTime.Nanosecond;
  AExtraDays := Integer(ExDays);
end;

function CoerceTemporalInstant(const AValue: TGocciaValue; const AMethod: string): TGocciaTemporalInstantValue;
var
  EpochMs: Int64;
  SubMs: Integer;
  Source: string;
begin
  if AValue is TGocciaTemporalInstantValue then
    Exit(TGocciaTemporalInstantValue(AValue));
  if AValue is TGocciaTemporalZonedDateTimeValue then
    Exit(TGocciaTemporalInstantValue.Create(
      TGocciaTemporalZonedDateTimeValue(AValue).EpochMilliseconds,
      TGocciaTemporalZonedDateTimeValue(AValue).SubMillisecondNanoseconds));
  if AValue is TGocciaStringLiteralValue then
    Source := TGocciaStringLiteralValue(AValue).Value
  else if AValue is TGocciaObjectValue then
    Source := AValue.ToStringLiteral.Value
  else
  begin
    ThrowTypeError(AMethod + ' requires an Instant, ZonedDateTime, string, or object',
      SSuggestTemporalFromArg);
    Exit(nil);
  end;

  if not TryParseTemporalInstantString(Source, EpochMs, SubMs) then
    ThrowRangeError(Format(SErrorTemporalInvalidISOStringFor, ['instant', AMethod]),
      SSuggestTemporalISOFormat);
  Result := TGocciaTemporalInstantValue.Create(EpochMs, SubMs);
end;

{ TGocciaTemporalInstantValue }

constructor TGocciaTemporalInstantValue.Create(const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer);
var
  Shared: TGocciaSharedPrototype;
begin
  inherited Create(nil);
  FEpochMilliseconds := AEpochMilliseconds;
  FSubMillisecondNanoseconds := ASubMillisecondNanoseconds;

  // Normalize sub-ms nanoseconds into 0..999999 range
  if (FSubMillisecondNanoseconds >= 1000000) or (FSubMillisecondNanoseconds < 0) then
  begin
    if FSubMillisecondNanoseconds >= 0 then
    begin
      Inc(FEpochMilliseconds, FSubMillisecondNanoseconds div 1000000);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds mod 1000000;
    end
    else
    begin
      // For negative values, borrow enough whole milliseconds
      // (FSubMillisecondNanoseconds - 999999) div 1000000 gives the negative delta
      Inc(FEpochMilliseconds, (FSubMillisecondNanoseconds - 999999) div 1000000);
      FSubMillisecondNanoseconds := FSubMillisecondNanoseconds - ((FSubMillisecondNanoseconds - 999999) div 1000000) * 1000000;
    end;
  end;

  InitializePrototype;
  Shared := GetTemporalInstantShared;
  if Assigned(Shared) then
    FPrototype := Shared.Prototype;
end;

procedure TGocciaTemporalInstantValue.InitializePrototype;
var
  Members: TGocciaMemberCollection;
  Shared: TGocciaSharedPrototype;
begin
  if not Assigned(CurrentRealm) then Exit;
  if Assigned(GetTemporalInstantShared) then Exit;
  Shared := TGocciaSharedPrototype.Create(Self);
  CurrentRealm.SetOwnedSlot(GTemporalInstantSharedSlot, Shared);
  if Length(FPrototypeMembers) = 0 then
  begin
    Members := TGocciaMemberCollection.Create;
    try
      Members.AddAccessor('epochMilliseconds', GetEpochMilliseconds, nil, [pfConfigurable]);
      Members.AddAccessor('epochNanoseconds', GetEpochNanoseconds, nil, [pfConfigurable]);
      Members.AddMethod(InstantAdd, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantSubtract, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantUntil, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantSince, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantRound, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantEquals, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToLocaleString, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToJSON, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantValueOf, 0, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddMethod(InstantToZonedDateTimeISO, 1, gmkPrototypeMethod, [gmfNoFunctionPrototype]);
      Members.AddSymbolDataProperty(
        TGocciaSymbolValue.WellKnownToStringTag,
        TGocciaStringLiteralValue.Create('Temporal.Instant'),
        [pfConfigurable]);
      FPrototypeMembers := Members.ToDefinitions;
    finally
      Members.Free;
    end;
  end;
  RegisterMemberDefinitions(Shared.Prototype, FPrototypeMembers);
end;

class procedure TGocciaTemporalInstantValue.ExposePrototype(const AConstructor: TGocciaObjectValue);
var
  Shared: TGocciaSharedPrototype;
begin
  Shared := GetTemporalInstantShared;
  if not Assigned(Shared) then
  begin
    TGocciaTemporalInstantValue.Create(0, 0);
    Shared := GetTemporalInstantShared;
  end;
  if Assigned(Shared) then
    ExposeSharedPrototypeOnConstructor(Shared, AConstructor);
end;

function TGocciaTemporalInstantValue.ToStringTag: string;
begin
  Result := 'Temporal.Instant';
end;

{ Getters }

function TGocciaTemporalInstantValue.GetEpochMilliseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  Result := TGocciaNumberLiteralValue.Create(AsInstant(AThisValue, 'get Instant.epochMilliseconds').FEpochMilliseconds);
end;

function TGocciaTemporalInstantValue.GetEpochNanoseconds(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  BigNs: TBigInteger;
begin
  Inst := AsInstant(AThisValue, 'get Instant.epochNanoseconds');
  BigNs := TBigInteger.FromInt64(Inst.FEpochMilliseconds)
    .Multiply(TBigInteger.FromInt64(1000000))
    .Add(TBigInteger.FromInt64(Inst.FSubMillisecondNanoseconds));
  Result := TGocciaBigIntValue.Create(BigNs);
end;

{ Methods }

function TGocciaTemporalInstantValue.InstantAdd(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Dur: TGocciaTemporalDurationValue;
  NewMs: Int64;
  NewSubMs: Integer;
  NewMsBig, SubNsTotal: TBigInteger;
begin
  try
  Inst := AsInstant(AThisValue, 'Instant.prototype.add');
  Dur := CoerceInstantDuration(AArgs.GetElement(0), 'Instant.prototype.add');

  // Instant only supports time-based duration
  if (not Dur.YearsBig.IsZero) or (not Dur.MonthsBig.IsZero) or
     (not Dur.WeeksBig.IsZero) or (not Dur.DaysBig.IsZero) then
    ThrowRangeError(SErrorInstantAddNoCalendar, SSuggestTemporalDurationArg);

  NewMsBig := TBigInteger.FromInt64(Inst.FEpochMilliseconds)
    .Add(Dur.DaysBig.Multiply(TBigInteger.FromInt64(86400000)))
    .Add(Dur.HoursBig.Multiply(TBigInteger.FromInt64(3600000)))
    .Add(Dur.MinutesBig.Multiply(TBigInteger.FromInt64(60000)))
    .Add(Dur.SecondsBig.Multiply(TBigInteger.FromInt64(1000)))
    .Add(Dur.MillisecondsBig);
  SubNsTotal := TBigInteger.FromInt64(Inst.FSubMillisecondNanoseconds)
    .Add(Dur.MicrosecondsBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_MICROSECOND)))
    .Add(Dur.NanosecondsBig);
  NormalizeInstantSubMillisecondNanoseconds(NewMsBig, SubNsTotal, NewSubMs);
  NewMs := BigIntToInstantInt64(NewMsBig, 'epochMilliseconds');
  if not IsValidEpochNanoseconds(EpochNanosecondsFromParts(NewMs, NewSubMs)) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function TGocciaTemporalInstantValue.InstantSubtract(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Dur: TGocciaTemporalDurationValue;
  NewMs: Int64;
  NewSubMs: Integer;
  NewMsBig, SubNsTotal: TBigInteger;
begin
  try
  Inst := AsInstant(AThisValue, 'Instant.prototype.subtract');
  Dur := CoerceInstantDuration(AArgs.GetElement(0), 'Instant.prototype.subtract');

  if (not Dur.YearsBig.IsZero) or (not Dur.MonthsBig.IsZero) or
     (not Dur.WeeksBig.IsZero) or (not Dur.DaysBig.IsZero) then
    ThrowRangeError(SErrorInstantSubtractNoCalendar, SSuggestTemporalDurationArg);

  NewMsBig := TBigInteger.FromInt64(Inst.FEpochMilliseconds)
    .Subtract(Dur.DaysBig.Multiply(TBigInteger.FromInt64(86400000)))
    .Subtract(Dur.HoursBig.Multiply(TBigInteger.FromInt64(3600000)))
    .Subtract(Dur.MinutesBig.Multiply(TBigInteger.FromInt64(60000)))
    .Subtract(Dur.SecondsBig.Multiply(TBigInteger.FromInt64(1000)))
    .Subtract(Dur.MillisecondsBig);
  SubNsTotal := TBigInteger.FromInt64(Inst.FSubMillisecondNanoseconds)
    .Subtract(Dur.MicrosecondsBig.Multiply(TBigInteger.FromInt64(NANOSECONDS_PER_MICROSECOND)))
    .Subtract(Dur.NanosecondsBig);
  NormalizeInstantSubMillisecondNanoseconds(NewMsBig, SubNsTotal, NewSubMs);
  NewMs := BigIntToInstantInt64(NewMsBig, 'epochMilliseconds');
  if not IsValidEpochNanoseconds(EpochNanosecondsFromParts(NewMs, NewSubMs)) then
    ThrowRangeError(SErrorTemporalInstantOutOfRange, SSuggestTemporalInstantRange);

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
  except
    on E: ETemporalDurationInt64Overflow do
      ThrowRangeError(E.Message, SSuggestTemporalDurationRange);
  end;
end;

function InstantDiffToUnits(const ATimeDuration: TBigInteger;
  const ALargestUnit: TTemporalUnit): TGocciaValue;
var
  Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds: TBigInteger;
  Days: Int64;

  function ToFloat64Representable(const AValue: TBigInteger): TBigInteger;
  begin
    Result := TBigInteger.FromDouble(AValue.ToDouble);
  end;

begin
  BalanceTimeDurationToFields(ATimeDuration, ALargestUnit,
    Hours, Minutes, Seconds, Milliseconds, Microseconds, Nanoseconds, Days);
  Result := TGocciaTemporalDurationValue.CreateFromBigIntegers(
    TBigInteger.Zero, TBigInteger.Zero, TBigInteger.Zero, TBigInteger.FromInt64(Days),
    ToFloat64Representable(Hours), ToFloat64Representable(Minutes),
    ToFloat64Representable(Seconds), ToFloat64Representable(Milliseconds),
    ToFloat64Representable(Microseconds), ToFloat64Representable(Nanoseconds));
end;

function DefaultInstantLargestUnit(const ASmallestUnit: TTemporalUnit): TTemporalUnit;
begin
  if ASmallestUnit in [tuHour, tuMinute] then
    Result := ASmallestUnit
  else
    Result := tuSecond;
end;

procedure ValidateInstantDiffRoundingIncrement(const AIncrement: Integer;
  const ASmallestUnit, ALargestUnit: TTemporalUnit);
var
  MaxVal: Integer;
begin
  if ASmallestUnit = ALargestUnit then
    Exit;
  if AIncrement <= 1 then
    Exit;
  case ASmallestUnit of
    tuHour: MaxVal := 24;
    tuMinute,
    tuSecond: MaxVal := 60;
    tuMillisecond,
    tuMicrosecond,
    tuNanosecond: MaxVal := 1000;
  else
    Exit;
  end;
  if (AIncrement >= MaxVal) or (MaxVal mod AIncrement <> 0) then
    ThrowRangeError(Format(SErrorRoundingIncrementDivisor, [AIncrement, MaxVal]),
      SSuggestTemporalRoundArg);
end;

function TGocciaTemporalInstantValue.InstantUntil(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  StartNs, EndNs, DiffNs, IncrementNs: TBigInteger;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.until');
  Other := CoerceTemporalInstant(AArgs.GetElement(0), 'Instant.prototype.until');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if LargestUnit = tuAuto then
    LargestUnit := DefaultInstantLargestUnit(SmallestUnit);
  if not (LargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.until', 'largestUnit']), SSuggestTemporalValidUnits);

  if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.until', 'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateInstantDiffRoundingIncrement(RIncrement, SmallestUnit, LargestUnit);

  StartNs := EpochNanosecondsFromParts(Inst.FEpochMilliseconds, Inst.FSubMillisecondNanoseconds);
  EndNs := EpochNanosecondsFromParts(Other.FEpochMilliseconds, Other.FSubMillisecondNanoseconds);
  DiffNs := TimeDurationFromEpochNanosecondsDifference(EndNs, StartNs);
  if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
  begin
    IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
      .Multiply(TBigInteger.FromInt64(RIncrement));
    DiffNs := RoundTimeDurationToIncrement(DiffNs, IncrementNs, RMode);
  end;
  Result := InstantDiffToUnits(DiffNs, LargestUnit);
end;

function TGocciaTemporalInstantValue.InstantSince(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
  OptionsObj: TGocciaObjectValue;
  LargestUnit, SmallestUnit: TTemporalUnit;
  RMode: TTemporalRoundingMode;
  RIncrement: Integer;
  StartNs, EndNs, DiffNs, IncrementNs: TBigInteger;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.since');
  Other := CoerceTemporalInstant(AArgs.GetElement(0), 'Instant.prototype.since');

  OptionsObj := GetDiffOptions(AArgs, 1);
  LargestUnit := GetLargestUnit(OptionsObj, tuAuto);
  RIncrement := GetRoundingIncrement(OptionsObj, 1);
  RMode := GetRoundingMode(OptionsObj, rmTrunc);
  SmallestUnit := GetSmallestUnit(OptionsObj, tuNanosecond);
  if LargestUnit = tuAuto then
    LargestUnit := DefaultInstantLargestUnit(SmallestUnit);
  if not (LargestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.since', 'largestUnit']), SSuggestTemporalValidUnits);

  if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond, tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.since', 'smallestUnit']), SSuggestTemporalValidUnits);
  if Ord(LargestUnit) > Ord(SmallestUnit) then
    ThrowRangeError(SErrorDurationRoundLargestSmallerThanSmallest, SSuggestTemporalRoundArg);
  ValidateInstantDiffRoundingIncrement(RIncrement, SmallestUnit, LargestUnit);

  StartNs := EpochNanosecondsFromParts(Other.FEpochMilliseconds, Other.FSubMillisecondNanoseconds);
  EndNs := EpochNanosecondsFromParts(Inst.FEpochMilliseconds, Inst.FSubMillisecondNanoseconds);
  DiffNs := TimeDurationFromEpochNanosecondsDifference(EndNs, StartNs);
  if (SmallestUnit <> tuNanosecond) or (RIncrement <> 1) then
  begin
    IncrementNs := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
      .Multiply(TBigInteger.FromInt64(RIncrement));
    DiffNs := RoundTimeDurationToIncrement(DiffNs, IncrementNs, RMode);
  end;
  Result := InstantDiffToUnits(DiffNs, LargestUnit);
end;

function TGocciaTemporalInstantValue.InstantRound(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  UnitStr: string;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  NewMs: Int64;
  NewSubMs: Integer;
  SmallestUnit: TTemporalUnit;
  Mode: TTemporalRoundingMode;
  Increment: Integer;
  BigTotal, BigDivisor, BigRounded, BigMillion: TBigInteger;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.round');
  Arg := AArgs.GetElement(0);

  Mode := rmHalfExpand;
  Increment := 1;

  if Arg is TGocciaStringLiteralValue then
  begin
    UnitStr := TGocciaStringLiteralValue(Arg).Value;
    if not GetTemporalUnitFromString(UnitStr, SmallestUnit) then
      ThrowRangeError(Format(SErrorTemporalInvalidUnitFor, ['Instant.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
  end
  else if Arg is TGocciaObjectValue then
  begin
    OptionsObj := TGocciaObjectValue(Arg);
    Increment := GetRoundingIncrement(OptionsObj, 1);
    Mode := GetRoundingMode(OptionsObj, rmHalfExpand);
    SmallestUnit := GetSmallestUnit(OptionsObj, tuNone);
    if SmallestUnit = tuNone then
      ThrowRangeError(SErrorTemporalRoundRequiresSmallestUnit, SSuggestTemporalRoundArg);
  end
  else
  begin
    ThrowTypeError(Format(SErrorTemporalRoundRequiresStringOrOptions, ['Instant']), SSuggestTemporalRoundArg);
    SmallestUnit := tuNanosecond;
  end;

  case SmallestUnit of
    tuHour: UnitStr := 'hour';
    tuMinute: UnitStr := 'minute';
    tuSecond: UnitStr := 'second';
    tuMillisecond: UnitStr := 'millisecond';
    tuMicrosecond: UnitStr := 'microsecond';
    tuNanosecond: UnitStr := 'nanosecond';
    tuDay: UnitStr := 'day';
  else
    UnitStr := '';
  end;
  if not (SmallestUnit in [tuHour, tuMinute, tuSecond, tuMillisecond,
    tuMicrosecond, tuNanosecond]) then
    ThrowRangeError(Format(SErrorTemporalInvalidUnitFor,
      ['Instant.prototype.round', UnitStr]), SSuggestTemporalValidUnits);
  ValidateInstantRoundingIncrement(Increment, SmallestUnit);

  // Use TBigInteger to avoid Int64 overflow on epochMs * 1000000
  BigMillion := TBigInteger.FromInt64(1000000);
  BigTotal := TBigInteger.FromInt64(Inst.FEpochMilliseconds)
    .Multiply(BigMillion)
    .Add(TBigInteger.FromInt64(Inst.FSubMillisecondNanoseconds));
  BigDivisor := TBigInteger.FromInt64(UnitToNanoseconds(SmallestUnit))
    .Multiply(TBigInteger.FromInt64(Increment));
  BigRounded := RoundInstantEpochNanosecondsToIncrement(BigTotal, BigDivisor, Mode);

  SplitInstantEpochNanoseconds(BigRounded, NewMs, NewSubMs);

  Result := TGocciaTemporalInstantValue.Create(NewMs, NewSubMs);
end;

function TGocciaTemporalInstantValue.InstantEquals(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst, Other: TGocciaTemporalInstantValue;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.equals');
  Other := CoerceTemporalInstant(AArgs.GetElement(0), 'Instant.prototype.equals');

  if (Inst.FEpochMilliseconds = Other.FEpochMilliseconds) and
     (Inst.FSubMillisecondNanoseconds = Other.FSubMillisecondNanoseconds) then
    Result := TGocciaBooleanLiteralValue.TrueValue
  else
    Result := TGocciaBooleanLiteralValue.FalseValue;
end;

function TGocciaTemporalInstantValue.InstantToString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  EpochDays, RemainingMs: Int64;
  Hour, Minute, Second, Ms: Integer;
  Us, Ns: Integer;
  Arg: TGocciaValue;
  OptionsObj: TGocciaObjectValue;
  FracDigits, ExtraDays: Integer;
  Mode: TTemporalRoundingMode;
  S: string;
  V: TGocciaValue;
  HasTimeZone: Boolean;
  HasSmallestUnit: Boolean;
  TimeZoneId, OffsetText: string;
  SmallestUnitStr: string;
  OffsetSeconds: Integer;
  LocalEpochMilliseconds: Int64;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.toString');

  HasTimeZone := False;
  TimeZoneId := 'UTC';
  OffsetSeconds := 0;

  OptionsObj := nil;
  Arg := AArgs.GetElement(0);
  if Assigned(Arg) and not (Arg is TGocciaUndefinedLiteralValue) then
  begin
    if not (Arg is TGocciaObjectValue) then
      ThrowTypeError('options must be an object or undefined', SSuggestTemporalFromArg);
    OptionsObj := TGocciaObjectValue(Arg);
  end;

  ResolveInstantToStringOptions(OptionsObj, FracDigits, Mode,
    SmallestUnitStr, HasSmallestUnit);
  if Assigned(OptionsObj) then
  begin
    V := OptionsObj.GetProperty('timeZone');
    if Assigned(V) and not (V is TGocciaUndefinedLiteralValue) then
    begin
      if not (V is TGocciaStringLiteralValue) then
        ThrowTypeError('Instant.prototype.toString timeZone must be a string',
          SSuggestTemporalTimezone);
      TimeZoneId := CanonicalizeTemporalTimeZoneIdentifier(
        TGocciaStringLiteralValue(V).Value);
      OffsetSeconds := GetUtcOffsetSeconds(TimeZoneId,
        InstantEpochSecond(Inst.FEpochMilliseconds));
      HasTimeZone := True;
    end;
  end;
  ApplyInstantSmallestUnit(SmallestUnitStr, HasSmallestUnit, FracDigits);

  LocalEpochMilliseconds := Inst.FEpochMilliseconds +
    Int64(OffsetSeconds) * 1000;

  EpochDays := LocalEpochMilliseconds div Int64(86400000);
  RemainingMs := LocalEpochMilliseconds mod Int64(86400000);
  if RemainingMs < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMs, Int64(86400000));
  end;

  DateRec := EpochDaysToDate(EpochDays);
  Hour := Integer(RemainingMs div 3600000);
  RemainingMs := RemainingMs mod 3600000;
  Minute := Integer(RemainingMs div 60000);
  RemainingMs := RemainingMs mod 60000;
  Second := Integer(RemainingMs div 1000);
  Ms := Integer(RemainingMs mod 1000);

  Us := Inst.FSubMillisecondNanoseconds div 1000;
  Ns := Inst.FSubMillisecondNanoseconds mod 1000;

  ExtraDays := 0;
  RoundInstantTimeForToString(Hour, Minute, Second, Ms, Us, Ns, ExtraDays,
    FracDigits, Mode);
  if ExtraDays <> 0 then
    DateRec := AddDaysToDate(DateRec.Year, DateRec.Month, DateRec.Day, ExtraDays);

  if HasTimeZone then
    OffsetText := FormatInstantOffsetString(OffsetSeconds)
  else
    OffsetText := 'Z';

  if FracDigits = -2 then // smallestUnit: minute
    S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
         PadTwo(Hour) + ':' + PadTwo(Minute) + OffsetText
  else
    S := FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
         FormatTimeWithPrecision(Hour, Minute, Second, Ms, Us, Ns, FracDigits) + OffsetText;

  Result := TGocciaStringLiteralValue.Create(S);
end;

function TGocciaTemporalInstantValue.InstantToJSON(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  DateRec: TTemporalDateRecord;
  EpochDays, RemainingMs: Int64;
  Hour, Minute, Second, Ms: Integer;
  Us, Ns: Integer;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.toJSON');

  EpochDays := Inst.FEpochMilliseconds div Int64(86400000);
  RemainingMs := Inst.FEpochMilliseconds mod Int64(86400000);
  if RemainingMs < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMs, Int64(86400000));
  end;

  DateRec := EpochDaysToDate(EpochDays);
  Hour := Integer(RemainingMs div 3600000);
  RemainingMs := RemainingMs mod 3600000;
  Minute := Integer(RemainingMs div 60000);
  RemainingMs := RemainingMs mod 60000;
  Second := Integer(RemainingMs div 1000);
  Ms := Integer(RemainingMs mod 1000);

  Us := Inst.FSubMillisecondNanoseconds div 1000;
  Ns := Inst.FSubMillisecondNanoseconds mod 1000;

  Result := TGocciaStringLiteralValue.Create(
    FormatDateString(DateRec.Year, DateRec.Month, DateRec.Day) + 'T' +
    FormatTimeString(Hour, Minute, Second, Ms, Us, Ns) + 'Z');
end;

function TGocciaTemporalInstantValue.InstantValueOf(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  ThrowTypeError(Format(SErrorTemporalValueOf, ['Instant', 'epochMilliseconds, epochNanoseconds, or compare']), SSuggestTemporalNoValueOf);
  Result := nil;
end;

function TGocciaTemporalInstantValue.InstantToLocaleString(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
begin
  AsInstant(AThisValue, 'Instant.prototype.toLocaleString');
  Result := FormatTemporalValueToLocaleString(AThisValue, AArgs.GetElement(0),
    AArgs.GetElement(1));
end;

function TGocciaTemporalInstantValue.InstantToZonedDateTimeISO(const AArgs: TGocciaArgumentsCollection; const AThisValue: TGocciaValue): TGocciaValue;
var
  Inst: TGocciaTemporalInstantValue;
  Arg: TGocciaValue;
  TZ: string;
begin
  Inst := AsInstant(AThisValue, 'Instant.prototype.toZonedDateTimeISO');
  if AArgs.Length < 1 then
    ThrowTypeError('Instant.prototype.toZonedDateTimeISO requires a time zone argument', SSuggestTemporalTimezone);
  Arg := AArgs.GetElement(0);
  if Arg is TGocciaTemporalZonedDateTimeValue then
    TZ := TGocciaTemporalZonedDateTimeValue(Arg).TimeZone
  else if Arg is TGocciaStringLiteralValue then
    TZ := TGocciaStringLiteralValue(Arg).Value
  else
  begin
    ThrowTypeError('Instant.prototype.toZonedDateTimeISO requires a string time zone or ZonedDateTime', SSuggestTemporalTimezone);
    TZ := '';
  end;
  Result := TGocciaTemporalZonedDateTimeValue.Create(
    Inst.FEpochMilliseconds, Inst.FSubMillisecondNanoseconds, TZ);
end;

initialization
  RegisterThreadvarCleanup(@ClearThreadvarMembers);
  GTemporalInstantSharedSlot := RegisterRealmOwnedSlot('Temporal.Instant.shared');

end.

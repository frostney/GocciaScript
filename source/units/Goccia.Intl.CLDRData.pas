unit Goccia.Intl.CLDRData;

{$I Goccia.inc}

interface

uses
  IntlTypes;

function TryGetLikelySubtags(const ATag: string; out AMaximized: string): Boolean;
function TryGetLanguageAlias(const ATag: string; out AReplacement: string): Boolean;
function TryGetTerritoryAlias(const ACode: string; out AReplacement: string): Boolean;
function TryGetScriptAlias(const ACode: string; out AReplacement: string): Boolean;
function TryGetGrandfatheredTag(const ATag: string; out AReplacement: string): Boolean;
function TryGetPluralRules(const ALocale: string; ACardinal: Boolean;
  out ARules: TIntlPluralRuleSet): Boolean;
function TryGetNumberPattern(const ALocale: string; AStyle: TIntlNumberStyle;
  out APattern: string): Boolean;
function TryGetDatePattern(const ALocale: string;
  ADateStyle, ATimeStyle: TIntlDateTimeStyle; out APattern: string): Boolean;
function TryGetListPattern(const ALocale: string; AType: TIntlListFormatType;
  AStyle: TIntlListFormatStyle; out APattern: TIntlListPattern): Boolean;
function TryGetRelativeTimePattern(const ALocale: string;
  AUnit: TIntlRelativeTimeUnit; out APattern: TIntlRelativeTimePattern): Boolean;
function TryGetDisplayName(const ALocale, ACode: string;
  AType: TIntlDisplayNameType; out AName: string): Boolean;
function TryGetCurrencyInfo(const ALocale, ACurrency: string;
  out ASymbol, ANarrowSymbol: string; out ADigits: Integer): Boolean;
function TryGetNumberSymbol(const ALocale, ASymbolName: string;
  out AValue: string): Boolean;
function TryGetLocaleCalendars(const ARegion: string; out ACalendars: IntlTypes.TStringArray): Boolean;
function TryGetLocaleCollations(out ACollations: IntlTypes.TStringArray): Boolean;
function TryGetLocaleHourCycles(const ALocale, ARegion: string;
  out AHourCycles: IntlTypes.TStringArray): Boolean;
function TryGetLocaleNumberingSystems(const ALocale: string;
  out ANumberingSystems: IntlTypes.TStringArray): Boolean;
function TryGetLocaleTimeZones(const ARegion: string; out ATimeZones: IntlTypes.TStringArray): Boolean;
function TryGetLocaleTextDirection(const ALocale: string; out ADirection: string): Boolean;
function TryGetLocaleWeekInfo(const ARegion: string; out AFirstDay,
  AWeekendStart, AWeekendEnd, AMinimalDays: Integer): Boolean;

implementation

{$IFDEF GOCCIA_INTL_EMBEDDED_CLDR}
uses
  Classes,
  SysUtils,

  EmbeddedResourceReader,
  Generated.IntlData,
  LazyPublishedCache;

const
  CLDR_RCDATA_RESOURCE_TYPE = MAKEINTRESOURCE(10);
  CLDR_DATA_MAGIC: TEmbeddedResourceMagic =
    (Ord('G'), Ord('O'), Ord('C'), Ord('C'), Ord('I'), Ord('A'), Ord('C'), Ord('L'));

var
  CLDRResourceCache: TLazyPublishedCache<TBytes>;

function LoadCLDRResource(const AKey: string; out ABuffer: TBytes): Boolean;
var
  Stream: TResourceStream;
  BufferSize: Integer;
begin
  Result := False;
  SetLength(ABuffer, 0);
  Stream := nil;
  try
    Stream := TResourceStream.Create(HInstance, AKey,
      CLDR_RCDATA_RESOURCE_TYPE);
    if Stream.Size > High(Integer) then
    begin
      Stream.Free;
      Exit;
    end;

    BufferSize := Integer(Stream.Size);
    SetLength(ABuffer, BufferSize);
    if BufferSize > 0 then
      Stream.ReadBuffer(ABuffer[0], BufferSize);
    Stream.Free;
    Stream := nil;
    Result := True;
  except
    Stream.Free;
  end;
end;

function TryReadEmbeddedResource(out ABuffer: TBytes): Boolean;
begin
  if CLDRResourceCache.Ensure(GeneratedIntlDataResourceName, @LoadCLDRResource) then
    ABuffer := CLDRResourceCache.Data
  else
    SetLength(ABuffer, 0);
  Result := Length(ABuffer) > 0;
end;

function TryGetSectionData(const ASectionName: string;
  out AResource: TBytes; out ADataOffset, ADataLength: Integer): Boolean;
var
  Container: TEmbeddedResourceContainer;
  Entry: TEmbeddedResourceEntry;
begin
  Result := False;
  ADataOffset := 0;
  ADataLength := 0;
  SetLength(AResource, 0);

  if not TryReadEmbeddedResource(AResource) then
    Exit;

  if not TryReadEmbeddedResourceContainer(AResource, CLDR_DATA_MAGIC,
     Container) then
    Exit;

  if not TryFindEmbeddedResourceEntry(AResource, ASectionName, Container,
     Entry) then
    Exit;

  Result := TryGetEmbeddedResourceEntryDataBounds(AResource, Container, Entry,
    ADataOffset, ADataLength);
end;

function TryGetSectionDataWithFallback(const APrefix, ALocale: string;
  out AResource: TBytes; out ADataOffset, ADataLength: Integer): Boolean;
var
  SectionName, FallbackLocale: string;
  DashPos: Integer;
begin
  SectionName := APrefix + ALocale;
  Result := TryGetSectionData(SectionName, AResource, ADataOffset, ADataLength);
  if Result then
    Exit;

  FallbackLocale := ALocale;
  repeat
    DashPos := Length(FallbackLocale);
    while (DashPos > 0) and (FallbackLocale[DashPos] <> '-') do
      Dec(DashPos);
    if DashPos <= 0 then
      Exit;
    FallbackLocale := Copy(FallbackLocale, 1, DashPos - 1);
    SectionName := APrefix + FallbackLocale;
    Result := TryGetSectionData(SectionName, AResource, ADataOffset, ADataLength);
  until Result;
end;

function TryGetKeyValue(const ABuffer: TBytes; ADataOffset, ADataLength: Integer;
  const AKey: string; out AValue: string): Boolean;
var
  Count: Integer;
  StringAreaOffset: Integer;
  LowIndex, HighIndex, MiddleIndex: Integer;
  EntryOffset: Integer;
  KeyOffset, KeyLength, ValueOffset, ValueLength: Integer;
  AbsoluteKeyOffset: Int64;
  EntryKey: string;
  CompareResult: Integer;
begin
  Result := False;
  AValue := '';

  if ADataLength < 4 then
    Exit;

  if not HasBytesAvailable(ABuffer, ADataOffset, 4) then
    Exit;

  if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, ADataOffset), Count) then
    Exit;

  if Count = 0 then
    Exit;

  StringAreaOffset := ADataOffset + 4 + Count * 16;
  if not HasBytesAvailable(ABuffer, ADataOffset, ADataLength) then
    Exit;

  LowIndex := 0;
  HighIndex := Count - 1;

  while LowIndex <= HighIndex do
  begin
    MiddleIndex := LowIndex + (HighIndex - LowIndex) div 2;
    EntryOffset := ADataOffset + 4 + MiddleIndex * 16;

    if not HasBytesAvailable(ABuffer, EntryOffset, 16) then
      Exit;

    if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, EntryOffset), KeyOffset) then
      Exit;
    if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, EntryOffset + 4), KeyLength) then
      Exit;
    if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, EntryOffset + 8), ValueOffset) then
      Exit;
    if not TryUInt32ToInteger(ReadUInt32LE(ABuffer, EntryOffset + 12), ValueLength) then
      Exit;

    AbsoluteKeyOffset := Int64(StringAreaOffset) + KeyOffset;
    if (AbsoluteKeyOffset < 0) or (AbsoluteKeyOffset > High(Integer)) then
      Exit;

    if not HasBytesAvailable(ABuffer, Integer(AbsoluteKeyOffset), KeyLength) then
      Exit;

    EntryKey := CopyStringFromBytes(ABuffer, Integer(AbsoluteKeyOffset), KeyLength);
    CompareResult := CompareStr(AKey, EntryKey);

    if CompareResult = 0 then
    begin
      AbsoluteKeyOffset := Int64(StringAreaOffset) + ValueOffset;
      if (AbsoluteKeyOffset < 0) or (AbsoluteKeyOffset > High(Integer)) then
        Exit;
      if not HasBytesAvailable(ABuffer, Integer(AbsoluteKeyOffset), ValueLength) then
        Exit;
      AValue := CopyStringFromBytes(ABuffer, Integer(AbsoluteKeyOffset), ValueLength);
      Result := True;
      Exit;
    end;

    if CompareResult < 0 then
      HighIndex := MiddleIndex - 1
    else
      LowIndex := MiddleIndex + 1;
  end;
end;

function TryLookupSimple(const ASectionName, AKey: string;
  out AValue: string): Boolean;
var
  Resource: TBytes;
  DataOffset, DataLength: Integer;
begin
  Result := False;
  AValue := '';

  if not TryGetSectionData(ASectionName, Resource, DataOffset, DataLength) then
    Exit;

  Result := TryGetKeyValue(Resource, DataOffset, DataLength, AKey, AValue);
end;

function TryLookupWithFallback(const APrefix, ALocale, AKey: string;
  out AValue: string): Boolean;
var
  Resource: TBytes;
  DataOffset, DataLength: Integer;
begin
  Result := False;
  AValue := '';

  if not TryGetSectionDataWithFallback(APrefix, ALocale, Resource, DataOffset, DataLength) then
    Exit;

  Result := TryGetKeyValue(Resource, DataOffset, DataLength, AKey, AValue);
end;

function TryLookupKeyWithFallback(const ASectionName, AKey: string;
  out AValue: string): Boolean;
var
  Candidate: string;
  DashPos: Integer;
begin
  Result := False;
  AValue := '';

  Candidate := AKey;
  repeat
    if TryLookupSimple(ASectionName, Candidate, AValue) then
    begin
      Result := True;
      Exit;
    end;

    DashPos := Length(Candidate);
    while (DashPos > 0) and (Candidate[DashPos] <> '-') do
      Dec(DashPos);
    if DashPos <= 0 then
      Break;
    Candidate := Copy(Candidate, 1, DashPos - 1);
  until Candidate = '';
end;

function TryLookupRegionWithDefault(const ASectionName, ARegion: string;
  out AValue: string): Boolean;
begin
  Result := TryLookupSimple(ASectionName, ARegion, AValue);
  if not Result then
    Result := TryLookupSimple(ASectionName, '001', AValue);
end;

function SplitSpaceSeparated(const AValue: string): IntlTypes.TStringArray;
var
  Count, StartIndex, Index: Integer;
begin
  Count := 0;
  SetLength(Result, 0);
  StartIndex := 1;

  for Index := 1 to Length(AValue) + 1 do
  begin
    if (Index > Length(AValue)) or (AValue[Index] = ' ') then
    begin
      if Index > StartIndex then
      begin
        Inc(Count);
        SetLength(Result, Count);
        Result[Count - 1] := Copy(AValue, StartIndex, Index - StartIndex);
      end;
      StartIndex := Index + 1;
    end;
  end;
end;

function TryParseInteger(const AValue: string; out ANumber: Integer): Boolean;
var
  ErrorCode: Integer;
begin
  Val(AValue, ANumber, ErrorCode);
  Result := ErrorCode = 0;
end;

function TryGetLikelySubtags(const ATag: string; out AMaximized: string): Boolean;
begin
  Result := TryLookupSimple('likely-subtags', ATag, AMaximized);
  if not Result then
    Result := TryLookupSimple('likely-subtags', LowerCase(ATag), AMaximized);
end;

function TryGetLanguageAlias(const ATag: string; out AReplacement: string): Boolean;
begin
  Result := TryLookupSimple('language-aliases', ATag, AReplacement);
end;

function TryGetTerritoryAlias(const ACode: string; out AReplacement: string): Boolean;
begin
  Result := TryLookupSimple('territory-aliases', ACode, AReplacement);
end;

function TryGetScriptAlias(const ACode: string; out AReplacement: string): Boolean;
begin
  Result := TryLookupSimple('script-aliases', ACode, AReplacement);
end;

function TryGetGrandfatheredTag(const ATag: string; out AReplacement: string): Boolean;
begin
  Result := TryLookupSimple('grandfathered-tags', LowerCase(ATag), AReplacement);
end;

function TryGetPluralRules(const ALocale: string; ACardinal: Boolean;
  out ARules: TIntlPluralRuleSet): Boolean;
var
  Resource: TBytes;
  SectionName: string;
  DataOffset, DataLength: Integer;
  Value: string;
  FoundAny: Boolean;
begin
  ARules.Zero := '';
  ARules.One := '';
  ARules.Two := '';
  ARules.Few := '';
  ARules.Many := '';
  ARules.Other := '';

  if ACardinal then
    SectionName := 'plural-rules-cardinal'
  else
    SectionName := 'plural-rules-ordinal';

  if not TryGetSectionData(SectionName, Resource, DataOffset, DataLength) then
  begin
    Result := False;
    Exit;
  end;

  FoundAny := False;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    ALocale + '/zero', Value) then
  begin
    ARules.Zero := Value;
    FoundAny := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    ALocale + '/one', Value) then
  begin
    ARules.One := Value;
    FoundAny := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    ALocale + '/two', Value) then
  begin
    ARules.Two := Value;
    FoundAny := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    ALocale + '/few', Value) then
  begin
    ARules.Few := Value;
    FoundAny := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    ALocale + '/many', Value) then
  begin
    ARules.Many := Value;
    FoundAny := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    ALocale + '/other', Value) then
  begin
    ARules.Other := Value;
    FoundAny := True;
  end;

  Result := FoundAny;
end;

function TryGetNumberPattern(const ALocale: string; AStyle: TIntlNumberStyle;
  out APattern: string): Boolean;
var
  Key: string;
begin
  case AStyle of
    insDecimal: Key := 'decimal';
    insCurrency: Key := 'currency';
    insPercent: Key := 'percent';
  else
    begin
      Result := False;
      APattern := '';
      Exit;
    end;
  end;

  Result := TryLookupWithFallback('number-patterns/', ALocale, Key, APattern);
end;

function TryGetDatePattern(const ALocale: string;
  ADateStyle, ATimeStyle: TIntlDateTimeStyle; out APattern: string): Boolean;
begin
  Result := False;
  APattern := '';
end;

function TryGetListPattern(const ALocale: string; AType: TIntlListFormatType;
  AStyle: TIntlListFormatStyle; out APattern: TIntlListPattern): Boolean;
var
  Resource: TBytes;
  TypeName, SectionName, FallbackLocale: string;
  DataOffset, DataLength: Integer;
  Value: string;
  DashPos: Integer;
  Found: Boolean;
begin
  APattern.Start := '';
  APattern.Middle := '';
  APattern.EndPart := '';
  APattern.Pair := '';

  case AType of
    ilftConjunction: TypeName := 'conjunction';
    ilftDisjunction: TypeName := 'disjunction';
    ilftUnit: TypeName := 'unit';
  else
    begin
      Result := False;
      Exit;
    end;
  end;

  SectionName := 'list-patterns/' + ALocale + '/' + TypeName;
  Found := TryGetSectionData(SectionName, Resource, DataOffset, DataLength);

  if not Found then
  begin
    FallbackLocale := ALocale;
    repeat
      DashPos := Length(FallbackLocale);
      while (DashPos > 0) and (FallbackLocale[DashPos] <> '-') do
        Dec(DashPos);
      if DashPos <= 0 then
        Break;
      FallbackLocale := Copy(FallbackLocale, 1, DashPos - 1);
      SectionName := 'list-patterns/' + FallbackLocale + '/' + TypeName;
      Found := TryGetSectionData(SectionName, Resource, DataOffset, DataLength);
    until Found;
  end;

  if not Found then
  begin
    Result := False;
    Exit;
  end;

  Result := False;

  if TryGetKeyValue(Resource, DataOffset, DataLength, 'start', Value) then
  begin
    APattern.Start := Value;
    Result := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength, 'middle', Value) then
  begin
    APattern.Middle := Value;
    Result := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength, 'end', Value) then
  begin
    APattern.EndPart := Value;
    Result := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength, 'pair', Value) then
  begin
    APattern.Pair := Value;
    Result := True;
  end;

end;

function TryGetRelativeTimePattern(const ALocale: string;
  AUnit: TIntlRelativeTimeUnit; out APattern: TIntlRelativeTimePattern): Boolean;
var
  Resource: TBytes;
  UnitName: string;
  DataOffset, DataLength: Integer;
  Value: string;
begin
  APattern.Future := '';
  APattern.Past := '';

  case AUnit of
    irtuYear: UnitName := 'year';
    irtuQuarter: UnitName := 'quarter';
    irtuMonth: UnitName := 'month';
    irtuWeek: UnitName := 'week';
    irtuDay: UnitName := 'day';
    irtuHour: UnitName := 'hour';
    irtuMinute: UnitName := 'minute';
    irtuSecond: UnitName := 'second';
  else
    begin
      Result := False;
      Exit;
    end;
  end;

  if not TryGetSectionDataWithFallback('relative-time/', ALocale,
    Resource, DataOffset, DataLength) then
  begin
    Result := False;
    Exit;
  end;

  Result := False;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    UnitName + '/future-other', Value) then
  begin
    APattern.Future := Value;
    Result := True;
  end;

  if TryGetKeyValue(Resource, DataOffset, DataLength,
    UnitName + '/past-other', Value) then
  begin
    APattern.Past := Value;
    Result := True;
  end;
end;

function TryGetDisplayName(const ALocale, ACode: string;
  AType: TIntlDisplayNameType; out AName: string): Boolean;
var
  Resource: TBytes;
  TypeName, SectionName, FallbackLocale: string;
  DataOffset, DataLength: Integer;
  DashPos: Integer;
  Found: Boolean;
begin
  AName := '';

  case AType of
    idntLanguage: TypeName := 'languages';
    idntRegion: TypeName := 'territories';
    idntScript: TypeName := 'scripts';
  else
    begin
      Result := False;
      Exit;
    end;
  end;

  SectionName := 'display-names/' + ALocale + '/' + TypeName;
  Found := TryGetSectionData(SectionName, Resource, DataOffset, DataLength);

  if not Found then
  begin
    FallbackLocale := ALocale;
    repeat
      DashPos := Length(FallbackLocale);
      while (DashPos > 0) and (FallbackLocale[DashPos] <> '-') do
        Dec(DashPos);
      if DashPos <= 0 then
        Break;
      FallbackLocale := Copy(FallbackLocale, 1, DashPos - 1);
      SectionName := 'display-names/' + FallbackLocale + '/' + TypeName;
      Found := TryGetSectionData(SectionName, Resource, DataOffset, DataLength);
    until Found;
  end;

  if not Found then
  begin
    Result := False;
    Exit;
  end;

  Result := TryGetKeyValue(Resource, DataOffset, DataLength, ACode, AName);
end;

function TryGetCurrencyInfo(const ALocale, ACurrency: string;
  out ASymbol, ANarrowSymbol: string; out ADigits: Integer): Boolean;
var
  Value: string;
  ColonPos, DigitValue, ErrorCode: Integer;
begin
  ASymbol := ACurrency;
  ANarrowSymbol := ACurrency;
  ADigits := 2;

  Result := TryLookupSimple('currency-data', ACurrency, Value);
  if not Result then
    Exit;

  ColonPos := Pos(':', Value);
  if ColonPos > 0 then
  begin
    Val(Copy(Value, 1, ColonPos - 1), DigitValue, ErrorCode);
    if ErrorCode = 0 then
      ADigits := DigitValue;
  end
  else
  begin
    Val(Value, DigitValue, ErrorCode);
    if ErrorCode = 0 then
      ADigits := DigitValue;
  end;

  ASymbol := ACurrency;
  ANarrowSymbol := ACurrency;
end;

function TryGetNumberSymbol(const ALocale, ASymbolName: string;
  out AValue: string): Boolean;
begin
  Result := TryLookupWithFallback('number-symbols/', ALocale, ASymbolName, AValue);
end;

function TryGetLocaleCalendars(const ARegion: string; out ACalendars: IntlTypes.TStringArray): Boolean;
var
  Value: string;
begin
  Result := TryLookupRegionWithDefault('locale-calendars', ARegion, Value);
  if Result then
    ACalendars := SplitSpaceSeparated(Value)
  else
    SetLength(ACalendars, 0);
end;

function TryGetLocaleCollations(out ACollations: IntlTypes.TStringArray): Boolean;
var
  Value: string;
begin
  Result := TryLookupSimple('locale-collations', 'default', Value);
  if Result then
    ACollations := SplitSpaceSeparated(Value)
  else
    SetLength(ACollations, 0);
end;

function TryGetLocaleHourCycles(const ALocale, ARegion: string;
  out AHourCycles: IntlTypes.TStringArray): Boolean;
var
  Value: string;
begin
  Result := (ALocale <> '') and
    TryLookupKeyWithFallback('locale-hour-cycles', ALocale, Value);
  if not Result then
    Result := TryLookupRegionWithDefault('locale-hour-cycles', ARegion, Value);

  if Result then
    AHourCycles := SplitSpaceSeparated(Value)
  else
    SetLength(AHourCycles, 0);
end;

function TryGetLocaleNumberingSystems(const ALocale: string;
  out ANumberingSystems: IntlTypes.TStringArray): Boolean;
var
  Value: string;
begin
  Result := TryLookupKeyWithFallback('locale-numbering-systems', ALocale, Value);
  if not Result then
    Result := TryLookupSimple('locale-numbering-systems', 'und', Value);

  if Result then
    ANumberingSystems := SplitSpaceSeparated(Value)
  else
    SetLength(ANumberingSystems, 0);
end;

function TryGetLocaleTimeZones(const ARegion: string; out ATimeZones: IntlTypes.TStringArray): Boolean;
var
  Value: string;
begin
  Result := TryLookupSimple('locale-time-zones', ARegion, Value);
  if Result then
    ATimeZones := SplitSpaceSeparated(Value)
  else
    SetLength(ATimeZones, 0);
end;

function TryGetLocaleTextDirection(const ALocale: string; out ADirection: string): Boolean;
begin
  Result := TryLookupKeyWithFallback('locale-text-directions', ALocale, ADirection);
  if not Result then
  begin
    ADirection := 'ltr';
    Result := True;
  end;
end;

function TryGetLocaleWeekInfo(const ARegion: string; out AFirstDay,
  AWeekendStart, AWeekendEnd, AMinimalDays: Integer): Boolean;
var
  Value: string;
  FirstColon, SecondColon, ThirdColon: Integer;
begin
  AFirstDay := 1;
  AWeekendStart := 6;
  AWeekendEnd := 7;
  AMinimalDays := 1;

  Result := TryLookupRegionWithDefault('locale-week-info', ARegion, Value);
  if not Result then
    Exit;

  FirstColon := Pos(':', Value);
  SecondColon := Pos(':', Copy(Value, FirstColon + 1, Length(Value)));
  if SecondColon > 0 then
    Inc(SecondColon, FirstColon);
  ThirdColon := Pos(':', Copy(Value, SecondColon + 1, Length(Value)));
  if ThirdColon > 0 then
    Inc(ThirdColon, SecondColon);

  Result := (FirstColon > 0) and (SecondColon > FirstColon) and
    (ThirdColon > SecondColon) and
    TryParseInteger(Copy(Value, 1, FirstColon - 1), AFirstDay) and
    TryParseInteger(Copy(Value, FirstColon + 1, SecondColon - FirstColon - 1), AWeekendStart) and
    TryParseInteger(Copy(Value, SecondColon + 1, ThirdColon - SecondColon - 1), AWeekendEnd) and
    TryParseInteger(Copy(Value, ThirdColon + 1, Length(Value) - ThirdColon), AMinimalDays);
end;

initialization
  CLDRResourceCache.Init;

finalization
  CLDRResourceCache.Done;

{$ELSE}

function TryGetLikelySubtags(const ATag: string; out AMaximized: string): Boolean;
begin
  Result := False;
  AMaximized := '';
end;

function TryGetLanguageAlias(const ATag: string; out AReplacement: string): Boolean;
begin
  Result := False;
  AReplacement := '';
end;

function TryGetTerritoryAlias(const ACode: string; out AReplacement: string): Boolean;
begin
  Result := False;
  AReplacement := '';
end;

function TryGetScriptAlias(const ACode: string; out AReplacement: string): Boolean;
begin
  Result := False;
  AReplacement := '';
end;

function TryGetGrandfatheredTag(const ATag: string; out AReplacement: string): Boolean;
begin
  Result := False;
  AReplacement := '';
end;

function TryGetPluralRules(const ALocale: string; ACardinal: Boolean;
  out ARules: TIntlPluralRuleSet): Boolean;
begin
  Result := False;
  ARules.Zero := '';
  ARules.One := '';
  ARules.Two := '';
  ARules.Few := '';
  ARules.Many := '';
  ARules.Other := '';
end;

function TryGetNumberPattern(const ALocale: string; AStyle: TIntlNumberStyle;
  out APattern: string): Boolean;
begin
  Result := False;
  APattern := '';
end;

function TryGetDatePattern(const ALocale: string;
  ADateStyle, ATimeStyle: TIntlDateTimeStyle; out APattern: string): Boolean;
begin
  Result := False;
  APattern := '';
end;

function TryGetListPattern(const ALocale: string; AType: TIntlListFormatType;
  AStyle: TIntlListFormatStyle; out APattern: TIntlListPattern): Boolean;
begin
  Result := False;
  APattern.Start := '';
  APattern.Middle := '';
  APattern.EndPart := '';
  APattern.Pair := '';
end;

function TryGetRelativeTimePattern(const ALocale: string;
  AUnit: TIntlRelativeTimeUnit; out APattern: TIntlRelativeTimePattern): Boolean;
begin
  Result := False;
  APattern.Future := '';
  APattern.Past := '';
end;

function TryGetDisplayName(const ALocale, ACode: string;
  AType: TIntlDisplayNameType; out AName: string): Boolean;
begin
  Result := False;
  AName := '';
end;

function TryGetCurrencyInfo(const ALocale, ACurrency: string;
  out ASymbol, ANarrowSymbol: string; out ADigits: Integer): Boolean;
begin
  Result := False;
  ASymbol := '';
  ANarrowSymbol := '';
  ADigits := 2;
end;

function TryGetNumberSymbol(const ALocale, ASymbolName: string;
  out AValue: string): Boolean;
begin
  Result := False;
  AValue := '';
end;

function TryGetLocaleCalendars(const ARegion: string; out ACalendars: IntlTypes.TStringArray): Boolean;
begin
  Result := False;
  SetLength(ACalendars, 0);
end;

function TryGetLocaleCollations(out ACollations: IntlTypes.TStringArray): Boolean;
begin
  Result := False;
  SetLength(ACollations, 0);
end;

function TryGetLocaleHourCycles(const ALocale, ARegion: string;
  out AHourCycles: IntlTypes.TStringArray): Boolean;
begin
  Result := False;
  SetLength(AHourCycles, 0);
end;

function TryGetLocaleNumberingSystems(const ALocale: string;
  out ANumberingSystems: IntlTypes.TStringArray): Boolean;
begin
  Result := False;
  SetLength(ANumberingSystems, 0);
end;

function TryGetLocaleTimeZones(const ARegion: string; out ATimeZones: IntlTypes.TStringArray): Boolean;
begin
  Result := False;
  SetLength(ATimeZones, 0);
end;

function TryGetLocaleTextDirection(const ALocale: string; out ADirection: string): Boolean;
begin
  Result := False;
  ADirection := '';
end;

function TryGetLocaleWeekInfo(const ARegion: string; out AFirstDay,
  AWeekendStart, AWeekendEnd, AMinimalDays: Integer): Boolean;
begin
  Result := False;
  AFirstDay := 0;
  AWeekendStart := 0;
  AWeekendEnd := 0;
  AMinimalDays := 0;
end;

{$ENDIF}

end.

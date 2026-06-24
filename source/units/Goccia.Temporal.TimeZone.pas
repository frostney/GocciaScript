unit Goccia.Temporal.TimeZone;

{$I Goccia.inc}

interface

type
  TTemporalTimeZoneDisambiguation = (ttzdCompatible, ttzdEarlier, ttzdLater, ttzdReject);
  TTemporalTimeZoneIdentifierArray = array of string;

function GetSystemTimeZoneId: string;
function IsValidTimeZone(const ATimeZone: string): Boolean;
function IsSupportedCanonicalTimeZoneIdentifier(const ATimeZone: string): Boolean;
function GetAvailablePrimaryTimeZoneIdentifiers: TTemporalTimeZoneIdentifierArray;
function TryCanonicalizeTimeZoneIdentifierCase(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
function TimeZoneIdentifiersEqual(const ALeft, ARight: string): Boolean;
function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
function TryGetTimeZoneTransitionMilliseconds(const ATimeZone: string;
  const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer;
  const ANext: Boolean; out ATransitionEpochMilliseconds: Int64): Boolean;
function LocalDateTimeToEpochMilliseconds(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string): Int64;
function LocalDateTimeToEpochMillisecondsWithDisambiguation(const AYear, AMonth,
  ADay, AHour, AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string;
  const ADisambiguation: TTemporalTimeZoneDisambiguation): Int64;
function StartOfDayToEpochMilliseconds(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string): Int64;
function FormatOffsetString(const AOffsetSeconds: Integer): string;
function ParseOffsetString(const AStr: string; out AOffsetSeconds: Integer): Boolean;
procedure ClearTimeZoneCache;

implementation

uses
  SysUtils,
  Goccia.Temporal.Utils,
  Goccia.Temporal.TimeZoneData,
  Goccia.Values.ErrorHelper,
  TimeZoneInformationFile,
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  DynLibs,
  ICU,
  {$ENDIF}
  Classes;

type
  TTimeZoneIdentifierName = string[64];

  TTimeZonePrimaryIdentifierMapEntry = record
    Identifier: TTimeZoneIdentifierName;
    PrimaryIdentifier: TTimeZoneIdentifierName;
  end;

const
  UTC_TIMEZONE_ID = 'UTC';
  GOCCIA_TZDIR_ENV = 'GOCCIA_TZDIR';
  UNIX_ZONEINFO_PATH = '/usr/share/zoneinfo';
  MACOS_DEFAULT_ZONEINFO_PATH = '/usr/share/zoneinfo.default';
  {$IFDEF UNIX}
  LOCALTIME_PATH = '/etc/localtime';
  {$ENDIF}
  MINUTES_PER_HOUR = 60;
  SECONDS_PER_MINUTE = 60;
  MILLISECONDS_PER_SECOND = 1000;
  MILLISECONDS_PER_MINUTE = SECONDS_PER_MINUTE * MILLISECONDS_PER_SECOND;
  MILLISECONDS_PER_HOUR = MINUTES_PER_HOUR * MILLISECONDS_PER_MINUTE;
  MILLISECONDS_PER_DAY = 24 * MILLISECONDS_PER_HOUR;
  MAX_SYMLINK_LENGTH = 1024;
  KNOWN_TIMEZONE_PRIMARY_IDENTIFIER_COUNT = 152;
  {$IFDEF MSWINDOWS}
  ICU_LIBRARY = 'icu.dll';
  ICU_I18N_LIBRARY = 'icuin.dll';
  ICU_SUCCESS = 0;
  ICU_TIMEZONE_ID_CAPACITY = 128;
  ICU_CAL_GREGORIAN = 1;
  ICU_CAL_ZONE_OFFSET = 15;
  ICU_CAL_DST_OFFSET = 16;
  ICU_TZ_TRANSITION_NEXT = 0;
  ICU_TRANSITION_WINDOW_START_SECONDS = -5364662400; // 1800-01-01T00:00:00Z
  ICU_TRANSITION_WINDOW_END_SECONDS = 7258118400; // 2200-01-01T00:00:00Z
  ICU_MAX_TRANSITION_COUNT = 4096;
  {$ENDIF}
  KNOWN_TIMEZONE_PRIMARY_IDENTIFIERS: array[0..KNOWN_TIMEZONE_PRIMARY_IDENTIFIER_COUNT - 1] of TTimeZonePrimaryIdentifierMapEntry = (
    (Identifier: 'Europe/Nicosia'; PrimaryIdentifier: 'Asia/Nicosia'),
    (Identifier: 'Asia/Ashkhabad'; PrimaryIdentifier: 'Asia/Ashgabat'),
    (Identifier: 'Asia/Calcutta'; PrimaryIdentifier: 'Asia/Kolkata'),
    (Identifier: 'Asia/Choibalsan'; PrimaryIdentifier: 'Asia/Ulaanbaatar'),
    (Identifier: 'Asia/Chongqing'; PrimaryIdentifier: 'Asia/Shanghai'),
    (Identifier: 'Asia/Chungking'; PrimaryIdentifier: 'Asia/Shanghai'),
    (Identifier: 'Asia/Dacca'; PrimaryIdentifier: 'Asia/Dhaka'),
    (Identifier: 'Asia/Harbin'; PrimaryIdentifier: 'Asia/Shanghai'),
    (Identifier: 'Asia/Istanbul'; PrimaryIdentifier: 'Europe/Istanbul'),
    (Identifier: 'Asia/Kashgar'; PrimaryIdentifier: 'Asia/Urumqi'),
    (Identifier: 'Asia/Katmandu'; PrimaryIdentifier: 'Asia/Kathmandu'),
    (Identifier: 'Asia/Macao'; PrimaryIdentifier: 'Asia/Macau'),
    (Identifier: 'Asia/Rangoon'; PrimaryIdentifier: 'Asia/Yangon'),
    (Identifier: 'Asia/Saigon'; PrimaryIdentifier: 'Asia/Ho_Chi_Minh'),
    (Identifier: 'Asia/Tel_Aviv'; PrimaryIdentifier: 'Asia/Jerusalem'),
    (Identifier: 'Asia/Thimbu'; PrimaryIdentifier: 'Asia/Thimphu'),
    (Identifier: 'Asia/Ujung_Pandang'; PrimaryIdentifier: 'Asia/Makassar'),
    (Identifier: 'Asia/Ulan_Bator'; PrimaryIdentifier: 'Asia/Ulaanbaatar'),
    (Identifier: 'Africa/Asmera'; PrimaryIdentifier: 'Africa/Asmara'),
    (Identifier: 'Africa/Timbuktu'; PrimaryIdentifier: 'Africa/Bamako'),
    (Identifier: 'Antarctica/South_Pole'; PrimaryIdentifier: 'Antarctica/McMurdo'),
    (Identifier: 'Australia/ACT'; PrimaryIdentifier: 'Australia/Sydney'),
    (Identifier: 'Australia/Canberra'; PrimaryIdentifier: 'Australia/Sydney'),
    (Identifier: 'Australia/Currie'; PrimaryIdentifier: 'Australia/Hobart'),
    (Identifier: 'Australia/LHI'; PrimaryIdentifier: 'Australia/Lord_Howe'),
    (Identifier: 'Australia/NSW'; PrimaryIdentifier: 'Australia/Sydney'),
    (Identifier: 'Australia/North'; PrimaryIdentifier: 'Australia/Darwin'),
    (Identifier: 'Australia/Queensland'; PrimaryIdentifier: 'Australia/Brisbane'),
    (Identifier: 'Australia/South'; PrimaryIdentifier: 'Australia/Adelaide'),
    (Identifier: 'Australia/Tasmania'; PrimaryIdentifier: 'Australia/Hobart'),
    (Identifier: 'Australia/Victoria'; PrimaryIdentifier: 'Australia/Melbourne'),
    (Identifier: 'Australia/West'; PrimaryIdentifier: 'Australia/Perth'),
    (Identifier: 'Australia/Yancowinna'; PrimaryIdentifier: 'Australia/Broken_Hill'),
    (Identifier: 'Pacific/Enderbury'; PrimaryIdentifier: 'Pacific/Kanton'),
    (Identifier: 'Pacific/Johnston'; PrimaryIdentifier: 'Pacific/Honolulu'),
    (Identifier: 'Pacific/Ponape'; PrimaryIdentifier: 'Pacific/Pohnpei'),
    (Identifier: 'Pacific/Samoa'; PrimaryIdentifier: 'Pacific/Pago_Pago'),
    (Identifier: 'Pacific/Truk'; PrimaryIdentifier: 'Pacific/Chuuk'),
    (Identifier: 'Pacific/Yap'; PrimaryIdentifier: 'Pacific/Chuuk'),
    (Identifier: 'Europe/Belfast'; PrimaryIdentifier: 'Europe/London'),
    (Identifier: 'Europe/Kiev'; PrimaryIdentifier: 'Europe/Kyiv'),
    (Identifier: 'Europe/Tiraspol'; PrimaryIdentifier: 'Europe/Chisinau'),
    (Identifier: 'Europe/Uzhgorod'; PrimaryIdentifier: 'Europe/Kyiv'),
    (Identifier: 'Europe/Zaporozhye'; PrimaryIdentifier: 'Europe/Kyiv'),
    (Identifier: 'America/Argentina/ComodRivadavia'; PrimaryIdentifier: 'America/Argentina/Catamarca'),
    (Identifier: 'America/Atka'; PrimaryIdentifier: 'America/Adak'),
    (Identifier: 'America/Buenos_Aires'; PrimaryIdentifier: 'America/Argentina/Buenos_Aires'),
    (Identifier: 'America/Catamarca'; PrimaryIdentifier: 'America/Argentina/Catamarca'),
    (Identifier: 'America/Coral_Harbour'; PrimaryIdentifier: 'America/Atikokan'),
    (Identifier: 'America/Cordoba'; PrimaryIdentifier: 'America/Argentina/Cordoba'),
    (Identifier: 'America/Ensenada'; PrimaryIdentifier: 'America/Tijuana'),
    (Identifier: 'America/Fort_Wayne'; PrimaryIdentifier: 'America/Indiana/Indianapolis'),
    (Identifier: 'America/Godthab'; PrimaryIdentifier: 'America/Nuuk'),
    (Identifier: 'America/Indianapolis'; PrimaryIdentifier: 'America/Indiana/Indianapolis'),
    (Identifier: 'America/Jujuy'; PrimaryIdentifier: 'America/Argentina/Jujuy'),
    (Identifier: 'America/Knox_IN'; PrimaryIdentifier: 'America/Indiana/Knox'),
    (Identifier: 'America/Louisville'; PrimaryIdentifier: 'America/Kentucky/Louisville'),
    (Identifier: 'America/Mendoza'; PrimaryIdentifier: 'America/Argentina/Mendoza'),
    (Identifier: 'America/Montreal'; PrimaryIdentifier: 'America/Toronto'),
    (Identifier: 'America/Nipigon'; PrimaryIdentifier: 'America/Toronto'),
    (Identifier: 'America/Pangnirtung'; PrimaryIdentifier: 'America/Iqaluit'),
    (Identifier: 'America/Porto_Acre'; PrimaryIdentifier: 'America/Rio_Branco'),
    (Identifier: 'America/Rainy_River'; PrimaryIdentifier: 'America/Winnipeg'),
    (Identifier: 'America/Rosario'; PrimaryIdentifier: 'America/Argentina/Cordoba'),
    (Identifier: 'America/Santa_Isabel'; PrimaryIdentifier: 'America/Tijuana'),
    (Identifier: 'America/Shiprock'; PrimaryIdentifier: 'America/Denver'),
    (Identifier: 'America/Thunder_Bay'; PrimaryIdentifier: 'America/Toronto'),
    (Identifier: 'America/Virgin'; PrimaryIdentifier: 'America/St_Thomas'),
    (Identifier: 'America/Yellowknife'; PrimaryIdentifier: 'America/Edmonton'),
    (Identifier: 'US/Alaska'; PrimaryIdentifier: 'America/Anchorage'),
    (Identifier: 'US/Aleutian'; PrimaryIdentifier: 'America/Adak'),
    (Identifier: 'US/Arizona'; PrimaryIdentifier: 'America/Phoenix'),
    (Identifier: 'US/Central'; PrimaryIdentifier: 'America/Chicago'),
    (Identifier: 'US/East-Indiana'; PrimaryIdentifier: 'America/Indiana/Indianapolis'),
    (Identifier: 'US/Eastern'; PrimaryIdentifier: 'America/New_York'),
    (Identifier: 'US/Hawaii'; PrimaryIdentifier: 'Pacific/Honolulu'),
    (Identifier: 'US/Indiana-Starke'; PrimaryIdentifier: 'America/Indiana/Knox'),
    (Identifier: 'US/Michigan'; PrimaryIdentifier: 'America/Detroit'),
    (Identifier: 'US/Mountain'; PrimaryIdentifier: 'America/Denver'),
    (Identifier: 'US/Pacific'; PrimaryIdentifier: 'America/Los_Angeles'),
    (Identifier: 'US/Samoa'; PrimaryIdentifier: 'Pacific/Pago_Pago'),
    (Identifier: 'Atlantic/Faeroe'; PrimaryIdentifier: 'Atlantic/Faroe'),
    (Identifier: 'Atlantic/Jan_Mayen'; PrimaryIdentifier: 'Arctic/Longyearbyen'),
    (Identifier: 'Brazil/Acre'; PrimaryIdentifier: 'America/Rio_Branco'),
    (Identifier: 'Brazil/DeNoronha'; PrimaryIdentifier: 'America/Noronha'),
    (Identifier: 'Brazil/East'; PrimaryIdentifier: 'America/Sao_Paulo'),
    (Identifier: 'Brazil/West'; PrimaryIdentifier: 'America/Manaus'),
    (Identifier: 'CET'; PrimaryIdentifier: 'Europe/Brussels'),
    (Identifier: 'CST6CDT'; PrimaryIdentifier: 'America/Chicago'),
    (Identifier: 'Canada/Atlantic'; PrimaryIdentifier: 'America/Halifax'),
    (Identifier: 'Canada/Central'; PrimaryIdentifier: 'America/Winnipeg'),
    (Identifier: 'Canada/Eastern'; PrimaryIdentifier: 'America/Toronto'),
    (Identifier: 'Canada/Mountain'; PrimaryIdentifier: 'America/Edmonton'),
    (Identifier: 'Canada/Newfoundland'; PrimaryIdentifier: 'America/St_Johns'),
    (Identifier: 'Canada/Pacific'; PrimaryIdentifier: 'America/Vancouver'),
    (Identifier: 'Canada/Saskatchewan'; PrimaryIdentifier: 'America/Regina'),
    (Identifier: 'Canada/Yukon'; PrimaryIdentifier: 'America/Whitehorse'),
    (Identifier: 'Chile/Continental'; PrimaryIdentifier: 'America/Santiago'),
    (Identifier: 'Chile/EasterIsland'; PrimaryIdentifier: 'Pacific/Easter'),
    (Identifier: 'Cuba'; PrimaryIdentifier: 'America/Havana'),
    (Identifier: 'EET'; PrimaryIdentifier: 'Europe/Athens'),
    (Identifier: 'EST'; PrimaryIdentifier: 'America/Panama'),
    (Identifier: 'EST5EDT'; PrimaryIdentifier: 'America/New_York'),
    (Identifier: 'Egypt'; PrimaryIdentifier: 'Africa/Cairo'),
    (Identifier: 'Eire'; PrimaryIdentifier: 'Europe/Dublin'),
    (Identifier: 'Etc/GMT'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/GMT+0'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/GMT-0'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/GMT0'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/Greenwich'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/UCT'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/UTC'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/Universal'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Etc/Zulu'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'GB'; PrimaryIdentifier: 'Europe/London'),
    (Identifier: 'GB-Eire'; PrimaryIdentifier: 'Europe/London'),
    (Identifier: 'GMT'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'GMT+0'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'GMT-0'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'GMT0'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Greenwich'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'HST'; PrimaryIdentifier: 'Pacific/Honolulu'),
    (Identifier: 'Hongkong'; PrimaryIdentifier: 'Asia/Hong_Kong'),
    (Identifier: 'Iceland'; PrimaryIdentifier: 'Atlantic/Reykjavik'),
    (Identifier: 'Iran'; PrimaryIdentifier: 'Asia/Tehran'),
    (Identifier: 'Israel'; PrimaryIdentifier: 'Asia/Jerusalem'),
    (Identifier: 'Jamaica'; PrimaryIdentifier: 'America/Jamaica'),
    (Identifier: 'Japan'; PrimaryIdentifier: 'Asia/Tokyo'),
    (Identifier: 'Kwajalein'; PrimaryIdentifier: 'Pacific/Kwajalein'),
    (Identifier: 'Libya'; PrimaryIdentifier: 'Africa/Tripoli'),
    (Identifier: 'MET'; PrimaryIdentifier: 'Europe/Brussels'),
    (Identifier: 'MST'; PrimaryIdentifier: 'America/Phoenix'),
    (Identifier: 'MST7MDT'; PrimaryIdentifier: 'America/Denver'),
    (Identifier: 'Mexico/BajaNorte'; PrimaryIdentifier: 'America/Tijuana'),
    (Identifier: 'Mexico/BajaSur'; PrimaryIdentifier: 'America/Mazatlan'),
    (Identifier: 'Mexico/General'; PrimaryIdentifier: 'America/Mexico_City'),
    (Identifier: 'NZ'; PrimaryIdentifier: 'Pacific/Auckland'),
    (Identifier: 'NZ-CHAT'; PrimaryIdentifier: 'Pacific/Chatham'),
    (Identifier: 'Navajo'; PrimaryIdentifier: 'America/Denver'),
    (Identifier: 'PRC'; PrimaryIdentifier: 'Asia/Shanghai'),
    (Identifier: 'PST8PDT'; PrimaryIdentifier: 'America/Los_Angeles'),
    (Identifier: 'Poland'; PrimaryIdentifier: 'Europe/Warsaw'),
    (Identifier: 'Portugal'; PrimaryIdentifier: 'Europe/Lisbon'),
    (Identifier: 'ROC'; PrimaryIdentifier: 'Asia/Taipei'),
    (Identifier: 'ROK'; PrimaryIdentifier: 'Asia/Seoul'),
    (Identifier: 'Singapore'; PrimaryIdentifier: 'Asia/Singapore'),
    (Identifier: 'Turkey'; PrimaryIdentifier: 'Europe/Istanbul'),
    (Identifier: 'UCT'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'Universal'; PrimaryIdentifier: 'UTC'),
    (Identifier: 'W-SU'; PrimaryIdentifier: 'Europe/Moscow'),
    (Identifier: 'WET'; PrimaryIdentifier: 'Europe/Lisbon'),
    (Identifier: 'Zulu'; PrimaryIdentifier: 'UTC')
  );

{$IFDEF MSWINDOWS}
type
  TICUCalendar = Pointer;
  TICUDate = Double;
  TICUErrorCode = LongInt;
  TICUCalendarOpen = function(const AZoneId: PWideChar; const AZoneIdLength: LongInt;
    const ALocale: PAnsiChar; const ACalendarType: LongInt;
    var AStatus: TICUErrorCode): TICUCalendar; cdecl;
  TICUCalendarClose = procedure(const ACalendar: TICUCalendar); cdecl;
  TICUCalendarSetMillis = procedure(const ACalendar: TICUCalendar;
    const AMilliseconds: TICUDate; var AStatus: TICUErrorCode); cdecl;
  TICUCalendarGet = function(const ACalendar: TICUCalendar; const AField: LongInt;
    var AStatus: TICUErrorCode): LongInt; cdecl;
  TICUCalendarGetTimeZoneTransitionDate = function(const ACalendar: TICUCalendar;
    const ATransitionType: LongInt; var ATransition: TICUDate;
    var AStatus: TICUErrorCode): ByteBool; cdecl;
  TICUCalendarGetCanonicalTimeZoneID = function(const AId: PWideChar;
    const AIdLength: LongInt; AResult: PWideChar; const AResultCapacity: LongInt;
    var AIsSystemId: ByteBool; var AStatus: TICUErrorCode): LongInt; cdecl;
  TICUCalendarGetDefaultTimeZone = function(AResult: PWideChar;
    const AResultCapacity: LongInt; var AStatus: TICUErrorCode): LongInt; cdecl;

  TWindowsICU = record
    Handle: TLibHandle;
    OpenCalendar: TICUCalendarOpen;
    CloseCalendar: TICUCalendarClose;
    SetMillis: TICUCalendarSetMillis;
    GetField: TICUCalendarGet;
    GetTimeZoneTransitionDate: TICUCalendarGetTimeZoneTransitionDate;
    GetCanonicalTimeZoneID: TICUCalendarGetCanonicalTimeZoneID;
    GetDefaultTimeZone: TICUCalendarGetDefaultTimeZone;
  end;
{$ENDIF}

type
  TTimeZonePathCacheEntry = record
    Identifier: string;
    Path: string;
    Found: Boolean;
  end;

  TTimeZoneCaseCacheEntry = record
    Identifier: string;
    CanonicalIdentifier: string;
    Found: Boolean;
  end;

  TTimeZoneEpochMillisecondsArray = array of Int64;

threadvar
  CachedTimeZones: array of TTimeZoneInformationData;
  CachedTimeZoneCount: Integer;
  CachedTimeZonePaths: array of TTimeZonePathCacheEntry;
  CachedTimeZonePathCount: Integer;
  CachedTimeZoneCases: array of TTimeZoneCaseCacheEntry;
  CachedTimeZoneCaseCount: Integer;
  CachedAvailablePrimaryTimeZoneIdentifiers: TTemporalTimeZoneIdentifierArray;
  CachedAvailablePrimaryTimeZoneIdentifiersLoaded: Boolean;

function ASCIIEqualsIgnoreCase(const ALeft, ARight: string): Boolean;
begin
  Result := SameText(ALeft, ARight);
end;

function TryKnownTimeZoneIdentifierCase(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
var
  Entry: TTimeZonePrimaryIdentifierMapEntry;
begin
  Result := False;
  ACanonicalTimeZone := '';

  for Entry in KNOWN_TIMEZONE_PRIMARY_IDENTIFIERS do
  begin
    if ASCIIEqualsIgnoreCase(ATimeZone, Entry.Identifier) then
    begin
      ACanonicalTimeZone := Entry.Identifier;
      Result := True;
      Exit;
    end;

    if ASCIIEqualsIgnoreCase(ATimeZone, Entry.PrimaryIdentifier) then
    begin
      ACanonicalTimeZone := Entry.PrimaryIdentifier;
      Result := True;
      Exit;
    end;
  end;
end;

function TryKnownPrimaryTimeZoneIdentifier(const ATimeZone: string;
  out APrimaryTimeZone: string): Boolean;
var
  Entry: TTimeZonePrimaryIdentifierMapEntry;
begin
  Result := False;
  APrimaryTimeZone := '';

  if ASCIIEqualsIgnoreCase(ATimeZone, UTC_TIMEZONE_ID) then
  begin
    APrimaryTimeZone := UTC_TIMEZONE_ID;
    Result := True;
    Exit;
  end;

  for Entry in KNOWN_TIMEZONE_PRIMARY_IDENTIFIERS do
  begin
    if ASCIIEqualsIgnoreCase(ATimeZone, Entry.Identifier) or
       ASCIIEqualsIgnoreCase(ATimeZone, Entry.PrimaryIdentifier) then
    begin
      APrimaryTimeZone := Entry.PrimaryIdentifier;
      Result := True;
      Exit;
    end;
  end;
end;

procedure AddUniqueTimeZoneIdentifier(var AIdentifiers: TTemporalTimeZoneIdentifierArray;
  var ACount: Integer; const AIdentifier: string);
var
  Index: Integer;
begin
  if AIdentifier = '' then
    Exit;

  for Index := 0 to ACount - 1 do
    if AIdentifiers[Index] = AIdentifier then
      Exit;

  if ACount >= Length(AIdentifiers) then
    SetLength(AIdentifiers, ACount + 64);
  AIdentifiers[ACount] := AIdentifier;
  Inc(ACount);
end;

procedure SortTimeZoneIdentifiers(var AIdentifiers: TTemporalTimeZoneIdentifierArray);
var
  I, J: Integer;
  Temp: string;
begin
  for I := 0 to Length(AIdentifiers) - 2 do
    for J := I + 1 to Length(AIdentifiers) - 1 do
      if CompareStr(AIdentifiers[I], AIdentifiers[J]) > 0 then
      begin
        Temp := AIdentifiers[I];
        AIdentifiers[I] := AIdentifiers[J];
        AIdentifiers[J] := Temp;
      end;
end;

function IsPrimaryAvailableTimeZoneIdentifier(const AIdentifier: string): Boolean;
var
  PrimaryIdentifier: string;
begin
  Result := (not TryKnownPrimaryTimeZoneIdentifier(AIdentifier,
    PrimaryIdentifier)) or ASCIIEqualsIgnoreCase(AIdentifier, PrimaryIdentifier);
end;

function TryLoadTimeZoneDataFromFile(const ATimeZone, AFilePath: string;
  out AData: TTimeZoneInformationData): Boolean; forward;

function ShouldSkipZoneInfoIdentifier(const AIdentifier: string): Boolean;
var
  LowerIdentifier: string;
begin
  LowerIdentifier := LowerCase(AIdentifier);
  Result := (AIdentifier = '') or
    (LowerIdentifier = 'localtime') or
    (LowerIdentifier = 'posixrules') or
    (LowerIdentifier = 'leapseconds') or
    (LowerIdentifier = 'tzdata.zi') or
    (LowerIdentifier = 'zone.tab') or
    (LowerIdentifier = 'zone1970.tab') or
    (LowerIdentifier = 'iso3166.tab') or
    (Pos('posix/', LowerIdentifier) = 1) or
    (Pos('right/', LowerIdentifier) = 1) or
    (Pos('systemv/', LowerIdentifier) = 1);
end;

procedure AddTimeZoneIdentifiersFromDirectory(const ACurrentDirectory,
  APrefix: string; var AIdentifiers: TTemporalTimeZoneIdentifierArray;
  var ACount: Integer; const ADepth: Integer);
var
  Search: TSearchRec;
  Status: Integer;
  Name, Identifier, Path: string;
  Data: TTimeZoneInformationData;
begin
  if (ACurrentDirectory = '') or (ADepth > 8) or
     not DirectoryExists(ACurrentDirectory) then
    Exit;

  Status := FindFirst(IncludeTrailingPathDelimiter(ACurrentDirectory) + '*',
    faAnyFile, Search);
  try
    while Status = 0 do
    begin
      Name := Search.Name;
      if (Name <> '.') and (Name <> '..') then
      begin
        if APrefix = '' then
          Identifier := Name
        else
          Identifier := APrefix + '/' + Name;
        Path := IncludeTrailingPathDelimiter(ACurrentDirectory) + Name;

        if (Search.Attr and faDirectory) <> 0 then
        begin
          if not ShouldSkipZoneInfoIdentifier(Identifier + '/') then
            AddTimeZoneIdentifiersFromDirectory(Path, Identifier, AIdentifiers,
              ACount, ADepth + 1);
        end
        else if IsPrimaryAvailableTimeZoneIdentifier(Identifier) and
                not ShouldSkipZoneInfoIdentifier(Identifier) and
                TryLoadTimeZoneDataFromFile(Identifier, Path, Data) then
          AddUniqueTimeZoneIdentifier(AIdentifiers, ACount, Identifier);
      end;
      Status := FindNext(Search);
    end;
  finally
    FindClose(Search);
  end;
end;

// ECMA-402 ES2026 §6.5.3 AvailablePrimaryTimeZoneIdentifiers()
function BuildAvailablePrimaryTimeZoneIdentifiers: TTemporalTimeZoneIdentifierArray;
var
  EmbeddedFileNames: TEmbeddedTimeZoneFileNameArray;
  Count, Index: Integer;
  EnvDirectory: string;
begin
  Count := 0;
  SetLength(Result, 0);

  if TryGetEmbeddedTimeZoneFileNames(EmbeddedFileNames) then
  begin
    for Index := 0 to Length(EmbeddedFileNames) - 1 do
      if IsPrimaryAvailableTimeZoneIdentifier(EmbeddedFileNames[Index]) then
        AddUniqueTimeZoneIdentifier(Result, Count, EmbeddedFileNames[Index]);
  end
  else
  begin
    EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
    if EnvDirectory <> '' then
      AddTimeZoneIdentifiersFromDirectory(EnvDirectory, '', Result, Count, 0);
    AddTimeZoneIdentifiersFromDirectory(UNIX_ZONEINFO_PATH, '', Result, Count,
      0);
    AddTimeZoneIdentifiersFromDirectory(MACOS_DEFAULT_ZONEINFO_PATH, '', Result,
      Count, 0);
  end;

  AddUniqueTimeZoneIdentifier(Result, Count, UTC_TIMEZONE_ID);
  SetLength(Result, Count);
  SortTimeZoneIdentifiers(Result);
end;

function GetAvailablePrimaryTimeZoneIdentifiers: TTemporalTimeZoneIdentifierArray;
begin
  if not CachedAvailablePrimaryTimeZoneIdentifiersLoaded then
  begin
    CachedAvailablePrimaryTimeZoneIdentifiers :=
      BuildAvailablePrimaryTimeZoneIdentifiers;
    CachedAvailablePrimaryTimeZoneIdentifiersLoaded := True;
  end;

  Result := CachedAvailablePrimaryTimeZoneIdentifiers;
end;

{$IFDEF MSWINDOWS}
var
  WindowsICU: TWindowsICU;
  WindowsICULoadAttempted: Boolean;
  WindowsICUAvailable: Boolean;
  WindowsICUInitLock: TRTLCriticalSection;

function ICUSucceeded(const AStatus: TICUErrorCode): Boolean;
begin
  Result := AStatus <= ICU_SUCCESS;
end;

function TryLoadCalendarFunctions(const AHandle: TLibHandle;
  out AICU: TWindowsICU): Boolean;
var
  Symbol: Pointer;
begin
  Result := False;
  FillChar(AICU, SizeOf(AICU), 0);
  AICU.Handle := AHandle;

  Symbol := ICUGetProcAddress('ucal_open');
  if not Assigned(Symbol) then Exit;
  AICU.OpenCalendar := TICUCalendarOpen(Symbol);

  Symbol := ICUGetProcAddress('ucal_close');
  if not Assigned(Symbol) then Exit;
  AICU.CloseCalendar := TICUCalendarClose(Symbol);

  Symbol := ICUGetProcAddress('ucal_setMillis');
  if not Assigned(Symbol) then Exit;
  AICU.SetMillis := TICUCalendarSetMillis(Symbol);

  Symbol := ICUGetProcAddress('ucal_get');
  if not Assigned(Symbol) then Exit;
  AICU.GetField := TICUCalendarGet(Symbol);

  Symbol := ICUGetProcAddress('ucal_getTimeZoneTransitionDate');
  if not Assigned(Symbol) then Exit;
  AICU.GetTimeZoneTransitionDate := TICUCalendarGetTimeZoneTransitionDate(Symbol);

  Symbol := ICUGetProcAddress('ucal_getCanonicalTimeZoneID');
  if not Assigned(Symbol) then Exit;
  AICU.GetCanonicalTimeZoneID := TICUCalendarGetCanonicalTimeZoneID(Symbol);

  Symbol := ICUGetProcAddress('ucal_getDefaultTimeZone');
  if not Assigned(Symbol) then Exit;
  AICU.GetDefaultTimeZone := TICUCalendarGetDefaultTimeZone(Symbol);

  Result := True;
end;

function TryGetWindowsICU(out AICU: TWindowsICU): Boolean;
var
  Handle: TLibHandle;
  LoadedICU: TWindowsICU;
begin
  EnterCriticalSection(WindowsICUInitLock);
  try
    if not WindowsICULoadAttempted then
    begin
      WindowsICULoadAttempted := True;
      if TryGetICULibraryHandle(Handle) then
        WindowsICUAvailable := TryLoadCalendarFunctions(Handle, LoadedICU);
      if WindowsICUAvailable then
        WindowsICU := LoadedICU;
    end;

    AICU := WindowsICU;
    Result := WindowsICUAvailable;
  finally
    LeaveCriticalSection(WindowsICUInitLock);
  end;
end;

function UnicodeBufferToString(const ABuffer: array of WideChar;
  const ALength: Integer): string;
var
  Value: UnicodeString;
begin
  SetLength(Value, ALength);
  if ALength > 0 then
    Move(ABuffer[0], Value[1], ALength * SizeOf(WideChar));
  Result := string(Value);
end;

function TryCanonicalizeWindowsICUTimeZone(const AICU: TWindowsICU;
  const ATimeZone: string; out ACanonicalTimeZone: string): Boolean;
var
  InputId: UnicodeString;
  Buffer: array[0..ICU_TIMEZONE_ID_CAPACITY - 1] of WideChar;
  Status: TICUErrorCode;
  IsSystemId: ByteBool;
  ResultLength: LongInt;
begin
  Result := False;
  ACanonicalTimeZone := '';
  InputId := UnicodeString(ATimeZone);
  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  IsSystemId := False;

  ResultLength := AICU.GetCanonicalTimeZoneID(PWideChar(InputId), Length(InputId),
    @Buffer[0], ICU_TIMEZONE_ID_CAPACITY, IsSystemId, Status);
  if (not ICUSucceeded(Status)) or (not IsSystemId) or
     (ResultLength <= 0) or (ResultLength >= ICU_TIMEZONE_ID_CAPACITY) then
    Exit;

  ACanonicalTimeZone := UnicodeBufferToString(Buffer, ResultLength);
  Result := ACanonicalTimeZone <> 'Etc/Unknown';
end;

function TryOpenWindowsICUCalendar(const AICU: TWindowsICU;
  const ATimeZone: string; out ACalendar: TICUCalendar): Boolean;
var
  ZoneId: UnicodeString;
  Status: TICUErrorCode;
begin
  Result := False;
  ACalendar := nil;
  ZoneId := UnicodeString(ATimeZone);
  Status := ICU_SUCCESS;
  ACalendar := AICU.OpenCalendar(PWideChar(ZoneId), Length(ZoneId), nil,
    ICU_CAL_GREGORIAN, Status);
  Result := ICUSucceeded(Status) and Assigned(ACalendar);
end;

function EpochSecondsToICUMilliseconds(const AEpochSeconds: Int64): TICUDate;
var
  Seconds: TICUDate;
begin
  Seconds := AEpochSeconds;
  Result := Seconds * MILLISECONDS_PER_SECOND;
end;

function ICUMillisecondsToEpochSeconds(const AMilliseconds: TICUDate): Int64;
var
  Milliseconds: Int64;
begin
  Milliseconds := Round(AMilliseconds);
  Result := Milliseconds div MILLISECONDS_PER_SECOND;
  if (Milliseconds < 0) and (Milliseconds mod MILLISECONDS_PER_SECOND <> 0) then
    Dec(Result);
end;

function TryGetWindowsICUOffsetSeconds(const AICU: TWindowsICU;
  const ACalendar: TICUCalendar; const AEpochSeconds: Int64;
  out AOffsetSeconds: Integer): Boolean;
var
  Status: TICUErrorCode;
  ZoneOffset, DstOffset: LongInt;
begin
  Result := False;
  AOffsetSeconds := 0;

  Status := ICU_SUCCESS;
  AICU.SetMillis(ACalendar, EpochSecondsToICUMilliseconds(AEpochSeconds), Status);
  if not ICUSucceeded(Status) then
    Exit;

  Status := ICU_SUCCESS;
  ZoneOffset := AICU.GetField(ACalendar, ICU_CAL_ZONE_OFFSET, Status);
  if not ICUSucceeded(Status) then
    Exit;

  Status := ICU_SUCCESS;
  DstOffset := AICU.GetField(ACalendar, ICU_CAL_DST_OFFSET, Status);
  if not ICUSucceeded(Status) then
    Exit;

  AOffsetSeconds := (ZoneOffset + DstOffset) div MILLISECONDS_PER_SECOND;
  Result := True;
end;

procedure AppendWindowsICUTransition(var AData: TTimeZoneInformationData;
  const ATransitionEpochSeconds: Int64; const AOffsetSeconds: Integer);
var
  TransitionIndex: Integer;
begin
  TransitionIndex := Length(AData.Transitions);
  SetLength(AData.Transitions, TransitionIndex + 1);
  AData.Transitions[TransitionIndex].TransitionEpochSeconds := ATransitionEpochSeconds;
  AData.Transitions[TransitionIndex].UtcOffsetSeconds := AOffsetSeconds;
end;

function TryLoadTimeZoneDataFromWindowsICU(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  ICU: TWindowsICU;
  Calendar: TICUCalendar;
  CanonicalTimeZone: string;
  CurrentEpochSeconds, TransitionEpochSeconds: Int64;
  TransitionMilliseconds: TICUDate;
  OffsetSeconds, TransitionCount: Integer;
  Status: TICUErrorCode;
begin
  Result := False;

  if not TryGetWindowsICU(ICU) then
    Exit;

  if not TryCanonicalizeWindowsICUTimeZone(ICU, ATimeZone, CanonicalTimeZone) then
    Exit;

  if not TryOpenWindowsICUCalendar(ICU, CanonicalTimeZone, Calendar) then
    Exit;

  try
    AData.Identifier := ATimeZone;
    SetLength(AData.Transitions, 0);

    if not TryGetWindowsICUOffsetSeconds(ICU, Calendar,
      ICU_TRANSITION_WINDOW_START_SECONDS, AData.DefaultOffsetSeconds) then
      Exit;

    CurrentEpochSeconds := ICU_TRANSITION_WINDOW_START_SECONDS;
    TransitionCount := 0;
    while (CurrentEpochSeconds < ICU_TRANSITION_WINDOW_END_SECONDS) and
          (TransitionCount < ICU_MAX_TRANSITION_COUNT) do
    begin
      Status := ICU_SUCCESS;
      ICU.SetMillis(Calendar, EpochSecondsToICUMilliseconds(CurrentEpochSeconds), Status);
      if not ICUSucceeded(Status) then
        Exit;

      Status := ICU_SUCCESS;
      TransitionMilliseconds := 0;
      if not ICU.GetTimeZoneTransitionDate(Calendar, ICU_TZ_TRANSITION_NEXT,
        TransitionMilliseconds, Status) then
        Break;

      if not ICUSucceeded(Status) then
        Exit;

      TransitionEpochSeconds := ICUMillisecondsToEpochSeconds(TransitionMilliseconds);
      if (TransitionEpochSeconds <= CurrentEpochSeconds) or
         (TransitionEpochSeconds > ICU_TRANSITION_WINDOW_END_SECONDS) then
        Break;

      if not TryGetWindowsICUOffsetSeconds(ICU, Calendar,
        TransitionEpochSeconds + 1, OffsetSeconds) then
        Exit;

      AppendWindowsICUTransition(AData, TransitionEpochSeconds, OffsetSeconds);
      CurrentEpochSeconds := TransitionEpochSeconds + 1;
      Inc(TransitionCount);
    end;

    Result := True;
  finally
    ICU.CloseCalendar(Calendar);
  end;
end;

function TryGetWindowsICUDefaultTimeZoneId(out ATimeZone: string): Boolean;
var
  ICU: TWindowsICU;
  Buffer: array[0..ICU_TIMEZONE_ID_CAPACITY - 1] of WideChar;
  Status: TICUErrorCode;
  ResultLength: LongInt;
begin
  Result := False;
  ATimeZone := '';

  if not TryGetWindowsICU(ICU) then
    Exit;

  FillChar(Buffer, SizeOf(Buffer), 0);
  Status := ICU_SUCCESS;
  ResultLength := ICU.GetDefaultTimeZone(@Buffer[0], ICU_TIMEZONE_ID_CAPACITY, Status);
  if (not ICUSucceeded(Status)) or (ResultLength <= 0) or
     (ResultLength >= ICU_TIMEZONE_ID_CAPACITY) then
    Exit;

  ATimeZone := UnicodeBufferToString(Buffer, ResultLength);
  Result := (ATimeZone <> '') and (ATimeZone <> 'Etc/Unknown');
end;
{$ENDIF}

function FindCachedTimeZone(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZoneCount - 1 do
  begin
    if CachedTimeZones[I].Identifier = ATimeZone then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function FindCachedTimeZonePath(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZonePathCount - 1 do
  begin
    if CachedTimeZonePaths[I].Identifier = ATimeZone then
      Exit(I);
  end;
  Result := -1;
end;

procedure CacheTimeZonePath(const ATimeZone, APath: string; const AFound: Boolean);
begin
  if CachedTimeZonePathCount >= Length(CachedTimeZonePaths) then
    SetLength(CachedTimeZonePaths, CachedTimeZonePathCount + 16);
  CachedTimeZonePaths[CachedTimeZonePathCount].Identifier := ATimeZone;
  CachedTimeZonePaths[CachedTimeZonePathCount].Path := APath;
  CachedTimeZonePaths[CachedTimeZonePathCount].Found := AFound;
  Inc(CachedTimeZonePathCount);
end;

function FindCachedTimeZoneCase(const ATimeZone: string): Integer;
var
  I: Integer;
begin
  for I := 0 to CachedTimeZoneCaseCount - 1 do
  begin
    if CachedTimeZoneCases[I].Identifier = ATimeZone then
      Exit(I);
  end;
  Result := -1;
end;

procedure CacheTimeZoneCase(const ATimeZone, ACanonicalTimeZone: string;
  const AFound: Boolean);
begin
  if CachedTimeZoneCaseCount >= Length(CachedTimeZoneCases) then
    SetLength(CachedTimeZoneCases, CachedTimeZoneCaseCount + 16);
  CachedTimeZoneCases[CachedTimeZoneCaseCount].Identifier := ATimeZone;
  CachedTimeZoneCases[CachedTimeZoneCaseCount].CanonicalIdentifier :=
    ACanonicalTimeZone;
  CachedTimeZoneCases[CachedTimeZoneCaseCount].Found := AFound;
  Inc(CachedTimeZoneCaseCount);
end;

function OffsetStringHasSubMinuteSyntax(const AValue: string): Boolean;
var
  Pos: Integer;
begin
  Result := False;
  if Length(AValue) < 6 then
    Exit;
  if not (AValue[1] in ['+', '-']) then
    Exit;

  Pos := 4;
  if (Pos <= Length(AValue)) and (AValue[Pos] = ':') then
    Inc(Pos);
  Inc(Pos, 2);
  if Pos > Length(AValue) then
    Exit;
  if (AValue[Pos] = ':') or (AValue[Pos] in ['0'..'9']) then
    Result := True;
end;

function IsValidTimeZoneName(const AName: string): Boolean;
var
  I, SegStart: Integer;
begin
  Result := False;
  if Length(AName) = 0 then Exit;
  // Reject absolute paths
  if AName[1] = '/' then Exit;
  // Reject Windows path separators, drive paths, and alternate data streams.
  if Pos('\', AName) > 0 then Exit;
  if Pos(':', AName) > 0 then Exit;
  // Check each path segment for . and ..
  SegStart := 1;
  for I := 1 to Length(AName) + 1 do
  begin
    if (I > Length(AName)) or (AName[I] = '/') then
    begin
      // Check segment
      if I = SegStart then Exit; // empty segment (double slash)
      if (I - SegStart = 1) and (AName[SegStart] = '.') then Exit; // "."
      if (I - SegStart = 2) and (AName[SegStart] = '.') and (AName[SegStart + 1] = '.') then Exit; // ".."
      SegStart := I + 1;
    end;
  end;
  Result := True;
end;

function TimeZoneNameToFilePath(const ATimeZone: string): string;
begin
  Result := StringReplace(ATimeZone, '/', PathDelim, [rfReplaceAll]);
end;

function TimeZonePathInDirectory(const ADirectory, ATimeZone: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory)) +
    TimeZoneNameToFilePath(ATimeZone);
end;

function TryFindCaseInsensitiveDirectoryEntry(const ADirectory,
  AName: string; out AActualName: string): Boolean;
var
  Search: TSearchRec;
  Status: Integer;
begin
  Result := False;
  AActualName := '';
  if (ADirectory = '') or not DirectoryExists(ADirectory) then
    Exit;

  Status := FindFirst(IncludeTrailingPathDelimiter(ADirectory) + '*',
    faAnyFile, Search);
  try
    while Status = 0 do
    begin
      if (Search.Name <> '.') and (Search.Name <> '..') and
         ASCIIEqualsIgnoreCase(Search.Name, AName) then
      begin
        AActualName := Search.Name;
        Exit(True);
      end;
      Status := FindNext(Search);
    end;
  finally
    FindClose(Search);
  end;
end;

function TryCanonicalizeTimeZoneInDirectory(const ADirectory,
  ATimeZone: string; out ACanonicalTimeZone: string): Boolean;
var
  CurrentDirectory, Segment, ActualName: string;
  StartIndex, I: Integer;
  IsLastSegment: Boolean;
begin
  Result := False;
  ACanonicalTimeZone := '';
  if (ADirectory = '') or (ATimeZone = '') then
    Exit;

  CurrentDirectory := IncludeTrailingPathDelimiter(ExpandFileName(ADirectory));
  StartIndex := 1;
  for I := 1 to Length(ATimeZone) + 1 do
  begin
    if (I > Length(ATimeZone)) or (ATimeZone[I] = '/') then
    begin
      Segment := Copy(ATimeZone, StartIndex, I - StartIndex);
      if (Segment = '') or
         not TryFindCaseInsensitiveDirectoryEntry(CurrentDirectory, Segment,
           ActualName) then
        Exit;

      IsLastSegment := I > Length(ATimeZone);
      if IsLastSegment and DirectoryExists(CurrentDirectory + ActualName) then
        Exit;

      if ACanonicalTimeZone <> '' then
        ACanonicalTimeZone := ACanonicalTimeZone + '/';
      ACanonicalTimeZone := ACanonicalTimeZone + ActualName;
      if not IsLastSegment then
        CurrentDirectory := IncludeTrailingPathDelimiter(CurrentDirectory +
          ActualName);
      StartIndex := I + 1;
    end;
  end;

  Result := ACanonicalTimeZone <> '';
end;

function TryCanonicalizeTimeZoneIdentifierCase(const ATimeZone: string;
  out ACanonicalTimeZone: string): Boolean;
var
  EnvDirectory: string;
  CachedIndex: Integer;
begin
  CachedIndex := FindCachedTimeZoneCase(ATimeZone);
  if CachedIndex >= 0 then
  begin
    ACanonicalTimeZone := CachedTimeZoneCases[CachedIndex].CanonicalIdentifier;
    Exit(CachedTimeZoneCases[CachedIndex].Found);
  end;

  Result := False;
  ACanonicalTimeZone := '';
  if not IsValidTimeZoneName(ATimeZone) then
  begin
    CacheTimeZoneCase(ATimeZone, '', False);
    Exit;
  end;

  EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
  if TryCanonicalizeTimeZoneInDirectory(EnvDirectory, ATimeZone,
    ACanonicalTimeZone) then
    Result := True;

  {$IFDEF UNIX}
  if (not Result) and TryCanonicalizeTimeZoneInDirectory(UNIX_ZONEINFO_PATH,
    ATimeZone, ACanonicalTimeZone) then
    Result := True;

  if (not Result) and TryCanonicalizeTimeZoneInDirectory(MACOS_DEFAULT_ZONEINFO_PATH,
    ATimeZone, ACanonicalTimeZone) then
    Result := True;
  {$ENDIF}

  if (not Result) and TryCanonicalizeEmbeddedTimeZoneFileName(ATimeZone,
    ACanonicalTimeZone) then
    Result := True;

  if (not Result) and TryKnownTimeZoneIdentifierCase(ATimeZone,
    ACanonicalTimeZone) then
    Result := True;

  CacheTimeZoneCase(ATimeZone, ACanonicalTimeZone, Result);
end;

function TryGetTimeZonePathInDirectory(const ADirectory, ATimeZone: string;
  out APath: string): Boolean;
var
  CanonicalTimeZone: string;
begin
  Result := False;
  APath := '';
  if TryCanonicalizeTimeZoneInDirectory(ADirectory, ATimeZone,
    CanonicalTimeZone) then
  begin
    APath := TimeZonePathInDirectory(ADirectory, CanonicalTimeZone);
    Result := FileExists(APath);
  end;
end;

function TryGetKnownTimeZonePath(const ATimeZone: string; out APath: string): Boolean;
var
  EnvDirectory: string;
  CachedIndex: Integer;
begin
  CachedIndex := FindCachedTimeZonePath(ATimeZone);
  if CachedIndex >= 0 then
  begin
    APath := CachedTimeZonePaths[CachedIndex].Path;
    Exit(CachedTimeZonePaths[CachedIndex].Found);
  end;

  Result := False;
  APath := '';
  EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
  if TryGetTimeZonePathInDirectory(EnvDirectory, ATimeZone, APath) then
    Result := True;

  {$IFDEF UNIX}
  if (not Result) and TryGetTimeZonePathInDirectory(MACOS_DEFAULT_ZONEINFO_PATH,
    ATimeZone, APath) then
    Result := True;

  if (not Result) and TryGetTimeZonePathInDirectory(UNIX_ZONEINFO_PATH,
    ATimeZone, APath) then
    Result := True;
  {$ENDIF}

  CacheTimeZonePath(ATimeZone, APath, Result);
end;

function IsUTCPrimaryEquivalent(const ATimeZone: string): Boolean;
begin
  Result := ASCIIEqualsIgnoreCase(ATimeZone, 'UTC') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/UTC') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT+0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT-0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/GMT0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT+0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT-0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'GMT0') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/Greenwich') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Greenwich') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/UCT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'UCT') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/Universal') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Universal') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Etc/Zulu') or
    ASCIIEqualsIgnoreCase(ATimeZone, 'Zulu');
end;

function IsSupportedCanonicalTimeZoneIdentifier(const ATimeZone: string): Boolean;
var
  AvailableTimeZones: TTemporalTimeZoneIdentifierArray;
  Index: Integer;
begin
  AvailableTimeZones := GetAvailablePrimaryTimeZoneIdentifiers;
  for Index := 0 to Length(AvailableTimeZones) - 1 do
    if AvailableTimeZones[Index] = ATimeZone then
      Exit(True);

  Result := False;
end;

function TimeZoneFilesEqual(const ALeft, ARight: string): Boolean;
{$IFDEF UNIX}
var
  LeftPath, RightPath: string;
  LeftStat, RightStat: TStat;
{$ENDIF}
begin
  Result := False;
  {$IFDEF UNIX}
  if not TryGetKnownTimeZonePath(ALeft, LeftPath) or
     not TryGetKnownTimeZonePath(ARight, RightPath) then
    Exit;
  if (fpStat(PChar(LeftPath), LeftStat) <> 0) or
     (fpStat(PChar(RightPath), RightStat) <> 0) then
    Exit;
  Result := (LeftStat.st_dev = RightStat.st_dev) and
    (LeftStat.st_ino = RightStat.st_ino);
  {$ENDIF}
end;

function TimeZoneIdentifiersEqual(const ALeft, ARight: string): Boolean;
var
  LeftOffset, RightOffset: Integer;
  LeftIsOffset, RightIsOffset: Boolean;
  LeftPrimary, RightPrimary: string;
  LeftHasPrimary, RightHasPrimary: Boolean;
begin
  if ALeft = ARight then
    Exit(True);

  LeftIsOffset := ParseOffsetString(ALeft, LeftOffset);
  RightIsOffset := ParseOffsetString(ARight, RightOffset);
  if LeftIsOffset or RightIsOffset then
    Exit(LeftIsOffset and RightIsOffset and (LeftOffset = RightOffset));

  if IsUTCPrimaryEquivalent(ALeft) and IsUTCPrimaryEquivalent(ARight) then
    Exit(True);

  LeftHasPrimary := TryKnownPrimaryTimeZoneIdentifier(ALeft, LeftPrimary);
  RightHasPrimary := TryKnownPrimaryTimeZoneIdentifier(ARight, RightPrimary);
  if LeftHasPrimary or RightHasPrimary then
  begin
    if not LeftHasPrimary then
      LeftPrimary := ALeft;
    if not RightHasPrimary then
      RightPrimary := ARight;
    Exit(ASCIIEqualsIgnoreCase(LeftPrimary, RightPrimary));
  end;

  if IsSupportedCanonicalTimeZoneIdentifier(ALeft) and
     IsSupportedCanonicalTimeZoneIdentifier(ARight) then
    Exit(False);

  Result := TimeZoneFilesEqual(ALeft, ARight);
end;

function TryLoadTimeZoneDataFromFile(const ATimeZone, AFilePath: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Stream: TFileStream;
  Buffer: TBytes;
  FileSize: Int64;
  BufferSize: Integer;
begin
  Result := False;

  if not FileExists(AFilePath) then
    Exit;

  Stream := nil;
  try
    Stream := TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
    FileSize := Stream.Size;
    if FileSize > High(Integer) then
    begin
      Stream.Free;
      Exit;
    end;

    BufferSize := Integer(FileSize);
    SetLength(Buffer, BufferSize);
    if BufferSize > 0 then
      Stream.ReadBuffer(Buffer[0], BufferSize);
    Stream.Free;
    Stream := nil;

    Result := TryParseTimeZoneInformationFile(ATimeZone, Buffer, AData);
  except
    Stream.Free;
  end;
end;

function TryLoadTimeZoneDataFromDirectory(const ADirectory, ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
begin
  Result := False;
  if ADirectory = '' then
    Exit;

  Result := TryLoadTimeZoneDataFromFile(ATimeZone,
    TimeZonePathInDirectory(ADirectory, ATimeZone), AData);
end;

function TryLoadTimeZoneDataFromEmbeddedData(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Buffer: TBytes;
begin
  Result := False;

  if not TryGetEmbeddedTimeZoneFile(ATimeZone, Buffer) then
    Exit;

  Result := TryParseTimeZoneInformationFile(ATimeZone, Buffer, AData);
end;

function TryLoadTimeZoneDataFromKnownLocations(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  EnvDirectory, PrimaryTimeZone: string;
begin
  Result := False;

  if TryKnownPrimaryTimeZoneIdentifier(ATimeZone, PrimaryTimeZone) and
     (not ASCIIEqualsIgnoreCase(ATimeZone, PrimaryTimeZone)) and
     TryLoadTimeZoneDataFromKnownLocations(PrimaryTimeZone, AData) then
  begin
    AData.Identifier := ATimeZone;
    Result := True;
    Exit;
  end;

  EnvDirectory := GetEnvironmentVariable(GOCCIA_TZDIR_ENV);
  if TryLoadTimeZoneDataFromDirectory(EnvDirectory, ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;

  {$IFDEF UNIX}
  if TryLoadTimeZoneDataFromDirectory(UNIX_ZONEINFO_PATH, ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;

  if TryLoadTimeZoneDataFromDirectory(MACOS_DEFAULT_ZONEINFO_PATH, ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  if TryLoadTimeZoneDataFromWindowsICU(ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;
  {$ENDIF}

  if TryLoadTimeZoneDataFromEmbeddedData(ATimeZone, AData) then
  begin
    Result := True;
    Exit;
  end;
end;

function LoadTimeZoneData(const ATimeZone: string; out AData: TTimeZoneInformationData): Boolean;
begin
  Result := False;

  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    AData.Identifier := UTC_TIMEZONE_ID;
    SetLength(AData.Transitions, 0);
    AData.DefaultOffsetSeconds := 0;
    Result := True;
    Exit;
  end;

  if not IsValidTimeZoneName(ATimeZone) then
    Exit;

  Result := TryLoadTimeZoneDataFromKnownLocations(ATimeZone, AData);
end;

function GetOrLoadTimeZoneData(const ATimeZone: string;
  out AData: TTimeZoneInformationData): Boolean;
var
  Index: Integer;
begin
  Index := FindCachedTimeZone(ATimeZone);
  if Index >= 0 then
  begin
    AData := CachedTimeZones[Index];
    Result := True;
    Exit;
  end;

  Result := LoadTimeZoneData(ATimeZone, AData);
  if not Result then
    Exit;

  // Add to cache
  if CachedTimeZoneCount >= Length(CachedTimeZones) then
    SetLength(CachedTimeZones, CachedTimeZoneCount + 4);
  CachedTimeZones[CachedTimeZoneCount] := AData;
  Inc(CachedTimeZoneCount);
end;

function BinarySearchTransitions(const ATransitions: TTimeZoneInformationTransitionArray;
  const ACount: Integer; const AEpochSeconds: Int64): Integer;
var
  Low, High, Mid: Integer;
begin
  // Find the last transition where TransitionEpochSeconds <= AEpochSeconds
  Result := -1;
  Low := 0;
  High := ACount - 1;
  while Low <= High do
  begin
    Mid := Low + (High - Low) div 2;
    if ATransitions[Mid].TransitionEpochSeconds <= AEpochSeconds then
    begin
      Result := Mid;
      Low := Mid + 1;
    end
    else
      High := Mid - 1;
  end;
end;

function GetSystemTimeZoneId: string;
{$IFDEF UNIX}
var
  LinkTarget: string;
  PrefixPos: Integer;
  Prefix: string;
  LocalTimeDirectory: string;
begin
  // Read the symlink target of /etc/localtime
  SetLength(LinkTarget, MAX_SYMLINK_LENGTH);
  PrefixPos := fpReadLink(LOCALTIME_PATH, @LinkTarget[1], MAX_SYMLINK_LENGTH);
  if PrefixPos > 0 then
  begin
    SetLength(LinkTarget, PrefixPos);
    if (Length(LinkTarget) > 0) and (LinkTarget[1] <> PathDelim) then
    begin
      LocalTimeDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(LOCALTIME_PATH));
      LinkTarget := ExpandFileName(LocalTimeDirectory + LinkTarget);
    end
    else
      LinkTarget := ExpandFileName(LinkTarget);

    Prefix := IncludeTrailingPathDelimiter(UNIX_ZONEINFO_PATH);
    PrefixPos := Pos(Prefix, LinkTarget);
    if PrefixPos = 1 then
    begin
      Result := Copy(LinkTarget, Length(Prefix) + 1, Length(LinkTarget) -
        Length(Prefix));
      Exit;
    end;

    Prefix := IncludeTrailingPathDelimiter(MACOS_DEFAULT_ZONEINFO_PATH);
    PrefixPos := Pos(Prefix, LinkTarget);
    if PrefixPos = 1 then
    begin
      Result := Copy(LinkTarget, Length(Prefix) + 1, Length(LinkTarget) -
        Length(Prefix));
      Exit;
    end;
  end;
  Result := UTC_TIMEZONE_ID;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
var
  TimeZone: string;
begin
  if TryGetWindowsICUDefaultTimeZoneId(TimeZone) then
  begin
    Result := TimeZone;
    Exit;
  end;
  Result := UTC_TIMEZONE_ID;
end;
{$ELSE}
begin
  Result := UTC_TIMEZONE_ID;
end;
{$ENDIF}
{$ENDIF}

function IsValidTimeZone(const ATimeZone: string): Boolean;
var
  Data: TTimeZoneInformationData;
  Dummy: Integer;
begin
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    Result := True;
    Exit;
  end;
  // Accept only minute-precision offset strings as valid timezone IDs.
  if ParseOffsetString(ATimeZone, Dummy) then
  begin
    Result := not OffsetStringHasSubMinuteSyntax(ATimeZone);
    Exit;
  end;
  if not IsValidTimeZoneName(ATimeZone) then
  begin
    Result := False;
    Exit;
  end;

  Result := GetOrLoadTimeZoneData(ATimeZone, Data);
end;

function GetUtcOffsetSeconds(const ATimeZone: string; const AEpochSeconds: Int64): Integer;
var
  Data: TTimeZoneInformationData;
  Index: Integer;
  OffsetSecs: Integer;
begin
  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    Result := 0;
    Exit;
  end;

  // Handle offset-based timezone IDs (e.g. +05:30, -08:00)
  if ParseOffsetString(ATimeZone, OffsetSecs) then
  begin
    Result := OffsetSecs;
    Exit;
  end;

  if not GetOrLoadTimeZoneData(ATimeZone, Data) then
  begin
    Result := 0;
    Exit;
  end;

  if Length(Data.Transitions) = 0 then
  begin
    Result := Data.DefaultOffsetSeconds;
    Exit;
  end;

  Index := BinarySearchTransitions(Data.Transitions, Length(Data.Transitions), AEpochSeconds);
  if Index < 0 then
    Result := Data.DefaultOffsetSeconds
  else
    Result := Data.Transitions[Index].UtcOffsetSeconds;
end;

function FloorEpochSecond(const AEpochMilliseconds: Int64): Int64;
begin
  Result := AEpochMilliseconds div MILLISECONDS_PER_SECOND;
  if (AEpochMilliseconds < 0) and
     (AEpochMilliseconds mod MILLISECONDS_PER_SECOND <> 0) then
    Dec(Result);
end;

function IsTransitionAfterInstant(const ATransitionEpochSeconds,
  AEpochMilliseconds: Int64): Boolean;
begin
  Result := ATransitionEpochSeconds > FloorEpochSecond(AEpochMilliseconds);
end;

function IsTransitionBeforeInstant(const ATransitionEpochSeconds,
  AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer): Boolean;
var
  EpochSeconds: Int64;
begin
  EpochSeconds := FloorEpochSecond(AEpochMilliseconds);
  if ATransitionEpochSeconds < EpochSeconds then
    Exit(True);
  if ATransitionEpochSeconds > EpochSeconds then
    Exit(False);

  Result := (AEpochMilliseconds <> ATransitionEpochSeconds * MILLISECONDS_PER_SECOND) or
    (ASubMillisecondNanoseconds <> 0);
end;

function IsOffsetTransition(const AData: TTimeZoneInformationData;
  const AIndex: Integer): Boolean;
var
  PreviousOffsetSeconds: Integer;
begin
  if AIndex <= 0 then
    PreviousOffsetSeconds := AData.DefaultOffsetSeconds
  else
    PreviousOffsetSeconds := AData.Transitions[AIndex - 1].UtcOffsetSeconds;

  Result := AData.Transitions[AIndex].UtcOffsetSeconds <> PreviousOffsetSeconds;
end;

function TryGetTimeZoneTransitionMilliseconds(const ATimeZone: string;
  const AEpochMilliseconds: Int64; const ASubMillisecondNanoseconds: Integer;
  const ANext: Boolean; out ATransitionEpochMilliseconds: Int64): Boolean;
var
  Data: TTimeZoneInformationData;
  OffsetSecs: Integer;
  I: Integer;
begin
  Result := False;
  ATransitionEpochMilliseconds := 0;

  if (ATimeZone = UTC_TIMEZONE_ID) or ParseOffsetString(ATimeZone, OffsetSecs) then
    Exit;
  if not GetOrLoadTimeZoneData(ATimeZone, Data) then
    Exit;

  if ANext then
  begin
    for I := 0 to Length(Data.Transitions) - 1 do
    begin
      if IsTransitionAfterInstant(Data.Transitions[I].TransitionEpochSeconds,
        AEpochMilliseconds) and IsOffsetTransition(Data, I) then
      begin
        ATransitionEpochMilliseconds :=
          Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
        Exit(True);
      end;
    end;
  end
  else
  begin
    for I := Length(Data.Transitions) - 1 downto 0 do
    begin
      if IsTransitionBeforeInstant(Data.Transitions[I].TransitionEpochSeconds,
        AEpochMilliseconds, ASubMillisecondNanoseconds) and
        IsOffsetTransition(Data, I) then
      begin
        ATransitionEpochMilliseconds :=
          Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
        Exit(True);
      end;
    end;
  end;
end;

function LocalDateTimeToLocalEpochMilliseconds(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer): Int64;
begin
  Result := DateToEpochDays(AYear, AMonth, ADay) * MILLISECONDS_PER_DAY +
            Int64(AHour) * MILLISECONDS_PER_HOUR +
            Int64(AMinute) * MILLISECONDS_PER_MINUTE +
            Int64(ASecond) * MILLISECONDS_PER_SECOND +
            AMillisecond;
end;

function EpochMillisecondsMatchesLocalDateTime(const AEpochMilliseconds: Int64;
  const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond: Integer;
  const ATimeZone: string): Boolean;
var
  OffsetSeconds: Integer;
  LocalEpochMilliseconds, EpochDays, RemainingMilliseconds: Int64;
  DateRec: TTemporalDateRecord;
  LocalHour, LocalMinute, LocalSecond, LocalMillisecond: Integer;
begin
  OffsetSeconds := GetUtcOffsetSeconds(ATimeZone,
    FloorEpochSecond(AEpochMilliseconds));
  LocalEpochMilliseconds := AEpochMilliseconds +
    Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND;

  EpochDays := LocalEpochMilliseconds div MILLISECONDS_PER_DAY;
  RemainingMilliseconds := LocalEpochMilliseconds mod MILLISECONDS_PER_DAY;
  if RemainingMilliseconds < 0 then
  begin
    Dec(EpochDays);
    Inc(RemainingMilliseconds, MILLISECONDS_PER_DAY);
  end;

  DateRec := EpochDaysToDate(EpochDays);
  LocalHour := Integer(RemainingMilliseconds div MILLISECONDS_PER_HOUR);
  RemainingMilliseconds := RemainingMilliseconds mod MILLISECONDS_PER_HOUR;
  LocalMinute := Integer(RemainingMilliseconds div MILLISECONDS_PER_MINUTE);
  RemainingMilliseconds := RemainingMilliseconds mod MILLISECONDS_PER_MINUTE;
  LocalSecond := Integer(RemainingMilliseconds div MILLISECONDS_PER_SECOND);
  LocalMillisecond := Integer(RemainingMilliseconds mod MILLISECONDS_PER_SECOND);

  Result := (DateRec.Year = AYear) and (DateRec.Month = AMonth) and
    (DateRec.Day = ADay) and (LocalHour = AHour) and
    (LocalMinute = AMinute) and (LocalSecond = ASecond) and
    (LocalMillisecond = AMillisecond);
end;

procedure AddPossibleEpochMilliseconds(var AInstants: TTimeZoneEpochMillisecondsArray;
  var ACount: Integer; const AEpochMilliseconds: Int64);
var
  I, InsertIndex: Integer;
begin
  InsertIndex := ACount;
  for I := 0 to ACount - 1 do
  begin
    if AInstants[I] = AEpochMilliseconds then
      Exit;
    if AInstants[I] > AEpochMilliseconds then
    begin
      InsertIndex := I;
      Break;
    end;
  end;

  if ACount >= Length(AInstants) then
    SetLength(AInstants, ACount + 4);
  for I := ACount downto InsertIndex + 1 do
    AInstants[I] := AInstants[I - 1];
  AInstants[InsertIndex] := AEpochMilliseconds;
  Inc(ACount);
end;

procedure ConsiderOffsetForLocalDateTime(var AInstants: TTimeZoneEpochMillisecondsArray;
  var ACount: Integer; const ALocalEpochMilliseconds: Int64;
  const AOffsetSeconds: Integer; const AYear, AMonth, ADay, AHour, AMinute,
  ASecond, AMillisecond: Integer; const ATimeZone: string);
var
  CandidateEpochMilliseconds: Int64;
begin
  CandidateEpochMilliseconds := ALocalEpochMilliseconds -
    Int64(AOffsetSeconds) * MILLISECONDS_PER_SECOND;
  if EpochMillisecondsMatchesLocalDateTime(CandidateEpochMilliseconds,
    AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
    ATimeZone) then
    AddPossibleEpochMilliseconds(AInstants, ACount, CandidateEpochMilliseconds);
end;

procedure GetPossibleEpochMillisecondsForLocalDateTime(const AYear, AMonth,
  ADay, AHour, AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string;
  out AInstants: TTimeZoneEpochMillisecondsArray; out ACount: Integer);
var
  Data: TTimeZoneInformationData;
  LocalEpochMilliseconds: Int64;
  OffsetSeconds, I: Integer;
begin
  SetLength(AInstants, 0);
  ACount := 0;
  LocalEpochMilliseconds := LocalDateTimeToLocalEpochMilliseconds(AYear,
    AMonth, ADay, AHour, AMinute, ASecond, AMillisecond);

  if ATimeZone = UTC_TIMEZONE_ID then
  begin
    AddPossibleEpochMilliseconds(AInstants, ACount, LocalEpochMilliseconds);
    Exit;
  end;

  if ParseOffsetString(ATimeZone, OffsetSeconds) then
  begin
    AddPossibleEpochMilliseconds(AInstants, ACount,
      LocalEpochMilliseconds - Int64(OffsetSeconds) * MILLISECONDS_PER_SECOND);
    Exit;
  end;

  if not GetOrLoadTimeZoneData(ATimeZone, Data) then
  begin
    ConsiderOffsetForLocalDateTime(AInstants, ACount, LocalEpochMilliseconds,
      0, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMillisecond,
      ATimeZone);
    Exit;
  end;

  ConsiderOffsetForLocalDateTime(AInstants, ACount, LocalEpochMilliseconds,
    Data.DefaultOffsetSeconds, AYear, AMonth, ADay, AHour, AMinute,
    ASecond, AMillisecond, ATimeZone);
  for I := 0 to Length(Data.Transitions) - 1 do
    ConsiderOffsetForLocalDateTime(AInstants, ACount, LocalEpochMilliseconds,
      Data.Transitions[I].UtcOffsetSeconds, AYear, AMonth, ADay, AHour,
      AMinute, ASecond, AMillisecond, ATimeZone);
end;

function TryFindGapForLocalEpochMilliseconds(const ATimeZone: string;
  const ALocalEpochMilliseconds: Int64; out APreviousOffsetSeconds,
  ANextOffsetSeconds: Integer; out ATransitionEpochMilliseconds: Int64): Boolean;
var
  Data: TTimeZoneInformationData;
  PreviousOffsetSeconds, NextOffsetSeconds, I: Integer;
  TransitionEpochMilliseconds, LocalBeforeTransition, LocalAfterTransition: Int64;
begin
  Result := False;
  APreviousOffsetSeconds := 0;
  ANextOffsetSeconds := 0;
  ATransitionEpochMilliseconds := 0;

  if (ATimeZone = UTC_TIMEZONE_ID) or
     ParseOffsetString(ATimeZone, PreviousOffsetSeconds) or
     (not GetOrLoadTimeZoneData(ATimeZone, Data)) then
    Exit;

  PreviousOffsetSeconds := Data.DefaultOffsetSeconds;
  for I := 0 to Length(Data.Transitions) - 1 do
  begin
    NextOffsetSeconds := Data.Transitions[I].UtcOffsetSeconds;
    if NextOffsetSeconds > PreviousOffsetSeconds then
    begin
      TransitionEpochMilliseconds :=
        Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
      LocalBeforeTransition := TransitionEpochMilliseconds +
        Int64(PreviousOffsetSeconds) * MILLISECONDS_PER_SECOND;
      LocalAfterTransition := TransitionEpochMilliseconds +
        Int64(NextOffsetSeconds) * MILLISECONDS_PER_SECOND;
      if (ALocalEpochMilliseconds >= LocalBeforeTransition) and
         (ALocalEpochMilliseconds < LocalAfterTransition) then
      begin
        APreviousOffsetSeconds := PreviousOffsetSeconds;
        ANextOffsetSeconds := NextOffsetSeconds;
        ATransitionEpochMilliseconds := TransitionEpochMilliseconds;
        Exit(True);
      end;
    end;
    PreviousOffsetSeconds := NextOffsetSeconds;
  end;
end;

function TryFindStartOfDayGap(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string; out ATransitionEpochMilliseconds: Int64): Boolean;
var
  Data: TTimeZoneInformationData;
  DateRec: TTemporalDateRecord;
  DayStartMilliseconds, DayEndMilliseconds: Int64;
  TransitionEpochMilliseconds, LocalBeforeTransition, LocalAfterTransition: Int64;
  PreviousOffsetSeconds, NextOffsetSeconds, I: Integer;
begin
  Result := False;
  ATransitionEpochMilliseconds := 0;

  if (ATimeZone = UTC_TIMEZONE_ID) or
     ParseOffsetString(ATimeZone, PreviousOffsetSeconds) or
     (not GetOrLoadTimeZoneData(ATimeZone, Data)) then
    Exit;

  DayStartMilliseconds := LocalDateTimeToLocalEpochMilliseconds(AYear,
    AMonth, ADay, 0, 0, 0, 0);
  DateRec := AddDaysToDate(AYear, AMonth, ADay, 1);
  DayEndMilliseconds := LocalDateTimeToLocalEpochMilliseconds(DateRec.Year,
    DateRec.Month, DateRec.Day, 0, 0, 0, 0);

  PreviousOffsetSeconds := Data.DefaultOffsetSeconds;
  for I := 0 to Length(Data.Transitions) - 1 do
  begin
    NextOffsetSeconds := Data.Transitions[I].UtcOffsetSeconds;
    if NextOffsetSeconds > PreviousOffsetSeconds then
    begin
      TransitionEpochMilliseconds :=
        Data.Transitions[I].TransitionEpochSeconds * MILLISECONDS_PER_SECOND;
      LocalBeforeTransition := TransitionEpochMilliseconds +
        Int64(PreviousOffsetSeconds) * MILLISECONDS_PER_SECOND;
      LocalAfterTransition := TransitionEpochMilliseconds +
        Int64(NextOffsetSeconds) * MILLISECONDS_PER_SECOND;
      if (DayStartMilliseconds >= LocalBeforeTransition) and
         (DayStartMilliseconds < LocalAfterTransition) and
         (LocalAfterTransition < DayEndMilliseconds) then
      begin
        ATransitionEpochMilliseconds := TransitionEpochMilliseconds;
        Exit(True);
      end;
    end;
    PreviousOffsetSeconds := NextOffsetSeconds;
  end;
end;

procedure ThrowDisambiguationRangeError;
begin
  ThrowRangeError('Temporal local date-time is ambiguous or does not exist in the time zone');
end;

function LocalDateTimeToEpochMilliseconds(const AYear, AMonth, ADay, AHour,
  AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string): Int64;
begin
  Result := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear, AMonth,
    ADay, AHour, AMinute, ASecond, AMillisecond, ATimeZone, ttzdCompatible);
end;

function LocalDateTimeToEpochMillisecondsWithDisambiguation(const AYear, AMonth,
  ADay, AHour, AMinute, ASecond, AMillisecond: Integer; const ATimeZone: string;
  const ADisambiguation: TTemporalTimeZoneDisambiguation): Int64;
var
  Instants: TTimeZoneEpochMillisecondsArray;
  InstantCount: Integer;
  LocalEpochMilliseconds: Int64;
  PreviousOffsetSeconds, NextOffsetSeconds: Integer;
  TransitionEpochMilliseconds: Int64;
begin
  GetPossibleEpochMillisecondsForLocalDateTime(AYear, AMonth, ADay, AHour,
    AMinute, ASecond, AMillisecond, ATimeZone, Instants, InstantCount);
  if InstantCount > 0 then
  begin
    if (ADisambiguation = ttzdReject) and (InstantCount <> 1) then
      ThrowDisambiguationRangeError;
    if ADisambiguation = ttzdLater then
      Exit(Instants[InstantCount - 1]);
    Exit(Instants[0]);
  end;

  if ADisambiguation = ttzdReject then
    ThrowDisambiguationRangeError;

  LocalEpochMilliseconds := LocalDateTimeToLocalEpochMilliseconds(AYear,
    AMonth, ADay, AHour, AMinute, ASecond, AMillisecond);
  if TryFindGapForLocalEpochMilliseconds(ATimeZone, LocalEpochMilliseconds,
    PreviousOffsetSeconds, NextOffsetSeconds, TransitionEpochMilliseconds) then
  begin
    if ADisambiguation = ttzdEarlier then
      Exit(LocalEpochMilliseconds - Int64(NextOffsetSeconds) *
        MILLISECONDS_PER_SECOND);
    Exit(LocalEpochMilliseconds - Int64(PreviousOffsetSeconds) *
      MILLISECONDS_PER_SECOND);
  end;

  Result := LocalEpochMilliseconds -
    Int64(GetUtcOffsetSeconds(ATimeZone, FloorEpochSecond(LocalEpochMilliseconds))) *
    MILLISECONDS_PER_SECOND;
end;

function StartOfDayToEpochMilliseconds(const AYear, AMonth, ADay: Integer;
  const ATimeZone: string): Int64;
var
  Instants: TTimeZoneEpochMillisecondsArray;
  InstantCount: Integer;
begin
  GetPossibleEpochMillisecondsForLocalDateTime(AYear, AMonth, ADay, 0, 0, 0,
    0, ATimeZone, Instants, InstantCount);
  if InstantCount > 0 then
    Exit(Instants[0]);

  if TryFindStartOfDayGap(AYear, AMonth, ADay, ATimeZone, Result) then
    Exit;

  Result := LocalDateTimeToEpochMillisecondsWithDisambiguation(AYear, AMonth,
    ADay, 0, 0, 0, 0, ATimeZone, ttzdCompatible);
end;

function FormatOffsetString(const AOffsetSeconds: Integer): string;
var
  AbsOffset, Hours, Minutes, Seconds: Integer;
  Sign: Char;
begin
  if AOffsetSeconds >= 0 then
    Sign := '+'
  else
    Sign := '-';

  AbsOffset := Abs(AOffsetSeconds);
  Hours := AbsOffset div (MINUTES_PER_HOUR * SECONDS_PER_MINUTE);
  Minutes := (AbsOffset mod (MINUTES_PER_HOUR * SECONDS_PER_MINUTE)) div SECONDS_PER_MINUTE;
  Seconds := AbsOffset mod SECONDS_PER_MINUTE;

  if Seconds = 0 then
    Result := Sign + Format('%.2d:%.2d', [Hours, Minutes])
  else
    Result := Sign + Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]);
end;

function ParseOffsetString(const AStr: string; out AOffsetSeconds: Integer): Boolean;
var
  Sign: Integer;
  Hours, Minutes, Seconds: Integer;
  Pos: Integer;
  HasColon: Boolean;
begin
  Result := False;
  AOffsetSeconds := 0;

  if Length(AStr) = 0 then
    Exit;

  // Handle 'Z' for UTC
  if (Length(AStr) = 1) and ((AStr[1] = 'Z') or (AStr[1] = 'z')) then
  begin
    AOffsetSeconds := 0;
    Result := True;
    Exit;
  end;

  // Must start with + or -
  case AStr[1] of
    '+': Sign := 1;
    '-': Sign := -1;
  else
    Exit;
  end;

  Pos := 2;

  // Parse hours (2 digits)
  if Pos + 1 > Length(AStr) then Exit;
  if (AStr[Pos] < '0') or (AStr[Pos] > '9') then Exit;
  if (AStr[Pos + 1] < '0') or (AStr[Pos + 1] > '9') then Exit;
  Hours := (Ord(AStr[Pos]) - Ord('0')) * 10 + (Ord(AStr[Pos + 1]) - Ord('0'));
  Inc(Pos, 2);

  // Check for optional colon separator
  if Pos > Length(AStr) then
  begin
    // +HH format (hours only)
    AOffsetSeconds := Sign * Hours * MINUTES_PER_HOUR * SECONDS_PER_MINUTE;
    Result := True;
    Exit;
  end;

  if AStr[Pos] = ':' then
  begin
    HasColon := True;
    Inc(Pos);
  end
  else
    HasColon := False;

  // Parse minutes (2 digits)
  if Pos + 1 > Length(AStr) then Exit;
  if (AStr[Pos] < '0') or (AStr[Pos] > '9') then Exit;
  if (AStr[Pos + 1] < '0') or (AStr[Pos + 1] > '9') then Exit;
  Minutes := (Ord(AStr[Pos]) - Ord('0')) * 10 + (Ord(AStr[Pos + 1]) - Ord('0'));
  Inc(Pos, 2);
  Seconds := 0;

  if Pos <= Length(AStr) then
  begin
    if HasColon then
    begin
      if AStr[Pos] <> ':' then
        Exit;
      Inc(Pos);
    end;

    if Pos + 1 > Length(AStr) then Exit;
    if (AStr[Pos] < '0') or (AStr[Pos] > '9') then Exit;
    if (AStr[Pos + 1] < '0') or (AStr[Pos + 1] > '9') then Exit;
    Seconds := (Ord(AStr[Pos]) - Ord('0')) * 10 +
      (Ord(AStr[Pos + 1]) - Ord('0'));
    Inc(Pos, 2);
  end;

  // Must be at end of string
  if Pos <= Length(AStr) then Exit;

  // Validate ranges
  if (Hours > 23) or (Minutes > 59) or (Seconds > 59) then Exit;

  AOffsetSeconds := Sign * (Hours * MINUTES_PER_HOUR * SECONDS_PER_MINUTE +
    Minutes * SECONDS_PER_MINUTE + Seconds);
  Result := True;
end;

procedure ClearTimeZoneCache;
begin
  SetLength(CachedTimeZones, 0);
  CachedTimeZoneCount := 0;
  SetLength(CachedTimeZonePaths, 0);
  CachedTimeZonePathCount := 0;
  SetLength(CachedTimeZoneCases, 0);
  CachedTimeZoneCaseCount := 0;
  SetLength(CachedAvailablePrimaryTimeZoneIdentifiers, 0);
  CachedAvailablePrimaryTimeZoneIdentifiersLoaded := False;
end;

initialization
  CachedTimeZoneCount := 0;
  CachedTimeZonePathCount := 0;
  CachedTimeZoneCaseCount := 0;
  CachedAvailablePrimaryTimeZoneIdentifiersLoaded := False;
  {$IFDEF MSWINDOWS}
  InitCriticalSection(WindowsICUInitLock);
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  DoneCriticalSection(WindowsICUInitLock);
  {$ENDIF}

end.

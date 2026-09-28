program Goccia.Temporal.TimeZone.Test;

{$I Goccia.inc}

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}

  TestingPascalLibrary,

  Goccia.Temporal.TimeZone;

type
  TTemporalTimeZoneTests = class(TTestSuite)
  private
    procedure TestLinuxZoneInfoLink;
    procedure TestRelativeZoneInfoLink;
    procedure TestMacOSDefaultZoneInfoLink;
    procedure TestMacOSTimeZoneDatabaseLink;
    procedure TestDotSegmentsAreResolved;
    procedure TestTargetOutsideZoneInfoFallsBackToUTC;
    procedure TestZoneInfoPrefixNeedsDirectoryBoundary;
    procedure TestEmptyOrBareDirectoryFallsBackToUTC;
    {$IFDEF UNIX}
    procedure TestSystemTimeZoneFollowsLocalTimeLink;
    {$ENDIF}
  public
    procedure SetupTests; override;
  end;

procedure TTemporalTimeZoneTests.SetupTests;
begin
  Test('a /usr/share/zoneinfo link names its zone', TestLinuxZoneInfoLink);
  Test('a relative link resolves against /etc', TestRelativeZoneInfoLink);
  Test('a macOS zoneinfo.default link names its zone',
    TestMacOSDefaultZoneInfoLink);
  Test('a macOS /var/db/timezone link names its zone',
    TestMacOSTimeZoneDatabaseLink);
  Test('dot segments in the link are resolved', TestDotSegmentsAreResolved);
  Test('a link outside zoneinfo falls back to UTC',
    TestTargetOutsideZoneInfoFallsBackToUTC);
  Test('the zoneinfo prefix must end at a directory boundary',
    TestZoneInfoPrefixNeedsDirectoryBoundary);
  Test('an empty link or the bare directory falls back to UTC',
    TestEmptyOrBareDirectoryFallsBackToUTC);
  {$IFDEF UNIX}
  Test('the system time zone follows the /etc/localtime link',
    TestSystemTimeZoneFollowsLocalTimeLink);
  {$ENDIF}
end;

procedure TTemporalTimeZoneTests.TestLinuxZoneInfoLink;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfo/Europe/London')).ToBe('Europe/London');
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfo/Etc/UTC')).ToBe('Etc/UTC');
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfo/America/Argentina/Buenos_Aires')).ToBe(
    'America/Argentina/Buenos_Aires');
end;

procedure TTemporalTimeZoneTests.TestRelativeZoneInfoLink;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '../usr/share/zoneinfo/America/New_York')).ToBe('America/New_York');
end;

procedure TTemporalTimeZoneTests.TestMacOSDefaultZoneInfoLink;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfo.default/Asia/Tokyo')).ToBe('Asia/Tokyo');
end;

procedure TTemporalTimeZoneTests.TestMacOSTimeZoneDatabaseLink;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/var/db/timezone/zoneinfo/Europe/Berlin')).ToBe('Europe/Berlin');
end;

procedure TTemporalTimeZoneTests.TestDotSegmentsAreResolved;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfo/./Europe/../Europe/Paris')).ToBe('Europe/Paris');
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr//share/zoneinfo/Australia/Sydney')).ToBe('Australia/Sydney');
end;

procedure TTemporalTimeZoneTests.TestTargetOutsideZoneInfoFallsBackToUTC;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/opt/zones/Europe/London')).ToBe('UTC');
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfo/../Europe/London')).ToBe('UTC');
end;

procedure TTemporalTimeZoneTests.TestZoneInfoPrefixNeedsDirectoryBoundary;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink(
    '/usr/share/zoneinfoextra/Europe/London')).ToBe('UTC');
end;

procedure TTemporalTimeZoneTests.TestEmptyOrBareDirectoryFallsBackToUTC;
begin
  Expect<string>(TimeZoneIdFromLocalTimeLink('')).ToBe('UTC');
  Expect<string>(TimeZoneIdFromLocalTimeLink('/usr/share/zoneinfo')).ToBe(
    'UTC');
  Expect<string>(TimeZoneIdFromLocalTimeLink('/usr/share/zoneinfo/')).ToBe(
    'UTC');
end;

{$IFDEF UNIX}
{ Reads the machine's own link independently of GetSystemTimeZoneId, so this
  holds whatever zone the machine is set to. }
procedure TTemporalTimeZoneTests.TestSystemTimeZoneFollowsLocalTimeLink;
var
  Expected: string;
begin
  Expected := TimeZoneIdFromLocalTimeLink(string(fpReadLink('/etc/localtime')));
  if not IsValidTimeZone(Expected) then
    Expected := 'UTC';
  Expect<string>(GetSystemTimeZoneId).ToBe(Expected);
end;
{$ENDIF}

begin
  TestRunnerProgram.AddSuite(TTemporalTimeZoneTests.Create(
    'Temporal time zone'));
  TestRunnerProgram.Run;

  ExitCode := TestResultToExitCode;
end.

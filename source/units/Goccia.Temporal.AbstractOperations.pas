unit Goccia.Temporal.AbstractOperations;

{$I Goccia.inc}

interface

uses
  Goccia.Values.ObjectValue,
  Goccia.Values.Primitives;

// TC39 Temporal §12.3.10 ToTemporalCalendarIdentifier
function ToTemporalCalendarIdentifier(const AValue: TGocciaValue;
  const AAllowISOStrings: Boolean = True): string;

// TC39 Temporal §12.3.11 GetTemporalCalendarIdentifierWithISODefault
function GetTemporalCalendarIdentifierWithISODefault(
  const AItem: TGocciaObjectValue): string;

implementation

uses
  Goccia.Error.Suggestions,
  Goccia.Temporal.Calendar,
  Goccia.Temporal.Utils,
  Goccia.Values.ErrorHelper,
  Goccia.Values.TemporalPlainDate,
  Goccia.Values.TemporalPlainDateTime,
  Goccia.Values.TemporalPlainMonthDay,
  Goccia.Values.TemporalPlainYearMonth,
  Goccia.Values.TemporalZonedDateTime;

function TryGetInitializedTemporalCalendarIdentifier(const AValue: TGocciaValue;
  out ACalendarId: string): Boolean;
begin
  Result := True;
  if AValue is TGocciaTemporalPlainDateValue then
    ACalendarId := TGocciaTemporalPlainDateValue(AValue).CalendarId
  else if AValue is TGocciaTemporalPlainDateTimeValue then
    ACalendarId := TGocciaTemporalPlainDateTimeValue(AValue).CalendarId
  else if AValue is TGocciaTemporalPlainMonthDayValue then
    ACalendarId := TGocciaTemporalPlainMonthDayValue(AValue).CalendarId
  else if AValue is TGocciaTemporalPlainYearMonthValue then
    ACalendarId := TGocciaTemporalPlainYearMonthValue(AValue).CalendarId
  else if AValue is TGocciaTemporalZonedDateTimeValue then
    ACalendarId := TGocciaTemporalZonedDateTimeValue(AValue).CalendarId
  else
  begin
    ACalendarId := '';
    Result := False;
  end;
end;

function ToTemporalCalendarIdentifier(const AValue: TGocciaValue;
  const AAllowISOStrings: Boolean): string;
var
  CalendarString: string;
begin
  if (AValue = nil) or (AValue is TGocciaUndefinedLiteralValue) then
    Exit('iso8601');

  if TryGetInitializedTemporalCalendarIdentifier(AValue, Result) then
    Exit;

  if not (AValue is TGocciaStringLiteralValue) then
    ThrowTypeError('Calendar must be a string or Temporal object',
      SSuggestTemporalFromArg);

  CalendarString := TGocciaStringLiteralValue(AValue).Value;
  if AAllowISOStrings then
    Result := ParseTemporalCalendarStringIdentifierStrict(CalendarString)
  else
    Result := CanonicalizeTemporalCalendarIdentifier(CalendarString);
  if Result = '' then
    ThrowRangeError('Unknown calendar: ' + CalendarString,
      SSuggestTemporalDateRange);
end;

function GetTemporalCalendarIdentifierWithISODefault(
  const AItem: TGocciaObjectValue): string;
var
  CalendarLike: TGocciaValue;
begin
  if TryGetInitializedTemporalCalendarIdentifier(AItem, Result) then
    Exit;

  CalendarLike := AItem.GetProperty('calendar');
  if (CalendarLike = nil) or
     (CalendarLike is TGocciaUndefinedLiteralValue) then
    Exit('iso8601');
  Result := ToTemporalCalendarIdentifier(CalendarLike);
end;

end.

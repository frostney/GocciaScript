unit CLI.Units;

{ Parsers for option values that carry a unit: byte sizes, durations, and
  plain counts. Shared by every binary so `--max-memory=64MiB` means the same
  thing on each command line and in each config file.

  - Bytes: a whole number, optionally followed by `KiB`, `MiB`, or `GiB`
    (binary multiples, case-insensitive, the trailing `B` optional). The
    decimal-looking `K`/`KB`/`M`/`MB`/`G`/`GB` are rejected as ambiguous.
  - Durations: a whole number of milliseconds, or a whole number followed by
    `ms`, `s`, or `m` (case-insensitive).
  - Counts: a non-negative whole number.

  No form accepts a sign, a fraction, or surrounding whitespace. }

{$I Shared.inc}

interface

function TryParseByteSize(const AText: string; out ABytes: Int64;
  out AError: string): Boolean;
function TryParseDurationMilliseconds(const AText: string; out AMs: Int64;
  out AError: string): Boolean;
function TryParseNonNegativeCount(const AText: string; out ACount: Int64;
  out AError: string): Boolean;

const
  BYTE_SIZE_ERROR = 'use a whole number of bytes, KiB, MiB, or GiB';
  DURATION_ERROR =
    'use a duration such as 500ms, 5s, or 2m, or plain milliseconds';
  COUNT_ERROR = 'use a non-negative whole number';
  TOO_LARGE_ERROR = 'value is too large';

implementation

uses
  SysUtils;

const
  BYTES_PER_KIBIBYTE = 1024;
  MILLISECONDS_PER_SECOND = 1000;
  SECONDS_PER_MINUTE = 60;
  { Every consumer stores a duration as a 32-bit millisecond count. }
  MAX_DURATION_MILLISECONDS = High(Integer);

{ Splits AText into a leading run of decimal digits and the rest. False when
  there are no leading digits or the number overflows Int64. }
function TrySplitNumber(const AText: string; out ANumber: Int64;
  out ASuffix: string; out AOverflow: Boolean): Boolean;
var
  I: Integer;
  Digit: Int64;
begin
  ANumber := 0;
  ASuffix := '';
  AOverflow := False;
  I := 1;
  while (I <= Length(AText)) and (AText[I] >= '0') and (AText[I] <= '9') do
  begin
    Digit := Ord(AText[I]) - Ord('0');
    if ANumber > (High(Int64) - Digit) div 10 then
    begin
      AOverflow := True;
      Exit(False);
    end;
    ANumber := ANumber * 10 + Digit;
    Inc(I);
  end;
  if I = 1 then
    Exit(False);
  ASuffix := Copy(AText, I, MaxInt);
  Result := True;
end;

function TryMultiply(const AValue, AFactor: Int64; out AResult: Int64): Boolean;
begin
  if (AFactor <> 0) and (AValue > High(Int64) div AFactor) then
    Exit(False);
  AResult := AValue * AFactor;
  Result := True;
end;

function TryParseByteSize(const AText: string; out ABytes: Int64;
  out AError: string): Boolean;
var
  Number, Factor: Int64;
  Suffix: string;
  Overflow: Boolean;
begin
  ABytes := 0;
  AError := '';
  if not TrySplitNumber(AText, Number, Suffix, Overflow) then
  begin
    if Overflow then
      AError := TOO_LARGE_ERROR
    else
      AError := BYTE_SIZE_ERROR;
    Exit(False);
  end;

  Suffix := LowerCase(Suffix);
  if (Suffix = '') or (Suffix = 'b') then
    Factor := 1
  else if (Suffix = 'kib') or (Suffix = 'ki') then
    Factor := BYTES_PER_KIBIBYTE
  else if (Suffix = 'mib') or (Suffix = 'mi') then
    Factor := BYTES_PER_KIBIBYTE * BYTES_PER_KIBIBYTE
  else if (Suffix = 'gib') or (Suffix = 'gi') then
    Factor := BYTES_PER_KIBIBYTE * BYTES_PER_KIBIBYTE * BYTES_PER_KIBIBYTE
  else if (Suffix = 'k') or (Suffix = 'kb') or (Suffix = 'm') or
     (Suffix = 'mb') or (Suffix = 'g') or (Suffix = 'gb') then
  begin
    AError := Format('"%s" is ambiguous; use KiB, MiB, or GiB, or a plain ' +
      'byte count', [Copy(AText, Length(AText) - Length(Suffix) + 1,
      MaxInt)]);
    Exit(False);
  end
  else
  begin
    AError := BYTE_SIZE_ERROR;
    Exit(False);
  end;

  if not TryMultiply(Number, Factor, ABytes) then
  begin
    AError := TOO_LARGE_ERROR;
    Exit(False);
  end;
  Result := True;
end;

function TryParseDurationMilliseconds(const AText: string; out AMs: Int64;
  out AError: string): Boolean;
var
  Number, Factor: Int64;
  Suffix: string;
  Overflow: Boolean;
begin
  AMs := 0;
  AError := '';
  if not TrySplitNumber(AText, Number, Suffix, Overflow) then
  begin
    if Overflow then
      AError := TOO_LARGE_ERROR
    else
      AError := DURATION_ERROR;
    Exit(False);
  end;

  Suffix := LowerCase(Suffix);
  if (Suffix = '') or (Suffix = 'ms') then
    Factor := 1
  else if Suffix = 's' then
    Factor := MILLISECONDS_PER_SECOND
  else if Suffix = 'm' then
    Factor := MILLISECONDS_PER_SECOND * SECONDS_PER_MINUTE
  else
  begin
    AError := DURATION_ERROR;
    Exit(False);
  end;

  if (not TryMultiply(Number, Factor, AMs)) or
     (AMs > MAX_DURATION_MILLISECONDS) then
  begin
    AMs := 0;
    AError := TOO_LARGE_ERROR;
    Exit(False);
  end;
  Result := True;
end;

function TryParseNonNegativeCount(const AText: string; out ACount: Int64;
  out AError: string): Boolean;
var
  Suffix: string;
  Overflow: Boolean;
begin
  AError := '';
  if not TrySplitNumber(AText, ACount, Suffix, Overflow) then
  begin
    ACount := 0;
    if Overflow then
      AError := TOO_LARGE_ERROR
    else
      AError := COUNT_ERROR;
    Exit(False);
  end;
  if Suffix <> '' then
  begin
    ACount := 0;
    AError := COUNT_ERROR;
    Exit(False);
  end;
  Result := True;
end;

end.

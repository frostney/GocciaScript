# Temporal Built-ins

*Temporal API reference — dates, times, durations, and time zones.*

Implements the [ECMAScript Temporal API](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Temporal). See the [Temporal documentation](https://tc39.es/proposal-temporal/docs/) for the full specification reference.

**GocciaScript differences:** ISO 8601 calendar only. `Duration.total()` and `Duration.round()` do not yet support `relativeTo` for calendar-relative conversions (years/months).

## Executive Summary

- **Modern date/time API** — Implements the ECMAScript Temporal API, replacing legacy `Date` with immutable, type-safe alternatives
- **Rich type system** — Provides distinct types for dates, times, date-times, instants, durations, year-months, month-days, and timezone-aware date-times
- **ISO 8601 only** — All types use the ISO 8601 calendar; operations return new instances (immutability by design)
- **Timezone support** — `ZonedDateTime` and `Temporal.Now` handle IANA timezone identifiers and DST transitions
- **Arithmetic and comparison** — Every type supports `add`, `subtract`, `until`, `since`, `equals`, and static `compare`

## Overview (`Goccia.Builtins.Temporal.pas`)

An implementation of the ECMAScript Temporal API providing modern date/time handling. ISO 8601 calendar only. All Temporal types are immutable — operations return new instances.

**Namespace structure:**

```javascript
Temporal.Now           // Current time utilities
Temporal.Duration      // Time duration representation
Temporal.Instant       // Absolute point in time (epoch-based)
Temporal.PlainDate     // Calendar date (no time/timezone)
Temporal.PlainTime     // Wall-clock time (no date/timezone)
Temporal.PlainDateTime // Date + time (no timezone)
Temporal.PlainYearMonth // Year and month (no day/time/timezone)
Temporal.PlainMonthDay  // Month and day (no year/time/timezone)
Temporal.ZonedDateTime  // Date + time + timezone
```

## Time Zone Data

`Temporal.ZonedDateTime` loads IANA timezone data from TZif zoneinfo data. Lookup order is:

1. `GOCCIA_TZDIR` when set
2. system zoneinfo directories on Unix-like platforms (`/usr/share/zoneinfo`, `/usr/share/zoneinfo.default`)
3. Windows ICU on Windows 10 and newer
4. embedded generated TZif resource data

The embedded data is generated from the upstream IANA timezone data distribution and linked as a FreePascal `.res` resource. This keeps named IANA zones and DST transitions available in single-binary builds even when neither system zoneinfo nor Windows ICU can provide the requested zone. Define `GOCCIA_TEMPORAL_NO_EMBEDDED_TZDATA` at compile time to omit the embedded fallback.

## Temporal.Duration

Represents a length of time with 10 components (years through nanoseconds).

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.Duration(y?, mo?, w?, d?, h?, min?, s?, ms?, us?, ns?)` | Create from components (all default to 0) |
| `Temporal.Duration.from(item)` | Create from string (`"P1Y2M3DT4H5M6S"`), Duration, or object |
| `Temporal.Duration.compare(one, two)` | Compare two durations (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `years`, `months`, `weeks`, `days` | Date components |
| `hours`, `minutes`, `seconds` | Time components |
| `milliseconds`, `microseconds`, `nanoseconds` | Sub-second components |
| `sign` | -1, 0, or 1 |
| `blank` | True if all components are zero |

| Method | Description |
|--------|-------------|
| `negated()` | Return negated duration |
| `abs()` | Return absolute duration |
| `add(other)` | Add another duration |
| `subtract(other)` | Subtract another duration |
| `with(fields)` | Return new duration with overridden fields |
| `total(unit)` | Convert to total of a single unit (e.g., `"hours"`). Accepts a string or options object `{ unit, relativeTo? }`. Throws `RangeError` if duration has non-zero years/months without `relativeTo`. Calendar-relative conversion (`relativeTo`) is not yet supported. |
| `round(options)` | Round the duration. Accepts a string (smallestUnit) or options object `{ smallestUnit, largestUnit, roundingMode, roundingIncrement }`. Rebalances components from `largestUnit` down. Throws `RangeError` if duration has years/months (requires `relativeTo`, not yet supported). At least `smallestUnit` or `largestUnit` must be specified. |
| `toString()` / `toJSON()` | ISO 8601 duration string (e.g., `"P1Y2M3DT4H5M6S"`) |
| `valueOf()` | Throws TypeError (prevents implicit coercion) |

## Temporal.PlainDate

Represents a calendar date without time or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainDate(year, month, day)` | Create from components |
| `Temporal.PlainDate.from(item [, options])` | Create from string (`"2024-03-15"`), PlainDate, or object. Options: `{ overflow }` where overflow is `"constrain"` (default, clamps out-of-range values) or `"reject"` (throws RangeError). |
| `Temporal.PlainDate.compare(one, two)` | Compare two dates (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `year`, `month`, `day` | Date components |
| `monthCode` | `"M01"` through `"M12"` |
| `dayOfWeek` | 1 (Monday) through 7 (Sunday) |
| `dayOfYear`, `weekOfYear`, `yearOfWeek` | ISO week-date components |
| `daysInWeek`, `daysInMonth`, `daysInYear`, `monthsInYear` | Calendar info |
| `inLeapYear` | Boolean |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new date with overridden fields |
| `add(duration)` / `subtract(duration)` | Date arithmetic |
| `until(other [, options])` / `since(other [, options])` | Difference as Duration. Options: `{ largestUnit, smallestUnit, roundingMode, roundingIncrement }`. Units: `"year"`, `"month"`, `"week"`, or `"day"` (defaults: largest `"day"`, smallest `"day"`). Rounding mode defaults to `"trunc"`; increment defaults to 1. |
| `equals(other)` | Equality check |
| `toPlainDateTime(time?)` | Combine with a time |
| `toPlainYearMonth()` | Extract year and month as PlainYearMonth |
| `toPlainMonthDay()` | Extract month and day as PlainMonthDay |
| `toZonedDateTime(timeZone)` | Combine with a timezone (string or `{ timeZone }` object) to create a ZonedDateTime at midnight |
| `toString()` / `toJSON()` | ISO date string (e.g., `"2024-03-15"`) |
| `valueOf()` | Throws TypeError |

## Temporal.PlainTime

Represents a wall-clock time without date or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainTime(h?, min?, s?, ms?, us?, ns?)` | Create from components (all default to 0) |
| `Temporal.PlainTime.from(item)` | Create from string (`"13:45:30"`), PlainTime, or object |
| `Temporal.PlainTime.compare(one, two)` | Compare two times (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `hour`, `minute`, `second` | Time components |
| `millisecond`, `microsecond`, `nanosecond` | Sub-second components |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new time with overridden fields |
| `add(duration)` / `subtract(duration)` | Time arithmetic (wraps at midnight) |
| `until(other [, options])` / `since(other [, options])` | Difference as Duration. Options: `{ largestUnit, smallestUnit, roundingMode, roundingIncrement }`. Units: `"hour"` through `"nanosecond"` (defaults: largest `"hour"`, smallest `"nanosecond"`). Rounding mode defaults to `"trunc"`; increment defaults to 1. |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `toString([options])` / `toJSON()` | ISO time string (e.g., `"13:45:30"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

## Temporal.PlainDateTime

Represents a date and time without timezone. Combines PlainDate and PlainTime.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainDateTime(y, mo, d, h?, min?, s?, ms?, us?, ns?)` | Create from components |
| `Temporal.PlainDateTime.from(item)` | Create from string, PlainDateTime, or object |
| `Temporal.PlainDateTime.compare(one, two)` | Compare two date-times (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| All PlainDate getters + all PlainTime getters | Combined date and time access |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new date-time with overridden fields |
| `withPlainTime(time?)` | Replace time component |
| `add(duration)` / `subtract(duration)` | Date-time arithmetic |
| `until(other [, options])` / `since(other [, options])` | Difference as Duration. Options: `{ largestUnit, smallestUnit, roundingMode, roundingIncrement }`. Any unit from `"year"` to `"nanosecond"` (defaults: largest `"day"`, smallest `"nanosecond"`). Mode defaults to `"trunc"`, increment to 1. |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `toPlainDate()` / `toPlainTime()` | Extract date or time component |
| `toPlainYearMonth()` | Extract year and month as PlainYearMonth |
| `toPlainMonthDay()` | Extract month and day as PlainMonthDay |
| `toZonedDateTime(timeZone)` | Combine with a timezone (string or `{ timeZone }` object) to create a ZonedDateTime |
| `toString([options])` / `toJSON()` | ISO string (e.g., `"2024-03-15T13:45:30"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

## Temporal.Instant

Represents an absolute point in time (epoch-based), independent of calendar or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.Instant(epochNanoseconds)` | Create from epoch nanoseconds (BigInt) |
| `Temporal.Instant.from(item)` | Create from string or Instant |
| `Temporal.Instant.fromEpochMilliseconds(ms)` | Create from epoch milliseconds (Number) |
| `Temporal.Instant.fromEpochNanoseconds(ns)` | Create from epoch nanoseconds (BigInt) |
| `Temporal.Instant.compare(one, two)` | Compare two instants (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `epochMilliseconds` | Milliseconds since Unix epoch (Number) |
| `epochNanoseconds` | Nanoseconds since Unix epoch (BigInt) |

| Method | Description |
|--------|-------------|
| `add(duration)` / `subtract(duration)` | Time arithmetic (no calendar units) |
| `until(other [, options])` / `since(other [, options])` | Difference as Duration. Options: `{ largestUnit, smallestUnit, roundingMode, roundingIncrement }`. `"hour"` through `"nanosecond"` only — calendar units not allowed (defaults: largest `"hour"`, smallest `"nanosecond"`). Rounding: mode `"trunc"`, increment 1. |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `toString([options])` / `toJSON()` | ISO string with UTC (e.g., `"2024-03-15T13:45:30Z"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

## Temporal.PlainYearMonth

Represents a year and month without a day, time, or timezone.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainYearMonth(year, month)` | Create from components |
| `Temporal.PlainYearMonth.from(item)` | Create from string (`"2024-03"`), PlainYearMonth, or object |
| `Temporal.PlainYearMonth.compare(one, two)` | Compare two year-months (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `year`, `month` | Date components |
| `monthCode` | `"M01"` through `"M12"` |
| `daysInMonth`, `daysInYear`, `monthsInYear` | Calendar info |
| `inLeapYear` | Boolean |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new year-month with overridden fields |
| `add(duration)` / `subtract(duration)` | Year-month arithmetic (years and months only) |
| `until(other [, options])` / `since(other [, options])` | Difference as Duration. Options: `{ largestUnit, smallestUnit, roundingMode, roundingIncrement }`. `"year"` (default largest) or `"month"` (default smallest). Supports rounding via mode (`"trunc"` default) and increment (default 1). |
| `equals(other)` | Equality check |
| `toPlainDate(item)` | Combine with a day (`{ day }`) to create a PlainDate |
| `toString()` / `toJSON()` | ISO string (e.g., `"2024-03"`) |
| `valueOf()` | Throws TypeError |

## Temporal.PlainMonthDay

Represents a month and day without a year, time, or timezone. Uses a reference year (1972) for validation.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.PlainMonthDay(month, day)` | Create from components |
| `Temporal.PlainMonthDay.from(item)` | Create from string (`"12-25"`), PlainMonthDay, or object with `{ monthCode, day }` |
| `Temporal.PlainMonthDay.compare(one, two)` | Compare two month-days (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `monthCode` | `"M01"` through `"M12"` |
| `day` | Day of month |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new month-day with overridden fields (accepts `monthCode` and `day`) |
| `equals(other)` | Equality check |
| `toPlainDate(item)` | Combine with a year (`{ year }`) to create a PlainDate |
| `toString()` / `toJSON()` | Month-day string (e.g., `"12-25"`) |
| `valueOf()` | Throws TypeError |

## Temporal.ZonedDateTime

Represents an absolute date and time in a specific timezone. Combines an instant (epoch-based) with a timezone identifier for wall-clock interpretation.

| Constructor / Static | Description |
|---------------------|-------------|
| `new Temporal.ZonedDateTime(epochNanoseconds, timeZone)` | Create from epoch nanoseconds (BigInt) and timezone ID |
| `Temporal.ZonedDateTime.from(item)` | Create from ISO string with timezone annotation (e.g., `"2024-03-15T13:45:30+05:30[Asia/Kolkata]"`), ZonedDateTime, or object |
| `Temporal.ZonedDateTime.compare(one, two)` | Compare two zoned date-times (-1, 0, 1) |

| Getter | Description |
|--------|-------------|
| `calendarId` | Always `"iso8601"` |
| `timeZoneId` | IANA timezone identifier (e.g., `"America/New_York"`) |
| `year`, `month`, `monthCode`, `day` | Date components (wall-clock, timezone-adjusted) |
| `dayOfWeek`, `dayOfYear`, `weekOfYear`, `yearOfWeek` | ISO week-date components |
| `daysInWeek`, `daysInMonth`, `daysInYear`, `monthsInYear` | Calendar info |
| `inLeapYear` | Boolean |
| `hoursInDay` | Hours in the wall-clock day (accounts for DST transitions) |
| `hour`, `minute`, `second` | Time components |
| `millisecond`, `microsecond`, `nanosecond` | Sub-second components |
| `offset` | UTC offset string (e.g., `"+05:30"`) |
| `offsetNanoseconds` | UTC offset in nanoseconds |
| `epochMilliseconds` | Milliseconds since Unix epoch (Number) |
| `epochNanoseconds` | Nanoseconds since Unix epoch (BigInt) |

| Method | Description |
|--------|-------------|
| `with(fields)` | Return new ZonedDateTime with overridden fields |
| `withPlainDate(date)` | Replace date component |
| `withPlainTime(time?)` | Replace time component |
| `withTimeZone(timeZone)` | Re-interpret the same instant in a different timezone |
| `add(duration)` / `subtract(duration)` | Date-time arithmetic |
| `until(other [, options])` / `since(other [, options])` | Difference as Duration. Options: `{ largestUnit, smallestUnit, roundingMode, roundingIncrement }`. Accepts any unit from `"year"` to `"nanosecond"` (defaults: largest `"hour"`, smallest `"nanosecond"`). Rounding via mode (`"trunc"` default) and increment (default 1). Calendar-aware for day-or-larger units, including DST-aware rounding across variable-length local days. |
| `round(options)` | Round to nearest unit. Accepts a string (smallestUnit) or options object `{ smallestUnit, roundingMode, roundingIncrement }`. |
| `equals(other)` | Equality check |
| `startOfDay()` | Return ZonedDateTime at the start of the wall-clock day |
| `toInstant()` | Extract the underlying Instant |
| `toPlainDate()` / `toPlainTime()` | Extract date or time component |
| `toPlainDateTime()` | Extract date-time without timezone |
| `toString([options])` / `toJSON()` | ISO string with offset and timezone annotation (e.g., `"2024-03-15T13:45:30+05:30[Asia/Kolkata]"`). `toString` accepts `{ fractionalSecondDigits }` (0-9 or `"auto"`). |
| `valueOf()` | Throws TypeError |

## Temporal.Now

Provides current time in various representations.

| Method | Description |
|--------|-------------|
| `Temporal.Now.timeZoneId()` | System timezone identifier (e.g., `"America/New_York"`) |
| `Temporal.Now.instant()` | Current time as Instant |
| `Temporal.Now.zonedDateTimeISO([timeZone])` | Current date-time as ZonedDateTime in the given timezone (defaults to system timezone) |
| `Temporal.Now.plainDateISO()` | Current date as PlainDate |
| `Temporal.Now.plainTimeISO()` | Current time as PlainTime |
| `Temporal.Now.plainDateTimeISO()` | Current date-time as PlainDateTime |

## Related documents

- [Built-in Objects](built-ins.md) — Full built-ins reference (all other types)
- [Language Features](language.md) — Language-level syntax and semantics

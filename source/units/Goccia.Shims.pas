unit Goccia.Shims;

{$I Goccia.inc}

{ Provides backwards-compatible JavaScript globals as GocciaScript shims.
  Modern APIs (e.g. Uint8Array.fromBase64/toBase64) are implemented natively;
  legacy APIs (e.g. atob/btoa) are shimmed on top of them here.

  Each shim's Source is a valid GocciaScript module with a single named export.
  LoadShimValue evaluates the module in a child scope and returns the exported
  value, which the engine binds into the global scope.

  To add a new shim, append a TGocciaShimDefinition entry to DEFAULT_SHIMS. }

interface

uses
  Classes,

  Goccia.Interpreter,
  Goccia.Values.Primitives;

type
  TGocciaShimDefinition = record
    Name: string;      // Exported name (must match the export in Source)
    FileName: string;  // Virtual filename for error reporting
    Source: string;     // GocciaScript module source with a single named export
  end;

  { Defers a single name-bound shim's lex/parse/evaluate until the global is
    first touched.  Holds the interpreter and shim definition; Materialize
    matches the of-object lazy factory the engine registers on globalThis.
    The engine owns the materializer for its lifetime. }
  TGocciaShimMaterializer = class
  private
    FInterpreter: TGocciaInterpreter;
    FShim: TGocciaShimDefinition;
  public
    constructor Create(const AInterpreter: TGocciaInterpreter;
      const AShim: TGocciaShimDefinition);
    function Materialize: TGocciaValue;
  end;

function DefaultShimCount: Integer;
function DefaultShim(const AIndex: Integer): TGocciaShimDefinition;

{ True for shims that only mutate Object.prototype and export no value, so they
  have no global name to bind lazily and must run eagerly at boot. }
function IsSideEffectShim(const AName: string): Boolean;

{ Populate AShims with the names of all default shims.
  Call before RegisterBuiltIns so Goccia.shims reflects the names. }
procedure RegisterDefaultShimNames(const AShims: TStringList);

{ Evaluate a shim module in a child scope and return its exported value.
  Uses the interpreter for evaluation — consistent with how
  InjectGlobalsFromModule already works in both engine modes. }
function LoadShimValue(const AInterpreter: TGocciaInterpreter;
  const AShim: TGocciaShimDefinition): TGocciaValue;

implementation

uses
  Goccia.AST.Node,
  Goccia.Evaluator,
  Goccia.Evaluator.Context,
  Goccia.Scope,
  Goccia.SourcePipeline;

const
  DEFAULT_SHIMS: array[0..13] of TGocciaShimDefinition = (
    ( // WHATWG HTML spec §8.3 — legacy btoa(data) via Uint8Array.toBase64
      Name: 'btoa';
      FileName: '<shim:btoa>';
      Source:
        'export const btoa = (...args: string[]): string => {'#10 +
        '  if (args.length === 0)'#10 +
        '    throw new TypeError('#10 +
        '      "Failed to execute ''btoa'': 1 argument required, but only 0 present."'#10 +
        '    );'#10 +
        '  const str: string = String(args[0]);'#10 +
        '  const codePoints: number[] = [...str].map((ch: string): number | undefined =>'#10 +
        '    ch.codePointAt(0)'#10 +
        '  ).filter((cp: number | undefined): boolean => cp !== undefined);'#10 +
        '  codePoints.forEach((cp: number): void => {'#10 +
        '    if (cp > 0xff)'#10 +
        '      throw new DOMException('#10 +
        '        "Failed to execute ''btoa'': The string to be encoded contains characters outside of the Latin1 range.",'#10 +
        '        "InvalidCharacterError"'#10 +
        '      );'#10 +
        '  });'#10 +
        '  return new Uint8Array(codePoints).toBase64();'#10 +
        '};'
    ),
    ( // WHATWG HTML spec §8.3 — legacy atob(data) via Uint8Array.fromBase64
      Name: 'atob';
      FileName: '<shim:atob>';
      Source:
        'export const atob = (...args: string[]): string => {'#10 +
        '  if (args.length === 0)'#10 +
        '    throw new TypeError('#10 +
        '      "Failed to execute ''atob'': 1 argument required, but only 0 present."'#10 +
        '    );'#10 +
        '  let str: string = String(args[0]);'#10 +
        '  str = str.replace(/[\t\n\f\r ]/g, "");'#10 +
        '  if (str.length > 0 && str.length % 4 === 0) {'#10 +
        '    if (str[str.length - 1] === "=") {'#10 +
        '      str = str.slice(0, -1);'#10 +
        '      if (str.length > 0 && str[str.length - 1] === "=")'#10 +
        '        str = str.slice(0, -1);'#10 +
        '    }'#10 +
        '  }'#10 +
        '  if (str.length % 4 === 1)'#10 +
        '    throw new DOMException('#10 +
        '      "Failed to execute ''atob'': The string to be decoded is not correctly encoded.",'#10 +
        '      "InvalidCharacterError"'#10 +
        '    );'#10 +
        '  [...str].forEach((ch: string): void => {'#10 +
        '    const c: number = ch.charCodeAt(0);'#10 +
        '    if (!(c >= 65 && c <= 90) && !(c >= 97 && c <= 122) &&'#10 +
        '        !(c >= 48 && c <= 57) && c !== 43 && c !== 47)'#10 +
        '      throw new DOMException('#10 +
        '        "Failed to execute ''atob'': The string to be decoded is not correctly encoded.",'#10 +
        '        "InvalidCharacterError"'#10 +
        '      );'#10 +
        '  });'#10 +
        '  const remainder: number = str.length % 4;'#10 +
        '  if (remainder === 2) str += "==";'#10 +
        '  else if (remainder === 3) str += "=";'#10 +
        '  if (str === "") return "";'#10 +
        '  return Uint8Array.fromBase64(str).reduce('#10 +
        '    (acc: string, b: number): string => acc + String.fromCharCode(b),'#10 +
        '    ""'#10 +
        '  );'#10 +
        '};'
    ),
    ( // ES2027 §19.2.5 — legacy parseInt via Number.parseInt
      Name: 'parseInt';
      FileName: '<shim:parseInt>';
      Source: 'export const parseInt = Number.parseInt;'
    ),
    ( // ES2027 §19.2.4 — legacy parseFloat via Number.parseFloat
      Name: 'parseFloat';
      FileName: '<shim:parseFloat>';
      Source: 'export const parseFloat = Number.parseFloat;'
    ),
    ( // ES2027 §19.2.2 — legacy isNaN (coerces argument to number)
      Name: 'isNaN';
      FileName: '<shim:isNaN>';
      Source:
        'export const isNaN = (x) =>' +
        ' Number.isNaN(Number(x));'
    ),
    ( // ES2027 §19.2.3 — legacy isFinite (coerces argument to number)
      Name: 'isFinite';
      FileName: '<shim:isFinite>';
      Source:
        'export const isFinite = (x) =>' +
        ' Number.isFinite(Number(x));'
    ),
    ( // Legacy Date constructor via Temporal
      Name: 'Date';
      FileName: '<shim:Date>';
      Source:
        'const dateTimeClipLimit: number = 8.64e15;'#10 +
        'const toDateInteger = (value: any): number => {'#10 +
        '  const numberValue: number = Number(value);'#10 +
        '  if (!Number.isFinite(numberValue)) return NaN;'#10 +
        '  return numberValue < 0 ? -Math.floor(-numberValue) : Math.floor(numberValue);'#10 +
        '};'#10 +
        'const legacyDateFullYear = (year: number): number =>'#10 +
        '  year >= 0 && year <= 99 ? (year >= 50 ? 1900 + year : 2000 + year) : year;'#10 +
        'const parseLegacyInteger = (text: any): number => {'#10 +
        '  const trimmed: string = String(text).trim();'#10 +
        '  if (trimmed === "") return NaN;'#10 +
        '  const value: number = Number(trimmed);'#10 +
        '  return Number.isFinite(value) && Math.trunc(value) === value ? value : NaN;'#10 +
        '};'#10 +
        'const legacyDateMonthNumber = (token: any): number => {'#10 +
        '  const lower: string = String(token).toLowerCase();'#10 +
        '  if (lower === "jan" || lower === "january") return 1;'#10 +
        '  if (lower === "feb" || lower === "february") return 2;'#10 +
        '  if (lower === "mar" || lower === "march") return 3;'#10 +
        '  if (lower === "apr" || lower === "april") return 4;'#10 +
        '  if (lower === "may") return 5;'#10 +
        '  if (lower === "jun" || lower === "june") return 6;'#10 +
        '  if (lower === "jul" || lower === "july") return 7;'#10 +
        '  if (lower === "aug" || lower === "august") return 8;'#10 +
        '  if (lower === "sep" || lower === "sept" || lower === "september") return 9;'#10 +
        '  if (lower === "oct" || lower === "october") return 10;'#10 +
        '  if (lower === "nov" || lower === "november") return 11;'#10 +
        '  if (lower === "dec" || lower === "december") return 12;'#10 +
        '  return NaN;'#10 +
        '};'#10 +
        'const makeDateEpochMilliseconds = (args: any[], timeZone: string): number =>'#10 +
        '  makeDateEpochMillisecondsRaw(args, timeZone, true);'#10 +
        'const legacyDateMonthStarts: any[] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];'#10 +
        'const legacyDateLeapMonthStarts: any[] = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335];'#10 +
        'const legacyDateIsLeapYear = (year: number): boolean =>'#10 +
        '  year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);'#10 +
        'const legacyDateDayFromYear = (year: number): number =>'#10 +
        '  365 * (year - 1970) + Math.floor((year - 1969) / 4) -'#10 +
        '  Math.floor((year - 1901) / 100) + Math.floor((year - 1601) / 400);'#10 +
        'const makeDateEpochMillisecondsRaw = (args: any[], timeZone: string, adjustTwoDigitYear: boolean): number => {'#10 +
        '  if (args.length === 0) return NaN;'#10 +
        '  const yearValue: number = toDateInteger(args[0]);'#10 +
        '  const monthValue: number = toDateInteger(args.length > 1 ? args[1] : 0);'#10 +
        '  const dayValue: number = toDateInteger(args.length > 2 ? args[2] : 1);'#10 +
        '  const hour: number = toDateInteger(args.length > 3 ? args[3] : 0);'#10 +
        '  const minute: number = toDateInteger(args.length > 4 ? args[4] : 0);'#10 +
        '  const second: number = toDateInteger(args.length > 5 ? args[5] : 0);'#10 +
        '  const millisecond: number = toDateInteger(args.length > 6 ? args[6] : 0);'#10 +
        '  if (!Number.isFinite(yearValue) || !Number.isFinite(monthValue) ||'#10 +
        '      !Number.isFinite(dayValue) || !Number.isFinite(hour) ||'#10 +
        '      !Number.isFinite(minute) || !Number.isFinite(second) ||'#10 +
        '      !Number.isFinite(millisecond)) return NaN;'#10 +
        '  const baseYear: number = adjustTwoDigitYear && yearValue >= 0 && yearValue <= 99 ?'#10 +
        '    1900 + yearValue : yearValue;'#10 +
        '  const monthOffset: number = Math.floor(monthValue / 12);'#10 +
        '  const year: number = baseYear + monthOffset;'#10 +
        '  const month: number = monthValue - monthOffset * 12;'#10 +
        '  const starts: any[] = legacyDateIsLeapYear(year) ? legacyDateLeapMonthStarts : legacyDateMonthStarts;'#10 +
        '  const dayNumber: number = legacyDateDayFromYear(year) + starts[month] + dayValue - 1;'#10 +
        '  const timeWithinDay: number = ((hour * 3600000 + minute * 60000) + second * 1000) + millisecond;'#10 +
        '  const value: number = dayNumber * 86400000 + timeWithinDay;'#10 +
        '  if (!Number.isFinite(value) || Math.abs(value) > dateTimeClipLimit) return NaN;'#10 +
        '  const clipped: number = Math.trunc(value);'#10 +
        '  if (timeZone === "UTC") return clipped;'#10 +
        '  const local = Temporal.Instant.fromEpochMilliseconds(clipped).toZonedDateTimeISO("UTC");'#10 +
        '  return Temporal.ZonedDateTime.from({'#10 +
        '    year: local.year, month: local.month, day: local.day,'#10 +
        '    hour: local.hour, minute: local.minute, second: local.second,'#10 +
        '    millisecond: local.millisecond, timeZone'#10 +
        '  }).epochMilliseconds;'#10 +
        '};'#10 +
        'const makeLegacyDateEpochMilliseconds = (year: number, month: number, day: number): number => {'#10 +
        '  if (month < 1 || month > 12 || day < 1 || day > 31) return NaN;'#10 +
        '  const fullYear: number = legacyDateFullYear(year);'#10 +
        '  const starts: any[] = legacyDateIsLeapYear(fullYear) ? legacyDateLeapMonthStarts : legacyDateMonthStarts;'#10 +
        '  const value: number = (legacyDateDayFromYear(fullYear) + starts[month - 1] + day - 1) * 86400000;'#10 +
        '  if (!Number.isFinite(value) || Math.abs(value) > dateTimeClipLimit) return NaN;'#10 +
        '  return value;'#10 +
        '};'#10 +
        'const parseLegacyDateString = (text: string): any => {'#10 +
        '  const slashIndex: number = text.indexOf("/");'#10 +
        '  if (slashIndex >= 0) {'#10 +
        '    const parts: any[] = text.split("/");'#10 +
        '    if (parts.length !== 3) return undefined;'#10 +
        '    const firstNumber: number = Number(parts[0]);'#10 +
        '    const secondNumber: number = Number(parts[1]);'#10 +
        '    const thirdNumber: number = Number(parts[2]);'#10 +
        '    const first: number = Math.trunc(firstNumber);'#10 +
        '    const second: number = Math.trunc(secondNumber);'#10 +
        '    const third: number = Math.trunc(thirdNumber);'#10 +
        '    if (first !== firstNumber || second !== secondNumber || third !== thirdNumber) return NaN;'#10 +
        '    if (first >= 1 && first <= 12)'#10 +
        '      return makeLegacyDateEpochMilliseconds(third, first, second);'#10 +
        '    if (first > 31)'#10 +
        '      return makeLegacyDateEpochMilliseconds(first, second, third);'#10 +
        '    return NaN;'#10 +
        '  }'#10 +
        '  const words: any[] = text.split(" ");'#10 +
        '  if (words.length !== 3) return undefined;'#10 +
        '  const firstMonth: number = legacyDateMonthNumber(words[0]);'#10 +
        '  const secondMonth: number = legacyDateMonthNumber(words[1]);'#10 +
        '  const thirdMonth: number = legacyDateMonthNumber(words[2]);'#10 +
        '  if (Number.isFinite(firstMonth)) {'#10 +
        '    const second: number = parseLegacyInteger(words[1]);'#10 +
        '    const third: number = parseLegacyInteger(words[2]);'#10 +
        '    if (second > 31)'#10 +
        '      return makeLegacyDateEpochMilliseconds(second, firstMonth, third);'#10 +
        '    return makeLegacyDateEpochMilliseconds(third, firstMonth, second);'#10 +
        '  }'#10 +
        '  if (Number.isFinite(secondMonth)) {'#10 +
        '    const first: number = parseLegacyInteger(words[0]);'#10 +
        '    const third: number = parseLegacyInteger(words[2]);'#10 +
        '    if (first > 31)'#10 +
        '      return makeLegacyDateEpochMilliseconds(first, secondMonth, third);'#10 +
        '    return makeLegacyDateEpochMilliseconds(third, secondMonth, first);'#10 +
        '  }'#10 +
        '  if (Number.isFinite(thirdMonth)) {'#10 +
        '    const first: number = parseLegacyInteger(words[0]);'#10 +
        '    const second: number = parseLegacyInteger(words[1]);'#10 +
        '    if (first > 31)'#10 +
        '      return makeLegacyDateEpochMilliseconds(first, thirdMonth, second);'#10 +
        '    return makeLegacyDateEpochMilliseconds(second, thirdMonth, first);'#10 +
        '  }'#10 +
        '  return undefined;'#10 +
        '};'#10 +
        'const isDecimalDigit = (ch: string): boolean => ch >= "0" && ch <= "9";'#10 +
        'const parseFixedDigits = (text: string, start: number, length: number): number => {'#10 +
        '  const token: string = text.slice(start, start + length);'#10 +
        '  if (token.length !== length) return NaN;'#10 +
        '  for (const ch of token)'#10 +
        '    if (!isDecimalDigit(ch)) return NaN;'#10 +
        '  const value: number = Number(token);'#10 +
        '  return Number.isFinite(value) && Math.trunc(value) === value ? value : NaN;'#10 +
        '};'#10 +
        'const parseExactDigits = (text: string, length: number): number =>'#10 +
        '  text.length === length ? parseFixedDigits(text, 0, length) : NaN;'#10 +
        'const parseYearToken = (text: string): number => {'#10 +
        '  if (text[0] === "-") return -parseLegacyInteger(text.slice(1));'#10 +
        '  if (text[0] === "+") return parseLegacyInteger(text.slice(1));'#10 +
        '  return parseLegacyInteger(text);'#10 +
        '};'#10 +
        'const isoDaysInMonth = (year: number, month: number): number => {'#10 +
        '  if (month < 1 || month > 12) return 0;'#10 +
        '  if (month === 2) return legacyDateIsLeapYear(year) ? 29 : 28;'#10 +
        '  if (month === 4 || month === 6 || month === 9 || month === 11) return 30;'#10 +
        '  return 31;'#10 +
        '};'#10 +
        'const isValidISODate = (year: number, month: number, day: number): boolean =>'#10 +
        '  month >= 1 && month <= 12 && day >= 1 && day <= isoDaysInMonth(year, month);'#10 +
        'const parseTimeToken = (text: string): any => {'#10 +
        '  const parts: any[] = text.split(":");'#10 +
        '  if (parts.length !== 2 && parts.length !== 3) return undefined;'#10 +
        '  const secondParts: any[] = parts.length === 3 ? String(parts[2]).split(".") : ["0"];'#10 +
        '  const hour: number = parseLegacyInteger(parts[0]);'#10 +
        '  const minute: number = parseLegacyInteger(parts[1]);'#10 +
        '  const second: number = parseLegacyInteger(secondParts[0]);'#10 +
        '  let millisecond: number = 0;'#10 +
        '  if (secondParts.length > 1) {'#10 +
        '    const fraction: string = (String(secondParts[1]) + "000").slice(0, 3);'#10 +
        '    millisecond = parseLegacyInteger(fraction);'#10 +
        '  }'#10 +
        '  if (!Number.isFinite(hour) || !Number.isFinite(minute) || !Number.isFinite(second) || !Number.isFinite(millisecond)) return undefined;'#10 +
        '  return [hour, minute, second, millisecond];'#10 +
        '};'#10 +
        'const parseISOTimeToken = (text: string): any => {'#10 +
        '  const parts: any[] = text.split(":");'#10 +
        '  if (parts.length !== 2 && parts.length !== 3) return undefined;'#10 +
        '  const hour: number = parseExactDigits(parts[0], 2);'#10 +
        '  const minute: number = parseExactDigits(parts[1], 2);'#10 +
        '  let second: number = 0;'#10 +
        '  let millisecond: number = 0;'#10 +
        '  if (parts.length === 3) {'#10 +
        '    const secondParts: any[] = String(parts[2]).split(".");'#10 +
        '    if (secondParts.length !== 1 && secondParts.length !== 2) return undefined;'#10 +
        '    second = parseExactDigits(secondParts[0], 2);'#10 +
        '    if (secondParts.length === 2) millisecond = parseExactDigits(secondParts[1], 3);'#10 +
        '  }'#10 +
        '  if (!Number.isFinite(hour) || !Number.isFinite(minute) || !Number.isFinite(second) || !Number.isFinite(millisecond)) return undefined;'#10 +
        '  if (hour < 0 || hour > 24 || minute < 0 || minute > 59 || second < 0 || second > 59) return undefined;'#10 +
        '  if (hour === 24 && (minute !== 0 || second !== 0 || millisecond !== 0)) return undefined;'#10 +
        '  return [hour, minute, second, millisecond];'#10 +
        '};'#10 +
        'const parseISOOffsetMilliseconds = (text: string): any => {'#10 +
        '  if (text === "") return 0;'#10 +
        '  if (text === "Z") return 0;'#10 +
        '  const sign: string = text[0];'#10 +
        '  if ((sign !== "+" && sign !== "-") || text.length !== 6 || text[3] !== ":") return undefined;'#10 +
        '  const hour: number = parseExactDigits(text.slice(1, 3), 2);'#10 +
        '  const minute: number = parseExactDigits(text.slice(4, 6), 2);'#10 +
        '  if (!Number.isFinite(hour) || !Number.isFinite(minute) || hour > 23 || minute > 59) return undefined;'#10 +
        '  const offset: number = (hour * 60 + minute) * 60000;'#10 +
        '  return sign === "+" ? offset : -offset;'#10 +
        '};'#10 +
        'const parseGMTOffsetMilliseconds = (token: string): number => {'#10 +
        '  if (token === "GMT" || token === "UTC") return 0;'#10 +
        '  if (token.slice(0, 3) !== "GMT") return NaN;'#10 +
        '  const sign: string = token[3];'#10 +
        '  const body: string = token.slice(4);'#10 +
        '  const parts: any[] = body.split(":");'#10 +
        '  const hour: number = parseLegacyInteger(parts[0]);'#10 +
        '  const minute: number = parts.length > 1 ? parseLegacyInteger(parts[1]) : 0;'#10 +
        '  if ((sign !== "+" && sign !== "-") || !Number.isFinite(hour) || !Number.isFinite(minute)) return NaN;'#10 +
        '  const offset: number = (hour * 60 + minute) * 60000;'#10 +
        '  return sign === "+" ? offset : -offset;'#10 +
        '};'#10 +
        'const parseDisplayDateString = (text: string): any => {'#10 +
        '  const parts: any[] = text.split(" ");'#10 +
        '  if (parts.length < 5) return undefined;'#10 +
        '  let year: number; let month: number; let day: number; let time: any; let offset: number;'#10 +
        '  if (String(parts[0]).indexOf(",") >= 0) {'#10 +
        '    day = parseLegacyInteger(parts[1]);'#10 +
        '    month = legacyDateMonthNumber(parts[2]);'#10 +
        '    year = parseYearToken(parts[3]);'#10 +
        '    time = parseTimeToken(parts[4]);'#10 +
        '    offset = 0;'#10 +
        '  } else {'#10 +
        '    month = legacyDateMonthNumber(parts[1]);'#10 +
        '    day = parseLegacyInteger(parts[2]);'#10 +
        '    year = parseYearToken(parts[3]);'#10 +
        '    time = parseTimeToken(parts[4]);'#10 +
        '    offset = parts.length > 5 ? parseGMTOffsetMilliseconds(parts[5]) : 0;'#10 +
        '  }'#10 +
        '  if (!Number.isFinite(year) || !Number.isFinite(month) || !Number.isFinite(day) || time === undefined || !Number.isFinite(offset)) return undefined;'#10 +
        '  return makeDateEpochMillisecondsRaw([year, month - 1, day, time[0], time[1], time[2], time[3]], "UTC", false) - offset;'#10 +
        '};'#10 +
        'const parseISODateString = (text: string): any => {'#10 +
        '  let position: number = 0;'#10 +
        '  let sign: number = 1;'#10 +
        '  let yearLength: number = 4;'#10 +
        '  if (text[position] === "+" || text[position] === "-") {'#10 +
        '    sign = text[position] === "-" ? -1 : 1;'#10 +
        '    position += 1;'#10 +
        '    yearLength = 6;'#10 +
        '  }'#10 +
        '  const yearPart: number = parseFixedDigits(text, position, yearLength);'#10 +
        '  if (!Number.isFinite(yearPart)) return undefined;'#10 +
        '  const year: number = sign * yearPart;'#10 +
        '  if (sign < 0 && yearPart === 0) return NaN;'#10 +
        '  position += yearLength;'#10 +
        '  if (position === text.length)'#10 +
        '    return makeDateEpochMillisecondsRaw([year, 0, 1, 0, 0, 0, 0], "UTC", false);'#10 +
        '  if (text[position] !== "-") return undefined;'#10 +
        '  const month: number = parseFixedDigits(text, position + 1, 2);'#10 +
        '  position += 3;'#10 +
        '  if (!Number.isFinite(month)) return NaN;'#10 +
        '  if (month < 1 || month > 12) return NaN;'#10 +
        '  if (position === text.length)'#10 +
        '    return makeDateEpochMillisecondsRaw([year, month - 1, 1, 0, 0, 0, 0], "UTC", false);'#10 +
        '  if (text[position] !== "-") return undefined;'#10 +
        '  const day: number = parseFixedDigits(text, position + 1, 2);'#10 +
        '  position += 3;'#10 +
        '  if (!Number.isFinite(day)) return NaN;'#10 +
        '  if (!isValidISODate(year, month, day)) return NaN;'#10 +
        '  if (position === text.length)'#10 +
        '    return makeDateEpochMillisecondsRaw([year, month - 1, day, 0, 0, 0, 0], "UTC", false);'#10 +
        '  if (text[position] !== "T") return undefined;'#10 +
        '  let timeText: string = text.slice(position + 1);'#10 +
        '  let offset: number = 0;'#10 +
        '  const zIndex: number = timeText.indexOf("Z");'#10 +
        '  const plusIndex: number = timeText.indexOf("+");'#10 +
        '  const minusIndex: number = timeText.indexOf("-");'#10 +
        '  let offsetIndex: number = -1;'#10 +
        '  if (zIndex >= 0) offsetIndex = zIndex;'#10 +
        '  else if (plusIndex >= 0) offsetIndex = plusIndex;'#10 +
        '  else if (minusIndex >= 0) offsetIndex = minusIndex;'#10 +
        '  const timePart: string = offsetIndex >= 0 ? timeText.slice(0, offsetIndex) : timeText;'#10 +
        '  const offsetPart: string = offsetIndex >= 0 ? timeText.slice(offsetIndex) : "";'#10 +
        '  const time: any = parseISOTimeToken(timePart);'#10 +
        '  if (time === undefined) return NaN;'#10 +
        '  if (offsetPart === "")'#10 +
        '    return makeDateEpochMillisecondsRaw([year, month - 1, day, time[0], time[1], time[2], time[3]], Temporal.Now.timeZoneId(), false);'#10 +
        '  const parsedOffset: any = parseISOOffsetMilliseconds(offsetPart);'#10 +
        '  if (parsedOffset === undefined) return NaN;'#10 +
        '  offset = parsedOffset;'#10 +
        '  return makeDateEpochMillisecondsRaw([year, month - 1, day, time[0], time[1], time[2], time[3]], "UTC", false) - offset;'#10 +
        '};'#10 +
        'const parseDateStringToEpoch = (str: any): number => {'#10 +
        '  const text: string = String(str).trim();'#10 +
        '  const legacy: any = parseLegacyDateString(text);'#10 +
        '  if (legacy !== undefined) return legacy;'#10 +
        '  const iso: any = parseISODateString(text);'#10 +
        '  if (iso !== undefined) return iso;'#10 +
        '  const display: any = parseDisplayDateString(text);'#10 +
        '  if (display !== undefined) return display;'#10 +
        '  try { return Temporal.Instant.from(text).epochMilliseconds; }'#10 +
        '  catch (e) {}'#10 +
        '  return NaN;'#10 +
        '};'#10 +
        'const __GocciaDateSlots = new WeakMap();'#10 +
        'const __GocciaDateIsObject = (value: any): boolean =>'#10 +
        '  (typeof value === "object" && value !== null) || typeof value === "function";'#10 +
        'const __GocciaDateHasSlot = (date: any): boolean =>'#10 +
        '  __GocciaDateSlots.get(date) !== undefined;'#10 +
        'const __GocciaDateOrdinaryToPrimitive = (object: any, hint: string): any => {'#10 +
        '  const first: string = hint === "string" ? "toString" : "valueOf";'#10 +
        '  const second: string = hint === "string" ? "valueOf" : "toString";'#10 +
        '  const tryMethod = (name: string): any => {'#10 +
        '    const method: any = object[name];'#10 +
        '    if (typeof method !== "function") return { found: false, value: undefined };'#10 +
        '    const value: any = method.call(object);'#10 +
        '    if (__GocciaDateIsObject(value)) return { found: false, value: undefined };'#10 +
        '    return { found: true, value };'#10 +
        '  };'#10 +
        '  const firstResult: any = tryMethod(first);'#10 +
        '  if (firstResult.found) return firstResult.value;'#10 +
        '  const secondResult: any = tryMethod(second);'#10 +
        '  if (secondResult.found) return secondResult.value;'#10 +
        '  throw new TypeError("Cannot convert object to primitive value");'#10 +
        '};'#10 +
        'const __GocciaDateToPrimitive = (value: any): any => {'#10 +
        '  if (!__GocciaDateIsObject(value)) return value;'#10 +
        '  const exotic: any = value[Symbol.toPrimitive];'#10 +
        '  if (exotic !== undefined && exotic !== null) {'#10 +
        '    if (typeof exotic !== "function") throw new TypeError("@@toPrimitive must be a function");'#10 +
        '    const result: any = exotic.call(value, "default");'#10 +
        '    if (__GocciaDateIsObject(result)) throw new TypeError("@@toPrimitive must return a primitive value");'#10 +
        '    return result;'#10 +
        '  }'#10 +
        '  return __GocciaDateOrdinaryToPrimitive(value, "number");'#10 +
        '};'#10 +
        'const __GocciaDateTimeClip = (epochMilliseconds: any): number => {'#10 +
        '  const t: number = Math.trunc(Number(epochMilliseconds));'#10 +
        '  if (!Number.isFinite(t) || Math.abs(t) > dateTimeClipLimit) return NaN;'#10 +
        '  return t === 0 ? 0 : t;'#10 +
        '};'#10 +
        'const __GocciaDatePad = (value: number, length: number): string => {'#10 +
        '  return String(Math.abs(Math.trunc(value))).padStart(length, "0");'#10 +
        '};'#10 +
        'const __GocciaDateYearString = (year: number): string =>'#10 +
        '  year < 0 ? "-" + __GocciaDatePad(year, 4) : __GocciaDatePad(year, 4);'#10 +
        'const __GocciaDateISOYearString = (year: number): string =>'#10 +
        '  year >= 0 && year <= 9999 ? __GocciaDatePad(year, 4) :'#10 +
        '    (year < 0 ? "-" : "+") + __GocciaDatePad(year, 6);'#10 +
        'const __GocciaDateOffsetString = (offset: string): string => offset.replace(":", "");'#10 +
        'const __GocciaDateFromSingleArgument = (value: any): number => {'#10 +
        '  if (typeof value === "number") return __GocciaDateTimeClip(value);'#10 +
        '  if (__GocciaDateHasSlot(value)) return __GocciaDateGetSlot(value);'#10 +
        '  const primitive: any = __GocciaDateToPrimitive(value);'#10 +
        '  return typeof primitive === "string" ? parseDateStringToEpoch(primitive) : __GocciaDateTimeClip(primitive);'#10 +
        '};'#10 +
        'const __GocciaDateDefaultPrototypeForNewTarget = (newTarget: any): any => {'#10 +
        '  try {'#10 +
        '    const FunctionCtor: any = newTarget.constructor;'#10 +
        '    if (typeof FunctionCtor === "function") {'#10 +
        '      const proto: any = FunctionCtor("return Date.prototype")();'#10 +
        '      if (__GocciaDateIsObject(proto)) return proto;'#10 +
        '    }'#10 +
        '  } catch (e) {}'#10 +
        '  return __GocciaDateClass.prototype;'#10 +
        '};'#10 +
        '// A Date slot only ever stores a number (never undefined), so a missing'#10 +
        '// entry reads back as undefined and doubles as the brand check. This keeps'#10 +
        '// the hot accessors to a single WeakMap lookup instead of has()+get().'#10 +
        'const __GocciaDateRequire = (date: any): void => {'#10 +
        '  if (__GocciaDateSlots.get(date) === undefined) throw new TypeError("Date object expected");'#10 +
        '};'#10 +
        'const __GocciaDateGetSlot = (date: any): number => {'#10 +
        '  const value: any = __GocciaDateSlots.get(date);'#10 +
        '  if (value === undefined) throw new TypeError("Date object expected");'#10 +
        '  return value;'#10 +
        '};'#10 +
        'const __GocciaDateSetSlot = (date: any, value: number): number => {'#10 +
        '  __GocciaDateSlots.set(date, value);'#10 +
        '  return value;'#10 +
        '};'#10 +
        'const __GocciaDateClass = class Date {'#10 +
        '  static {'#10 +
        '    Object.defineProperty(this.prototype, Symbol.toStringTag, {'#10 +
        '      get(): any { return __GocciaDateSlots.get(this) === undefined ? undefined : "Date"; },'#10 +
        '      set(value: any): void {'#10 +
        '        Object.defineProperty(this, Symbol.toStringTag, {'#10 +
        '          value,'#10 +
        '          writable: true,'#10 +
        '          configurable: true'#10 +
        '        });'#10 +
        '      },'#10 +
        '      configurable: true'#10 +
        '    });'#10 +
        '  }'#10 +
        '  static now(): number { return Temporal.Now.instant().epochMilliseconds; }'#10 +
        '  static parse(str: string): number { return parseDateStringToEpoch(str); }'#10 +
        '  static UTC(...args: any[]): number {'#10 +
        '    return makeDateEpochMilliseconds(args, "UTC");'#10 +
        '  }'#10 +
        '  constructor(...args: any[]) {'#10 +
        '    let ms: number;'#10 +
        '    if (args.length === 0) {'#10 +
        '      ms = Temporal.Now.instant().epochMilliseconds;'#10 +
        '    } else if (args.length === 1) {'#10 +
        '      ms = __GocciaDateFromSingleArgument(args[0]);'#10 +
        '    } else {'#10 +
        '      ms = makeDateEpochMilliseconds(args, Temporal.Now.timeZoneId());'#10 +
        '    }'#10 +
        '    __GocciaDateSetSlot(this, ms);'#10 +
        '  }'#10 +
        '  static #require(date: any): void { __GocciaDateRequire(date); }'#10 +
        '  static #get(date: any): number { return __GocciaDateGetSlot(date); }'#10 +
        '  static #set(date: any, value: number): number { return __GocciaDateSetSlot(date, value); }'#10 +
        '  static #valid(date: any): boolean { return Number.isFinite(Date.#get(date)); }'#10 +
        '  static #local(date: any): any { const ms: number = Date.#get(date); return Number.isFinite(ms) ? new Temporal.ZonedDateTime(BigInt(ms) * 1000000n, Temporal.Now.timeZoneId()) : null; }'#10 +
        '  static #utc(date: any): any { const ms: number = Date.#get(date); return Number.isFinite(ms) ? new Temporal.ZonedDateTime(BigInt(ms) * 1000000n, "UTC") : null; }'#10 +
        '  getTime(): number { return __GocciaDateGetSlot(this); }'#10 +
        '  valueOf(): number { return __GocciaDateGetSlot(this); }'#10 +
        '  [Symbol.toPrimitive](hint: string): any {'#10 +
        '    if (!__GocciaDateIsObject(this)) throw new TypeError("Date.prototype[Symbol.toPrimitive] called on non-object");'#10 +
        '    if (hint === "string" || hint === "default") return __GocciaDateOrdinaryToPrimitive(this, "string");'#10 +
        '    if (hint === "number") return __GocciaDateOrdinaryToPrimitive(this, "number");'#10 +
        '    throw new TypeError("Date.prototype[Symbol.toPrimitive] invalid hint");'#10 +
        '  }'#10 +
        '  getFullYear(): number { const z = Date.#local(this); return z ? z.year : NaN; }'#10 +
        '  getMonth(): number { const z = Date.#local(this); return z ? z.month - 1 : NaN; }'#10 +
        '  getDate(): number { const z = Date.#local(this); return z ? z.day : NaN; }'#10 +
        '  getDay(): number { const z = Date.#local(this); return z ? z.dayOfWeek % 7 : NaN; }'#10 +
        '  getHours(): number { const z = Date.#local(this); return z ? z.hour : NaN; }'#10 +
        '  getMinutes(): number { const z = Date.#local(this); return z ? z.minute : NaN; }'#10 +
        '  getSeconds(): number { const z = Date.#local(this); return z ? z.second : NaN; }'#10 +
        '  getMilliseconds(): number { const z = Date.#local(this); return z ? z.millisecond : NaN; }'#10 +
        '  getUTCFullYear(): number { const z = Date.#utc(this); return z ? z.year : NaN; }'#10 +
        '  getUTCMonth(): number { const z = Date.#utc(this); return z ? z.month - 1 : NaN; }'#10 +
        '  getUTCDate(): number { const z = Date.#utc(this); return z ? z.day : NaN; }'#10 +
        '  getUTCDay(): number { const z = Date.#utc(this); return z ? z.dayOfWeek % 7 : NaN; }'#10 +
        '  getUTCHours(): number { const z = Date.#utc(this); return z ? z.hour : NaN; }'#10 +
        '  getUTCMinutes(): number { const z = Date.#utc(this); return z ? z.minute : NaN; }'#10 +
        '  getUTCSeconds(): number { const z = Date.#utc(this); return z ? z.second : NaN; }'#10 +
        '  getUTCMilliseconds(): number { const z = Date.#utc(this); return z ? z.millisecond : NaN; }'#10 +
        '  static #clip(epochMilliseconds: number): number {'#10 +
        '    return __GocciaDateTimeClip(epochMilliseconds);'#10 +
        '  }'#10 +
        '  static #epochFromParts(year: number, month: number, day: number, hour: number, minute: number, second: number, millisecond: number, timeZone: string): number {'#10 +
        '    if (!Number.isFinite(year) || !Number.isFinite(month) || !Number.isFinite(day) ||'#10 +
        '        !Number.isFinite(hour) || !Number.isFinite(minute) || !Number.isFinite(second) ||'#10 +
        '        !Number.isFinite(millisecond)) return NaN;'#10 +
        '    return makeDateEpochMillisecondsRaw([year, month, day, hour, minute, second, millisecond], timeZone, false);'#10 +
        '  }'#10 +
        '  static #setFromParts(date: any, timeZone: string, z: any, year: number, month: number, day: number, hour: number, minute: number, second: number, millisecond: number): number {'#10 +
        '    Date.#require(date);'#10 +
        '    if (!z) return NaN;'#10 +
        '    return Date.#set(date, Date.#epochFromParts(year, month, day, hour, minute, second, millisecond, timeZone));'#10 +
        '  }'#10 +
        '  setTime(time: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    return Date.#set(this, Date.#clip(toDateInteger(time)));'#10 +
        '  }'#10 +
        '  setMilliseconds(ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const n = toDateInteger(ms);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, z ? z.hour : NaN, z ? z.minute : NaN, z ? z.second : NaN, n);'#10 +
        '  }'#10 +
        '  setUTCMilliseconds(ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const n = toDateInteger(ms);'#10 +
        '    return Date.#setFromParts(this, "UTC", z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, z ? z.hour : NaN, z ? z.minute : NaN, z ? z.second : NaN, n);'#10 +
        '  }'#10 +
        '  setSeconds(sec: number, ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const s = toDateInteger(sec); const milli = arguments.length > 1 ? toDateInteger(ms) : (z ? z.millisecond : NaN);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, z ? z.hour : NaN, z ? z.minute : NaN, s, milli);'#10 +
        '  }'#10 +
        '  setUTCSeconds(sec: number, ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const s = toDateInteger(sec); const milli = arguments.length > 1 ? toDateInteger(ms) : (z ? z.millisecond : NaN);'#10 +
        '    return Date.#setFromParts(this, "UTC", z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, z ? z.hour : NaN, z ? z.minute : NaN, s, milli);'#10 +
        '  }'#10 +
        '  setMinutes(min: number, sec: number, ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const m = toDateInteger(min); const s = arguments.length > 1 ? toDateInteger(sec) : (z ? z.second : NaN); const milli = arguments.length > 2 ? toDateInteger(ms) : (z ? z.millisecond : NaN);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, z ? z.hour : NaN, m, s, milli);'#10 +
        '  }'#10 +
        '  setUTCMinutes(min: number, sec: number, ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const m = toDateInteger(min); const s = arguments.length > 1 ? toDateInteger(sec) : (z ? z.second : NaN); const milli = arguments.length > 2 ? toDateInteger(ms) : (z ? z.millisecond : NaN);'#10 +
        '    return Date.#setFromParts(this, "UTC", z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, z ? z.hour : NaN, m, s, milli);'#10 +
        '  }'#10 +
        '  setHours(hour: number, min: number, sec: number, ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const h = toDateInteger(hour); const m = arguments.length > 1 ? toDateInteger(min) : (z ? z.minute : NaN); const s = arguments.length > 2 ? toDateInteger(sec) : (z ? z.second : NaN); const milli = arguments.length > 3 ? toDateInteger(ms) : (z ? z.millisecond : NaN);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, h, m, s, milli);'#10 +
        '  }'#10 +
        '  setUTCHours(hour: number, min: number, sec: number, ms: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const h = toDateInteger(hour); const m = arguments.length > 1 ? toDateInteger(min) : (z ? z.minute : NaN); const s = arguments.length > 2 ? toDateInteger(sec) : (z ? z.second : NaN); const milli = arguments.length > 3 ? toDateInteger(ms) : (z ? z.millisecond : NaN);'#10 +
        '    return Date.#setFromParts(this, "UTC", z, z ? z.year : NaN, z ? z.month - 1 : NaN, z ? z.day : NaN, h, m, s, milli);'#10 +
        '  }'#10 +
        '  setDate(day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const d = toDateInteger(day);'#10 +
        '    if (!z) return NaN;'#10 +
        '    if (!Number.isFinite(d)) return Date.#set(this, NaN);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z, z.year, z.month - 1, d, z.hour, z.minute, z.second, z.millisecond);'#10 +
        '  }'#10 +
        '  setUTCDate(day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const d = toDateInteger(day);'#10 +
        '    if (!z) return NaN;'#10 +
        '    if (!Number.isFinite(d)) return Date.#set(this, NaN);'#10 +
        '    return Date.#setFromParts(this, "UTC", z, z.year, z.month - 1, d, z.hour, z.minute, z.second, z.millisecond);'#10 +
        '  }'#10 +
        '  setMonth(month: number, day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const m = toDateInteger(month); const d = arguments.length > 1 ? toDateInteger(day) : (z ? z.day : NaN);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z, z ? z.year : NaN, m, d, z ? z.hour : NaN, z ? z.minute : NaN, z ? z.second : NaN, z ? z.millisecond : NaN);'#10 +
        '  }'#10 +
        '  setUTCMonth(month: number, day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const m = toDateInteger(month); const d = arguments.length > 1 ? toDateInteger(day) : (z ? z.day : NaN);'#10 +
        '    return Date.#setFromParts(this, "UTC", z, z ? z.year : NaN, m, d, z ? z.hour : NaN, z ? z.minute : NaN, z ? z.second : NaN, z ? z.millisecond : NaN);'#10 +
        '  }'#10 +
        '  setFullYear(year: number, month: number, day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this); const y = toDateInteger(year); const m = arguments.length > 1 ? toDateInteger(month) : (z ? z.month - 1 : 0); const d = arguments.length > 2 ? toDateInteger(day) : (z ? z.day : 1);'#10 +
        '    return Date.#setFromParts(this, Temporal.Now.timeZoneId(), z || true, y, m, d, z ? z.hour : 0, z ? z.minute : 0, z ? z.second : 0, z ? z.millisecond : 0);'#10 +
        '  }'#10 +
        '  setUTCFullYear(year: number, month: number, day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const y = toDateInteger(year); const m = arguments.length > 1 ? toDateInteger(month) : (z ? z.month - 1 : 0); const d = arguments.length > 2 ? toDateInteger(day) : (z ? z.day : 1);'#10 +
        '    return Date.#setFromParts(this, "UTC", z || true, y, m, d, z ? z.hour : 0, z ? z.minute : 0, z ? z.second : 0, z ? z.millisecond : 0);'#10 +
        '  }'#10 +
        '  getTimezoneOffset(): number { const z = Date.#local(this); if (!z) return NaN; const minutes: number = z.offsetNanoseconds / 60000000000; return minutes === 0 ? 0 : -minutes; }'#10 +
        '  toISOString(): string {'#10 +
        '    const z = Date.#utc(this);'#10 +
        '    if (!z) throw new RangeError("Invalid time value");'#10 +
        '    return __GocciaDateISOYearString(z.year) + "-" + __GocciaDatePad(z.month, 2) + "-" + __GocciaDatePad(z.day, 2) + "T" +'#10 +
        '      __GocciaDatePad(z.hour, 2) + ":" + __GocciaDatePad(z.minute, 2) + ":" + __GocciaDatePad(z.second, 2) + "." +'#10 +
        '      __GocciaDatePad(z.millisecond, 3) + "Z";'#10 +
        '  }'#10 +
        '  toJSON(key: any): string | null {'#10 +
        '    if (this === null || this === undefined) throw new TypeError("Date.prototype.toJSON called on null or undefined");'#10 +
        '    const object: any = Object(this);'#10 +
        '    const isObject = (value: any): boolean => (typeof value === "object" && value !== null) || typeof value === "function";'#10 +
        '    let primitive: any;'#10 +
        '    let hasPrimitive: boolean = false;'#10 +
        '    const exotic: any = object[Symbol.toPrimitive];'#10 +
        '    if (exotic !== undefined && exotic !== null) {'#10 +
        '      if (typeof exotic !== "function") throw new TypeError("Date.prototype.toJSON @@toPrimitive is not a function");'#10 +
        '      primitive = exotic.call(object, "number");'#10 +
        '      hasPrimitive = true;'#10 +
        '      if (isObject(primitive)) throw new TypeError("Date.prototype.toJSON @@toPrimitive returned an object");'#10 +
        '    } else {'#10 +
        '      const valueOf: any = object.valueOf;'#10 +
        '      if (typeof valueOf === "function") { primitive = valueOf.call(object); hasPrimitive = true; }'#10 +
        '      if (!hasPrimitive || isObject(primitive)) {'#10 +
        '        const toString: any = object.toString;'#10 +
        '        if (typeof toString === "function") { primitive = toString.call(object); hasPrimitive = true; }'#10 +
        '      }'#10 +
        '      if (!hasPrimitive || isObject(primitive)) throw new TypeError("Date.prototype.toJSON cannot convert receiver to primitive");'#10 +
        '    }'#10 +
        '    if (typeof primitive === "number" && !Number.isFinite(primitive)) return null;'#10 +
        '    const toISO: any = object.toISOString;'#10 +
        '    if (typeof toISO !== "function") throw new TypeError("Date.prototype.toJSON toISOString is not a function");'#10 +
        '    return toISO.call(object);'#10 +
        '  }'#10 +
        '  toString(): string {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this);'#10 +
        '    if (!z) return "Invalid Date";'#10 +
        '    const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];'#10 +
        '    const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];'#10 +
        '    return days[z.dayOfWeek % 7] + " " + months[z.month - 1] + " " + __GocciaDatePad(z.day, 2) + " " +'#10 +
        '      __GocciaDateYearString(z.year) + " " + __GocciaDatePad(z.hour, 2) + ":" + __GocciaDatePad(z.minute, 2) + ":" + __GocciaDatePad(z.second, 2) + " GMT" + __GocciaDateOffsetString(z.offset);'#10 +
        '  }'#10 +
        '  toDateString(): string {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this);'#10 +
        '    if (!z) return "Invalid Date";'#10 +
        '    const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];'#10 +
        '    const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];'#10 +
        '    return days[z.dayOfWeek % 7] + " " + months[z.month - 1] + " " + __GocciaDatePad(z.day, 2) + " " + __GocciaDateYearString(z.year);'#10 +
        '  }'#10 +
        '  toTimeString(): string {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#local(this);'#10 +
        '    if (!z) return "Invalid Date";'#10 +
        '    return __GocciaDatePad(z.hour, 2) + ":" + __GocciaDatePad(z.minute, 2) + ":" + __GocciaDatePad(z.second, 2) + " GMT" + __GocciaDateOffsetString(z.offset);'#10 +
        '  }'#10 +
        '  toUTCString(): string {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this);'#10 +
        '    if (!z) return "Invalid Date";'#10 +
        '    const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];'#10 +
        '    const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];'#10 +
        '    return days[z.dayOfWeek % 7] + ", " + __GocciaDatePad(z.day, 2) + " " + months[z.month - 1] + " " +'#10 +
        '      __GocciaDateYearString(z.year) + " " + __GocciaDatePad(z.hour, 2) + ":" + __GocciaDatePad(z.minute, 2) + ":" + __GocciaDatePad(z.second, 2) + " GMT";'#10 +
        '  }'#10 +
        '  toGMTString(): string { return this.toUTCString(); }'#10 +
        '  static #localeOptions(options: any, needDate: boolean, needTime: boolean): any {'#10 +
        '    const out: any = Object.create(null);'#10 +
        '    if (options !== undefined) {'#10 +
        '      ["localeMatcher", "calendar", "numberingSystem", "timeZone", "hour12", "hourCycle", "formatMatcher",'#10 +
        '       "weekday", "era", "year", "month", "day", "dayPeriod", "hour", "minute", "second",'#10 +
        '       "fractionalSecondDigits", "timeZoneName", "dateStyle", "timeStyle"].forEach((key: string): void => {'#10 +
        '        const value: any = options[key];'#10 +
        '        if (value !== undefined) out[key] = value;'#10 +
        '      });'#10 +
        '    }'#10 +
        '    const hasDate = out.weekday !== undefined || out.year !== undefined || out.month !== undefined || out.day !== undefined || out.dateStyle !== undefined;'#10 +
        '    const hasTime = out.dayPeriod !== undefined || out.hour !== undefined || out.minute !== undefined || out.second !== undefined || out.fractionalSecondDigits !== undefined || out.timeStyle !== undefined;'#10 +
        '    const needDefaults = needDate && needTime ? !hasDate && !hasTime : needDate ? !hasDate : needTime ? !hasTime : false;'#10 +
        '    if (needDefaults && needDate) { out.year = "numeric"; out.month = "numeric"; out.day = "numeric"; }'#10 +
        '    if (needDefaults && needTime) { out.hour = "numeric"; out.minute = "numeric"; out.second = "numeric"; }'#10 +
        '    return out;'#10 +
        '  }'#10 +
        '  toLocaleString(...args: any[]): string {'#10 +
        '    Date.#require(this);'#10 +
        '    if (!Date.#valid(this)) return "Invalid Date";'#10 +
        '    void Intl;'#10 +
        '    return new __GocciaIntlDateTimeFormat(args[0], Date.#localeOptions(args[1], true, true)).format(Date.#get(this));'#10 +
        '  }'#10 +
        '  toLocaleDateString(...args: any[]): string {'#10 +
        '    Date.#require(this);'#10 +
        '    if (!Date.#valid(this)) return "Invalid Date";'#10 +
        '    void Intl;'#10 +
        '    return new __GocciaIntlDateTimeFormat(args[0], Date.#localeOptions(args[1], true, false)).format(Date.#get(this));'#10 +
        '  }'#10 +
        '  toLocaleTimeString(...args: any[]): string {'#10 +
        '    Date.#require(this);'#10 +
        '    if (!Date.#valid(this)) return "Invalid Date";'#10 +
        '    void Intl;'#10 +
        '    return new __GocciaIntlDateTimeFormat(args[0], Date.#localeOptions(args[1], false, true)).format(Date.#get(this));'#10 +
        '  }'#10 +
        '  toTemporalInstant(): any {'#10 +
        '    const value: number = __GocciaDateGetSlot(this);'#10 +
        '    if (!Number.isFinite(value)) throw new RangeError("Invalid time value");'#10 +
        '    return Temporal.Instant.fromEpochMilliseconds(value);'#10 +
        '  }'#10 +
        '};'#10 +
        'const __GocciaDateConstructor = function(...args: any[]): any {'#10 +
        '  if (new.target === undefined) return new __GocciaDateConstructor().toString();'#10 +
        '  if (new.target !== __GocciaDateConstructor && !__GocciaDateIsObject(new.target.prototype))'#10 +
        '    Object.setPrototypeOf(this, __GocciaDateDefaultPrototypeForNewTarget(new.target));'#10 +
        '  let ms: number;'#10 +
        '  if (args.length === 0) {'#10 +
        '    ms = Temporal.Now.instant().epochMilliseconds;'#10 +
        '  } else if (args.length === 1) {'#10 +
        '    ms = __GocciaDateFromSingleArgument(args[0]);'#10 +
        '  } else {'#10 +
        '    ms = makeDateEpochMilliseconds(args, Temporal.Now.timeZoneId());'#10 +
        '  }'#10 +
        '  __GocciaDateSetSlot(this, ms);'#10 +
        '};'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "name", { value: "Date", configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "length", { value: 7, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateClass.UTC, "length", { value: 7, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateClass.prototype, Symbol.toPrimitive, { writable: false, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "now", { value: __GocciaDateClass.now, writable: true, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "parse", { value: __GocciaDateClass.parse, writable: true, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "UTC", { value: __GocciaDateClass.UTC, writable: true, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "prototype", { value: __GocciaDateClass.prototype, writable: false, configurable: false });'#10 +
        'Object.defineProperty(__GocciaDateClass.prototype, "constructor", { value: __GocciaDateConstructor, writable: true, configurable: true });'#10 +
        'export const Date = __GocciaDateConstructor;'
    ),
    ( // toLocaleString for Number/Array via Intl. Installed eagerly: these
      // prototype mutations were previously a side effect of the (now lazy)
      // Date shim's static block, so they must not depend on Date being
      // materialized. Intl is referenced only when a method is called, so this
      // does not force Intl materialization at boot.
      Name: 'numberArrayToLocaleString';
      FileName: '<shim:numberArrayToLocaleString>';
      Source:
        'export const numberArrayToLocaleString = ((numberProto: any, arrayProto: any): any => {'#10 +
        '  const numberLocale = {'#10 +
        '    toLocaleString(...args: any[]): string {'#10 +
        '      const value: number = Number.prototype.valueOf.call(this);'#10 +
        '      void Intl;'#10 +
        '      return new __GocciaIntlNumberFormat(args[0], args[1]).format(value);'#10 +
        '    }'#10 +
        '  }.toLocaleString;'#10 +
        '  const arrayLocale = {'#10 +
        '    toLocaleString(...args: any[]): string {'#10 +
        '      if (this === null || this === undefined) throw new TypeError("Array.prototype.toLocaleString called on null or undefined");'#10 +
        '      const array: any = Object(this);'#10 +
        '      const length: number = Number(array.length);'#10 +
        '      const len: number = !Number.isFinite(length) || length <= 0 ? 0 : Math.trunc(length);'#10 +
        '      const elementString = (index: number): string => {'#10 +
        '        const nextElement: any = array[index];'#10 +
        '        if (nextElement !== undefined && nextElement !== null) {'#10 +
        '          const method: any = nextElement.toLocaleString;'#10 +
        '          if (typeof method !== "function")'#10 +
        '            throw new TypeError("Array.prototype.toLocaleString element toLocaleString is not a function");'#10 +
        '          return String(method.call(nextElement, args[0], args[1]));'#10 +
        '        }'#10 +
        '        return "";'#10 +
        '      };'#10 +
        '      const build = (index: number, result: string): string => index >= len ? result : build(index + 1, result + (index > 0 ? "," : "") + elementString(index));'#10 +
        '      return build(0, "");'#10 +
        '    }'#10 +
        '  }.toLocaleString;'#10 +
        '  Object.defineProperty(numberProto, "toLocaleString", { value: numberLocale, writable: true, configurable: true });'#10 +
        '  Object.defineProperty(arrayProto, "toLocaleString", { value: arrayLocale, writable: true, configurable: true });'#10 +
        '  return numberLocale;'#10 +
        '})(Number.prototype, Array.prototype);'
    ),
    ( // ES2026 §20.1.3.2 — legacy hasOwnProperty via Object.hasOwn
      Name: 'hasOwnProperty';
      FileName: '<shim:hasOwnProperty>';
      Source:
        'export const hasOwnProperty = ((proto) => {'#10 +
        '  if (!proto.hasOwnProperty) {'#10 +
        '    Object.defineProperty(proto, "hasOwnProperty", {'#10 +
        '      value(prop) { return Object.hasOwn(this, prop); },'#10 +
        '      writable: true, enumerable: false, configurable: true'#10 +
        '    });'#10 +
        '  }'#10 +
        '  return proto.hasOwnProperty;'#10 +
        '})(Object.getPrototypeOf({}));'
    ),
    ( // ES2026 §20.1.3.8 Object.prototype.__proto__
      Name: '__proto__';
      FileName: '<shim:__proto__>';
      Source:
        'export const __proto__ = ((objectPrototype) => {'#10 +
        '  Object.defineProperty(objectPrototype, "__proto__", {'#10 +
        '    get() { return Object.getPrototypeOf(this); },'#10 +
        '    set(value) {'#10 +
        '      if (this === null || this === undefined) throw new TypeError("Object.prototype.__proto__ called on null or undefined");'#10 +
        '      if (value !== null && typeof value !== "object" && typeof value !== "function") return undefined;'#10 +
        '      if (typeof this !== "object" && typeof this !== "function") return undefined;'#10 +
        '      Object.setPrototypeOf(this, value);'#10 +
        '      return undefined;'#10 +
        '    },'#10 +
        '    enumerable: false, configurable: true'#10 +
        '  });'#10 +
        '  const descriptor = Object.getOwnPropertyDescriptor(objectPrototype, "__proto__");'#10 +
        '  Object.defineProperty(descriptor.get, "name", { value: "get __proto__", configurable: true });'#10 +
        '  Object.defineProperty(descriptor.set, "name", { value: "set __proto__", configurable: true });'#10 +
        '  return descriptor;'#10 +
        '})(Object.getPrototypeOf({}));'
    ),
    ( // ES2026 §20.1.3.9.1 Object.prototype.__defineGetter__(P, getter)
      Name: 'defineGetter';
      FileName: '<shim:defineGetter>';
      Source:
        'export const defineGetter = ((proto) => {'#10 +
        '  Object.defineProperty(proto, "__defineGetter__", {'#10 +
        '    value(prop, getter) {'#10 +
        '      if (this === null || this === undefined) throw new TypeError("__defineGetter__ called on null or undefined");'#10 +
        '      if (typeof getter !== "function") throw new TypeError("Object.prototype.__defineGetter__ getter must be callable");'#10 +
        '      Object.defineProperty(Object(this), prop, { get: getter, enumerable: true, configurable: true });'#10 +
        '      return undefined;'#10 +
        '    },'#10 +
        '    writable: true, enumerable: false, configurable: true'#10 +
        '  });'#10 +
        '  Object.defineProperty(proto.__defineGetter__, "name", { value: "__defineGetter__", configurable: true });'#10 +
        '  Object.defineProperty(proto.__defineGetter__, "length", { value: 2, configurable: true });'#10 +
        '  return proto.__defineGetter__;'#10 +
        '})(Object.getPrototypeOf({}));'
    ),
    ( // ES2026 §20.1.3.9.2 Object.prototype.__defineSetter__(P, setter)
      Name: 'defineSetter';
      FileName: '<shim:defineSetter>';
      Source:
        'export const defineSetter = ((proto) => {'#10 +
        '  Object.defineProperty(proto, "__defineSetter__", {'#10 +
        '    value(prop, setter) {'#10 +
        '      if (this === null || this === undefined) throw new TypeError("__defineSetter__ called on null or undefined");'#10 +
        '      if (typeof setter !== "function") throw new TypeError("Object.prototype.__defineSetter__ setter must be callable");'#10 +
        '      Object.defineProperty(Object(this), prop, { set: setter, enumerable: true, configurable: true });'#10 +
        '      return undefined;'#10 +
        '    },'#10 +
        '    writable: true, enumerable: false, configurable: true'#10 +
        '  });'#10 +
        '  Object.defineProperty(proto.__defineSetter__, "name", { value: "__defineSetter__", configurable: true });'#10 +
        '  Object.defineProperty(proto.__defineSetter__, "length", { value: 2, configurable: true });'#10 +
        '  return proto.__defineSetter__;'#10 +
        '})(Object.getPrototypeOf({}));'
    ),
    ( // ES2026 Annex B §B.2.2.4 Object.prototype.__lookupGetter__(P)
      Name: 'lookupGetter';
      FileName: '<shim:lookupGetter>';
      Source:
        'export const lookupGetter = ((proto) => {'#10 +
        '  Object.defineProperty(proto, "__lookupGetter__", {'#10 +
        '    value(prop) {'#10 +
        '      if (this === null || this === undefined) throw new TypeError("__lookupGetter__ called on null or undefined");'#10 +
        '      const key = Reflect.ownKeys(Object.defineProperty({}, prop, { value: undefined }))[0];'#10 +
        '      const find = (object) => {'#10 +
        '        if (object === null) return undefined;'#10 +
        '        const descriptor = Object.getOwnPropertyDescriptor(object, key);'#10 +
        '        if (descriptor !== undefined) return descriptor.get;'#10 +
        '        return find(Object.getPrototypeOf(object));'#10 +
        '      };'#10 +
        '      return find(this);'#10 +
        '    },'#10 +
        '    writable: true, enumerable: false, configurable: true'#10 +
        '  });'#10 +
        '  Object.defineProperty(proto.__lookupGetter__, "name", { value: "__lookupGetter__", configurable: true });'#10 +
        '  return proto.__lookupGetter__;'#10 +
        '})(Object.getPrototypeOf({}));'
    ),
    ( // ES2026 Annex B §B.2.2.5 Object.prototype.__lookupSetter__(P)
      Name: 'lookupSetter';
      FileName: '<shim:lookupSetter>';
      Source:
        'export const lookupSetter = ((proto) => {'#10 +
        '  Object.defineProperty(proto, "__lookupSetter__", {'#10 +
        '    value(prop) {'#10 +
        '      if (this === null || this === undefined) throw new TypeError("__lookupSetter__ called on null or undefined");'#10 +
        '      const key = Reflect.ownKeys(Object.defineProperty({}, prop, { value: undefined }))[0];'#10 +
        '      const find = (object) => {'#10 +
        '        if (object === null) return undefined;'#10 +
        '        const descriptor = Object.getOwnPropertyDescriptor(object, key);'#10 +
        '        if (descriptor !== undefined) return descriptor.set;'#10 +
        '        return find(Object.getPrototypeOf(object));'#10 +
        '      };'#10 +
        '      return find(this);'#10 +
        '    },'#10 +
        '    writable: true, enumerable: false, configurable: true'#10 +
        '  });'#10 +
        '  Object.defineProperty(proto.__lookupSetter__, "name", { value: "__lookupSetter__", configurable: true });'#10 +
        '  return proto.__lookupSetter__;'#10 +
        '})(Object.getPrototypeOf({}));'
    )
  );

function DefaultShimCount: Integer;
begin
  Result := Length(DEFAULT_SHIMS);
end;

function DefaultShim(const AIndex: Integer): TGocciaShimDefinition;
begin
  Result := DEFAULT_SHIMS[AIndex];
end;

procedure RegisterDefaultShimNames(const AShims: TStringList);
var
  I: Integer;
begin
  for I := Low(DEFAULT_SHIMS) to High(DEFAULT_SHIMS) do
    AShims.Add(DEFAULT_SHIMS[I].Name);
end;

function LoadShimValue(const AInterpreter: TGocciaInterpreter;
  const AShim: TGocciaShimDefinition): TGocciaValue;
var
  ModuleParseResult: TGocciaSourcePipelineModuleResult;
  PipelineOptions: TGocciaSourcePipelineOptions;
  ProgramNode: TGocciaProgram;
  ModuleScope: TGocciaScope;
  Context: TGocciaEvaluationContext;
  I: Integer;
begin
  PipelineOptions := TGocciaSourcePipeline.DefaultOptions;
  PipelineOptions.Preprocessors := [];
  PipelineOptions.Compatibility := [cfFunction];
  PipelineOptions.SourceType := stModule;
  ModuleParseResult := TGocciaSourcePipeline.ParseModuleSource(
    UTF8String(AShim.Source), AShim.FileName, PipelineOptions);
  try
    ProgramNode := ModuleParseResult.TakeProgramNode;
    try
      ModuleScope := AInterpreter.GlobalScope.CreateChild(skModule,
        'Shim:' + AShim.Name);
      // ES2026 §16.2.1.6.4 InitializeEnvironment: a Module
      // Environment Record's [[ThisValue]] is undefined.
      ModuleScope.ThisValue := TGocciaUndefinedLiteralValue.UndefinedValue;
      // Built-in shims may need argument-count presence checks without
      // enabling the legacy arguments object for user code.
      ModuleScope.ArgumentsObjectEnabled := True;
      Context := AInterpreter.CreateEvaluationContext;
      Context.Scope := ModuleScope;
      Context.HideFunctionSourceText := True;
      for I := 0 to ProgramNode.Body.Count - 1 do
        EvaluateStatement(ProgramNode.Body[I], Context);
      Result := ModuleScope.GetValue(AShim.Name);
    finally
      ProgramNode.Free;
    end;
  finally
    ModuleParseResult.Free;
  end;
end;

function IsSideEffectShim(const AName: string): Boolean;
begin
  Result := (AName = 'numberArrayToLocaleString') or
    (AName = 'hasOwnProperty') or (AName = '__proto__') or
    (AName = 'defineGetter') or (AName = 'defineSetter') or
    (AName = 'lookupGetter') or (AName = 'lookupSetter');
end;

constructor TGocciaShimMaterializer.Create(
  const AInterpreter: TGocciaInterpreter; const AShim: TGocciaShimDefinition);
begin
  inherited Create;
  FInterpreter := AInterpreter;
  FShim := AShim;
end;

function TGocciaShimMaterializer.Materialize: TGocciaValue;
begin
  Result := LoadShimValue(FInterpreter, FShim);
end;

end.

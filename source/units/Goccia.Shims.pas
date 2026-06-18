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

function DefaultShimCount: Integer;
function DefaultShim(const AIndex: Integer): TGocciaShimDefinition;

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
  DEFAULT_SHIMS: array[0..12] of TGocciaShimDefinition = (
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
        '  return Number.isFinite(numberValue) ? Math.trunc(numberValue) : NaN;'#10 +
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
        'const makeDateEpochMilliseconds = (args: any[], timeZone: string): number => {'#10 +
        '  if (args.length === 0) return NaN;'#10 +
        '  const yearValue: number = toDateInteger(args[0]);'#10 +
        '  const month: number = toDateInteger(args.length > 1 ? args[1] : 0);'#10 +
        '  const day: number = toDateInteger(args.length > 2 ? args[2] : 1);'#10 +
        '  const hour: number = toDateInteger(args.length > 3 ? args[3] : 0);'#10 +
        '  const minute: number = toDateInteger(args.length > 4 ? args[4] : 0);'#10 +
        '  const second: number = toDateInteger(args.length > 5 ? args[5] : 0);'#10 +
        '  const millisecond: number = toDateInteger(args.length > 6 ? args[6] : 0);'#10 +
        '  if (!Number.isFinite(yearValue) || !Number.isFinite(month) ||'#10 +
        '      !Number.isFinite(day) || !Number.isFinite(hour) ||'#10 +
        '      !Number.isFinite(minute) || !Number.isFinite(second) ||'#10 +
        '      !Number.isFinite(millisecond)) return NaN;'#10 +
        '  const fullYear: number = yearValue >= 0 && yearValue <= 99 ?'#10 +
        '    1900 + yearValue : yearValue;'#10 +
        '  try {'#10 +
        '    const dt = new Temporal.PlainDateTime(fullYear, 1, 1, 0, 0, 0, 0).add({'#10 +
        '      months: month,'#10 +
        '      days: day - 1,'#10 +
        '      hours: hour,'#10 +
        '      minutes: minute,'#10 +
        '      seconds: second,'#10 +
        '      milliseconds: millisecond'#10 +
        '    });'#10 +
        '    const epochMilliseconds: number = dt.toZonedDateTime(timeZone).epochMilliseconds;'#10 +
        '    if (!Number.isFinite(epochMilliseconds) ||'#10 +
        '        Math.abs(epochMilliseconds) > dateTimeClipLimit)'#10 +
        '      return NaN;'#10 +
        '    return Math.trunc(epochMilliseconds);'#10 +
        '  } catch (e) { return NaN; }'#10 +
        '};'#10 +
        'const legacyDateMonthStarts: any[] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];'#10 +
        'const legacyDateLeapMonthStarts: any[] = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335];'#10 +
        'const legacyDateIsLeapYear = (year: number): boolean =>'#10 +
        '  year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);'#10 +
        'const legacyDateDayFromYear = (year: number): number =>'#10 +
        '  365 * (year - 1970) + Math.floor((year - 1969) / 4) -'#10 +
        '  Math.floor((year - 1901) / 100) + Math.floor((year - 1601) / 400);'#10 +
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
        'const parseDateStringToEpoch = (str: any): number => {'#10 +
        '  const text: string = String(str).trim();'#10 +
        '  const legacy: any = parseLegacyDateString(text);'#10 +
        '  if (legacy !== undefined) return legacy;'#10 +
        '  try { return Temporal.Instant.from(text).epochMilliseconds; }'#10 +
        '  catch (e) { return NaN; }'#10 +
        '};'#10 +
        'const __GocciaShimNumberFormat: any = Intl.NumberFormat;'#10 +
        'const __GocciaShimDateTimeFormat: any = Intl.DateTimeFormat;'#10 +
        'const __GocciaDateSlots = new WeakMap();'#10 +
        'const __GocciaDateHasSlot = (date: any): boolean => __GocciaDateSlots.has(date);'#10 +
        'const __GocciaDateRequire = (date: any): void => {'#10 +
        '  if (!__GocciaDateHasSlot(date)) throw new TypeError("Date object expected");'#10 +
        '};'#10 +
        'const __GocciaDateGetSlot = (date: any): number => {'#10 +
        '  __GocciaDateRequire(date);'#10 +
        '  return __GocciaDateSlots.get(date);'#10 +
        '};'#10 +
        'const __GocciaDateSetSlot = (date: any, value: number): number => {'#10 +
        '  __GocciaDateSlots.set(date, value);'#10 +
        '  return value;'#10 +
        '};'#10 +
        'const __GocciaDateClass = class Date {'#10 +
        '  static {'#10 +
        '    const numberLocale = {'#10 +
        '      toLocaleString(...args: any[]): string {'#10 +
        '        const value: number = Number.prototype.valueOf.call(this);'#10 +
        '        return new __GocciaShimNumberFormat(args[0], args[1]).format(value);'#10 +
        '      }'#10 +
        '    }.toLocaleString;'#10 +
        '    const arrayLocale = {'#10 +
        '      toLocaleString(...args: any[]): string {'#10 +
        '        if (this === null || this === undefined) throw new TypeError("Array.prototype.toLocaleString called on null or undefined");'#10 +
        '        const array: any = Object(this);'#10 +
        '        const length: number = Number(array.length);'#10 +
        '        const len: number = !Number.isFinite(length) || length <= 0 ? 0 : Math.trunc(length);'#10 +
        '        const elementString = (index: number): string => {'#10 +
        '          const nextElement: any = array[index];'#10 +
        '          if (nextElement !== undefined && nextElement !== null) {'#10 +
        '            const method: any = Object(nextElement).toLocaleString;'#10 +
        '            if (typeof method !== "function")'#10 +
        '              throw new TypeError("Array.prototype.toLocaleString element toLocaleString is not a function");'#10 +
        '            return String(method.call(nextElement, args[0], args[1]));'#10 +
        '          }'#10 +
        '          return "";'#10 +
        '        };'#10 +
        '        const build = (index: number, result: string): string => index >= len ? result : build(index + 1, result + (index > 0 ? "," : "") + elementString(index));'#10 +
        '        return build(0, "");'#10 +
        '      }'#10 +
        '    }.toLocaleString;'#10 +
        '    Object.defineProperty(Number.prototype, "toLocaleString", { value: numberLocale, writable: true, configurable: true });'#10 +
        '    Object.defineProperty(Array.prototype, "toLocaleString", { value: arrayLocale, writable: true, configurable: true });'#10 +
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
        '      const v = args[0];'#10 +
        '      if (typeof v === "number") {'#10 +
        '        const t = Math.trunc(v);'#10 +
        '        ms = Number.isFinite(t) && Math.abs(t) <= 8.64e15 ? t : NaN;'#10 +
        '      } else { ms = Date.parse(String(v)); }'#10 +
        '    } else {'#10 +
        '      ms = makeDateEpochMilliseconds(args, Temporal.Now.timeZoneId());'#10 +
        '    }'#10 +
        '    __GocciaDateSetSlot(this, ms);'#10 +
        '  }'#10 +
        '  static #require(date: any): void { __GocciaDateRequire(date); }'#10 +
        '  static #get(date: any): number { return __GocciaDateGetSlot(date); }'#10 +
        '  static #set(date: any, value: number): number { return __GocciaDateSetSlot(date, value); }'#10 +
        '  static #valid(date: any): boolean { return Number.isFinite(Date.#get(date)); }'#10 +
        '  static #local(date: any): any { return Date.#valid(date) ? new Temporal.ZonedDateTime(BigInt(Date.#get(date)) * 1000000n, Temporal.Now.timeZoneId()) : null; }'#10 +
        '  static #utc(date: any): any { return Date.#valid(date) ? new Temporal.ZonedDateTime(BigInt(Date.#get(date)) * 1000000n, "UTC") : null; }'#10 +
        '  getTime(): number { return __GocciaDateGetSlot(this); }'#10 +
        '  valueOf(): number { return __GocciaDateGetSlot(this); }'#10 +
        '  [Symbol.toPrimitive](hint: string): any {'#10 +
        '    Date.#require(this);'#10 +
        '    if (hint === "string" || hint === "default") return this.toString();'#10 +
        '    if (hint === "number") return this.valueOf();'#10 +
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
        '    const t = Math.trunc(epochMilliseconds);'#10 +
        '    return Number.isFinite(t) && Math.abs(t) <= dateTimeClipLimit ? t : NaN;'#10 +
        '  }'#10 +
        '  static #epochFromParts(year: number, month: number, day: number, hour: number, minute: number, second: number, millisecond: number, timeZone: string): number {'#10 +
        '    if (!Number.isFinite(year) || !Number.isFinite(month) || !Number.isFinite(day) ||'#10 +
        '        !Number.isFinite(hour) || !Number.isFinite(minute) || !Number.isFinite(second) ||'#10 +
        '        !Number.isFinite(millisecond)) return NaN;'#10 +
        '    try {'#10 +
        '      const dt = new Temporal.PlainDateTime(Math.trunc(year), 1, 1, 0, 0, 0, 0).add({'#10 +
        '        months: Math.trunc(month), days: Math.trunc(day) - 1,'#10 +
        '        hours: Math.trunc(hour), minutes: Math.trunc(minute),'#10 +
        '        seconds: Math.trunc(second), milliseconds: Math.trunc(millisecond)'#10 +
        '      });'#10 +
        '      return Date.#clip(dt.toZonedDateTime(timeZone).epochMilliseconds);'#10 +
        '    } catch (e) { return NaN; }'#10 +
        '  }'#10 +
        '  static #setFromParts(date: any, timeZone: string, z: any, year: number, month: number, day: number, hour: number, minute: number, second: number, millisecond: number): number {'#10 +
        '    Date.#require(date);'#10 +
        '    if (!z) return Date.#set(date, NaN);'#10 +
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
        '    try { return Date.#set(this, Date.#clip(z.add({ days: Math.trunc(d) - z.day }).epochMilliseconds)); }'#10 +
        '    catch (e) { return Date.#set(this, NaN); }'#10 +
        '  }'#10 +
        '  setUTCDate(day: number): number {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this); const d = toDateInteger(day);'#10 +
        '    if (!z) return NaN;'#10 +
        '    if (!Number.isFinite(d)) return Date.#set(this, NaN);'#10 +
        '    try { return Date.#set(this, Date.#clip(z.add({ days: Math.trunc(d) - z.day }).epochMilliseconds)); }'#10 +
        '    catch (e) { return Date.#set(this, NaN); }'#10 +
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
        '  getTimezoneOffset(): number { const z = Date.#local(this); return z ? -(z.offsetNanoseconds / 60000000000) : NaN; }'#10 +
        '  toISOString(): string { if (!Date.#valid(this)) throw new RangeError("Invalid time value"); return Temporal.Instant.fromEpochMilliseconds(Date.#get(this)).toString(); }'#10 +
        '  toJSON(): string | null {'#10 +
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
        '    const pad = (n: number): string => n < 10 ? "0" + String(n) : String(n);'#10 +
        '    const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];'#10 +
        '    const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];'#10 +
        '    return days[z.dayOfWeek % 7] + " " + months[z.month - 1] + " " + pad(z.day) + " " +'#10 +
        '      String(z.year) + " " + pad(z.hour) + ":" + pad(z.minute) + ":" + pad(z.second) + " GMT" + z.offset;'#10 +
        '  }'#10 +
        '  toUTCString(): string {'#10 +
        '    Date.#require(this);'#10 +
        '    const z = Date.#utc(this);'#10 +
        '    if (!z) return "Invalid Date";'#10 +
        '    const pad = (n: number): string => n < 10 ? "0" + String(n) : String(n);'#10 +
        '    const days = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];'#10 +
        '    const months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];'#10 +
        '    return days[z.dayOfWeek % 7] + ", " + pad(z.day) + " " + months[z.month - 1] + " " +'#10 +
        '      String(z.year) + " " + pad(z.hour) + ":" + pad(z.minute) + ":" + pad(z.second) + " GMT";'#10 +
        '  }'#10 +
        '  toGMTString(): string { return this.toUTCString(); }'#10 +
        '  static #localeOptions(options: any, needDate: boolean, needTime: boolean): any {'#10 +
        '    const out: any = {};'#10 +
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
        '    const needDefaults = !hasDate && !hasTime;'#10 +
        '    if (needDefaults && needDate) { out.year = "numeric"; out.month = "numeric"; out.day = "numeric"; }'#10 +
        '    if (needDefaults && needTime) { out.hour = "numeric"; out.minute = "numeric"; out.second = "numeric"; }'#10 +
        '    return out;'#10 +
        '  }'#10 +
        '  toLocaleString(...args: any[]): string {'#10 +
        '    Date.#require(this);'#10 +
        '    if (!Date.#valid(this)) return "Invalid Date";'#10 +
        '    return new __GocciaShimDateTimeFormat(args[0], Date.#localeOptions(args[1], true, true)).format(Date.#get(this));'#10 +
        '  }'#10 +
        '  toLocaleDateString(...args: any[]): string {'#10 +
        '    Date.#require(this);'#10 +
        '    if (!Date.#valid(this)) return "Invalid Date";'#10 +
        '    return new __GocciaShimDateTimeFormat(args[0], Date.#localeOptions(args[1], true, false)).format(Date.#get(this));'#10 +
        '  }'#10 +
        '  toLocaleTimeString(...args: any[]): string {'#10 +
        '    Date.#require(this);'#10 +
        '    if (!Date.#valid(this)) return "Invalid Date";'#10 +
        '    return new __GocciaShimDateTimeFormat(args[0], Date.#localeOptions(args[1], false, true)).format(Date.#get(this));'#10 +
        '  }'#10 +
        '};'#10 +
        'const __GocciaDateConstructor = function(...args: any[]): any {'#10 +
        '  if (new.target === undefined) return new __GocciaDateConstructor().toString();'#10 +
        '  let ms: number;'#10 +
        '  if (args.length === 0) {'#10 +
        '    ms = Temporal.Now.instant().epochMilliseconds;'#10 +
        '  } else if (args.length === 1) {'#10 +
        '    const v = args[0];'#10 +
        '    if (typeof v === "number") {'#10 +
        '      const t = Math.trunc(v);'#10 +
        '      ms = Number.isFinite(t) && Math.abs(t) <= 8.64e15 ? t : NaN;'#10 +
        '    } else { ms = parseDateStringToEpoch(v); }'#10 +
        '  } else {'#10 +
        '    ms = makeDateEpochMilliseconds(args, Temporal.Now.timeZoneId());'#10 +
        '  }'#10 +
        '  __GocciaDateSetSlot(this, ms);'#10 +
        '};'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "now", { value: __GocciaDateClass.now, writable: true, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "parse", { value: __GocciaDateClass.parse, writable: true, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "UTC", { value: __GocciaDateClass.UTC, writable: true, configurable: true });'#10 +
        'Object.defineProperty(__GocciaDateConstructor, "prototype", { value: __GocciaDateClass.prototype, writable: false, configurable: false });'#10 +
        'Object.defineProperty(__GocciaDateClass.prototype, "constructor", { value: __GocciaDateConstructor, writable: true, configurable: true });'#10 +
        'export const Date = __GocciaDateConstructor;'
    ),
    ( // ES2026 §20.1.3.2 — legacy hasOwnProperty via Object.hasOwn
      Name: 'hasOwnProperty';
      FileName: '<shim:hasOwnProperty>';
      Source:
        'export const hasOwnProperty = ((proto) => {'#10 +
        '  Object.defineProperty(proto, "hasOwnProperty", {'#10 +
        '    value(prop) { return Object.hasOwn(this, prop); },'#10 +
        '    writable: true, enumerable: false, configurable: true'#10 +
        '  });'#10 +
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

end.

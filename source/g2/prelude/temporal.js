(function (G, nowNs, tzData, tzIndex, systemTZ, markIntrinsic, calOp) {
  "use strict";
  // Temporal (ECMA-262 Temporal): the abstract operations and the objects. Each Temporal class keeps its
  // internal slots in one private field (#s) and exports a brand check and a slot reader from a static block, so
  // the operations below can test any class's brand. Exact times and time durations are BigInt nanoseconds.
  // G2.Temporal gives nowNs (the clock), tzData (a zone's TZif bytes), tzIndex (tzdata.zi) and systemTZ.

  // ---------------- primordials ----------------
  var ObjectDefineProperty = Object.defineProperty, ObjectCreate = Object.create, ObjectFreeze = Object.freeze;
  var MathAbs = Math.abs, MathFloor = Math.floor, MathTrunc = Math.trunc, MathMax = Math.max, MathMin = Math.min;
  var NumberIsFinite = Number.isFinite, NumberIsInteger = Number.isInteger;
  var TypeErr = TypeError, RangeErr = RangeError, Str = String, Num = Number, Big = BigInt;
  var SymToStringTag = Symbol.toStringTag, SymToPrimitive = Symbol.toPrimitive;
  var StringFromCharCode = String.fromCharCode;
  var ArrayIsArray = Array.isArray;

  // ASCII-lowercase (toLowerCase would also fold non-ASCII letters, as U+0130 to "i")
  function asciiLower(s) {
    var r = "";
    for (var i = 0; i < s.length; i++) { var c = s.charCodeAt(i); r += c >= 65 && c <= 90 ? StringFromCharCode(c + 32) : s.charAt(i); }
    return r;
  }
  // an Object: typeof says "object" or "function", or "undefined" for an [[IsHTMLDDA]] object (B.3.6)
  function isObject(v) {
    var t = typeof v;
    return (t === "object" && v !== null) || t === "function" || (t === "undefined" && v !== undefined);
  }
  // IsCallable: typeof says "function", or "undefined" for a callable [[IsHTMLDDA]] object (B.3.6)
  function isCallable(f) { var t = typeof f; return t === "function" || (t === "undefined" && f !== undefined); }
  function typeError(m) { return new TypeErr(m); }
  function rangeError(m) { return new RangeErr(m); }
  function def(o, k, v) { ObjectDefineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true }); }
  function pad(n, w) { var s = Str(n); while (s.length < w) s = "0" + s; return s; }
  function sign(n) { return n < 0 ? -1 : n > 0 ? 1 : 0; }
  function bsign(n) { return n < 0n ? -1 : n > 0n ? 1 : 0; }
  function babs(n) { return n < 0n ? -n : n; }
  // floor division and modulo for Numbers and BigInts
  function fdiv(a, b) { return MathFloor(a / b); }
  function fmod(a, b) { var r = a % b; return r !== 0 && (r < 0) !== (b < 0) ? r + b : r + 0; }
  function bfdiv(a, b) { var q = a / b; if ((a % b !== 0n) && ((a < 0n) !== (b < 0n))) q -= 1n; return q; }
  function bfmod(a, b) { var r = a % b; if (r !== 0n && ((r < 0n) !== (b < 0n))) r += b; return r; }

  // the correctly rounded Number of the rational p / q (q > 0)
  function ratToNumber(p, q) {
    if (q < 0n) { p = -p; q = -q; }
    if (p === 0n) return 0;
    var neg = p < 0n; if (neg) p = -p;
    // scale so the integer quotient has at least 65 significant bits, then round once with a sticky bit
    var shift = 0;
    var bitsP = p.toString(2).length, bitsQ = q.toString(2).length;
    shift = 66 - (bitsP - bitsQ);
    var num = shift > 0 ? p << Big(shift) : p, den = shift < 0 ? q << Big(-shift) : q;
    var m = num / den, rem = num % den;
    if (rem !== 0n) m = m | 1n;
    var r = Num(m);
    // r * 2^-shift, exactly (powers of two)
    var s = shift;
    while (s > 1000) { r /= 2 ** 1000; s -= 1000; }
    while (s < -1000) { r *= 2 ** 1000; s += 1000; }
    r = s >= 0 ? r / 2 ** s : r * 2 ** -s;
    return neg ? -r : r;
  }

  // ---------------- conversions (ECMA-262 + Temporal) ----------------
  function ToNumberV(v) { if (typeof v === "bigint") throw typeError("Cannot convert a BigInt to a number"); return +v; }
  function ToIntegerWithTruncation(v) {
    var n = ToNumberV(v);
    if (n !== n || n === Infinity || n === -Infinity) throw rangeError("value must be finite");
    return MathTrunc(n) + 0;
  }
  function ToPositiveIntegerWithTruncation(v) {
    var n = ToIntegerWithTruncation(v);
    if (n <= 0) throw rangeError("value must be positive");
    return n;
  }
  function ToIntegerIfIntegral(v) {
    var n = ToNumberV(v);
    if (!NumberIsFinite(n) || !NumberIsInteger(n)) throw rangeError("value must be an integer");
    return n + 0;
  }
  function ToStringV(v) { if (typeof v === "symbol") throw typeError("Cannot convert a Symbol to a string"); return Str(v); }
  function ToPrimitiveString(v) {
    // ToPrimitive(v, string)
    if (!isObject(v)) return v;
    var ex = v[SymToPrimitive];
    if (ex !== undefined && ex !== null) {
      if (!isCallable(ex)) throw typeError("@@toPrimitive is not callable");
      var r = ex.call(v, "string");
      if (isObject(r)) throw typeError("@@toPrimitive returned an object");
      return r;
    }
    var names = ["toString", "valueOf"];
    for (var i = 0; i < 2; i++) {
      var f = v[names[i]];
      if (isCallable(f)) { var r2 = f.call(v); if (!isObject(r2)) return r2; }
    }
    throw typeError("Cannot convert object to primitive value");
  }

  // ---------------- date equations ----------------
  var nsPerDay = 86400000000000n, msPerDay = 86400000;
  var maxTimeDuration = 9007199254740991999999999n;   // 2^53 * 10^9 - 1
  var nsMaxInstant = 8640000000000000000000n, nsMinInstant = -8640000000000000000000n;

  function isLeap(y) { return (y % 4 === 0 && y % 100 !== 0) || y % 400 === 0; }
  function daysInYear(y) { return isLeap(y) ? 366 : 365; }
  function ISODaysInMonth(y, m) {
    if (m === 2) return isLeap(y) ? 29 : 28;
    return (m === 4 || m === 6 || m === 9 || m === 11) ? 30 : 31;
  }
  // days from 1970-01-01 to y-m-d (m 1-based), proleptic Gregorian
  function ISODateToEpochDays(y, m, d) {
    // shift to a year starting in March
    var yy = m <= 2 ? y - 1 : y;
    var era = fdiv(yy, 400);
    var yoe = yy - era * 400;
    var mp = (m + 9) % 12;
    var doy = fdiv(153 * mp + 2, 5) + d - 1;
    var doe = yoe * 365 + fdiv(yoe, 4) - fdiv(yoe, 100) + doy;
    return era * 146097 + doe - 719468;
  }
  function epochDaysToISODate(z) {
    z += 719468;
    var era = fdiv(z, 146097);
    var doe = z - era * 146097;
    var yoe = fdiv(doe - fdiv(doe, 1460) + fdiv(doe, 36524) - fdiv(doe, 146096), 365);
    var y = yoe + era * 400;
    var doy = doe - (365 * yoe + fdiv(yoe, 4) - fdiv(yoe, 100));
    var mp = fdiv(5 * doy + 2, 153);
    var d = doy - fdiv(153 * mp + 2, 5) + 1;
    var m = mp < 10 ? mp + 3 : mp - 9;
    return { year: m <= 2 ? y + 1 : y, month: m, day: d };
  }
  function ISODayOfWeek(d) { var w = fmod(ISODateToEpochDays(d.year, d.month, d.day) + 4, 7); return w === 0 ? 7 : w; }
  function ISODayOfYear(d) { return ISODateToEpochDays(d.year, d.month, d.day) - ISODateToEpochDays(d.year, 1, 1) + 1; }
  function ISOWeekOfYear(d) {
    var year = d.year, doy = ISODayOfYear(d), dow = ISODayOfWeek(d);
    var week = fdiv(doy + 7 - dow + 3, 7);
    if (week < 1) {
      var dj = ISODayOfWeek({ year: year, month: 1, day: 1 });
      if (dj === 5 || (dj === 6 && isLeap(year - 1))) return { week: 53, year: year - 1 };
      return { week: 52, year: year - 1 };
    }
    if (week === 53) {
      if (daysInYear(year) - doy < 4 - dow) return { week: 1, year: year + 1 };
    }
    return { week: week, year: year };
  }

  // ---------------- ISO date / time records ----------------
  function ISODate(y, m, d) { return { year: y, month: m, day: d }; }
  function IsValidISODate(y, m, d) { return m >= 1 && m <= 12 && d >= 1 && d <= ISODaysInMonth(y, m); }
  function RegulateISODate(y, m, d, overflow) {
    if (overflow === "constrain") {
      m = MathMin(MathMax(m, 1), 12);
      d = MathMin(MathMax(d, 1), ISODaysInMonth(y, m));
      return ISODate(y, m, d);
    }
    if (!IsValidISODate(y, m, d)) throw rangeError("invalid ISO date");
    return ISODate(y, m, d);
  }
  function BalanceISOYearMonth(y, m) { m -= 1; return { year: y + fdiv(m, 12), month: fmod(m, 12) + 1 }; }
  function AddDaysToISODate(d, days) { return epochDaysToISODate(ISODateToEpochDays(d.year, d.month, d.day) + days); }
  function CompareISODate(a, b) {
    if (a.year !== b.year) return a.year > b.year ? 1 : -1;
    if (a.month !== b.month) return a.month > b.month ? 1 : -1;
    if (a.day !== b.day) return a.day > b.day ? 1 : -1;
    return 0;
  }
  function Time(h, mi, s, ms, us, ns) {
    return { days: 0, hour: h, minute: mi, second: s, millisecond: ms, microsecond: us, nanosecond: ns };
  }
  function MidnightTime() { return Time(0, 0, 0, 0, 0, 0); }
  function IsValidTime(h, mi, s, ms, us, ns) {
    return h >= 0 && h <= 23 && mi >= 0 && mi <= 59 && s >= 0 && s <= 59 && ms >= 0 && ms <= 999 &&
      us >= 0 && us <= 999 && ns >= 0 && ns <= 999;
  }
  function RegulateTime(h, mi, s, ms, us, ns, overflow) {
    if (overflow === "constrain") {
      return Time(MathMin(MathMax(h, 0), 23), MathMin(MathMax(mi, 0), 59), MathMin(MathMax(s, 0), 59),
        MathMin(MathMax(ms, 0), 999), MathMin(MathMax(us, 0), 999), MathMin(MathMax(ns, 0), 999));
    }
    if (!IsValidTime(h, mi, s, ms, us, ns)) throw rangeError("invalid time");
    return Time(h, mi, s, ms, us, ns);
  }
  // time of day in nanoseconds (a BigInt)
  function TimeToNs(t) {
    return Big(t.hour) * 3600000000000n + Big(t.minute) * 60000000000n + Big(t.second) * 1000000000n +
      Big(t.millisecond) * 1000000n + Big(t.microsecond) * 1000n + Big(t.nanosecond);
  }
  // BalanceTime on a nanosecond count of any size: the time of day and whole days
  function BalanceTimeNs(ns) {
    var days = bfdiv(ns, nsPerDay), r = ns - days * nsPerDay;
    var t = Time(Num(r / 3600000000000n), Num(r / 60000000000n % 60n), Num(r / 1000000000n % 60n),
      Num(r / 1000000n % 1000n), Num(r / 1000n % 1000n), Num(r % 1000n));
    t.days = Num(days);
    return t;
  }
  function AddTimeNs(t, ns) { return BalanceTimeNs(TimeToNs(t) + ns); }
  function CompareTimeRecord(a, b) { var x = TimeToNs(a), y = TimeToNs(b); return x > y ? 1 : x < y ? -1 : 0; }
  function DateTime(d, t) { return { isoDate: d, time: t }; }
  function CompareISODateTime(a, b) { var c = CompareISODate(a.isoDate, b.isoDate); return c !== 0 ? c : CompareTimeRecord(a.time, b.time); }
  // epoch nanoseconds of a date-time read as UTC
  function GetUTCEpochNanoseconds(dt) {
    return Big(ISODateToEpochDays(dt.isoDate.year, dt.isoDate.month, dt.isoDate.day)) * nsPerDay + TimeToNs(dt.time);
  }
  function BalanceISODateTime(d, t) {
    var bt = BalanceTimeNs(TimeToNs(t) + Big(t.days || 0) * nsPerDay);
    var nd = AddDaysToISODate(d, bt.days);
    bt.days = 0;
    return DateTime(nd, bt);
  }
  function ISODateTimeWithinLimits(dt) {
    var days = ISODateToEpochDays(dt.isoDate.year, dt.isoDate.month, dt.isoDate.day);
    if (MathAbs(days) > 100000001) return false;
    var ns = Big(days) * nsPerDay + TimeToNs(dt.time);
    if (ns <= nsMinInstant - nsPerDay) return false;
    if (ns >= nsMaxInstant + nsPerDay) return false;
    return true;
  }
  function ISODateWithinLimits(d) { return ISODateTimeWithinLimits(DateTime(d, Time(12, 0, 0, 0, 0, 0))); }
  function ISOYearMonthWithinLimits(d) {
    if (d.year < -271821 || d.year > 275760) return false;
    if (d.year === -271821 && d.month < 4) return false;
    if (d.year === 275760 && d.month > 9) return false;
    return true;
  }
  function IsValidEpochNanoseconds(ns) { return ns >= nsMinInstant && ns <= nsMaxInstant; }

  // ---------------- units ----------------
  // [singular, plural, nanoseconds (BigInt, 0n for calendar units), category]
  var UNITS = ["year", "month", "week", "day", "hour", "minute", "second", "millisecond", "microsecond", "nanosecond"];
  var UNIT_NS = [0n, 0n, 0n, nsPerDay, 3600000000000n, 60000000000n, 1000000000n, 1000000n, 1000n, 1n];
  function unitIndex(u) { return UNITS.indexOf(u); }
  function unitNs(u) { return UNIT_NS[unitIndex(u)]; }
  function LargerOfTwoTemporalUnits(a, b) { return unitIndex(a) <= unitIndex(b) ? a : b; }
  function IsCalendarUnit(u) { return u === "year" || u === "month" || u === "week"; }
  function IsDateUnit(u) { return unitIndex(u) <= 3; }
  function MaximumTemporalDurationRoundingIncrement(u) {
    if (IsDateUnit(u)) return undefined;
    if (u === "hour") return 24;
    if (u === "minute" || u === "second") return 60;
    return 1000;
  }

  // ---------------- options ----------------
  function GetOptionsObject(o) {
    if (o === undefined) return ObjectCreate(null);
    if (isObject(o)) return o;
    throw typeError("options must be an object");
  }
  function GetStringOption(o, name, allowed, dflt) {
    var v = o[name];
    if (v === undefined) {
      if (dflt === REQUIRED) throw rangeError(name + " is required");
      return dflt;
    }
    v = ToStringV(v);
    if (allowed !== undefined && allowed.indexOf(v) < 0) throw rangeError(v + " is not a valid value for " + name);
    return v;
  }
  var REQUIRED = {};
  function GetTemporalOverflowOption(o) { return GetStringOption(o, "overflow", ["constrain", "reject"], "constrain"); }
  function GetTemporalDisambiguationOption(o) {
    return GetStringOption(o, "disambiguation", ["compatible", "earlier", "later", "reject"], "compatible");
  }
  function GetTemporalOffsetOption(o, dflt) {
    return GetStringOption(o, "offset", ["prefer", "use", "ignore", "reject"], dflt);
  }
  function GetTemporalShowCalendarNameOption(o) {
    return GetStringOption(o, "calendarName", ["auto", "always", "never", "critical"], "auto");
  }
  function GetTemporalShowTimeZoneNameOption(o) {
    return GetStringOption(o, "timeZoneName", ["auto", "never", "critical"], "auto");
  }
  function GetTemporalShowOffsetOption(o) { return GetStringOption(o, "offset", ["auto", "never"], "auto"); }
  function GetDirectionOption(o) { return GetStringOption(o, "direction", ["next", "previous"], REQUIRED); }
  var ROUNDING_MODES = ["ceil", "floor", "expand", "trunc", "halfCeil", "halfFloor", "halfExpand", "halfTrunc", "halfEven"];
  function GetRoundingModeOption(o, dflt) { return GetStringOption(o, "roundingMode", ROUNDING_MODES, dflt); }
  function GetRoundingIncrementOption(o) {
    var v = o.roundingIncrement;
    if (v === undefined) return 1;
    var n = ToNumberV(v);
    if (!NumberIsFinite(n)) throw rangeError("roundingIncrement must be finite");
    var i = MathTrunc(n);
    if (i < 1 || i > 1e9) throw rangeError("roundingIncrement out of range");
    return i;
  }
  function ValidateTemporalRoundingIncrement(inc, dividend, inclusive) {
    var max = inclusive ? dividend : dividend - 1;
    if (inc > max) throw rangeError("roundingIncrement out of range");
    if (dividend % inc !== 0) throw rangeError("roundingIncrement must divide the unit");
  }
  function GetTemporalFractionalSecondDigitsOption(o) {
    var v = o.fractionalSecondDigits;
    if (v === undefined) return "auto";
    if (typeof v !== "number") {
      if (ToStringV(v) !== "auto") throw rangeError("fractionalSecondDigits must be auto or 0-9");
      return "auto";
    }
    if (v !== v || v === Infinity || v === -Infinity) throw rangeError("fractionalSecondDigits out of range");
    var d = MathFloor(v);
    if (d < 0 || d > 9) throw rangeError("fractionalSecondDigits out of range");
    return d;
  }
  // GetTemporalUnitValuedOption: the unit (singular), "auto", or the default; plural names accepted
  var UNIT_NAMES = {};
  for (var ui = 0; ui < UNITS.length; ui++) { UNIT_NAMES[UNITS[ui]] = UNITS[ui]; UNIT_NAMES[UNITS[ui] + "s"] = UNITS[ui]; }
  function GetTemporalUnitValuedOption(o, name, dflt) {
    var v = o[name];
    if (v === undefined) {
      if (dflt === REQUIRED) throw rangeError(name + " is required");
      return dflt;
    }
    v = ToStringV(v);
    if (v === "auto") return "auto";
    var u = UNIT_NAMES[v];
    if (u === undefined || !Object.prototype.hasOwnProperty.call(UNIT_NAMES, v)) throw rangeError(v + " is not a valid value for " + name);
    return u;
  }
  // ValidateTemporalUnitValue(value, unitGroup, extraValues)
  function ValidateTemporalUnitValue(value, group, extra) {
    if (value === undefined) return;
    if (extra !== undefined && extra.indexOf(value) >= 0) return;
    if (value === "auto") throw rangeError("auto is not allowed here");
    var i = unitIndex(value);
    if (group === "date" && i > 3) throw rangeError(value + " is not a date unit");
    if (group === "time" && i < 4) throw rangeError(value + " is not a time unit");
  }

  // ---------------- rounding ----------------
  function NegateRoundingMode(m) {
    switch (m) {
      case "ceil": return "floor"; case "floor": return "ceil";
      case "halfCeil": return "halfFloor"; case "halfFloor": return "halfCeil";
    }
    return m;
  }
  function GetUnsignedRoundingMode(mode, negative) {
    switch (mode) {
      case "ceil": return negative ? "zero" : "infinity";
      case "floor": return negative ? "infinity" : "zero";
      case "expand": return "infinity";
      case "trunc": return "zero";
      case "halfCeil": return negative ? "half-zero" : "half-infinity";
      case "halfFloor": return negative ? "half-infinity" : "half-zero";
      case "halfExpand": return "half-infinity";
      case "halfTrunc": return "half-zero";
    }
    return "half-even";
  }
  // ApplyUnsignedRoundingMode on BigInts: x between r1 and r2 = r1 + 1 step; cmp is the comparison of x - r1 with
  // r2 - x (the half), even is whether r1 is an even multiple
  function applyUnsigned(cmpHalf, r1Even, mode) {
    if (mode === "zero") return false;
    if (mode === "infinity") return true;
    if (cmpHalf < 0) return false;
    if (cmpHalf > 0) return true;
    if (mode === "half-zero") return false;
    if (mode === "half-infinity") return true;
    return !r1Even;
  }
  // RoundNumberToIncrement for a BigInt x and a BigInt increment
  function roundBigToIncrement(x, inc, mode) {
    var q = x / inc, r = x % inc;   // truncating
    if (r === 0n) return x;
    var neg = x < 0n;
    var um = GetUnsignedRoundingMode(mode, neg);
    var ar = babs(r), aq = babs(q);
    var twice = ar * 2n;
    var cmpHalf = twice < inc ? -1 : twice > inc ? 1 : 0;
    var up = applyUnsigned(cmpHalf, aq % 2n === 0n, um);
    var mag = up ? aq + 1n : aq;
    return (neg ? -mag : mag) * inc;
  }
  function roundBigToIncrementAsIfPositive(x, inc, mode) {
    var q = bfdiv(x, inc), r = x - q * inc;
    if (r === 0n) return x;
    var um = GetUnsignedRoundingMode(mode, false);
    var twice = r * 2n;
    var cmpHalf = twice < inc ? -1 : twice > inc ? 1 : 0;
    var up = applyUnsigned(cmpHalf, q % 2n === 0n, um);
    return (up ? q + 1n : q) * inc;
  }
  // RoundNumberToIncrement on Numbers that are exact integers (date units)
  function roundNumToIncrement(x, inc, mode) { return Num(roundBigToIncrement(Big(x), Big(inc), mode)); }

  // ---------------- formatting ----------------
  function FormatFractionalSeconds(subNs, precision) {
    if (precision === "auto") {
      if (subNs === 0) return "";
      var s = pad(subNs, 9);
      while (s.charCodeAt(s.length - 1) === 48) s = s.slice(0, -1);
      return "." + s;
    }
    if (precision === 0) return "";
    return "." + pad(subNs, 9).slice(0, precision);
  }
  function FormatTimeString(h, mi, s, subNs, precision) {
    var r = pad(h, 2) + ":" + pad(mi, 2);
    if (precision === "minute") return r;
    return r + ":" + pad(s, 2) + FormatFractionalSeconds(subNs, precision);
  }
  function PadISOYear(y) {
    if (y >= 0 && y <= 9999) return pad(y, 4);
    return (y < 0 ? "-" : "+") + pad(MathAbs(y), 6);
  }
  function FormatOffsetNs(offsetNs, style) {
    // style: "minutes" (+HH:MM) or "seconds" (+HH:MM[:SS[.fff]] when not whole minutes)
    var sg = offsetNs < 0 ? "-" : "+";
    var a = MathAbs(offsetNs);
    var h = MathFloor(a / 3600e9), m = MathFloor(a / 60e9) % 60, s = MathFloor(a / 1e9) % 60, sub = a % 1e9;
    var r = sg + pad(h, 2) + ":" + pad(m, 2);
    if (style === "seconds" && (s !== 0 || sub !== 0)) r += ":" + pad(s, 2) + FormatFractionalSeconds(sub, "auto");
    return r;
  }
  function ToSecondsStringPrecisionRecord(smallestUnit, fsd) {
    switch (smallestUnit) {
      case "minute": return { precision: "minute", unit: "minute", increment: 1 };
      case "second": return { precision: 0, unit: "second", increment: 1 };
      case "millisecond": return { precision: 3, unit: "millisecond", increment: 1 };
      case "microsecond": return { precision: 6, unit: "microsecond", increment: 1 };
      case "nanosecond": return { precision: 9, unit: "nanosecond", increment: 1 };
    }
    if (fsd === "auto") return { precision: "auto", unit: "nanosecond", increment: 1 };
    if (fsd === 0) return { precision: 0, unit: "second", increment: 1 };
    if (fsd <= 3) return { precision: fsd, unit: "millisecond", increment: 10 ** (3 - fsd) };
    if (fsd <= 6) return { precision: fsd, unit: "microsecond", increment: 10 ** (6 - fsd) };
    return { precision: fsd, unit: "nanosecond", increment: 10 ** (9 - fsd) };
  }

  // ---------------- RFC 9557 / ISO 8601 parsing ----------------
  // A scanner over the grammar of the spec's "RFC 9557 / ISO 8601 grammar" section. Each part returns the new
  // position or -1; results go into the record r.
  function isDigit(c) { return c >= 48 && c <= 57; }
  function digitsAt(s, i, n) {
    if (i + n > s.length) return -1;
    var v = 0;
    for (var k = 0; k < n; k++) { var c = s.charCodeAt(i + k); if (!isDigit(c)) return -1; v = v * 10 + (c - 48); }
    return v;
  }
  function isSign(c) { return c === 43 || c === 45 || c === 0x2212; }
  function signOf(c) { return c === 43 ? 1 : -1; }
  // DateYear
  function pYear(s, i, r) {
    var c = s.charCodeAt(i);
    if (isSign(c)) {
      if (c === 0x2212) return -1;
      var v = digitsAt(s, i + 1, 6);
      if (v < 0) return -1;
      if (v === 0 && c === 45) return -1;   // -000000
      r.year = v * signOf(c);
      return i + 7;
    }
    var y = digitsAt(s, i, 4);
    if (y < 0) return -1;
    r.year = y;
    return i + 4;
  }
  function pMonth(s, i, r) { var m = digitsAt(s, i, 2); if (m < 1 || m > 12) return -1; r.month = m; return i + 2; }
  function pDay(s, i, r) { var d = digitsAt(s, i, 2); if (d < 1 || d > 31) return -1; r.day = d; return i + 2; }
  function validMonthDay(m, d) {
    if (d === 31 && (m === 2 || m === 4 || m === 6 || m === 9 || m === 11)) return false;
    if (m === 2 && d === 30) return false;
    return true;
  }
  // DateSpec[Extended], either form, with IsValidDate
  function pDate(s, i, r) {
    var j = pYear(s, i, r); if (j < 0) return -1;
    var ext = s.charCodeAt(j) === 45;
    if (ext) j++;
    j = pMonth(s, j, r); if (j < 0) return -1;
    if (ext) { if (s.charCodeAt(j) !== 45) return -1; j++; }
    else if (s.charCodeAt(j) === 45) return -1;
    j = pDay(s, j, r); if (j < 0) return -1;
    if (!validMonthDay(r.month, r.day)) return -1;
    if (r.month === 2 && r.day === 29 && !isLeap(r.year)) return -1;
    return j;
  }
  function pYearMonth(s, i, r) {
    var j = pYear(s, i, r); if (j < 0) return -1;
    if (s.charCodeAt(j) === 45) j++;
    return pMonth(s, j, r);
  }
  function pMonthDay(s, i, r) {
    var j = i;
    if (s.charCodeAt(j) === 45 && s.charCodeAt(j + 1) === 45) j += 2;
    j = pMonth(s, j, r); if (j < 0) return -1;
    if (s.charCodeAt(j) === 45) j++;
    j = pDay(s, j, r); if (j < 0) return -1;
    if (!validMonthDay(r.month, r.day)) return -1;
    return j;
  }
  // TemporalDecimalFraction: the digits (1 to 9)
  function pFraction(s, i, r, key) {
    var c = s.charCodeAt(i);
    if (c !== 46 && c !== 44) return i;
    var j = i + 1, k = j;
    while (k < s.length && isDigit(s.charCodeAt(k)) && k - j < 9) k++;
    if (k === j) return -1;
    if (k < s.length && isDigit(s.charCodeAt(k))) return -1;
    r[key] = s.slice(j, k);
    return k;
  }
  // TimeSpec, either form: hour [minute [second fraction?]]
  function pTime(s, i, r) {
    var h = digitsAt(s, i, 2); if (h < 0 || h > 23) return -1;
    r.hour = h; r.minute = 0; r.second = 0; r.fraction = undefined;
    var j = i + 2;
    var ext = s.charCodeAt(j) === 58;
    var j2 = ext ? j + 1 : j;
    var mi = digitsAt(s, j2, 2);
    if (mi < 0 || mi > 59) return ext ? -1 : j;
    r.minute = mi; j = j2 + 2;
    var c = s.charCodeAt(j);
    if (ext) { if (c !== 58) return j; j++; }
    else if (c === 58) return -1;
    var se = digitsAt(s, j, 2);
    if (se < 0 || se > 60) return ext ? -1 : j;
    r.second = se === 60 ? 59 : se;
    j += 2;
    return pFraction(s, j, r, "fraction");
  }
  // UTCOffset[SubMinutePrecision]: returns the end; the source text is s.slice(i, end)
  function pOffset(s, i, subMinute) {
    var c = s.charCodeAt(i);
    if (!isSign(c) || c === 0x2212) return -1;
    var h = digitsAt(s, i + 1, 2); if (h < 0 || h > 23) return -1;
    var j = i + 3;
    var ext = s.charCodeAt(j) === 58;
    var j2 = ext ? j + 1 : j;
    var m = digitsAt(s, j2, 2);
    if (m < 0 || m > 59) return ext ? -1 : j;
    j = j2 + 2;
    if (!subMinute) return j;
    var c2 = s.charCodeAt(j);
    if (ext) { if (c2 !== 58) return j; j++; }
    else if (c2 === 58) return -1;
    var se = digitsAt(s, j, 2);
    if (se < 0 || se > 59) return ext ? -1 : j;
    j += 2;
    var dummy = {};
    return pFraction(s, j, dummy, "f");
  }
  function isTZLeading(c) { return (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c === 46 || c === 95; }
  function isTZChar(c) { return isTZLeading(c) || isDigit(c) || c === 45 || c === 43; }
  // TimeZoneIdentifier: an offset (no sub-minute part) or an IANA-style name; the end or -1
  function pTimeZoneIdentifier(s, i) {
    var e = pOffset(s, i, false);
    if (e >= 0) return e;
    var j = i;
    while (true) {
      if (!isTZLeading(s.charCodeAt(j))) return -1;
      j++;
      while (j < s.length && isTZChar(s.charCodeAt(j))) j++;
      if (s.charCodeAt(j) !== 47) return j;
      j++;
    }
  }
  // TimeZoneAnnotation? Annotations? up to the end of the string
  function pAnnotations(s, i, r) {
    if (s.charCodeAt(i) === 91) {
      var j = i + 1;
      if (s.charCodeAt(j) === 33) j++;
      var e = pTimeZoneIdentifier(s, j);
      if (e >= 0 && s.charCodeAt(e) === 93) {
        r.tzAnnotation = s.slice(j, e);
        i = e + 1;
      }
    }
    var calendarCritical = false;
    while (i < s.length) {
      if (s.charCodeAt(i) !== 91) return -1;
      var k = i + 1, critical = false;
      if (s.charCodeAt(k) === 33) { critical = true; k++; }
      var ks = k, c = s.charCodeAt(k);
      if (!((c >= 97 && c <= 122) || c === 95)) return -1;
      k++;
      while (k < s.length) {
        c = s.charCodeAt(k);
        if ((c >= 97 && c <= 122) || c === 95 || isDigit(c) || c === 45) k++; else break;
      }
      var key = s.slice(ks, k);
      if (s.charCodeAt(k) !== 61) return -1;
      k++;
      var vs = k;
      while (true) {
        var cs = k;
        while (k < s.length) {
          c = s.charCodeAt(k);
          if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122) || isDigit(c)) k++; else break;
        }
        if (k === cs) return -1;
        if (s.charCodeAt(k) === 45) { k++; continue; }
        break;
      }
      var value = s.slice(vs, k);
      if (s.charCodeAt(k) !== 93) return -1;
      i = k + 1;
      r.annotations.push({ key: key, value: value, critical: critical });
    }
    // ParseISODateTime's annotation rules
    for (var a = 0; a < r.annotations.length; a++) {
      var an = r.annotations[a];
      if (an.key === "u-ca") {
        if (r.calendar === undefined) { r.calendar = an.value; calendarCritical = an.critical; }
        else if (an.critical || calendarCritical) throw rangeError("conflicting calendar annotations");
      } else if (an.critical) throw rangeError("unknown critical annotation " + an.key);
    }
    return i;
  }
  function newParse() {
    return { year: undefined, month: undefined, day: undefined, hour: undefined, minute: undefined,
      second: undefined, fraction: undefined, z: false, offset: undefined, tzAnnotation: undefined,
      calendar: undefined, annotations: [], hasDay: false, hasYear: false, hasTime: false };
  }
  function isDateTimeSep(c) { return c === 84 || c === 116 || c === 32; }
  // AnnotatedDateTime[Zoned, TimeRequired] (and TemporalInstantString when instant)
  function tryDateTime(s, zoned, timeRequired, instant) {
    var r = newParse();
    var j = pDate(s, 0, r);
    if (j < 0) return null;
    r.hasDay = true; r.hasYear = true;
    if (j < s.length && isDateTimeSep(s.charCodeAt(j))) {
      j = pTime(s, j + 1, r);
      if (j < 0) return null;
      r.hasTime = true;
      var c = s.charCodeAt(j);
      if ((zoned || instant) && (c === 90 || c === 122)) { r.z = true; j++; }
      else {
        var e = pOffset(s, j, true);
        if (e >= 0) { r.offset = s.slice(j, e); j = e; }
      }
    } else if (timeRequired || instant) return null;
    if (instant && !r.z && r.offset === undefined) return null;
    r.annotations = [];
    var save = { cal: r.calendar };
    try { j = pAnnotations(s, j, r); } catch (err) { r.error = err; }
    if (j < 0 || j !== s.length) return null;
    if (zoned && r.tzAnnotation === undefined) return null;
    if (r.error) throw r.error;
    return r;
  }
  function tryTimeOnly(s) {
    var r = newParse(), j = 0, designated = false;
    var c0 = s.charCodeAt(0);
    if (c0 === 84 || c0 === 116) { designated = true; j = 1; }
    j = pTime(s, j, r);
    if (j < 0) return null;
    r.hasTime = true;
    var timeEnd = j;
    var e = pOffset(s, j, true);
    if (e >= 0) { r.offset = s.slice(j, e); j = e; }
    var c = s.charCodeAt(j);
    if (c === 90 || c === 122) return null;
    var tEnd = j;
    try { j = pAnnotations(s, j, r); } catch (err) { r.error = err; }
    if (j < 0 || j !== s.length) return null;
    if (!designated) {
      // the early errors: Time DateTimeUTCOffset? must not also be a month-day or a year-month
      var t = s.slice(0, tEnd), q = newParse();
      var md = pMonthDay(t, 0, q);
      if (md === t.length) return null;
      q = newParse();
      var ym = pYearMonth(t, 0, q);
      if (ym === t.length) return null;
    }
    if (r.error) throw r.error;
    return r;
  }
  function tryYearMonth(s) {
    var r = newParse();
    var j = pYearMonth(s, 0, r);
    if (j < 0) return null;
    r.hasYear = true; r.day = 1;
    try { j = pAnnotations(s, j, r); } catch (err) { r.error = err; }
    if (j < 0 || j !== s.length) return null;
    if (r.error) throw r.error;
    return r;
  }
  function tryMonthDay(s) {
    var r = newParse();
    var j = pMonthDay(s, 0, r);
    if (j < 0) return null;
    r.hasDay = true;
    try { j = pAnnotations(s, j, r); } catch (err) { r.error = err; }
    if (j < 0 || j !== s.length) return null;
    if (r.error) throw r.error;
    return r;
  }
  // ParseISODateTime(s, goals): goals is a list of "zoned", "datetime", "instant", "time", "monthday", "yearmonth"
  function ParseISODateTime(s, goals) {
    var r = null, goal;
    for (var g = 0; g < goals.length && r === null; g++) {
      goal = goals[g];
      switch (goal) {
        case "zoned": r = tryDateTime(s, true, false, false); break;
        case "datetime": r = tryDateTime(s, false, false, false); break;
        case "instant": r = tryDateTime(s, false, false, true); break;
        case "time": r = tryTimeOnly(s); if (r === null) r = tryDateTime(s, false, true, false); break;
        case "monthday": r = tryMonthDay(s); if (r === null) r = tryDateTime(s, false, false, false); break;
        case "yearmonth": r = tryYearMonth(s); if (r === null) r = tryDateTime(s, false, false, false); break;
      }
      if (r !== null) {
        if (goal === "yearmonth" && !r.hasDay && r.calendar !== undefined && asciiLower(r.calendar) !== "iso8601")
          throw rangeError("a year-month string without a day must use the ISO 8601 calendar");
        if (goal === "monthday" && !r.hasYear) {
          if (r.calendar !== undefined && asciiLower(r.calendar) !== "iso8601")
            throw rangeError("a month-day string without a year must use the ISO 8601 calendar");
        }
      }
    }
    if (r === null) throw rangeError("invalid ISO 8601 string: " + s);
    var f = (r.fraction || "") + "000000000";
    var res = {
      year: r.hasYear ? r.year : undefined,
      month: r.month, day: r.day === undefined ? 1 : r.day,
      time: r.hasTime ? Time(r.hour, r.minute, r.second, +f.slice(0, 3), +f.slice(3, 6), +f.slice(6, 9)) : "start-of-day",
      timeZone: { z: r.z, offsetString: r.offset, tzAnnotation: r.tzAnnotation },
      calendar: r.calendar
    };
    return res;
  }
  var ALL_GOALS = ["zoned", "datetime", "instant", "time", "monthday", "yearmonth"];
  // AnnotationValue (a bare calendar type)
  function isAnnotationValue(s) {
    if (s.length === 0) return false;
    var parts = s.split("-");
    for (var i = 0; i < parts.length; i++) {
      var p = parts[i];
      if (p.length === 0) return false;
      for (var k = 0; k < p.length; k++) {
        var c = p.charCodeAt(k);
        if (!((c >= 65 && c <= 90) || (c >= 97 && c <= 122) || isDigit(c))) return false;
      }
    }
    return true;
  }
  function ParseTemporalCalendarString(s) {
    var r;
    try { r = ParseISODateTime(s, ALL_GOALS); } catch (e) { r = null; }
    if (r !== null) return r.calendar === undefined ? "iso8601" : r.calendar;
    if (!isAnnotationValue(s)) throw rangeError("invalid calendar: " + s);
    return s;
  }
  // UTC offsets: the offset in nanoseconds of a string that is a UTCOffset[+SubMinutePrecision]
  function ParseDateTimeUTCOffset(s) {
    if (pOffset(s, 0, true) !== s.length) throw rangeError("invalid UTC offset: " + s);
    var sg = s.charCodeAt(0) === 45 ? -1 : 1;
    var h = +s.slice(1, 3), rest = s.slice(3);
    if (rest.charCodeAt(0) === 58) rest = rest.replace(/:/g, "");
    var m = rest.length >= 2 ? +rest.slice(0, 2) : 0;
    var sec = rest.length >= 4 ? +rest.slice(2, 4) : 0;
    var frac = rest.length > 5 ? (rest.slice(5) + "000000000").slice(0, 9) : "0";
    return sg * (((h * 60 + m) * 60 + sec) * 1e9 + +frac);
  }
  function IsOffsetTimeZoneIdentifier(s) { return pOffset(s, 0, false) === s.length; }
  // ParseTimeZoneIdentifier: { name } or { offsetMinutes }
  function ParseTimeZoneIdentifier(s) {
    if (pTimeZoneIdentifier(s, 0) !== s.length) throw rangeError("invalid time zone: " + s);
    if (IsOffsetTimeZoneIdentifier(s)) {
      var ns = ParseDateTimeUTCOffset(s);
      return { name: undefined, offsetMinutes: ns / 60e9 };
    }
    return { name: s, offsetMinutes: undefined };
  }
  function ParseTemporalTimeZoneString(s) {
    if (pTimeZoneIdentifier(s, 0) === s.length) return ParseTimeZoneIdentifier(s);
    var r = ParseISODateTime(s, ALL_GOALS);
    var tz = r.timeZone;
    if (tz.tzAnnotation !== undefined) return ParseTimeZoneIdentifier(tz.tzAnnotation);
    if (tz.z) return ParseTimeZoneIdentifier("UTC");
    if (tz.offsetString !== undefined) {
      if (!IsOffsetTimeZoneIdentifier(tz.offsetString)) throw rangeError("sub-minute offsets are not time zones");
      return ParseTimeZoneIdentifier(tz.offsetString);
    }
    throw rangeError("no time zone in " + s);
  }
  // ParseTemporalDurationString: the ten components
  function ParseTemporalDurationString(s) {
    var i = 0, n = s.length, sg = 1;
    var c = s.charCodeAt(0);
    if (c === 43 || c === 45) { sg = c === 45 ? -1 : 1; i++; }
    c = s.charCodeAt(i);
    if (c !== 80 && c !== 112) throw rangeError("invalid duration: " + s);
    i++;
    var v = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    var order = "YMWD", inTime = false, lastDate = -1, lastTime = -1, any = false, fractionSeen = false;
    var fracH, fracM;
    function digits() {
      var st = i;
      while (i < n && isDigit(s.charCodeAt(i))) i++;
      if (i === st) throw rangeError("invalid duration: " + s);
      return s.slice(st, i);
    }
    while (i < n) {
      c = s.charCodeAt(i);
      if (c === 84 || c === 116) {
        if (inTime) throw rangeError("invalid duration: " + s);
        inTime = true; i++;
        if (i >= n) throw rangeError("invalid duration: " + s);
        continue;
      }
      if (fractionSeen) throw rangeError("invalid duration: " + s);
      var ds = digits();
      var frac = undefined;
      c = s.charCodeAt(i);
      if (c === 46 || c === 44) {
        var r0 = {};
        var e = pFraction(s, i, r0, "f");
        if (e < 0) throw rangeError("invalid duration: " + s);
        frac = r0.f; i = e;
      }
      var d = s.charAt(i).toUpperCase(); i++;
      if (!inTime) {
        var k = order.indexOf(d);
        if (k < 0 || k <= lastDate || frac !== undefined) throw rangeError("invalid duration: " + s);
        lastDate = k;
        v[k] = ToIntegerWithTruncation(ds);
      } else {
        var t = "HMS".indexOf(d);
        if (t < 0 || t <= lastTime) throw rangeError("invalid duration: " + s);
        lastTime = t;
        if (frac !== undefined) fractionSeen = true;
        if (t === 0) { v[4] = ToIntegerWithTruncation(ds); fracH = frac; }
        else if (t === 1) { v[5] = ToIntegerWithTruncation(ds); fracM = frac; }
        else v[6] = ToIntegerWithTruncation(ds), v[10] = frac;
      }
      any = true;
    }
    if (!any || (inTime && lastTime < 0)) throw rangeError("invalid duration: " + s);
    // fractions: carried down as exact nanoseconds of the unit
    var subNs = 0n;
    function fracNs(f, unitNsB) { return Big(f + "") * unitNsB / 10n ** Big(f.length); }
    var hours = v[4], minutes = v[5], seconds = v[6];
    var totalSubNs;
    if (fracH !== undefined) {
      totalSubNs = fracNs(fracH, 3600000000000n);
      minutes = Num(totalSubNs / 60000000000n); totalSubNs %= 60000000000n;
      seconds = Num(totalSubNs / 1000000000n); totalSubNs %= 1000000000n;
    } else if (fracM !== undefined) {
      totalSubNs = fracNs(fracM, 60000000000n);
      seconds = Num(totalSubNs / 1000000000n); totalSubNs %= 1000000000n;
    } else if (v[10] !== undefined) {
      totalSubNs = fracNs(v[10], 1000000000n);
    } else totalSubNs = 0n;
    var ms = Num(totalSubNs / 1000000n), us = Num(totalSubNs / 1000n % 1000n), ns = Num(totalSubNs % 1000n);
    return CreateTemporalDuration(sg * v[0], sg * v[1], sg * v[2], sg * v[3], sg * hours, sg * minutes,
      sg * seconds, sg * ms, sg * us, sg * ns);
  }
  // ToOffsetString: a value read for an "offset" property
  function ToOffsetString(v) {
    var o = ToPrimitiveString(v);
    if (typeof o !== "string") throw typeError("offset must be a string");
    ParseDateTimeUTCOffset(o);
    return o;
  }

  // ---------------- time zones ----------------
  // The zone database: tzdata.zi names the zones (Z lines) and links (L target link); a zone's rules come from
  // its TZif file. UTC is the primary identifier its aliases resolve to, as ECMA-402 requires.
  var zoneIndex = null;   // lowercased name -> { id, primary }
  var UTC_ALIASES = ["Etc/UTC", "Etc/GMT", "GMT", "Etc/Universal", "Etc/Zulu", "Etc/UCT", "Etc/Greenwich", "UCT",
    "Universal", "Zulu", "Greenwich", "Etc/GMT0", "Etc/GMT+0", "Etc/GMT-0", "GMT0", "GMT+0", "GMT-0"];
  function loadZoneIndex() {
    zoneIndex = ObjectCreate(null);
    zoneIndex["utc"] = { id: "UTC", primary: "UTC" };
    var text = tzIndex();
    if (typeof text === "string") {
      var lines = text.split("\n"), links = [];
      for (var i = 0; i < lines.length; i++) {
        var l = lines[i];
        if (l.charCodeAt(0) === 90 && l.charCodeAt(1) === 32) {          // "Z name ..."
          var name = l.slice(2).split(" ")[0];
          if (zoneIndex[asciiLower(name)] === undefined) zoneIndex[asciiLower(name)] = { id: name, primary: name };
        } else if (l.charCodeAt(0) === 76 && l.charCodeAt(1) === 32) {   // "L target link"
          var p = l.slice(2).split(" ");
          links.push(p);
        }
      }
      for (var k = 0; k < links.length; k++) {
        var target = links[k][0], link = links[k][1];
        if (zoneIndex[asciiLower(link)] !== undefined) continue;
        var t = zoneIndex[asciiLower(target)];
        zoneIndex[asciiLower(link)] = { id: link, primary: t !== undefined ? t.primary : target };
      }
    }
    for (var u = 0; u < UTC_ALIASES.length; u++) zoneIndex[asciiLower(UTC_ALIASES[u])] = { id: UTC_ALIASES[u], primary: "UTC" };
  }
  // GetAvailableNamedTimeZoneIdentifier: { identifier, primaryIdentifier } or undefined
  function GetAvailableNamedTimeZoneIdentifier(name) {
    if (zoneIndex === null) loadZoneIndex();
    var r = zoneIndex[asciiLower(name)];
    if (r === undefined) return undefined;
    return { identifier: r.id, primaryIdentifier: r.primary };
  }
  function FormatOffsetTimeZoneIdentifier(offsetMinutes, unseparated) {
    var sg = offsetMinutes >= 0 ? "+" : "-", a = MathAbs(offsetMinutes);
    return sg + pad(MathFloor(a / 60), 2) + (unseparated ? "" : ":") + pad(a % 60, 2);
  }
  function FormatUTCOffsetNanoseconds(ns) { return FormatOffsetNs(ns, "seconds"); }
  function FormatDateTimeUTCOffsetRounded(ns) {
    var r = Num(roundBigToIncrement(Big(ns), 60000000000n, "halfExpand"));
    return FormatOffsetTimeZoneIdentifier(r / 60e9);
  }
  function ToTemporalTimeZoneIdentifier(v) {
    if (isZonedDateTime(v)) return getZDT(v).timeZone;
    if (typeof v !== "string") throw typeError("time zone must be a string");
    var p = ParseTemporalTimeZoneString(v);
    if (p.offsetMinutes !== undefined) return FormatOffsetTimeZoneIdentifier(p.offsetMinutes);
    var rec = GetAvailableNamedTimeZoneIdentifier(p.name);
    if (rec === undefined) throw rangeError("unknown time zone: " + p.name);
    return rec.identifier;
  }
  function TimeZoneEquals(a, b) {
    if (a === b) return true;
    if (!IsOffsetTimeZoneIdentifier(a) && !IsOffsetTimeZoneIdentifier(b)) {
      // the primary identifiers Intl.supportedValuesOf lists (Goccia.Temporal.TimeZone's), not the tz index's links
      var eq = calOp(8, a, b);
      if (eq === true || eq === false) return eq;
      var ra = GetAvailableNamedTimeZoneIdentifier(a), rb = GetAvailableNamedTimeZoneIdentifier(b);
      if (ra !== undefined && rb !== undefined && ra.primaryIdentifier === rb.primaryIdentifier) return true;
    }
    return false;
  }

  // A zone's rules: transitions (epoch seconds, ascending), the UTC offset (seconds) from each one, the offset
  // before the first, and the POSIX TZ rule for times past the last transition.
  var zoneCache = ObjectCreate(null);
  function be32(s, i) { var v = (s.charCodeAt(i) << 24) | (s.charCodeAt(i + 1) << 16) | (s.charCodeAt(i + 2) << 8) | s.charCodeAt(i + 3); return v | 0; }
  function be64(s, i) { return be32(s, i) * 4294967296 + (be32(s, i + 4) >>> 0); }
  function parseTZif(b) {
    if (b.slice(0, 4) !== "TZif") return null;
    var version = b.charCodeAt(4);
    function counts(at) {
      return { isut: be32(b, at + 20), isstd: be32(b, at + 24), leap: be32(b, at + 28), time: be32(b, at + 32),
        type: be32(b, at + 36), chr: be32(b, at + 40) };
    }
    var c = counts(0), at = 44, tsize = 4;
    if (version >= 50) {
      at += c.time * 5 + c.type * 6 + c.chr + c.leap * 8 + c.isstd + c.isut;
      c = counts(at); at += 44; tsize = 8;
    }
    var times = [], idx = [], types = [];
    for (var i = 0; i < c.time; i++) times.push(tsize === 8 ? be64(b, at + i * 8) : be32(b, at + i * 4));
    at += c.time * tsize;
    for (i = 0; i < c.time; i++) idx.push(b.charCodeAt(at + i));
    at += c.time;
    for (i = 0; i < c.type; i++) types.push({ off: be32(b, at + i * 6), dst: b.charCodeAt(at + i * 6 + 4) });
    at += c.type * 6 + c.chr + c.leap * (tsize + 4) + c.isstd + c.isut;
    var footer = null;
    if (tsize === 8 && b.charCodeAt(at) === 10) {
      var e = b.indexOf("\n", at + 1);
      if (e > at) footer = b.slice(at + 1, e);
    }
    // offsets after each transition, dropping those that do not change the offset
    var tr = [], offs = [];
    var initial = types.length > 0 ? types[0].off : 0;
    var prev = initial;
    for (i = 0; i < times.length; i++) {
      var o = types[idx[i]].off;
      if (o === prev) continue;
      tr.push(times[i]); offs.push(o); prev = o;
    }
    return { transitions: tr, offsets: offs, initial: initial, rule: footer ? parsePosixTZ(footer) : null };
  }
  // POSIX TZ: std offset [dst [offset] [,start[/time],end[/time]]]
  function parsePosixTZ(s) {
    var i = 0;
    function name() {
      if (s.charCodeAt(i) === 60) { var e = s.indexOf(">", i); if (e < 0) return null; i = e + 1; return true; }
      var st = i;
      while (i < s.length) { var c = s.charCodeAt(i); if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122)) i++; else break; }
      return i - st >= 3 ? true : null;
    }
    function offset() {
      // hh[:mm[:ss]], POSIX sign (west positive): the seconds east of UTC
      var sg = 1, c = s.charCodeAt(i);
      if (c === 43) i++; else if (c === 45) { sg = -1; i++; }
      var parts = [0, 0, 0], k = 0, st = i;
      while (k < 3) {
        var v = 0, d0 = i;
        while (i < s.length && isDigit(s.charCodeAt(i))) { v = v * 10 + s.charCodeAt(i) - 48; i++; }
        if (i === d0) return null;
        parts[k++] = v;
        if (s.charCodeAt(i) !== 58) break;
        i++;
      }
      if (i === st) return null;
      return sg * (parts[0] * 3600 + parts[1] * 60 + parts[2]);
    }
    function dateRule() {
      var c = s.charCodeAt(i), r = {};
      if (c === 77) {   // Mm.w.d
        i++;
        var m = 0; while (isDigit(s.charCodeAt(i))) m = m * 10 + s.charCodeAt(i++) - 48;
        if (s.charCodeAt(i++) !== 46) return null;
        var w = s.charCodeAt(i++) - 48;
        if (s.charCodeAt(i++) !== 46) return null;
        var d = s.charCodeAt(i++) - 48;
        r = { kind: "M", m: m, w: w, d: d };
      } else if (c === 74) {   // Jn: 1-365, no Feb 29
        i++;
        var n = 0; while (isDigit(s.charCodeAt(i))) n = n * 10 + s.charCodeAt(i++) - 48;
        r = { kind: "J", n: n };
      } else {
        var n2 = 0, st = i; while (isDigit(s.charCodeAt(i))) n2 = n2 * 10 + s.charCodeAt(i++) - 48;
        if (i === st) return null;
        r = { kind: "N", n: n2 };
      }
      r.time = 7200;
      if (s.charCodeAt(i) === 47) { i++; var t = offset(); if (t === null) return null; r.time = t; }
      return r;
    }
    if (!name()) return null;
    var std = offset(); if (std === null) return null;
    std = -std;
    if (i >= s.length) return { std: std, dst: null };
    if (!name()) return null;
    var dst = std + 3600;
    if (i < s.length && s.charCodeAt(i) !== 44) { var o = offset(); if (o === null) return null; dst = -o; }
    if (s.charCodeAt(i) !== 44) return { std: std, dst: null };
    i++;
    var start = dateRule(); if (start === null || s.charCodeAt(i) !== 44) return null;
    i++;
    var end = dateRule(); if (end === null) return null;
    return { std: std, dst: dst, start: start, end: end };
  }
  // the epoch seconds of a POSIX rule date in year y, at its local time, with the given offset in effect
  function posixRuleTime(r, y, off) {
    var day;
    if (r.kind === "J") {
      var n = r.n;
      if (isLeap(y) && n >= 60) n += 1;
      day = ISODateToEpochDays(y, 1, 1) + n - 1;
    } else if (r.kind === "N") day = ISODateToEpochDays(y, 1, 1) + r.n;
    else {
      var first = ISODateToEpochDays(y, r.m, 1);
      var dow = fmod(first + 4, 7);   // 0 = Sunday
      var d = first + fmod(r.d - dow, 7) + (r.w - 1) * 7;
      if (r.w === 5) { var dim = ISODaysInMonth(y, r.m); while (d - first >= dim) d -= 7; }
      day = d;
    }
    return day * 86400 + r.time - off;
  }
  // the transitions of a POSIX rule in year y: [[t, offsetAfter], ...] ascending
  function posixYearTransitions(rule, y) {
    var a = posixRuleTime(rule.start, y, rule.std), b = posixRuleTime(rule.end, y, rule.dst);
    var r = [[a, rule.dst], [b, rule.std]];
    if (b < a) r = [[b, rule.std], [a, rule.dst]];
    return r;
  }
  function posixOffsetAt(rule, t) {
    if (rule.dst === null) return rule.std;
    var y = epochDaysToISODate(MathFloor(t / 86400)).year;
    var best = null;
    for (var yy = y - 1; yy <= y + 1; yy++) {
      var tr = posixYearTransitions(rule, yy);
      for (var k = 0; k < 2; k++) if (tr[k][0] <= t) best = tr[k];
    }
    return best === null ? rule.std : best[1];
  }
  function getZone(id) {
    var z = zoneCache[id];
    if (z !== undefined) return z;
    // a link's rules are its primary zone's (CET is Europe/Brussels's), whatever the tz files keep under its name
    var primary = id === "UTC" ? undefined : calOp(9, id);
    var data = id === "UTC" ? undefined : tzData(typeof primary === "string" ? primary : id);
    if (typeof primary === "string" && typeof data !== "string") data = tzData(id);
    z = typeof data === "string" ? parseTZif(data) : null;
    if (z === null) z = { transitions: [], offsets: [], initial: 0, rule: null };
    zoneCache[id] = z;
    return z;
  }
  function zoneOffsetSeconds(z, t) {
    var tr = z.transitions, n = tr.length;
    if (n === 0 || t < tr[0]) return n === 0 && z.rule !== null ? posixOffsetAt(z.rule, t) : z.initial;
    if (t >= tr[n - 1] && z.rule !== null) {
      // past the file's last transition the footer rule applies (it agrees with the last transition)
      if (z.rule.dst === null) return z.offsets[n - 1];
      var last = tr[n - 1];
      if (t >= last) {
        var o = posixOffsetAt(z.rule, t);
        // until the rule's first transition after the last listed one, the listed offset holds
        var nx = posixNext(z.rule, last);
        if (nx === null || t < nx) return z.offsets[n - 1];
        return o;
      }
    }
    var lo = 0, hi = n - 1;
    while (lo < hi) { var mid = (lo + hi + 1) >> 1; if (tr[mid] <= t) lo = mid; else hi = mid - 1; }
    return z.offsets[lo];
  }
  // the first POSIX rule transition strictly after t that changes the offset
  function posixNext(rule, t) {
    if (rule.dst === null) return null;
    var y = epochDaysToISODate(MathFloor(t / 86400)).year;
    for (var yy = y - 1; yy <= y + 2; yy++) {
      var tr = posixYearTransitions(rule, yy);
      for (var k = 0; k < 2; k++) if (tr[k][0] > t && posixOffsetAt(rule, tr[k][0] - 1) !== tr[k][1]) return tr[k][0];
    }
    return null;
  }
  function posixPrev(rule, t) {
    if (rule.dst === null) return null;
    var y = epochDaysToISODate(MathFloor(t / 86400)).year;
    for (var yy = y + 1; yy >= y - 2; yy--) {
      var tr = posixYearTransitions(rule, yy);
      for (var k = 1; k >= 0; k--) if (tr[k][0] < t && posixOffsetAt(rule, tr[k][0] - 1) !== tr[k][1]) return tr[k][0];
    }
    return null;
  }
  function epochSecondsOf(ns) { return Num(bfdiv(ns, 1000000000n)); }
  function GetNamedTimeZoneOffsetNanoseconds(id, ns) {
    if (id === "UTC") return 0;
    var r = GetAvailableNamedTimeZoneIdentifier(id);
    return zoneOffsetSeconds(getZone(r === undefined ? id : r.primaryIdentifier), epochSecondsOf(ns)) * 1e9;
  }
  function zoneForId(id) {
    var r = GetAvailableNamedTimeZoneIdentifier(id);
    return getZone(r === undefined ? id : r.primaryIdentifier);
  }
  function GetNamedTimeZoneNextTransition(id, ns) {
    if (id === "UTC") return null;
    var z = zoneForId(id), t = epochSecondsOf(ns), tr = z.transitions, n = tr.length, res = null;
    // a transition at second s happens at s * 1e9; strictly after ns
    var lo = 0, hi = n;
    while (lo < hi) { var mid = (lo + hi) >> 1; if (Big(tr[mid]) * 1000000000n > ns) hi = mid; else lo = mid + 1; }
    if (lo < n) res = tr[lo];
    else if (z.rule !== null) {
      var from = n > 0 ? MathMax(t, tr[n - 1]) : t;
      var nx = posixNext(z.rule, from);
      while (nx !== null && Big(nx) * 1000000000n <= ns) nx = posixNext(z.rule, nx);
      res = nx;
    }
    if (res === null) return null;
    var r = Big(res) * 1000000000n;
    return r > nsMaxInstant ? null : r;
  }
  function GetNamedTimeZonePreviousTransition(id, ns) {
    if (id === "UTC") return null;
    var z = zoneForId(id), t = epochSecondsOf(ns), tr = z.transitions, n = tr.length, res = null;
    if (z.rule !== null && n > 0 && t > tr[n - 1]) {
      var p = posixPrev(z.rule, t + 1);
      while (p !== null && Big(p) * 1000000000n >= ns) p = posixPrev(z.rule, p);
      if (p !== null && p > tr[n - 1]) res = p;
    } else if (z.rule !== null && n === 0) {
      var p2 = posixPrev(z.rule, t + 1);
      while (p2 !== null && Big(p2) * 1000000000n >= ns) p2 = posixPrev(z.rule, p2);
      res = p2;
    }
    if (res === null) {
      var lo = -1, hi = n - 1;
      while (lo < hi) { var mid = (lo + hi + 1) >> 1; if (Big(tr[mid]) * 1000000000n < ns) lo = mid; else hi = mid - 1; }
      if (lo >= 0) res = tr[lo];
    }
    if (res === null) return null;
    var r = Big(res) * 1000000000n;
    return r < nsMinInstant ? null : r;
  }
  function GetNamedTimeZoneEpochNanoseconds(id, dt) {
    var local = GetUTCEpochNanoseconds(dt);
    if (id === "UTC") return [local];
    var z = zoneForId(id), ls = epochSecondsOf(local);
    var cands = [zoneOffsetSeconds(z, ls - 86400), zoneOffsetSeconds(z, ls), zoneOffsetSeconds(z, ls + 86400)];
    var res = [], seen = [];
    for (var i = 0; i < 3; i++) {
      var o = cands[i];
      if (seen.indexOf(o) >= 0) continue;
      seen.push(o);
      var e = local - Big(o) * 1000000000n;
      if (zoneOffsetSeconds(z, epochSecondsOf(e)) === o) res.push(e);
    }
    res.sort(function (a, b) { return a < b ? -1 : a > b ? 1 : 0; });
    return res;
  }
  function GetOffsetNanosecondsFor(tz, ns) {
    if (IsOffsetTimeZoneIdentifier(tz)) return ParseDateTimeUTCOffset(tz);
    return GetNamedTimeZoneOffsetNanoseconds(tz, ns);
  }
  function GetISODateTimeFor(tz, ns) {
    var off = GetOffsetNanosecondsFor(tz, ns);
    return epochNsToDateTime(ns + Big(off));
  }
  function epochNsToDateTime(ns) {
    var days = bfdiv(ns, nsPerDay), rem = ns - days * nsPerDay;
    var t = BalanceTimeNs(rem);
    t.days = 0;
    return DateTime(epochDaysToISODate(Num(days)), t);
  }
  function CheckISODaysRange(d) {
    if (MathAbs(ISODateToEpochDays(d.year, d.month, d.day)) > 100000000) throw rangeError("date out of range");
  }
  function GetPossibleEpochNanoseconds(tz, dt) {
    var res;
    if (IsOffsetTimeZoneIdentifier(tz)) {
      var off = ParseDateTimeUTCOffset(tz);
      var ns = GetUTCEpochNanoseconds(dt) - Big(off);
      CheckISODaysRange(epochNsToDateTime(ns).isoDate);
      res = [ns];
    } else res = GetNamedTimeZoneEpochNanoseconds(tz, dt);
    for (var i = 0; i < res.length; i++) if (!IsValidEpochNanoseconds(res[i])) throw rangeError("instant out of range");
    return res;
  }
  function DisambiguatePossibleEpochNanoseconds(poss, tz, dt, disambiguation) {
    var n = poss.length;
    if (n === 1) return poss[0];
    if (n !== 0) {
      if (disambiguation === "earlier" || disambiguation === "compatible") return poss[0];
      if (disambiguation === "later") return poss[n - 1];
      throw rangeError("ambiguous local time");
    }
    if (disambiguation === "reject") throw rangeError("local time does not exist");
    var utc = GetUTCEpochNanoseconds(dt);
    var dayBefore = utc - nsPerDay, dayAfter = utc + nsPerDay;
    if (!IsValidEpochNanoseconds(dayBefore) || !IsValidEpochNanoseconds(dayAfter)) throw rangeError("instant out of range");
    var offBefore = GetOffsetNanosecondsFor(tz, dayBefore), offAfter = GetOffsetNanosecondsFor(tz, dayAfter);
    var nanoseconds = offAfter - offBefore;
    if (disambiguation === "earlier") {
      var et = AddTimeNs(dt.time, Big(-nanoseconds));
      var ed = DateTime(AddDaysToISODate(dt.isoDate, et.days), et);
      var pe = GetPossibleEpochNanoseconds(tz, ed);
      return pe[0];
    }
    var lt = AddTimeNs(dt.time, Big(nanoseconds));
    var ld = DateTime(AddDaysToISODate(dt.isoDate, lt.days), lt);
    var pl = GetPossibleEpochNanoseconds(tz, ld);
    return pl[pl.length - 1];
  }
  function GetEpochNanosecondsFor(tz, dt, disambiguation) {
    return DisambiguatePossibleEpochNanoseconds(GetPossibleEpochNanoseconds(tz, dt), tz, dt, disambiguation);
  }
  function GetStartOfDay(tz, d) {
    var dt = DateTime(d, MidnightTime());
    var poss = GetPossibleEpochNanoseconds(tz, dt);
    if (poss.length > 0) return poss[0];
    // midnight is in a gap: the day starts at the transition that skips it, the first whose local time
    // reaches that midnight
    var t = GetNamedTimeZoneNextTransition(tz, GetUTCEpochNanoseconds(dt) - 2n * nsPerDay);
    while (t !== null && CompareISODateTime(GetISODateTimeFor(tz, t), dt) < 0) t = GetNamedTimeZoneNextTransition(tz, t);
    return t;
  }
  function SystemTimeZoneIdentifier() {
    var s = systemTZ();
    if (typeof s === "string") {
      var r = GetAvailableNamedTimeZoneIdentifier(s);
      if (r !== undefined) return r.primaryIdentifier;
    }
    return "UTC";
  }

  // ---------------- calendars ----------------
  // the calendar types of the Intl era and monthCode proposal (Table 1); every one but iso8601 is computed by calOp
  // (G2.Temporal: Goccia.Temporal.Calendar over ICU) and that proposal's operations further below
  var CALENDAR_TYPES = ["buddhist", "chinese", "coptic", "dangi", "ethioaa", "ethiopic", "gregory", "hebrew", "indian",
    "islamic-civil", "islamic-tbla", "islamic-umalqura", "iso8601", "japanese", "persian", "roc"];
  function CanonicalizeCalendar(id) {
    var l = asciiLower(id);
    if (l === "islamicc") return "islamic-civil";
    if (l === "ethiopic-amete-alem") return "ethioaa";
    if (CALENDAR_TYPES.indexOf(l) < 0) throw rangeError("unsupported calendar: " + id);
    return l;
  }
  function CalendarEquals(a, b) { return a === b; }
  function FormatCalendarAnnotation(id, show) {
    if (show === "never") return "";
    if (show === "auto" && id === "iso8601") return "";
    return "[" + (show === "critical" ? "!" : "") + "u-ca=" + id + "]";
  }
  function CreateMonthCode(n, leap) { return "M" + pad(n, 2) + (leap ? "L" : ""); }
  function ParseMonthCode(v) {
    var mc = ToPrimitiveString(v);
    if (typeof mc !== "string") throw typeError("monthCode must be a string");
    var ok = mc.length >= 3 && mc.length <= 4 && mc.charCodeAt(0) === 77 && isDigit(mc.charCodeAt(1)) && isDigit(mc.charCodeAt(2));
    if (ok && mc.length === 4 && mc.charCodeAt(3) !== 76) ok = false;
    var n = ok ? +mc.slice(1, 3) : -1;
    if (ok && n === 0 && mc.length !== 4) ok = false;
    if (!ok) throw rangeError("invalid monthCode: " + mc);
    return { monthNumber: n, isLeapMonth: mc.length === 4 };
  }
  // the Calendar Fields Record table: property, conversion; in lexicographic order of the property names
  var FIELD_DEFS = {
    day: "pos", era: "str", eraYear: "int", hour: "int", microsecond: "int", millisecond: "int", minute: "int",
    month: "pos", monthCode: "mc", nanosecond: "int", offset: "off", second: "int", timeZone: "tz", year: "int"
  };
  var TIME_FIELD_DEFAULTS = { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 };
  var DATE_FIELDS = ["year", "month", "monthCode", "day"];
  var TIME_FIELDS = ["hour", "minute", "second", "millisecond", "microsecond", "nanosecond"];
  // PrepareCalendarFields(calendar, fields, calendarFieldNames, nonCalendarFieldNames, required ("partial" or a list))
  function PrepareCalendarFields(calendar, obj, calNames, nonCalNames, required) {
    var names = calNames.concat(nonCalNames);
    // CalendarExtraFields: era and eraYear with year, for a calendar with eras
    if (calNames.indexOf("year") >= 0 && CalendarSupportsEra(calendar)) names.push("era", "eraYear");
    names.sort();
    var result = {}, any = false;
    for (var i = 0; i < names.length; i++) {
      var p = names[i], v = obj[p];
      if (v !== undefined) {
        any = true;
        switch (FIELD_DEFS[p]) {
          case "int": v = ToIntegerWithTruncation(v); break;
          case "pos": v = ToPositiveIntegerWithTruncation(v); break;
          case "str": v = ToStringV(v); break;
          case "tz": v = ToTemporalTimeZoneIdentifier(v); break;
          case "mc": var pm = ParseMonthCode(v); v = CreateMonthCode(pm.monthNumber, pm.isLeapMonth); break;
          case "off": v = ToOffsetString(v); break;
        }
        result[p] = v;
      } else if (required !== "partial") {
        if (required.indexOf(p) >= 0) throw typeError(p + " is required");
        if (TIME_FIELD_DEFAULTS[p] !== undefined) result[p] = TIME_FIELD_DEFAULTS[p];
      }
    }
    if (required === "partial" && !any) throw typeError("at least one field is required");
    return result;
  }
  function CalendarFieldKeysToIgnore(calendar, keys) {
    var r = [], eras = calendar !== "iso8601" && CalendarSupportsEra(calendar), mid = calendar === "japanese";
    for (var i = 0; i < keys.length; i++) {
      var k = keys[i];
      r.push(k);
      if (k === "month") r.push("monthCode"); else if (k === "monthCode") r.push("month");
      if (eras && (k === "era" || k === "eraYear" || k === "year")) r.push("era", "eraYear", "year");
      if (mid && (k === "day" || k === "month" || k === "monthCode")) r.push("era", "eraYear");
    }
    return r;
  }
  var FIELD_ORDER = ["era", "eraYear", "year", "month", "monthCode", "day", "hour", "minute", "second", "millisecond",
    "microsecond", "nanosecond", "offset", "timeZone"];
  function CalendarMergeFields(calendar, fields, additional) {
    var addKeys = [];
    for (var i = 0; i < FIELD_ORDER.length; i++) if (additional[FIELD_ORDER[i]] !== undefined) addKeys.push(FIELD_ORDER[i]);
    var overridden = CalendarFieldKeysToIgnore(calendar, addKeys);
    var merged = {};
    for (i = 0; i < FIELD_ORDER.length; i++) {
      var k = FIELD_ORDER[i];
      if (fields[k] !== undefined && overridden.indexOf(k) < 0) merged[k] = fields[k];
      if (additional[k] !== undefined) merged[k] = additional[k];
    }
    return merged;
  }
  function CalendarResolveFields(calendar, fields, type) {
    if (calendar !== "iso8601") return NonISOResolveFields(calendar, fields, type);
    var needsYear = type === "date" || type === "year-month", needsDay = type === "date" || type === "month-day";
    if (needsYear && fields.year === undefined) throw typeError("year is required");
    if (needsDay && fields.day === undefined) throw typeError("day is required");
    if (fields.month === undefined && fields.monthCode === undefined) throw typeError("month or monthCode is required");
    if (fields.monthCode !== undefined) {
      var pm = ParseMonthCode(fields.monthCode);
      if (pm.isLeapMonth) throw rangeError("no leap months in the ISO 8601 calendar");
      if (pm.monthNumber > 12) throw rangeError("invalid monthCode");
      if (fields.month !== undefined && fields.month !== pm.monthNumber) throw rangeError("month and monthCode disagree");
      fields.month = pm.monthNumber;
    }
  }
  function CalendarDateFromFields(calendar, fields, overflow) {
    CalendarResolveFields(calendar, fields, "date");
    var r = calendar === "iso8601" ? RegulateISODate(fields.year, fields.month, fields.day, overflow) :
      NonISOCalendarDateToISO(calendar, fields, overflow);
    if (!ISODateWithinLimits(r)) throw rangeError("date out of range");
    return r;
  }
  function CalendarYearMonthFromFields(calendar, fields, overflow) {
    if (calendar !== "iso8601") {
      CalendarResolveFields(calendar, fields, "year-month");
      fields.day = 1;
      var nr = NonISOCalendarDateToISO(calendar, fields, overflow);
      if (!ISOYearMonthWithinLimits(nr)) throw rangeError("year-month out of range");
      return nr;
    }
    fields.day = 1;
    CalendarResolveFields(calendar, fields, "year-month");
    var r = RegulateISODate(fields.year, fields.month, fields.day, overflow);
    if (!ISOYearMonthWithinLimits(r)) throw rangeError("year-month out of range");
    return r;
  }
  function CalendarMonthDayFromFields(calendar, fields, overflow) {
    CalendarResolveFields(calendar, fields, "month-day");
    if (calendar !== "iso8601") return NonISOMonthDayToISOReferenceDate(calendar, fields, overflow);
    var y = fields.year === undefined ? 1972 : fields.year;
    var r = RegulateISODate(y, fields.month, fields.day, overflow);
    return ISODate(1972, r.month, r.day);
  }
  function CalendarDateAdd(calendar, d, dur, overflow) {
    if (calendar !== "iso8601") return NonISODateAdd(calendar, d, dur, overflow);
    var ym = BalanceISOYearMonth(d.year + dur.years, d.month + dur.months);
    var reg = RegulateISODate(ym.year, ym.month, d.day, overflow);
    var r = AddDaysToISODate(reg, dur.days + 7 * dur.weeks);
    if (!ISODateWithinLimits(r)) throw rangeError("date out of range");
    return r;
  }
  function compareSurpasses(sg, y, m, d, t) {
    if (y !== t.year) return sg * (y - t.year) > 0;
    if (m !== t.month) return sg * (m - t.month) > 0;
    if (d !== t.day) return sg * (d - t.day) > 0;
    return false;
  }
  function ISODateSurpasses(sg, base, t, years, months, weeks, days) {
    var y0 = base.year + years;
    if (compareSurpasses(sg, y0, base.month, base.day, t)) return true;
    if (months === 0) return false;
    var ma = BalanceISOYearMonth(y0, base.month + months);
    if (compareSurpasses(sg, ma.year, ma.month, base.day, t)) return true;
    if (weeks === 0 && days === 0) return false;
    var reg = RegulateISODate(ma.year, ma.month, base.day, "constrain");
    var b = AddDaysToISODate(reg, 7 * weeks + days);
    return compareSurpasses(sg, b.year, b.month, b.day, t);
  }
  // the spec's candidate loop: the largest n (counting by sg) with test(n) false, from a starting estimate
  function largestNotSurpassing(sg, estimate, test) {
    var n = estimate;
    if (n !== 0 && test(n)) { while (n !== 0 && test(n)) n -= sg; return n; }
    while (!test(n + sg)) n += sg;
    return n;
  }
  function CalendarDateUntil(calendar, one, two, largestUnit) {
    if (calendar !== "iso8601") return NonISODateUntil(calendar, one, two, largestUnit);
    var cmp = CompareISODate(one, two);
    if (cmp === 0) return { years: 0, months: 0, weeks: 0, days: 0 };
    var sg = -cmp;
    var years = 0, months = 0, weeks = 0, days = 0;
    if (largestUnit === "year") {
      var ey = two.year - one.year;
      years = largestNotSurpassing(sg, ey, function (n) { return ISODateSurpasses(sg, one, two, n, 0, 0, 0); });
    }
    if (largestUnit === "year" || largestUnit === "month") {
      var em = (two.year - (one.year + years)) * 12 + (two.month - one.month);
      months = largestNotSurpassing(sg, em, function (n) { return ISODateSurpasses(sg, one, two, years, n, 0, 0); });
    }
    // the spec's week and day loops end at the days from the constrained intermediate date (its ISODateSurpasses
    // returns false when months is 0, so the loops are computed directly, as the reference implementation does)
    var start = BalanceISOYearMonth(one.year + years, one.month + months);
    var reg = RegulateISODate(start.year, start.month, one.day, "constrain");
    days = ISODateToEpochDays(two.year, two.month, two.day) - ISODateToEpochDays(reg.year, reg.month, reg.day);
    if (largestUnit === "week") { weeks = MathTrunc(days / 7); days = days - weeks * 7; }
    return CreateDateDurationRecord(years, months, weeks + 0, days + 0);
  }
  function CalendarISOToDate(calendar, d) {
    if (calendar !== "iso8601") return NonISOCalendarISOToDate(calendar, d);
    var w = ISOWeekOfYear(d);
    return {
      era: undefined, eraYear: undefined, year: d.year, month: d.month, monthCode: CreateMonthCode(d.month, false),
      day: d.day, dayOfWeek: ISODayOfWeek(d), dayOfYear: ISODayOfYear(d), weekOfYear: w, daysInWeek: 7,
      daysInMonth: ISODaysInMonth(d.year, d.month), daysInYear: daysInYear(d.year), monthsInYear: 12, inLeapYear: isLeap(d.year)
    };
  }

  // ---------------- non-ISO calendars (the Intl era and monthCode proposal, 4.1) ----------------
  // Table 2: each calendar's eras: [era, aliases, kind] with kind "e" (epoch), "n" (negative) or the offset year
  var ERAS = ObjectCreate(null);
  (function () {
    var islamic = [["ah", [], "e"], ["bh", [], "n"]];
    ERAS.buddhist = [["be", [], "e"]];
    ERAS.coptic = [["am", [], "e"]];
    ERAS.ethioaa = [["aa", [], "e"]];
    ERAS.ethiopic = [["am", [], "e"], ["aa", [], -5499]];
    ERAS.gregory = [["ce", ["ad"], "e"], ["bce", ["bc"], "n"]];
    ERAS.hebrew = [["am", [], "e"]];
    ERAS.indian = [["shaka", [], "e"]];
    ERAS["islamic-civil"] = islamic; ERAS["islamic-tbla"] = islamic; ERAS["islamic-umalqura"] = islamic;
    ERAS.japanese = [["reiwa", [], 2019], ["heisei", [], 1989], ["showa", [], 1926], ["taisho", [], 1912],
      ["meiji", [], 1868], ["ce", ["ad"], "e"], ["bce", ["bc"], "n"]];
    ERAS.persian = [["ap", [], "e"]];
    ERAS.roc = [["roc", [], "e"], ["broc", [], "n"]];
  })();
  function CalendarSupportsEra(cal) { return cal !== "iso8601" && ERAS[cal] !== undefined; }
  function eraRow(cal, era) {
    var rows = ERAS[cal];
    if (rows === undefined) return undefined;
    for (var i = 0; i < rows.length; i++) if (rows[i][0] === era || rows[i][1].indexOf(era) >= 0) return rows[i];
    return undefined;
  }
  function CanonicalizeEraInCalendar(cal, era) { var r = eraRow(cal, era); return r === undefined ? undefined : r[0]; }
  function ArithmeticYearForEraYear(cal, era, eraYear) {
    var k = eraRow(cal, era)[2];
    if (k === "e") return eraYear;
    if (k === "n") return 1 - eraYear;
    return k + eraYear - 1;
  }
  // Table 3: the leap month codes and how a missing one is constrained
  function leapKind(cal) {
    if (cal === "chinese" || cal === "dangi") return "back";
    if (cal === "hebrew") return "fwd";
    return undefined;
  }
  function IsValidMonthCodeForCalendar(cal, mc) {
    var pm = ParseMonthCode(mc), n = pm.monthNumber;
    if (!pm.isLeapMonth) return (n >= 1 && n <= 12) || (n === 13 && (cal === "coptic" || cal === "ethiopic" || cal === "ethioaa"));
    if (cal === "chinese" || cal === "dangi") return n >= 1 && n <= 12;
    return cal === "hebrew" && n === 5;
  }
  function calRangeError() { return rangeError("date outside the range of the calendar"); }
  var calCache = { cal: "", y: 0, m: 0, d: 0, r: null };
  function NonISOCalendarISOToDate(cal, d) {
    if (calCache.cal === cal && calCache.y === d.year && calCache.m === d.month && calCache.d === d.day) return calCache.r;
    var a = calOp(0, cal, d.year, d.month, d.day);
    if (a === null) throw calRangeError();
    var r = {
      era: a[4], eraYear: a[5], year: a[0], month: a[1], monthCode: a[2], day: a[3], dayOfWeek: a[6], dayOfYear: a[7],
      weekOfYear: { week: undefined, year: undefined }, daysInWeek: 7, daysInMonth: a[8], daysInYear: a[9],
      monthsInYear: a[10], inLeapYear: a[11]
    };
    if (!CalendarSupportsEra(cal)) { r.era = undefined; r.eraYear = undefined; }
    calCache = { cal: cal, y: d.year, m: d.month, d: d.day, r: r };
    return r;
  }
  // CalendarIntegersToISO: a RangeError for a date the calendar does not have
  function CalendarIntegersToISO(cal, y, m, d) {
    var a = calOp(1, cal, y, m, false, false, d, false);
    if (a === null) throw calRangeError();
    return ISODate(a[0], a[1], a[2]);
  }
  function CalendarMonthsInYear(cal, y) {
    var n = calOp(5, cal, y);
    if (n === null) n = NonISOCalendarISOToDate(cal, someDayOfMonth(cal, y, 1)).monthsInYear;
    return n;
  }
  // an ISO date in month m of year y: day 1, or (in the months at the edge of the representable range, whose first
  // days lie outside it) the first day of the month that converts
  function someDayOfMonth(cal, y, m) {
    var a = calOp(1, cal, y, m, false, false, 1, false);
    for (var d = 2; a === null && d <= 31; d++) a = calOp(1, cal, y, m, false, false, d, false);
    if (a === null) throw calRangeError();
    return ISODate(a[0], a[1], a[2]);
  }
  function CalendarDaysInMonth(cal, y, m) {
    return NonISOCalendarISOToDate(cal, someDayOfMonth(cal, y, m)).daysInMonth;
  }
  // the ISO date of day 1 of month code mc in year y, or null when the year has no such month
  function monthCodeStart(cal, y, mc) {
    var pm = ParseMonthCode(mc);
    var a = calOp(1, cal, y, pm.monthNumber, true, pm.isLeapMonth, 1, false);
    if (a === null) return null;
    var iso = ISODate(a[0], a[1], a[2]);
    return NonISOCalendarISOToDate(cal, iso).monthCode === mc ? iso : null;
  }
  function YearContainsMonthCode(cal, y, mc) {
    if (!ParseMonthCode(mc).isLeapMonth) return true;
    return monthCodeStart(cal, y, mc) !== null;
  }
  function ConstrainMonthCode(cal, y, mc, overflow) {
    if (YearContainsMonthCode(cal, y, mc)) return mc;
    if (overflow === "reject") throw rangeError("month code " + mc + " does not occur in the year");
    if (leapKind(cal) === "back") return CreateMonthCode(ParseMonthCode(mc).monthNumber, false);
    return "M06";
  }
  function MonthCodeToOrdinal(cal, y, mc) {
    if (leapKind(cal) === undefined) return ParseMonthCode(mc).monthNumber;
    var iso = monthCodeStart(cal, y, mc);
    if (iso === null) throw calRangeError();
    return NonISOCalendarISOToDate(cal, iso).month;
  }
  function BalanceNonISODate(cal, y, m, d) {
    var miy = CalendarMonthsInYear(cal, y);
    while (m <= 0) { y--; miy = CalendarMonthsInYear(cal, y); m += miy; }
    while (m > miy) { m -= miy; y++; miy = CalendarMonthsInYear(cal, y); }
    var dim = CalendarDaysInMonth(cal, y, m);
    while (d <= 0) {
      m--;
      if (m === 0) { y--; miy = CalendarMonthsInYear(cal, y); m = miy; }
      dim = CalendarDaysInMonth(cal, y, m);
      d += dim;
    }
    while (d > dim) {
      d -= dim;
      m++;
      if (m > miy) { y++; miy = CalendarMonthsInYear(cal, y); m = 1; }
      dim = CalendarDaysInMonth(cal, y, m);
    }
    return { year: y, month: m, day: d };
  }
  // CompareSurpasses(sign, year, monthOrCode, day, target)
  function CompareSurpasses(sg, y, mOrCode, d, t) {
    if (y !== t.year) return sg * (y - t.year) > 0;
    if (typeof mOrCode === "string") {
      if (mOrCode !== t.monthCode) return sg > 0 ? mOrCode > t.monthCode : t.monthCode > mOrCode;
    } else if (mOrCode !== t.month) return sg * (mOrCode - t.month) > 0;
    if (d !== t.day) return sg * (d - t.day) > 0;
    return false;
  }
  function NonISODateSurpasses(cal, sg, from, to, years, months, weeks, days) {
    var parts = NonISOCalendarISOToDate(cal, from), cd2 = NonISOCalendarISOToDate(cal, to);
    var y0 = parts.year + years;
    if (CompareSurpasses(sg, y0, parts.monthCode, parts.day, cd2)) return true;
    var m0 = MonthCodeToOrdinal(cal, y0, ConstrainMonthCode(cal, y0, parts.monthCode, "constrain"));
    var ma = BalanceNonISODate(cal, y0, m0 + months, 1);
    if (CompareSurpasses(sg, ma.year, ma.month, parts.day, cd2)) return true;
    if (weeks === 0 && days === 0) return false;
    var eom = BalanceNonISODate(cal, ma.year, ma.month + 1, 0);
    var rd = parts.day <= eom.day ? parts.day : eom.day;
    var b = BalanceNonISODate(cal, eom.year, eom.month, rd + 7 * weeks + days);
    return CompareSurpasses(sg, b.year, b.month, b.day, cd2);
  }
  function NonISODateAdd(cal, d, dur, overflow) {
    var parts = NonISOCalendarISOToDate(cal, d);
    var y0 = parts.year + dur.years;
    var m0 = MonthCodeToOrdinal(cal, y0, ConstrainMonthCode(cal, y0, parts.monthCode, overflow));
    var eom = BalanceNonISODate(cal, y0, m0 + dur.months + 1, 0);
    var rd = parts.day;
    if (rd > eom.day) {
      if (overflow === "reject") throw rangeError("the day does not occur in that month");
      rd = eom.day;
    }
    var r;
    if (dur.weeks === 0 && dur.days === 0) r = CalendarIntegersToISO(cal, eom.year, eom.month, rd);
    else {
      // days and weeks move along the ISO days of the regulated date
      r = AddDaysToISODate(CalendarIntegersToISO(cal, eom.year, eom.month, rd), 7 * dur.weeks + dur.days);
    }
    if (!ISODateWithinLimits(r)) throw rangeError("date out of range");
    return r;
  }
  function NonISODateUntil(cal, one, two, largestUnit) {
    var sg = -CompareISODate(one, two);
    if (sg === 0) return ZeroDateDuration();
    var years = 0, months = 0, weeks = 0, days = 0;
    var p1 = NonISOCalendarISOToDate(cal, one), p2 = NonISOCalendarISOToDate(cal, two);
    if (largestUnit === "year") {
      years = largestNotSurpassing(sg, p2.year - p1.year, function (n) { return NonISODateSurpasses(cal, sg, one, two, n, 0, 0, 0); });
    }
    if (largestUnit === "year" || largestUnit === "month") {
      var em = largestUnit === "month" ? (p2.year - p1.year) * 12 + (p2.month - p1.month) : 0;
      months = largestNotSurpassing(sg, em, function (n) { return NonISODateSurpasses(cal, sg, one, two, years, n, 0, 0); });
    }
    // the week and day loops count ISO days from the regulated intermediate date
    var y0 = p1.year + years;
    var m0 = MonthCodeToOrdinal(cal, y0, ConstrainMonthCode(cal, y0, p1.monthCode, "constrain"));
    var eom = BalanceNonISODate(cal, y0, m0 + months + 1, 0);
    var base = CalendarIntegersToISO(cal, eom.year, eom.month, p1.day <= eom.day ? p1.day : eom.day);
    days = ISODateToEpochDays(two.year, two.month, two.day) - ISODateToEpochDays(base.year, base.month, base.day);
    if (largestUnit === "week") { weeks = MathTrunc(days / 7); days = days - weeks * 7; }
    return CreateDateDurationRecord(years, months, weeks + 0, days + 0);
  }
  function NonISOResolveFields(cal, f, type) {
    var needsYear = type === "date" || type === "year-month" || f.monthCode === undefined || f.month !== undefined;
    var needsDay = type === "date" || type === "month-day";
    var eras = CalendarSupportsEra(cal);
    if (needsYear) {
      if (f.year === undefined) {
        if (!eras) throw typeError("year is required");
        if (f.era === undefined || f.eraYear === undefined) throw typeError("year, or era and eraYear, are required");
      }
      if (eras) {
        if (f.era !== undefined && f.eraYear === undefined) throw typeError("eraYear is required with era");
        if (f.eraYear !== undefined && f.era === undefined) throw typeError("era is required with eraYear");
      }
    }
    if (needsDay && f.day === undefined) throw typeError("day is required");
    if (f.month === undefined && f.monthCode === undefined) throw typeError("month or monthCode is required");
    if (eras && f.eraYear !== undefined) {
      var era = CanonicalizeEraInCalendar(cal, f.era);
      if (era === undefined) throw rangeError("invalid era: " + f.era);
      var ay = ArithmeticYearForEraYear(cal, era, f.eraYear);
      if (f.year !== undefined && f.year !== ay) throw rangeError("year and era/eraYear disagree");
      f.year = ay;
      f.era = undefined; f.eraYear = undefined;
    }
    if (f.monthCode !== undefined) {
      if (!IsValidMonthCodeForCalendar(cal, f.monthCode)) throw rangeError("invalid monthCode for " + cal + ": " + f.monthCode);
      if (f.year !== undefined) {
        var cmc = YearContainsMonthCode(cal, f.year, f.monthCode) ? f.monthCode : ConstrainMonthCode(cal, f.year, f.monthCode, "constrain");
        var m = MonthCodeToOrdinal(cal, f.year, cmc);
        if (f.month !== undefined && f.month !== m) throw rangeError("month and monthCode disagree");
        f.month = m;
      }
    }
  }
  // the calendar's arithmetic years in which an ISO date can lie (dates beyond them throw)
  var yearBounds = ObjectCreate(null);
  function yearInRange(cal, y) {
    var b = yearBounds[cal];
    if (b === undefined) {
      var lo = -1e9, hi = 1e9;
      try { lo = NonISOCalendarISOToDate(cal, ISODate(-271821, 4, 19)).year; } catch (e) { }
      try { hi = NonISOCalendarISOToDate(cal, ISODate(275760, 9, 13)).year; } catch (e) { }
      b = yearBounds[cal] = [lo, hi];
    }
    return y >= b[0] && y <= b[1];
  }
  function NonISOCalendarDateToISO(cal, f, overflow) {
    if (!yearInRange(cal, f.year)) throw rangeError("date out of range");
    if (f.monthCode !== undefined) ConstrainMonthCode(cal, f.year, f.monthCode, overflow);
    var miy = CalendarMonthsInYear(cal, f.year), month = f.month;
    if (month > miy) { if (overflow === "reject") throw rangeError("month out of range"); month = miy; }
    var dim = CalendarDaysInMonth(cal, f.year, month), day = f.day;
    if (day > dim) { if (overflow === "reject") throw rangeError("day out of range"); day = dim; }
    return CalendarIntegersToISO(cal, f.year, month, day);
  }
  // Table 6: the reference years of chinese and dangi month codes: [days 1-29, day 30] (null: none)
  var CHINESE_REF = {
    M01: [1972, 1970], M01L: [null, null], M02: [1972, 1972], M02L: [1947, null], M03: [1972, 1966], M03L: [1966, 1955],
    M04: [1972, 1970], M04L: [1963, 1944], M05: [1972, 1972], M05L: [1971, 1952], M06: [1972, 1971], M06L: [1960, 1941],
    M07: [1972, 1972], M07L: [1968, 1938], M08: [1972, 1971], M08L: [1957, null], M09: [1972, 1972], M09L: [2014, null],
    M10: [1972, 1972], M10L: [1984, null], M11: [1972, 1970], M11L: [2034, null], M12: [1972, 1972], M12L: [null, null]
  };
  // the ISO date of (mc, day) in the calendar year containing (or neighbouring) ISO year isoYear that lies in isoYear,
  // the latest one; null if none
  function monthDayInISOYear(cal, isoYear, mc, day) {
    var best = null;
    var cy = NonISOCalendarISOToDate(cal, ISODate(isoYear, 12, 31)).year;
    for (var y = cy; y >= cy - 1; y--) {
      var st = monthCodeStart(cal, y, mc);
      if (st === null) continue;
      if (day > NonISOCalendarISOToDate(cal, st).daysInMonth) continue;
      var iso = AddDaysToISODate(st, day - 1);
      if (iso.year === isoYear && (best === null || CompareISODate(iso, best) > 0)) best = iso;
    }
    return best;
  }
  function maxDaysInMonthCode(cal, mc) {
    var cy = NonISOCalendarISOToDate(cal, ISODate(1972, 1, 1)).year, max = 0;
    for (var y = cy - 12; y <= cy + 12; y++) {
      var st = monthCodeStart(cal, y, mc);
      if (st !== null) { var n = NonISOCalendarISOToDate(cal, st).daysInMonth; if (n > max) max = n; }
    }
    return max;
  }
  function NonISOMonthDayToISOReferenceDate(cal, f, overflow) {
    var mc, dim, chinese = cal === "chinese" || cal === "dangi";
    if (f.year !== undefined) {
      if (!yearInRange(cal, f.year)) throw rangeError("year out of range");
      var miy = CalendarMonthsInYear(cal, f.year), month = f.month;
      if (month > miy) { if (overflow === "reject") throw rangeError("month out of range"); month = miy; }
      if (f.monthCode === undefined) mc = NonISOCalendarISOToDate(cal, someDayOfMonth(cal, f.year, month)).monthCode;
      else mc = ConstrainMonthCode(cal, f.year, f.monthCode, overflow);
      dim = CalendarDaysInMonth(cal, f.year, month);
    } else {
      mc = f.monthCode;
      dim = chinese ? 30 : maxDaysInMonthCode(cal, mc);
    }
    var day = f.day;
    if (day > dim) { if (overflow === "reject") throw rangeError("day out of range"); day = dim; }
    var r;
    if (chinese) {
      var row = CHINESE_REF[mc];
      var refYear = day === 30 ? row[1] : row[0];
      if (mc === "M03" && day === 30 && cal === "dangi") refYear = 1968;
      if (mc === "M11L" && day <= 10) refYear = 2033;
      if (refYear === null) {
        if (overflow === "reject") throw rangeError("that month and day are not known to occur");
        mc = CreateMonthCode(ParseMonthCode(mc).monthNumber, false);
        row = CHINESE_REF[mc];
        refYear = day === 30 ? row[1] : row[0];
        if (mc === "M03" && day === 30 && cal === "dangi") refYear = 1968;
      }
      r = monthDayInISOYear(cal, refYear, mc, day);
      if (r === null) throw calRangeError();
      return r;
    }
    for (var y = 1972; y >= 1900; y--) { r = monthDayInISOYear(cal, y, mc, day); if (r !== null) return r; }
    for (y = 1973; y <= 2035; y++) {
      // the earliest in the year
      var cy = NonISOCalendarISOToDate(cal, ISODate(y, 1, 1)).year;
      for (var k = cy; k <= cy + 1; k++) {
        var st = monthCodeStart(cal, k, mc);
        if (st === null || day > NonISOCalendarISOToDate(cal, st).daysInMonth) continue;
        var iso = AddDaysToISODate(st, day - 1);
        if (iso.year === y) return iso;
      }
    }
    throw calRangeError();
  }

  function ISODateToFields(calendar, d, type) {
    var f = {};
    var cd = CalendarISOToDate(calendar, d);
    f.monthCode = cd.monthCode;
    if (type === "month-day" || type === "date") f.day = cd.day;
    if (type === "year-month" || type === "date") f.year = cd.year;
    return f;
  }
  function ToTemporalCalendarIdentifier(v) {
    if (isObject(v)) {
      var c = calendarSlotOf(v);
      if (c !== undefined) return c;
    }
    if (typeof v !== "string") throw typeError("calendar must be a string");
    return CanonicalizeCalendar(ParseTemporalCalendarString(v));
  }
  function GetTemporalCalendarIdentifierWithISODefault(item) {
    var c = calendarSlotOf(item);
    if (c !== undefined) return c;
    var cl = item.calendar;
    if (cl === undefined) return "iso8601";
    return ToTemporalCalendarIdentifier(cl);
  }

  // ---------------- duration records ----------------
  function DateDuration(y, mo, w, d) { return { years: y, months: mo, weeks: w, days: d }; }
  function ZeroDateDuration() { return DateDuration(0, 0, 0, 0); }
  function CreateDateDurationRecord(y, mo, w, d) {
    if (!IsValidDuration(y, mo, w, d, 0, 0, 0, 0, 0, 0)) throw rangeError("duration out of range");
    return DateDuration(y + 0, mo + 0, w + 0, d + 0);
  }
  function AdjustDateDurationRecord(dd, days, weeks, months) {
    return CreateDateDurationRecord(dd.years, months === undefined ? dd.months : months, weeks === undefined ? dd.weeks : weeks, days);
  }
  function DateDurationSign(dd) {
    var v = [dd.years, dd.months, dd.weeks, dd.days];
    for (var i = 0; i < 4; i++) { if (v[i] < 0) return -1; if (v[i] > 0) return 1; }
    return 0;
  }
  function Internal(dd, t) { return { date: dd, time: t }; }
  function InternalDurationSign(d) { var s = DateDurationSign(d.date); return s !== 0 ? s : bsign(d.time); }
  // exact nanoseconds of h/mi/s/ms/us/ns Numbers (which may be unsafe integers)
  function bigOf(n) { return Big(n); }
  function TimeDurationFromComponents(h, mi, s, ms, us, ns) {
    return bigOf(h) * 3600000000000n + bigOf(mi) * 60000000000n + bigOf(s) * 1000000000n + bigOf(ms) * 1000000n +
      bigOf(us) * 1000n + bigOf(ns);
  }
  function AddTimeDuration(a, b) { var r = a + b; if (babs(r) > maxTimeDuration) throw rangeError("duration out of range"); return r; }
  function Add24HourDaysToTimeDuration(d, days) {
    var r = d + bigOf(days) * nsPerDay;
    if (babs(r) > maxTimeDuration) throw rangeError("duration out of range");
    return r;
  }
  function IsValidDuration(y, mo, w, d, h, mi, s, ms, us, ns) {
    var v = [y, mo, w, d, h, mi, s, ms, us, ns], sg = 0;
    for (var i = 0; i < 10; i++) {
      if (!NumberIsFinite(v[i])) return false;
      if (v[i] < 0) { if (sg > 0) return false; sg = -1; }
      else if (v[i] > 0) { if (sg < 0) return false; sg = 1; }
    }
    if (MathAbs(y) >= 4294967296 || MathAbs(mo) >= 4294967296 || MathAbs(w) >= 4294967296) return false;
    var total = bigOf(d) * nsPerDay + TimeDurationFromComponents(h, mi, s, ms, us, ns);
    if (babs(total) > maxTimeDuration) return false;
    return true;
  }
  function DefaultTemporalLargestUnitOf(r) {
    var v = [r.years, r.months, r.weeks, r.days, r.hours, r.minutes, r.seconds, r.milliseconds, r.microseconds, r.nanoseconds];
    for (var i = 0; i < 10; i++) if (v[i] !== 0) return UNITS[i];
    return "nanosecond";
  }
  function ToInternalDurationRecord(r) {
    return Internal(DateDuration(r.years, r.months, r.weeks, r.days),
      TimeDurationFromComponents(r.hours, r.minutes, r.seconds, r.milliseconds, r.microseconds, r.nanoseconds));
  }
  function ToInternalDurationRecordWith24HourDays(r) {
    var t = TimeDurationFromComponents(r.hours, r.minutes, r.seconds, r.milliseconds, r.microseconds, r.nanoseconds);
    t = Add24HourDaysToTimeDuration(t, r.days);
    return Internal(DateDuration(r.years, r.months, r.weeks, 0), t);
  }
  function ToDateDurationRecordWithoutTime(r) {
    var i = ToInternalDurationRecordWith24HourDays(r);
    var days = Num(i.time / nsPerDay);
    return CreateDateDurationRecord(i.date.years, i.date.months, i.date.weeks, days);
  }
  // TemporalDurationFromInternal: balance the time part up to largestUnit
  function TemporalDurationFromInternal(d, largestUnit) {
    var sg = bsign(d.time), n = babs(d.time);
    var days = 0n, hours = 0n, minutes = 0n, seconds = 0n, ms = 0n, us = 0n;
    var li = unitIndex(largestUnit);
    if (li <= 6) {
      us = n / 1000n; n %= 1000n; ms = us / 1000n; us %= 1000n; seconds = ms / 1000n; ms %= 1000n;
      if (li <= 5) { minutes = seconds / 60n; seconds %= 60n; }
      if (li <= 4) { hours = minutes / 60n; minutes %= 60n; }
      if (li <= 3) { days = hours / 24n; hours %= 24n; }
    } else if (li === 7) { us = n / 1000n; n %= 1000n; ms = us / 1000n; us %= 1000n; }
    else if (li === 8) { us = n / 1000n; n %= 1000n; }
    function f(b) { return sg * Num(b); }
    return CreateTemporalDuration(d.date.years, d.date.months, d.date.weeks, d.date.days + f(days), f(hours),
      f(minutes), f(seconds), f(ms), f(us), f(n));
  }
  function RoundTimeDuration(t, increment, unit, mode) {
    var r = roundBigToIncrement(t, unitNs(unit) * Big(increment), mode);
    if (babs(r) > maxTimeDuration) throw rangeError("duration out of range");
    return r;
  }
  function TotalTimeDuration(t, unit) { return ratToNumber(t, unitNs(unit)); }
  function DifferenceTime(a, b) { return TimeToNs(b) - TimeToNs(a); }
  function RoundTime(t, increment, unit, mode) {
    var q;
    switch (unit) {
      case "day": case "hour": q = TimeToNs(t); break;
      case "minute": q = TimeToNs(t) - Big(t.hour) * 3600000000000n; break;
      case "second": q = TimeToNs(t) - Big(t.hour * 60 + t.minute) * 60000000000n; break;
      case "millisecond": q = Big(t.millisecond) * 1000000n + Big(t.microsecond) * 1000n + Big(t.nanosecond); break;
      case "microsecond": q = Big(t.microsecond) * 1000n + Big(t.nanosecond); break;
      default: q = Big(t.nanosecond);
    }
    var len = unit === "day" ? nsPerDay : unitNs(unit);
    var r = roundBigToIncrement(q, Big(increment) * len, mode);
    if (unit === "day") { var dt = MidnightTime(); dt.days = Num(r / nsPerDay); return dt; }
    var base;
    switch (unit) {
      case "hour": base = 0n; break;
      case "minute": base = Big(t.hour) * 3600000000000n; break;
      case "second": base = Big(t.hour * 60 + t.minute) * 60000000000n; break;
      case "millisecond": base = Big((t.hour * 60 + t.minute) * 60 + t.second) * 1000000000n; break;
      case "microsecond": base = Big((t.hour * 60 + t.minute) * 60 + t.second) * 1000000000n + Big(t.millisecond) * 1000000n; break;
      default: base = TimeToNs(t) - Big(t.nanosecond);
    }
    return BalanceTimeNs(base + r);
  }
  function RoundISODateTime(dt, increment, unit, mode) {
    var rt = RoundTime(dt.time, increment, unit, mode);
    var d = AddDaysToISODate(dt.isoDate, rt.days);
    rt.days = 0;
    return DateTime(d, rt);
  }
  function DifferenceISODateTime(a, b, calendar, largestUnit) {
    var td = DifferenceTime(a.time, b.time);
    var ts = bsign(td), ds = CompareISODate(a.isoDate, b.isoDate);
    var adjusted = b.isoDate;
    if (ts === ds) {
      adjusted = AddDaysToISODate(adjusted, ts);
      td = Add24HourDaysToTimeDuration(td, -ts);
    }
    var dlu = LargerOfTwoTemporalUnits("day", largestUnit);
    var dd = CalendarDateUntil(calendar, a.isoDate, adjusted, dlu);
    if (largestUnit !== dlu) {
      td = Add24HourDaysToTimeDuration(td, dd.days);
      dd = DateDuration(dd.years, dd.months, dd.weeks, 0);
    }
    return Internal(dd, td);
  }
  // ---------------- relative rounding (nudges) ----------------
  function epochNsForDateTime(dt, tz) { return tz === undefined ? GetUTCEpochNanoseconds(dt) : GetEpochNanosecondsFor(tz, dt, "compatible"); }
  function ComputeNudgeWindow(sg, d, originNs, dt, tz, calendar, increment, unit, additionalShift) {
    var r1, r2, sdd, edd;
    if (unit === "year") {
      var years = roundNumToIncrement(d.date.years, increment, "trunc");
      r1 = additionalShift ? years + increment * sg : years;
      r2 = r1 + increment * sg;
      sdd = CreateDateDurationRecord(r1, 0, 0, 0); edd = CreateDateDurationRecord(r2, 0, 0, 0);
    } else if (unit === "month") {
      var months = roundNumToIncrement(d.date.months, increment, "trunc");
      r1 = additionalShift ? months + increment * sg : months;
      r2 = r1 + increment * sg;
      sdd = AdjustDateDurationRecord(d.date, 0, 0, r1); edd = AdjustDateDurationRecord(d.date, 0, 0, r2);
    } else if (unit === "week") {
      var ym = AdjustDateDurationRecord(d.date, 0, 0);
      var ws = CalendarDateAdd(calendar, dt.isoDate, ym, "constrain");
      var we = AddDaysToISODate(ws, d.date.days);
      var u = CalendarDateUntil(calendar, ws, we, "week");
      var weeks = roundNumToIncrement(d.date.weeks + u.weeks, increment, "trunc");
      r1 = weeks; r2 = weeks + increment * sg;
      sdd = AdjustDateDurationRecord(d.date, 0, r1); edd = AdjustDateDurationRecord(d.date, 0, r2);
    } else {
      var days = roundNumToIncrement(d.date.days, increment, "trunc");
      r1 = days; r2 = days + increment * sg;
      sdd = AdjustDateDurationRecord(d.date, r1); edd = AdjustDateDurationRecord(d.date, r2);
    }
    var startNs;
    if (DateDurationSign(sdd) === 0) startNs = originNs;
    else startNs = epochNsForDateTime(DateTime(CalendarDateAdd(calendar, dt.isoDate, sdd, "constrain"), dt.time), tz);
    var endNs = epochNsForDateTime(DateTime(CalendarDateAdd(calendar, dt.isoDate, edd, "constrain"), dt.time), tz);
    return { r1: r1, r2: r2, startNs: startNs, endNs: endNs, startDuration: Internal(sdd, 0n), endDuration: Internal(edd, 0n) };
  }
  function NudgeToCalendarUnit(sg, d, originNs, destNs, dt, tz, calendar, increment, unit, mode) {
    var expanded = false;
    var w = ComputeNudgeWindow(sg, d, originNs, dt, tz, calendar, increment, unit, false);
    var inside = sg === 1 ? (w.startNs <= destNs && destNs <= w.endNs) : (w.endNs <= destNs && destNs <= w.startNs);
    if (!inside) { w = ComputeNudgeWindow(sg, d, originNs, dt, tz, calendar, increment, unit, true); expanded = true; }
    // progress = (dest - start) / (end - start); total = r1 + progress * increment * sg
    var num = destNs - w.startNs, den = w.endNs - w.startNs;
    var totalNum = Big(w.r1) * den + num * Big(increment * sg);
    var total = ratToNumber(totalNum, den);
    var um = GetUnsignedRoundingMode(mode, sg < 0);
    var roundedUp;
    if (num === den) roundedUp = true;
    else {
      // |total| between |r1| and |r2|: compare (|total| - |r1|) with the half increment, exactly
      var frac = babs(num * Big(increment));            // (|total| - |r1|) * den
      var half = babs(den) * Big(increment);             // the increment * den, twice the half
      var twice = frac * 2n;
      var cmpHalf = twice < half ? -1 : twice > half ? 1 : 0;
      if (frac === 0n) roundedUp = false;
      else roundedUp = applyUnsigned(cmpHalf, (MathAbs(w.r1) / increment) % 2 === 0, um);
    }
    var res;
    if (roundedUp) { expanded = true; res = { duration: w.endDuration, nudgedNs: w.endNs, expanded: expanded }; }
    else res = { duration: w.startDuration, nudgedNs: w.startNs, expanded: expanded };
    return { nudge: res, total: total };
  }
  function NudgeToZonedTime(sg, d, dt, tz, calendar, increment, unit, mode) {
    var start = CalendarDateAdd(calendar, dt.isoDate, d.date, "constrain");
    var sdt = DateTime(start, dt.time), edt = DateTime(AddDaysToISODate(start, sg), dt.time);
    var startNs = GetEpochNanosecondsFor(tz, sdt, "compatible"), endNs = GetEpochNanosecondsFor(tz, edt, "compatible");
    var daySpan = endNs - startNs;
    var len = unitNs(unit);
    var rounded = roundBigToIncrement(d.time, Big(increment) * len, mode);
    if (babs(rounded) > maxTimeDuration) throw rangeError("duration out of range");
    var beyond = rounded - daySpan;
    var didBeyond, dayDelta, nudged;
    if (bsign(beyond) !== -sg) {
      didBeyond = true; dayDelta = sg;
      rounded = roundBigToIncrement(beyond, Big(increment) * len, mode);
      if (babs(rounded) > maxTimeDuration) throw rangeError("duration out of range");
      nudged = endNs + rounded;
    } else { didBeyond = false; dayDelta = 0; nudged = startNs + rounded; }
    var dd = AdjustDateDurationRecord(d.date, d.date.days + dayDelta);
    return { duration: Internal(dd, rounded), nudgedNs: nudged, expanded: didBeyond };
  }
  function NudgeToDayOrTime(d, destNs, largestUnit, increment, smallestUnit, mode) {
    var t = Add24HourDaysToTimeDuration(d.time, d.date.days);
    var len = smallestUnit === "day" ? nsPerDay : unitNs(smallestUnit);
    var rounded = roundBigToIncrement(t, len * Big(increment), mode);
    if (babs(rounded) > maxTimeDuration) throw rangeError("duration out of range");
    var diff = rounded - t;
    var wholeDays = t / nsPerDay, roundedWholeDays = rounded / nsPerDay;
    var dayDelta = roundedWholeDays - wholeDays;
    var dds = bsign(dayDelta);
    var expanded = dds === bsign(t);
    var nudged = destNs + diff;
    var days = 0n, rem = rounded;
    if (IsDateUnit(largestUnit)) { days = roundedWholeDays; rem = rounded - roundedWholeDays * nsPerDay; }
    var dd = AdjustDateDurationRecord(d.date, Num(days));
    return { duration: Internal(dd, rem), nudgedNs: nudged, expanded: expanded };
  }
  function BubbleRelativeDuration(sg, d, nudgedNs, dt, tz, calendar, largestUnit, smallestUnit) {
    if (smallestUnit === largestUnit) return d;
    var li = unitIndex(largestUnit), si = unitIndex(smallestUnit);
    for (var ui = si - 1; ui >= li; ui--) {
      var u = UNITS[ui];
      if (u === "week" && largestUnit !== "week") continue;
      var ed;
      if (u === "year") ed = CreateDateDurationRecord(d.date.years + sg, 0, 0, 0);
      else if (u === "month") ed = AdjustDateDurationRecord(d.date, 0, 0, d.date.months + sg);
      else ed = AdjustDateDurationRecord(d.date, 0, d.date.weeks + sg);
      var end = CalendarDateAdd(calendar, dt.isoDate, ed, "constrain");
      var endNs = epochNsForDateTime(DateTime(end, dt.time), tz);
      var beyond = nudgedNs - endNs;
      if (bsign(beyond) !== -sg) d = Internal(ed, 0n);
      else break;
    }
    return d;
  }
  function RoundRelativeDuration(d, originNs, destNs, dt, tz, calendar, largestUnit, increment, smallestUnit, mode) {
    var irregular = IsCalendarUnit(smallestUnit) || (tz !== undefined && smallestUnit === "day");
    var sg = InternalDurationSign(d) < 0 ? -1 : 1;
    var nr;
    if (irregular) nr = NudgeToCalendarUnit(sg, d, originNs, destNs, dt, tz, calendar, increment, smallestUnit, mode).nudge;
    else if (tz !== undefined) nr = NudgeToZonedTime(sg, d, dt, tz, calendar, increment, smallestUnit, mode);
    else nr = NudgeToDayOrTime(d, destNs, largestUnit, increment, smallestUnit, mode);
    d = nr.duration;
    if (nr.expanded && smallestUnit !== "week") {
      var startUnit = LargerOfTwoTemporalUnits(smallestUnit, "day");
      d = BubbleRelativeDuration(sg, d, nr.nudgedNs, dt, tz, calendar, largestUnit, startUnit);
    }
    return d;
  }
  function TotalRelativeDuration(d, originNs, destNs, dt, tz, calendar, unit) {
    if (IsCalendarUnit(unit) || (tz !== undefined && unit === "day")) {
      var sg = InternalDurationSign(d) < 0 ? -1 : 1;
      return NudgeToCalendarUnit(sg, d, originNs, destNs, dt, tz, calendar, 1, unit, "trunc").total;
    }
    var t = Add24HourDaysToTimeDuration(d.time, d.date.days);
    return TotalTimeDuration(t, unit);
  }
  function DifferencePlainDateTimeWithRounding(a, b, calendar, largestUnit, increment, smallestUnit, mode) {
    if (CompareISODateTime(a, b) === 0) return Internal(ZeroDateDuration(), 0n);
    if (!ISODateTimeWithinLimits(a) || !ISODateTimeWithinLimits(b)) throw rangeError("date-time out of range");
    var diff = DifferenceISODateTime(a, b, calendar, largestUnit);
    if (smallestUnit === "nanosecond" && increment === 1) return diff;
    return RoundRelativeDuration(diff, GetUTCEpochNanoseconds(a), GetUTCEpochNanoseconds(b), a, undefined, calendar,
      largestUnit, increment, smallestUnit, mode);
  }
  function DifferencePlainDateTimeWithTotal(a, b, calendar, unit) {
    if (CompareISODateTime(a, b) === 0) return 0;
    if (!ISODateTimeWithinLimits(a) || !ISODateTimeWithinLimits(b)) throw rangeError("date-time out of range");
    var diff = DifferenceISODateTime(a, b, calendar, unit);
    if (unit === "nanosecond") return Num(diff.time);
    return TotalRelativeDuration(diff, GetUTCEpochNanoseconds(a), GetUTCEpochNanoseconds(b), a, undefined, calendar, unit);
  }
  function DifferenceInstant(ns1, ns2, increment, smallestUnit, mode) {
    return Internal(ZeroDateDuration(), RoundTimeDuration(ns2 - ns1, increment, smallestUnit, mode));
  }
  function AddInstant(ns, t) { var r = ns + t; if (!IsValidEpochNanoseconds(r)) throw rangeError("instant out of range"); return r; }
  function DifferenceZonedDateTime(ns1, ns2, tz, calendar, largestUnit) {
    if (ns1 === ns2) return Internal(ZeroDateDuration(), 0n);
    var sdt = GetISODateTimeFor(tz, ns1), edt = GetISODateTimeFor(tz, ns2);
    if (CompareISODate(sdt.isoDate, edt.isoDate) === 0) return Internal(ZeroDateDuration(), ns2 - ns1);
    var sg = ns2 - ns1 < 0n ? 1 : -1;
    var maxCorr = sg === -1 ? 2 : 1, corr = 0;
    var td = DifferenceTime(sdt.time, edt.time);
    if (bsign(td) === sg) corr++;
    var ok = false, idt;
    while (corr <= maxCorr && !ok) {
      idt = DateTime(AddDaysToISODate(edt.isoDate, corr * sg), sdt.time);
      var ins = GetEpochNanosecondsFor(tz, idt, "compatible");
      td = ns2 - ins;
      if (sg !== bsign(td)) ok = true;
      corr++;
    }
    var dlu = LargerOfTwoTemporalUnits(largestUnit, "day");
    var dd = CalendarDateUntil(calendar, sdt.isoDate, idt.isoDate, dlu);
    return Internal(dd, td);
  }
  function DifferenceZonedDateTimeWithRounding(ns1, ns2, tz, calendar, largestUnit, increment, smallestUnit, mode) {
    if (!IsDateUnit(largestUnit)) return DifferenceInstant(ns1, ns2, increment, smallestUnit, mode);
    var diff = DifferenceZonedDateTime(ns1, ns2, tz, calendar, largestUnit);
    if (smallestUnit === "nanosecond" && increment === 1) return diff;
    return RoundRelativeDuration(diff, ns1, ns2, GetISODateTimeFor(tz, ns1), tz, calendar, largestUnit, increment, smallestUnit, mode);
  }
  function DifferenceZonedDateTimeWithTotal(ns1, ns2, tz, calendar, unit) {
    if (!IsDateUnit(unit)) return TotalTimeDuration(ns2 - ns1, unit);
    var diff = DifferenceZonedDateTime(ns1, ns2, tz, calendar, unit);
    return TotalRelativeDuration(diff, ns1, ns2, GetISODateTimeFor(tz, ns1), tz, calendar, unit);
  }
  function AddZonedDateTime(ns, tz, calendar, d, overflow) {
    if (DateDurationSign(d.date) === 0) return AddInstant(ns, d.time);
    var dt = GetISODateTimeFor(tz, ns);
    var added = CalendarDateAdd(calendar, dt.isoDate, d.date, overflow);
    var idt = DateTime(added, dt.time);
    if (!ISODateTimeWithinLimits(idt)) throw rangeError("date-time out of range");
    return AddInstant(GetEpochNanosecondsFor(tz, idt, "compatible"), d.time);
  }
  function InterpretISODateTimeOffset(d, time, offsetBehaviour, offsetNs, tz, disambiguation, offsetOption, matchBehaviour) {
    if (time === "start-of-day") return GetStartOfDay(tz, d);
    var dt = DateTime(d, time);
    if (offsetBehaviour === "wall" || (offsetBehaviour === "option" && offsetOption === "ignore"))
      return GetEpochNanosecondsFor(tz, dt, disambiguation);
    if (offsetBehaviour === "exact" || (offsetBehaviour === "option" && offsetOption === "use")) {
      var ns = GetUTCEpochNanoseconds(dt) - Big(offsetNs);
      CheckISODaysRange(epochNsToDateTime(ns).isoDate);
      if (!IsValidEpochNanoseconds(ns)) throw rangeError("instant out of range");
      return ns;
    }
    CheckISODaysRange(d);
    var utc = GetUTCEpochNanoseconds(dt);
    var poss = GetPossibleEpochNanoseconds(tz, dt);
    for (var i = 0; i < poss.length; i++) {
      var co = Num(utc - poss[i]);
      if (co === offsetNs) return poss[i];
      if (matchBehaviour === "match-minutes") {
        var rc = Num(roundBigToIncrement(Big(co), 60000000000n, "halfExpand"));
        if (rc === offsetNs) return poss[i];
      }
    }
    if (offsetOption === "reject") throw rangeError("offset does not match the time zone");
    return DisambiguatePossibleEpochNanoseconds(poss, tz, dt, disambiguation);
  }
  function GetDifferenceSettings(op, o, group, disallowed, fallbackSmallest, smallestLargestDefault) {
    var largest = GetTemporalUnitValuedOption(o, "largestUnit", undefined);
    var inc = GetRoundingIncrementOption(o);
    var mode = GetRoundingModeOption(o, "trunc");
    var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
    ValidateTemporalUnitValue(largest, group, ["auto"]);
    if (largest === undefined) largest = "auto";
    if (disallowed.indexOf(largest) >= 0) throw rangeError(largest + " is not allowed");
    ValidateTemporalUnitValue(smallest, group);
    if (smallest === undefined) smallest = fallbackSmallest;
    if (disallowed.indexOf(smallest) >= 0) throw rangeError(smallest + " is not allowed");
    var dl = LargerOfTwoTemporalUnits(smallestLargestDefault, smallest);
    if (largest === "auto") largest = dl;
    if (LargerOfTwoTemporalUnits(largest, smallest) !== largest) throw rangeError("largestUnit is smaller than smallestUnit");
    var max = MaximumTemporalDurationRoundingIncrement(smallest);
    if (max !== undefined) ValidateTemporalRoundingIncrement(inc, max, false);
    if (op === "since") mode = NegateRoundingMode(mode);
    return { smallestUnit: smallest, largestUnit: largest, roundingMode: mode, roundingIncrement: inc };
  }

  // ---------------- classes ----------------
  var TOKEN = {};   // internal creation: new C(TOKEN, slots) skips the constructor's argument conversion
  function tagged(C, tag) { ObjectDefineProperty(C.prototype, SymToStringTag, { value: tag, writable: false, enumerable: false, configurable: true }); }
  function throwValueOf() { throw typeError("use compare() or equals() to compare Temporal values"); }
  var isDuration, getDur, isPlainTime, getPT, isPlainDate, getPD, isPlainDateTime, getPDT, isPlainYearMonth, getPYM,
    isPlainMonthDay, getPMD, isInstant, getInst, isZonedDateTime, getZDT;
  function calendarSlotOf(v) {
    if (!isObject(v)) return undefined;
    if (isPlainDate(v)) return getPD(v).calendar;
    if (isPlainDateTime(v)) return getPDT(v).calendar;
    if (isPlainYearMonth(v)) return getPYM(v).calendar;
    if (isPlainMonthDay(v)) return getPMD(v).calendar;
    if (isZonedDateTime(v)) return getZDT(v).calendar;
    return undefined;
  }
  function isTemporalObject(v) {
    return isPlainDate(v) || isPlainDateTime(v) || isPlainMonthDay(v) || isPlainTime(v) || isPlainYearMonth(v) || isZonedDateTime(v);
  }
  function IsPartialTemporalObject(v) {
    if (!isObject(v)) return false;
    if (isTemporalObject(v)) return false;
    if (v.calendar !== undefined) return false;
    if (v.timeZone !== undefined) return false;
    return true;
  }
  function requirePartial(v) { if (!IsPartialTemporalObject(v)) throw typeError("argument must be a partial Temporal-like object"); }
  function roundToObject(v, key) {
    if (v === undefined) throw typeError("options are required");
    if (typeof v === "string") { var o = ObjectCreate(null); o[key] = v; return o; }
    return GetOptionsObject(v);
  }

  // ---------------- Temporal.Duration ----------------
  var DUR_KEYS = ["years", "months", "weeks", "days", "hours", "minutes", "seconds", "milliseconds", "microseconds", "nanoseconds"];
  class Duration {
    #s;
    constructor(years = undefined, months = undefined, weeks = undefined, days = undefined, hours = undefined, minutes = undefined,
      seconds = undefined, milliseconds = undefined, microseconds = undefined, nanoseconds = undefined) {
      if (years === TOKEN) { this.#s = months; return; }
      var a = arguments, v = [];
      for (var i = 0; i < 10; i++) v.push(a[i] === undefined ? 0 : ToIntegerIfIntegral(a[i]));
      if (!IsValidDuration(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9])) throw rangeError("invalid duration");
      var s = {};
      for (i = 0; i < 10; i++) s[DUR_KEYS[i]] = v[i];
      this.#s = s;
    }
    static { isDuration = function (o) { return isObject(o) && #s in o; }; getDur = function (o) { return o.#s; }; }
    static from(item) { return ToTemporalDuration(item); }
    static compare(one, two, options = undefined) {
      one = getDur(ToTemporalDuration(one)); two = getDur(ToTemporalDuration(two));
      var o = GetOptionsObject(options);
      var rel = GetTemporalRelativeToOption(o);
      var same = true;
      for (var i = 0; i < 10; i++) if (one[DUR_KEYS[i]] !== two[DUR_KEYS[i]]) same = false;
      if (same) return 0;
      var lu1 = DefaultTemporalLargestUnitOf(one), lu2 = DefaultTemporalLargestUnitOf(two);
      var d1 = ToInternalDurationRecord(one), d2 = ToInternalDurationRecord(two);
      if (rel.zoned !== undefined && (IsDateUnit(lu1) || IsDateUnit(lu2))) {
        var z = getZDT(rel.zoned);
        var a1 = AddZonedDateTime(z.ns, z.timeZone, z.calendar, d1, "constrain");
        var a2 = AddZonedDateTime(z.ns, z.timeZone, z.calendar, d2, "constrain");
        return a1 > a2 ? 1 : a1 < a2 ? -1 : 0;
      }
      var days1, days2;
      if (IsCalendarUnit(lu1) || IsCalendarUnit(lu2)) {
        if (rel.plain === undefined) throw rangeError("relativeTo is required for calendar units");
        days1 = DateDurationDays(d1.date, rel.plain); days2 = DateDurationDays(d2.date, rel.plain);
      } else { days1 = one.days; days2 = two.days; }
      var t1 = Add24HourDaysToTimeDuration(d1.time, days1), t2 = Add24HourDaysToTimeDuration(d2.time, days2);
      return t1 > t2 ? 1 : t1 < t2 ? -1 : 0;
    }
    get years() { return getDur(this).years; }
    get months() { return getDur(this).months; }
    get weeks() { return getDur(this).weeks; }
    get days() { return getDur(this).days; }
    get hours() { return getDur(this).hours; }
    get minutes() { return getDur(this).minutes; }
    get seconds() { return getDur(this).seconds; }
    get milliseconds() { return getDur(this).milliseconds; }
    get microseconds() { return getDur(this).microseconds; }
    get nanoseconds() { return getDur(this).nanoseconds; }
    get sign() { return DurationSign(getDur(this)); }
    get blank() { return DurationSign(getDur(this)) === 0; }
    with(temporalDurationLike) {
      var d = getDur(this);
      var p = ToTemporalPartialDurationRecord(temporalDurationLike);
      var v = [];
      for (var i = 0; i < 10; i++) v.push(p[DUR_KEYS[i]] !== undefined ? p[DUR_KEYS[i]] : d[DUR_KEYS[i]]);
      return CreateTemporalDuration(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
    }
    negated() { return CreateNegatedTemporalDuration(getDur(this)); }
    abs() {
      var d = getDur(this), v = [];
      for (var i = 0; i < 10; i++) v.push(MathAbs(d[DUR_KEYS[i]]));
      return CreateTemporalDuration(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
    }
    add(other) { return AddDurations("add", getDur(this), other); }
    subtract(other) { return AddDurations("subtract", getDur(this), other); }
    round(roundTo) {
      var d = getDur(this);
      var o = roundToObject(roundTo, "smallestUnit");
      var smallestPresent = true, largestPresent = true;
      var largest = GetTemporalUnitValuedOption(o, "largestUnit", undefined);
      var rel = GetTemporalRelativeToOption(o);
      var inc = GetRoundingIncrementOption(o);
      var mode = GetRoundingModeOption(o, "halfExpand");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
      ValidateTemporalUnitValue(smallest, "datetime");
      if (smallest === undefined) { smallestPresent = false; smallest = "nanosecond"; }
      var existing = DefaultTemporalLargestUnitOf(d);
      var dflt = LargerOfTwoTemporalUnits(existing, smallest);
      if (largest === undefined) { largestPresent = false; largest = dflt; }
      else if (largest === "auto") largest = dflt;
      if (!smallestPresent && !largestPresent) throw rangeError("smallestUnit or largestUnit is required");
      if (LargerOfTwoTemporalUnits(largest, smallest) !== largest) throw rangeError("largestUnit is smaller than smallestUnit");
      var max = MaximumTemporalDurationRoundingIncrement(smallest);
      if (max !== undefined) ValidateTemporalRoundingIncrement(inc, max, false);
      if (inc > 1 && largest !== smallest && IsDateUnit(smallest)) throw rangeError("roundingIncrement must be 1 for a date unit smaller than largestUnit");
      var id;
      if (rel.zoned !== undefined) {
        var z = getZDT(rel.zoned);
        id = ToInternalDurationRecord(d);
        var target = AddZonedDateTime(z.ns, z.timeZone, z.calendar, id, "constrain");
        id = DifferenceZonedDateTimeWithRounding(z.ns, target, z.timeZone, z.calendar, largest, inc, smallest, mode);
        if (IsDateUnit(largest)) largest = "hour";
        return TemporalDurationFromInternal(id, largest);
      }
      if (rel.plain !== undefined) {
        var pd = getPD(rel.plain);
        id = ToInternalDurationRecordWith24HourDays(d);
        var tt = AddTimeNs(MidnightTime(), id.time);
        var dd = AdjustDateDurationRecord(id.date, tt.days);
        var td = CalendarDateAdd(pd.calendar, pd.isoDate, dd, "constrain");
        tt.days = 0;
        id = DifferencePlainDateTimeWithRounding(DateTime(pd.isoDate, MidnightTime()), DateTime(td, tt), pd.calendar,
          largest, inc, smallest, mode);
        return TemporalDurationFromInternal(id, largest);
      }
      if (IsCalendarUnit(existing) || IsCalendarUnit(largest)) throw rangeError("relativeTo is required for calendar units");
      id = ToInternalDurationRecordWith24HourDays(d);
      if (smallest === "day") {
        var rd = roundBigToIncrement(id.time, nsPerDay * Big(inc), mode) / nsPerDay;
        id = Internal(CreateDateDurationRecord(0, 0, 0, Num(rd)), 0n);
      } else id = Internal(ZeroDateDuration(), RoundTimeDuration(id.time, inc, smallest, mode));
      return TemporalDurationFromInternal(id, largest);
    }
    total(totalOf) {
      var d = getDur(this);
      var o = roundToObject(totalOf, "unit");
      var rel = GetTemporalRelativeToOption(o);
      var unit = GetTemporalUnitValuedOption(o, "unit", REQUIRED);
      ValidateTemporalUnitValue(unit, "datetime");
      var id, total;
      if (rel.zoned !== undefined) {
        var z = getZDT(rel.zoned);
        id = ToInternalDurationRecord(d);
        var target = AddZonedDateTime(z.ns, z.timeZone, z.calendar, id, "constrain");
        total = DifferenceZonedDateTimeWithTotal(z.ns, target, z.timeZone, z.calendar, unit);
      } else if (rel.plain !== undefined) {
        var pd = getPD(rel.plain);
        id = ToInternalDurationRecordWith24HourDays(d);
        var tt = AddTimeNs(MidnightTime(), id.time);
        var dd = AdjustDateDurationRecord(id.date, tt.days);
        var td = CalendarDateAdd(pd.calendar, pd.isoDate, dd, "constrain");
        tt.days = 0;
        total = DifferencePlainDateTimeWithTotal(DateTime(pd.isoDate, MidnightTime()), DateTime(td, tt), pd.calendar, unit);
      } else {
        var lu = DefaultTemporalLargestUnitOf(d);
        if (IsCalendarUnit(lu) || IsCalendarUnit(unit)) throw rangeError("relativeTo is required for calendar units");
        id = ToInternalDurationRecordWith24HourDays(d);
        total = TotalTimeDuration(id.time, unit);
      }
      return total;
    }
    toString(options = undefined) {
      var d = getDur(this);
      var o = GetOptionsObject(options);
      var digits = GetTemporalFractionalSecondDigitsOption(o);
      var mode = GetRoundingModeOption(o, "trunc");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
      ValidateTemporalUnitValue(smallest, "time");
      if (smallest === "hour" || smallest === "minute") throw rangeError("smallestUnit must be seconds or smaller");
      var pr = ToSecondsStringPrecisionRecord(smallest, digits);
      if (pr.unit === "nanosecond" && pr.increment === 1) return TemporalDurationToString(d, pr.precision);
      var lu = DefaultTemporalLargestUnitOf(d);
      var id = ToInternalDurationRecord(d);
      var t = RoundTimeDuration(id.time, pr.increment, pr.unit, mode);
      id = Internal(id.date, t);
      var rd = TemporalDurationFromInternal(id, LargerOfTwoTemporalUnits(lu, "second"));
      return TemporalDurationToString(getDur(rd), pr.precision);
    }
    toJSON() { return TemporalDurationToString(getDur(this), "auto"); }
    toLocaleString() { return TemporalDurationToString(getDur(this), "auto"); }
    valueOf() { throwValueOf(); }
  }
  tagged(Duration, "Temporal.Duration");
  function CreateTemporalDuration(y, mo, w, d, h, mi, s, ms, us, ns) {
    if (!IsValidDuration(y, mo, w, d, h, mi, s, ms, us, ns)) throw rangeError("invalid duration");
    return new Duration(TOKEN, { years: y + 0, months: mo + 0, weeks: w + 0, days: d + 0, hours: h + 0, minutes: mi + 0,
      seconds: s + 0, milliseconds: ms + 0, microseconds: us + 0, nanoseconds: ns + 0 });
  }
  function CreateNegatedTemporalDuration(r) {
    return CreateTemporalDuration(-r.years, -r.months, -r.weeks, -r.days, -r.hours, -r.minutes, -r.seconds,
      -r.milliseconds, -r.microseconds, -r.nanoseconds);
  }
  function DurationSign(r) {
    for (var i = 0; i < 10; i++) { var v = r[DUR_KEYS[i]]; if (v < 0) return -1; if (v > 0) return 1; }
    return 0;
  }
  function ToTemporalPartialDurationRecord(v) {
    if (!isObject(v)) throw typeError("duration-like must be an object");
    var r = {}, any = false;
    var order = ["days", "hours", "microseconds", "milliseconds", "minutes", "months", "nanoseconds", "seconds", "weeks", "years"];
    for (var i = 0; i < order.length; i++) {
      var x = v[order[i]];
      if (x !== undefined) { any = true; r[order[i]] = ToIntegerIfIntegral(x); }
    }
    if (!any) throw typeError("duration-like object has no duration properties");
    return r;
  }
  function ToTemporalDuration(item) {
    if (isDuration(item)) { var d = getDur(item); return CreateTemporalDuration(d.years, d.months, d.weeks, d.days, d.hours, d.minutes, d.seconds, d.milliseconds, d.microseconds, d.nanoseconds); }
    if (!isObject(item)) {
      if (typeof item !== "string") throw typeError("duration must be a string or an object");
      return ParseTemporalDurationString(item);
    }
    var p = ToTemporalPartialDurationRecord(item), v = [];
    for (var i = 0; i < 10; i++) v.push(p[DUR_KEYS[i]] !== undefined ? p[DUR_KEYS[i]] : 0);
    return CreateTemporalDuration(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
  }
  function AddDurations(op, d, other) {
    var o = getDur(ToTemporalDuration(other));
    if (op === "subtract") o = getDur(CreateNegatedTemporalDuration(o));
    var lu = LargerOfTwoTemporalUnits(DefaultTemporalLargestUnitOf(d), DefaultTemporalLargestUnitOf(o));
    if (IsCalendarUnit(lu)) throw rangeError("cannot add durations with calendar units without relativeTo");
    var d1 = ToInternalDurationRecordWith24HourDays(d), d2 = ToInternalDurationRecordWith24HourDays(o);
    return TemporalDurationFromInternal(Internal(ZeroDateDuration(), AddTimeDuration(d1.time, d2.time)), lu);
  }
  function DateDurationDays(dd, plainRelativeTo) {
    var ymw = AdjustDateDurationRecord(dd, 0);
    if (DateDurationSign(ymw) === 0) return dd.days;
    var p = getPD(plainRelativeTo);
    var later = CalendarDateAdd(p.calendar, p.isoDate, ymw, "constrain");
    return dd.days + ISODateToEpochDays(later.year, later.month, later.day) - ISODateToEpochDays(p.isoDate.year, p.isoDate.month, p.isoDate.day);
  }
  function TemporalDurationToString(d, precision) {
    var sg = DurationSign(d);
    var date = "";
    if (d.years !== 0) date += fmtInt(MathAbs(d.years)) + "Y";
    if (d.months !== 0) date += fmtInt(MathAbs(d.months)) + "M";
    if (d.weeks !== 0) date += fmtInt(MathAbs(d.weeks)) + "W";
    if (d.days !== 0) date += fmtInt(MathAbs(d.days)) + "D";
    var time = "";
    if (d.hours !== 0) time += fmtInt(MathAbs(d.hours)) + "H";
    if (d.minutes !== 0) time += fmtInt(MathAbs(d.minutes)) + "M";
    var lu = DefaultTemporalLargestUnitOf(d);
    var zeroMinutesAndHigher = unitIndex(lu) >= 6;
    var secs = TimeDurationFromComponents(0, 0, d.seconds, d.milliseconds, d.microseconds, d.nanoseconds);
    if (secs !== 0n || zeroMinutesAndHigher || precision !== "auto") {
      var a = babs(secs);
      time += (a / 1000000000n).toString() + FormatFractionalSeconds(Num(a % 1000000000n), precision) + "S";
    }
    var r = (sg < 0 ? "-" : "") + "P" + date;
    if (time !== "") r += "T" + time;
    return r;
  }
  function fmtInt(n) { return n < 1e21 ? Str(n) : Big(n).toString(); }
  // GetTemporalRelativeToOption: { plain, zoned }
  function GetTemporalRelativeToOption(o) {
    var v = o.relativeTo;
    if (v === undefined) return { plain: undefined, zoned: undefined };
    var offsetBehaviour = "option", matchBehaviour = "match-exactly", calendar, isoDate, time, timeZone, offsetString;
    if (isObject(v)) {
      if (isZonedDateTime(v)) return { plain: undefined, zoned: v };
      if (isPlainDate(v)) return { plain: v, zoned: undefined };
      if (isPlainDateTime(v)) { var pdt = getPDT(v); return { plain: CreateTemporalDate(pdt.isoDateTime.isoDate, pdt.calendar), zoned: undefined }; }
      calendar = GetTemporalCalendarIdentifierWithISODefault(v);
      var f = PrepareCalendarFields(calendar, v, DATE_FIELDS, TIME_FIELDS.concat(["offset", "timeZone"]), []);
      var res = InterpretTemporalDateTimeFields(calendar, f, "constrain");
      timeZone = f.timeZone; offsetString = f.offset;
      if (offsetString === undefined) offsetBehaviour = "wall";
      isoDate = res.isoDate; time = res.time;
    } else {
      if (typeof v !== "string") throw typeError("relativeTo must be a string or an object");
      var r = ParseISODateTime(v, ["zoned", "datetime"]);
      offsetString = r.timeZone.offsetString;
      var ann = r.timeZone.tzAnnotation;
      if (ann === undefined) timeZone = undefined;
      else {
        timeZone = ToTemporalTimeZoneIdentifier(ann);
        if (r.timeZone.z) offsetBehaviour = "exact";
        else if (offsetString === undefined) offsetBehaviour = "wall";
        matchBehaviour = "match-minutes";
        if (offsetString !== undefined && hasSubMinute(offsetString)) matchBehaviour = "match-exactly";
      }
      calendar = CanonicalizeCalendar(r.calendar === undefined ? "iso8601" : r.calendar);
      isoDate = ISODate(r.year, r.month, r.day);
      time = r.time;
    }
    if (timeZone === undefined) return { plain: CreateTemporalDate(isoDate, calendar), zoned: undefined };
    var offsetNs = offsetBehaviour === "option" ? ParseDateTimeUTCOffset(offsetString) : 0;
    var ns = InterpretISODateTimeOffset(isoDate, time, offsetBehaviour, offsetNs, timeZone, "compatible", "reject", matchBehaviour);
    return { plain: undefined, zoned: CreateTemporalZonedDateTime(ns, timeZone, calendar) };
  }
  // an offset string with more than one MinuteSecond (seconds)
  function hasSubMinute(s) { var t = s.slice(1).replace(/:/g, ""); return t.length > 4; }
  function InterpretTemporalDateTimeFields(calendar, f, overflow) {
    var d = CalendarDateFromFields(calendar, f, overflow);
    var t = RegulateTime(f.hour, f.minute, f.second, f.millisecond, f.microsecond, f.nanosecond, overflow);
    return DateTime(d, t);
  }

  // ---------------- Temporal.PlainTime ----------------
  class PlainTime {
    #s;
    constructor(hour = undefined, minute = undefined, second = undefined, millisecond = undefined, microsecond = undefined,
      nanosecond = undefined) {
      if (hour === TOKEN) { this.#s = minute; return; }
      var a = arguments, v = [];
      for (var i = 0; i < 6; i++) v.push(a[i] === undefined ? 0 : ToIntegerWithTruncation(a[i]));
      if (!IsValidTime(v[0], v[1], v[2], v[3], v[4], v[5])) throw rangeError("invalid time");
      this.#s = { time: Time(v[0], v[1], v[2], v[3], v[4], v[5]) };
    }
    static { isPlainTime = function (o) { return isObject(o) && #s in o; }; getPT = function (o) { return o.#s; }; }
    static from(item, options = undefined) { return ToTemporalTime(item, options); }
    static compare(one, two) { return CompareTimeRecord(getPT(ToTemporalTime(one)).time, getPT(ToTemporalTime(two)).time); }
    get hour() { return getPT(this).time.hour; }
    get minute() { return getPT(this).time.minute; }
    get second() { return getPT(this).time.second; }
    get millisecond() { return getPT(this).time.millisecond; }
    get microsecond() { return getPT(this).time.microsecond; }
    get nanosecond() { return getPT(this).time.nanosecond; }
    add(d) { return AddDurationToTime("add", getPT(this), d); }
    subtract(d) { return AddDurationToTime("subtract", getPT(this), d); }
    with(temporalTimeLike, options = undefined) {
      var t = getPT(this).time;
      requirePartial(temporalTimeLike);
      var p = ToTemporalTimeRecord(temporalTimeLike, "partial");
      var o = GetOptionsObject(options);
      var overflow = GetTemporalOverflowOption(o);
      return CreateTemporalTime(RegulateTime(p.hour !== undefined ? p.hour : t.hour, p.minute !== undefined ? p.minute : t.minute,
        p.second !== undefined ? p.second : t.second, p.millisecond !== undefined ? p.millisecond : t.millisecond,
        p.microsecond !== undefined ? p.microsecond : t.microsecond, p.nanosecond !== undefined ? p.nanosecond : t.nanosecond, overflow));
    }
    until(other, options = undefined) { return DifferenceTemporalPlainTime("until", getPT(this), other, options); }
    since(other, options = undefined) { return DifferenceTemporalPlainTime("since", getPT(this), other, options); }
    round(roundTo) {
      var t = getPT(this).time;
      var o = roundToObject(roundTo, "smallestUnit");
      var inc = GetRoundingIncrementOption(o);
      var mode = GetRoundingModeOption(o, "halfExpand");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", REQUIRED);
      ValidateTemporalUnitValue(smallest, "time");
      ValidateTemporalRoundingIncrement(inc, MaximumTemporalDurationRoundingIncrement(smallest), false);
      var r = RoundTime(t, inc, smallest, mode);
      r.days = 0;
      return CreateTemporalTime(r);
    }
    equals(other) { return CompareTimeRecord(getPT(this).time, getPT(ToTemporalTime(other)).time) === 0; }
    toString(options = undefined) {
      var t = getPT(this).time;
      var o = GetOptionsObject(options);
      var digits = GetTemporalFractionalSecondDigitsOption(o);
      var mode = GetRoundingModeOption(o, "trunc");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
      ValidateTemporalUnitValue(smallest, "time");
      if (smallest === "hour") throw rangeError("smallestUnit must not be hour");
      var pr = ToSecondsStringPrecisionRecord(smallest, digits);
      var r = RoundTime(t, pr.increment, pr.unit, mode);
      return TimeRecordToString(r, pr.precision);
    }
    toLocaleString() { return TimeRecordToString(getPT(this).time, "auto"); }
    toJSON() { return TimeRecordToString(getPT(this).time, "auto"); }
    valueOf() { throwValueOf(); }
  }
  tagged(PlainTime, "Temporal.PlainTime");
  function CreateTemporalTime(t) { return new PlainTime(TOKEN, { time: Time(t.hour, t.minute, t.second, t.millisecond, t.microsecond, t.nanosecond) }); }
  function subSecondNs(t) { return t.millisecond * 1e6 + t.microsecond * 1e3 + t.nanosecond; }
  function TimeRecordToString(t, precision) { return FormatTimeString(t.hour, t.minute, t.second, subSecondNs(t), precision); }
  function ToTemporalTimeRecord(v, completeness) {
    var r = completeness === "partial" ? {} : { hour: 0, minute: 0, second: 0, millisecond: 0, microsecond: 0, nanosecond: 0 };
    var any = false, order = ["hour", "microsecond", "millisecond", "minute", "nanosecond", "second"];
    for (var i = 0; i < order.length; i++) {
      var x = v[order[i]];
      if (x !== undefined) { r[order[i]] = ToIntegerWithTruncation(x); any = true; }
    }
    if (!any) throw typeError("time-like object has no time properties");
    return r;
  }
  function ToTemporalTime(item, options) {
    var t;
    if (isObject(item)) {
      if (isPlainTime(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); return CreateTemporalTime(getPT(item).time); }
      if (isPlainDateTime(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); return CreateTemporalTime(getPDT(item).isoDateTime.time); }
      if (isZonedDateTime(item)) {
        var z = getZDT(item), dt = GetISODateTimeFor(z.timeZone, z.ns);
        GetTemporalOverflowOption(GetOptionsObject(options));
        return CreateTemporalTime(dt.time);
      }
      var r = ToTemporalTimeRecord(item);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      t = RegulateTime(r.hour, r.minute, r.second, r.millisecond, r.microsecond, r.nanosecond, overflow);
    } else {
      if (typeof item !== "string") throw typeError("time must be a string or an object");
      var p = ParseISODateTime(item, ["time"]);
      t = p.time;
      GetTemporalOverflowOption(GetOptionsObject(options));
    }
    return CreateTemporalTime(t);
  }
  function ToTimeRecordOrMidnight(item) { return item === undefined ? MidnightTime() : getPT(ToTemporalTime(item)).time; }
  function AddDurationToTime(op, pt, d) {
    var dur = getDur(ToTemporalDuration(d));
    if (op === "subtract") dur = getDur(CreateNegatedTemporalDuration(dur));
    var id = ToInternalDurationRecord(dur);
    var r = AddTimeNs(pt.time, id.time);
    r.days = 0;
    return CreateTemporalTime(r);
  }
  function DifferenceTemporalPlainTime(op, pt, other, options) {
    var ot = getPT(ToTemporalTime(other));
    var o = GetOptionsObject(options);
    var st = GetDifferenceSettings(op, o, "time", [], "nanosecond", "hour");
    var td = DifferenceTime(pt.time, ot.time);
    td = RoundTimeDuration(td, st.roundingIncrement, st.smallestUnit, st.roundingMode);
    var r = TemporalDurationFromInternal(Internal(ZeroDateDuration(), td), st.largestUnit);
    return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
  }

  // ---------------- Temporal.PlainDate ----------------
  function toCalendarArg(c) {
    if (c === undefined) return "iso8601";
    if (typeof c !== "string") throw typeError("calendar must be a string");
    return CanonicalizeCalendar(c);
  }
  function dateGetters(C, get, slotDate) {
    function cd(o) { var s = get(o); return CalendarISOToDate(s.calendar, slotDate(s)); }
    var g = {
      calendarId: function () { return get(this).calendar; },
      era: function () { return cd(this).era; },
      eraYear: function () { return cd(this).eraYear; },
      year: function () { return cd(this).year; },
      month: function () { return cd(this).month; },
      monthCode: function () { return cd(this).monthCode; },
      day: function () { return cd(this).day; },
      dayOfWeek: function () { return cd(this).dayOfWeek; },
      dayOfYear: function () { return cd(this).dayOfYear; },
      weekOfYear: function () { return cd(this).weekOfYear.week; },
      yearOfWeek: function () { return cd(this).weekOfYear.year; },
      daysInWeek: function () { return cd(this).daysInWeek; },
      daysInMonth: function () { return cd(this).daysInMonth; },
      daysInYear: function () { return cd(this).daysInYear; },
      monthsInYear: function () { return cd(this).monthsInYear; },
      inLeapYear: function () { return cd(this).inLeapYear; }
    };
    return g;
  }
  function installGetters(C, names, g) {
    for (var i = 0; i < names.length; i++) {
      var n = names[i], f = g[n];
      ObjectDefineProperty(f, "name", { value: "get " + n, configurable: true });
      ObjectDefineProperty(C.prototype, n, { get: f, set: undefined, enumerable: false, configurable: true });
    }
  }
  var DATE_GETTERS = ["calendarId", "era", "eraYear", "year", "month", "monthCode", "day", "dayOfWeek", "dayOfYear",
    "weekOfYear", "yearOfWeek", "daysInWeek", "daysInMonth", "daysInYear", "monthsInYear", "inLeapYear"];
  class PlainDate {
    #s;
    constructor(isoYear, isoMonth, isoDay, calendar = undefined) {
      if (isoYear === TOKEN) { this.#s = isoMonth; return; }
      var y = ToIntegerWithTruncation(isoYear), m = ToIntegerWithTruncation(isoMonth), d = ToIntegerWithTruncation(isoDay);
      var c = toCalendarArg(calendar);
      if (!IsValidISODate(y, m, d)) throw rangeError("invalid date");
      var dd = ISODate(y, m, d);
      if (!ISODateWithinLimits(dd)) throw rangeError("date out of range");
      this.#s = { isoDate: dd, calendar: c };
    }
    static { isPlainDate = function (o) { return isObject(o) && #s in o; }; getPD = function (o) { return o.#s; }; }
    static from(item, options = undefined) { return ToTemporalDate(item, options); }
    static compare(one, two) { return CompareISODate(getPD(ToTemporalDate(one)).isoDate, getPD(ToTemporalDate(two)).isoDate); }
    toPlainYearMonth() {
      var s = getPD(this);
      var f = ISODateToFields(s.calendar, s.isoDate, "date");
      return CreateTemporalYearMonth(CalendarYearMonthFromFields(s.calendar, f, "constrain"), s.calendar);
    }
    toPlainMonthDay() {
      var s = getPD(this);
      var f = ISODateToFields(s.calendar, s.isoDate, "date");
      return CreateTemporalMonthDay(CalendarMonthDayFromFields(s.calendar, f, "constrain"), s.calendar);
    }
    add(d, options = undefined) { return AddDurationToDate("add", getPD(this), d, options); }
    subtract(d, options = undefined) { return AddDurationToDate("subtract", getPD(this), d, options); }
    with(temporalDateLike, options = undefined) {
      var s = getPD(this);
      requirePartial(temporalDateLike);
      var f = ISODateToFields(s.calendar, s.isoDate, "date");
      var p = PrepareCalendarFields(s.calendar, temporalDateLike, DATE_FIELDS, [], "partial");
      f = CalendarMergeFields(s.calendar, f, p);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalDate(CalendarDateFromFields(s.calendar, f, overflow), s.calendar);
    }
    withCalendar(calendarLike) { var s = getPD(this); return CreateTemporalDate(s.isoDate, ToTemporalCalendarIdentifier(calendarLike)); }
    until(other, options = undefined) { return DifferenceTemporalPlainDate("until", getPD(this), other, options); }
    since(other, options = undefined) { return DifferenceTemporalPlainDate("since", getPD(this), other, options); }
    equals(other) {
      var s = getPD(this), o = getPD(ToTemporalDate(other));
      return CompareISODate(s.isoDate, o.isoDate) === 0 && CalendarEquals(s.calendar, o.calendar);
    }
    toPlainDateTime(temporalTime = undefined) {
      var s = getPD(this);
      var t = ToTimeRecordOrMidnight(temporalTime);
      return CreateTemporalDateTime(DateTime(s.isoDate, t), s.calendar);
    }
    toZonedDateTime(item) {
      var s = getPD(this), tz, tt;
      if (isObject(item)) {
        var tzl = item.timeZone;
        if (tzl === undefined) { tz = ToTemporalTimeZoneIdentifier(item); tt = undefined; }
        else { tz = ToTemporalTimeZoneIdentifier(tzl); tt = item.plainTime; }
      } else { tz = ToTemporalTimeZoneIdentifier(item); tt = undefined; }
      var ns;
      if (tt === undefined) ns = GetStartOfDay(tz, s.isoDate);
      else {
        var t = getPT(ToTemporalTime(tt)).time;
        var dt = DateTime(s.isoDate, t);
        if (!ISODateTimeWithinLimits(dt)) throw rangeError("date-time out of range");
        ns = GetEpochNanosecondsFor(tz, dt, "compatible");
      }
      return CreateTemporalZonedDateTime(ns, tz, s.calendar);
    }
    toString(options = undefined) {
      var s = getPD(this);
      var show = GetTemporalShowCalendarNameOption(GetOptionsObject(options));
      return TemporalDateToString(s, show);
    }
    toLocaleString() { return TemporalDateToString(getPD(this), "auto"); }
    toJSON() { return TemporalDateToString(getPD(this), "auto"); }
    valueOf() { throwValueOf(); }
  }
  tagged(PlainDate, "Temporal.PlainDate");
  installGetters(PlainDate, DATE_GETTERS, dateGetters(PlainDate, function (o) { return getPD(o); }, function (s) { return s.isoDate; }));
  function CreateTemporalDate(d, calendar) {
    if (!ISODateWithinLimits(d)) throw rangeError("date out of range");
    return new PlainDate(TOKEN, { isoDate: ISODate(d.year, d.month, d.day), calendar: calendar });
  }
  function dateString(d) { return PadISOYear(d.year) + "-" + pad(d.month, 2) + "-" + pad(d.day, 2); }
  function TemporalDateToString(s, show) { return dateString(s.isoDate) + FormatCalendarAnnotation(s.calendar, show); }
  function ToTemporalDate(item, options) {
    if (isObject(item)) {
      if (isPlainDate(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); var s = getPD(item); return CreateTemporalDate(s.isoDate, s.calendar); }
      if (isZonedDateTime(item)) {
        var z = getZDT(item), dt = GetISODateTimeFor(z.timeZone, z.ns);
        GetTemporalOverflowOption(GetOptionsObject(options));
        return CreateTemporalDate(dt.isoDate, z.calendar);
      }
      if (isPlainDateTime(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); var p = getPDT(item); return CreateTemporalDate(p.isoDateTime.isoDate, p.calendar); }
      var calendar = GetTemporalCalendarIdentifierWithISODefault(item);
      var f = PrepareCalendarFields(calendar, item, DATE_FIELDS, [], []);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalDate(CalendarDateFromFields(calendar, f, overflow), calendar);
    }
    if (typeof item !== "string") throw typeError("date must be a string or an object");
    var r = ParseISODateTime(item, ["datetime"]);
    var cal = CanonicalizeCalendar(r.calendar === undefined ? "iso8601" : r.calendar);
    GetTemporalOverflowOption(GetOptionsObject(options));
    return CreateTemporalDate(ISODate(r.year, r.month, r.day), cal);
  }
  function AddDurationToDate(op, s, d, options) {
    var dur = getDur(ToTemporalDuration(d));
    if (op === "subtract") dur = getDur(CreateNegatedTemporalDuration(dur));
    var dd = ToDateDurationRecordWithoutTime(dur);
    var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
    return CreateTemporalDate(CalendarDateAdd(s.calendar, s.isoDate, dd, overflow), s.calendar);
  }
  function DifferenceTemporalPlainDate(op, s, other, options) {
    var o = getPD(ToTemporalDate(other));
    if (!CalendarEquals(s.calendar, o.calendar)) throw rangeError("calendars differ");
    var st = GetDifferenceSettings(op, GetOptionsObject(options), "date", [], "day", "day");
    if (CompareISODate(s.isoDate, o.isoDate) === 0) return CreateTemporalDuration(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    var dd = CalendarDateUntil(s.calendar, s.isoDate, o.isoDate, st.largestUnit);
    var d = Internal(dd, 0n);
    if (st.smallestUnit !== "day" || st.roundingIncrement !== 1) {
      var a = DateTime(s.isoDate, MidnightTime()), b = DateTime(o.isoDate, MidnightTime());
      d = RoundRelativeDuration(d, GetUTCEpochNanoseconds(a), GetUTCEpochNanoseconds(b), a, undefined, s.calendar,
        st.largestUnit, st.roundingIncrement, st.smallestUnit, st.roundingMode);
    }
    var r = TemporalDurationFromInternal(d, "day");
    return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
  }

  // ---------------- Temporal.PlainDateTime ----------------
  class PlainDateTime {
    #s;
    constructor(isoYear, isoMonth, isoDay, hour = 0, minute = 0, second = 0, millisecond = 0, microsecond = 0, nanosecond = 0, calendar = undefined) {
      if (isoYear === TOKEN) { this.#s = isoMonth; return; }
      var y = ToIntegerWithTruncation(isoYear), mo = ToIntegerWithTruncation(isoMonth), d = ToIntegerWithTruncation(isoDay);
      var h = hour === undefined ? 0 : ToIntegerWithTruncation(hour);
      var mi = minute === undefined ? 0 : ToIntegerWithTruncation(minute);
      var s = second === undefined ? 0 : ToIntegerWithTruncation(second);
      var ms = millisecond === undefined ? 0 : ToIntegerWithTruncation(millisecond);
      var us = microsecond === undefined ? 0 : ToIntegerWithTruncation(microsecond);
      var ns = nanosecond === undefined ? 0 : ToIntegerWithTruncation(nanosecond);
      var c = toCalendarArg(calendar);
      if (!IsValidISODate(y, mo, d)) throw rangeError("invalid date");
      if (!IsValidTime(h, mi, s, ms, us, ns)) throw rangeError("invalid time");
      var dt = DateTime(ISODate(y, mo, d), Time(h, mi, s, ms, us, ns));
      if (!ISODateTimeWithinLimits(dt)) throw rangeError("date-time out of range");
      this.#s = { isoDateTime: dt, calendar: c };
    }
    static { isPlainDateTime = function (o) { return isObject(o) && #s in o; }; getPDT = function (o) { return o.#s; }; }
    static from(item, options = undefined) { return ToTemporalDateTime(item, options); }
    static compare(one, two) { return CompareISODateTime(getPDT(ToTemporalDateTime(one)).isoDateTime, getPDT(ToTemporalDateTime(two)).isoDateTime); }
    get hour() { return getPDT(this).isoDateTime.time.hour; }
    get minute() { return getPDT(this).isoDateTime.time.minute; }
    get second() { return getPDT(this).isoDateTime.time.second; }
    get millisecond() { return getPDT(this).isoDateTime.time.millisecond; }
    get microsecond() { return getPDT(this).isoDateTime.time.microsecond; }
    get nanosecond() { return getPDT(this).isoDateTime.time.nanosecond; }
    with(temporalDateTimeLike, options = undefined) {
      var s = getPDT(this);
      requirePartial(temporalDateTimeLike);
      var f = ISODateToFields(s.calendar, s.isoDateTime.isoDate, "date");
      var t = s.isoDateTime.time;
      f.hour = t.hour; f.minute = t.minute; f.second = t.second; f.millisecond = t.millisecond; f.microsecond = t.microsecond; f.nanosecond = t.nanosecond;
      var p = PrepareCalendarFields(s.calendar, temporalDateTimeLike, DATE_FIELDS, TIME_FIELDS, "partial");
      f = CalendarMergeFields(s.calendar, f, p);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalDateTime(InterpretTemporalDateTimeFields(s.calendar, f, overflow), s.calendar);
    }
    withPlainTime(plainTimeLike = undefined) {
      var s = getPDT(this);
      return CreateTemporalDateTime(DateTime(s.isoDateTime.isoDate, ToTimeRecordOrMidnight(plainTimeLike)), s.calendar);
    }
    withCalendar(calendarLike) { var s = getPDT(this); return CreateTemporalDateTime(s.isoDateTime, ToTemporalCalendarIdentifier(calendarLike)); }
    add(d, options = undefined) { return AddDurationToDateTime("add", getPDT(this), d, options); }
    subtract(d, options = undefined) { return AddDurationToDateTime("subtract", getPDT(this), d, options); }
    until(other, options = undefined) { return DifferenceTemporalPlainDateTime("until", getPDT(this), other, options); }
    since(other, options = undefined) { return DifferenceTemporalPlainDateTime("since", getPDT(this), other, options); }
    round(roundTo) {
      var s = getPDT(this);
      var o = roundToObject(roundTo, "smallestUnit");
      var inc = GetRoundingIncrementOption(o);
      var mode = GetRoundingModeOption(o, "halfExpand");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", REQUIRED);
      ValidateTemporalUnitValue(smallest, "time", ["day"]);
      var max, inclusive;
      if (smallest === "day") { max = 1; inclusive = true; } else { max = MaximumTemporalDurationRoundingIncrement(smallest); inclusive = false; }
      ValidateTemporalRoundingIncrement(inc, max, inclusive);
      if (smallest === "nanosecond" && inc === 1) return CreateTemporalDateTime(s.isoDateTime, s.calendar);
      return CreateTemporalDateTime(RoundISODateTime(s.isoDateTime, inc, smallest, mode), s.calendar);
    }
    equals(other) {
      var s = getPDT(this), o = getPDT(ToTemporalDateTime(other));
      return CompareISODateTime(s.isoDateTime, o.isoDateTime) === 0 && CalendarEquals(s.calendar, o.calendar);
    }
    toString(options = undefined) {
      var s = getPDT(this);
      var o = GetOptionsObject(options);
      var show = GetTemporalShowCalendarNameOption(o);
      var digits = GetTemporalFractionalSecondDigitsOption(o);
      var mode = GetRoundingModeOption(o, "trunc");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
      ValidateTemporalUnitValue(smallest, "time");
      if (smallest === "hour") throw rangeError("smallestUnit must not be hour");
      var pr = ToSecondsStringPrecisionRecord(smallest, digits);
      var r = RoundISODateTime(s.isoDateTime, pr.increment, pr.unit, mode);
      if (!ISODateTimeWithinLimits(r)) throw rangeError("date-time out of range");
      return ISODateTimeToString(r, s.calendar, pr.precision, show);
    }
    toLocaleString() { var s = getPDT(this); return ISODateTimeToString(s.isoDateTime, s.calendar, "auto", "auto"); }
    toJSON() { var s = getPDT(this); return ISODateTimeToString(s.isoDateTime, s.calendar, "auto", "auto"); }
    valueOf() { throwValueOf(); }
    toZonedDateTime(temporalTimeZoneLike, options = undefined) {
      var s = getPDT(this);
      var tz = ToTemporalTimeZoneIdentifier(temporalTimeZoneLike);
      var dis = GetTemporalDisambiguationOption(GetOptionsObject(options));
      var ns = GetEpochNanosecondsFor(tz, s.isoDateTime, dis);
      return CreateTemporalZonedDateTime(ns, tz, s.calendar);
    }
    toPlainDate() { var s = getPDT(this); return CreateTemporalDate(s.isoDateTime.isoDate, s.calendar); }
    toPlainTime() { return CreateTemporalTime(getPDT(this).isoDateTime.time); }
  }
  tagged(PlainDateTime, "Temporal.PlainDateTime");
  installGetters(PlainDateTime, DATE_GETTERS, dateGetters(PlainDateTime, function (o) { return getPDT(o); }, function (s) { return s.isoDateTime.isoDate; }));
  function CreateTemporalDateTime(dt, calendar) {
    if (!ISODateTimeWithinLimits(dt)) throw rangeError("date-time out of range");
    var t = dt.time;
    return new PlainDateTime(TOKEN, { isoDateTime: DateTime(ISODate(dt.isoDate.year, dt.isoDate.month, dt.isoDate.day),
      Time(t.hour, t.minute, t.second, t.millisecond, t.microsecond, t.nanosecond)), calendar: calendar });
  }
  function ISODateTimeToString(dt, calendar, precision, show) {
    var t = dt.time;
    return dateString(dt.isoDate) + "T" + FormatTimeString(t.hour, t.minute, t.second, subSecondNs(t), precision) +
      FormatCalendarAnnotation(calendar, show);
  }
  function ToTemporalDateTime(item, options) {
    if (isObject(item)) {
      if (isPlainDateTime(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); var s = getPDT(item); return CreateTemporalDateTime(s.isoDateTime, s.calendar); }
      if (isZonedDateTime(item)) {
        var z = getZDT(item), dt = GetISODateTimeFor(z.timeZone, z.ns);
        GetTemporalOverflowOption(GetOptionsObject(options));
        return CreateTemporalDateTime(dt, z.calendar);
      }
      if (isPlainDate(item)) {
        GetTemporalOverflowOption(GetOptionsObject(options));
        var pd = getPD(item);
        return CreateTemporalDateTime(DateTime(pd.isoDate, MidnightTime()), pd.calendar);
      }
      var calendar = GetTemporalCalendarIdentifierWithISODefault(item);
      var f = PrepareCalendarFields(calendar, item, DATE_FIELDS, TIME_FIELDS, []);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalDateTime(InterpretTemporalDateTimeFields(calendar, f, overflow), calendar);
    }
    if (typeof item !== "string") throw typeError("date-time must be a string or an object");
    var r = ParseISODateTime(item, ["datetime"]);
    var time = r.time === "start-of-day" ? MidnightTime() : r.time;
    var cal = CanonicalizeCalendar(r.calendar === undefined ? "iso8601" : r.calendar);
    GetTemporalOverflowOption(GetOptionsObject(options));
    return CreateTemporalDateTime(DateTime(ISODate(r.year, r.month, r.day), time), cal);
  }
  function AddDurationToDateTime(op, s, d, options) {
    var dur = getDur(ToTemporalDuration(d));
    if (op === "subtract") dur = getDur(CreateNegatedTemporalDuration(dur));
    var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
    var id = ToInternalDurationRecordWith24HourDays(dur);
    var tr = AddTimeNs(s.isoDateTime.time, id.time);
    var dd = AdjustDateDurationRecord(id.date, tr.days);
    var added = CalendarDateAdd(s.calendar, s.isoDateTime.isoDate, dd, overflow);
    tr.days = 0;
    return CreateTemporalDateTime(DateTime(added, tr), s.calendar);
  }
  function DifferenceTemporalPlainDateTime(op, s, other, options) {
    var o = getPDT(ToTemporalDateTime(other));
    if (!CalendarEquals(s.calendar, o.calendar)) throw rangeError("calendars differ");
    var st = GetDifferenceSettings(op, GetOptionsObject(options), "datetime", [], "nanosecond", "day");
    if (CompareISODateTime(s.isoDateTime, o.isoDateTime) === 0) return CreateTemporalDuration(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    var id = DifferencePlainDateTimeWithRounding(s.isoDateTime, o.isoDateTime, s.calendar, st.largestUnit,
      st.roundingIncrement, st.smallestUnit, st.roundingMode);
    var r = TemporalDurationFromInternal(id, st.largestUnit);
    return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
  }

  // ---------------- Temporal.PlainYearMonth ----------------
  var YM_GETTERS = ["calendarId", "era", "eraYear", "year", "month", "monthCode", "daysInYear", "daysInMonth", "monthsInYear", "inLeapYear"];
  class PlainYearMonth {
    #s;
    constructor(isoYear, isoMonth, calendar = undefined, referenceISODay = undefined) {
      if (isoYear === TOKEN) { this.#s = isoMonth; return; }
      var y = ToIntegerWithTruncation(isoYear), m = ToIntegerWithTruncation(isoMonth);
      var c = toCalendarArg(calendar);
      var rd = referenceISODay === undefined ? 1 : ToIntegerWithTruncation(referenceISODay);
      if (!IsValidISODate(y, m, rd)) throw rangeError("invalid year-month");
      var d = ISODate(y, m, rd);
      if (!ISOYearMonthWithinLimits(d)) throw rangeError("year-month out of range");
      this.#s = { isoDate: d, calendar: c };
    }
    static { isPlainYearMonth = function (o) { return isObject(o) && #s in o; }; getPYM = function (o) { return o.#s; }; }
    static from(item, options = undefined) { return ToTemporalYearMonth(item, options); }
    static compare(one, two) { return CompareISODate(getPYM(ToTemporalYearMonth(one)).isoDate, getPYM(ToTemporalYearMonth(two)).isoDate); }
    with(temporalYearMonthLike, options = undefined) {
      var s = getPYM(this);
      requirePartial(temporalYearMonthLike);
      var f = ISODateToFields(s.calendar, s.isoDate, "year-month");
      var p = PrepareCalendarFields(s.calendar, temporalYearMonthLike, ["year", "month", "monthCode"], [], "partial");
      f = CalendarMergeFields(s.calendar, f, p);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalYearMonth(CalendarYearMonthFromFields(s.calendar, f, overflow), s.calendar);
    }
    add(d, options = undefined) { return AddDurationToYearMonth("add", getPYM(this), d, options); }
    subtract(d, options = undefined) { return AddDurationToYearMonth("subtract", getPYM(this), d, options); }
    until(other, options = undefined) { return DifferenceTemporalPlainYearMonth("until", getPYM(this), other, options); }
    since(other, options = undefined) { return DifferenceTemporalPlainYearMonth("since", getPYM(this), other, options); }
    equals(other) {
      var s = getPYM(this), o = getPYM(ToTemporalYearMonth(other));
      return CompareISODate(s.isoDate, o.isoDate) === 0 && CalendarEquals(s.calendar, o.calendar);
    }
    toString(options = undefined) { var s = getPYM(this); return TemporalYearMonthToString(s, GetTemporalShowCalendarNameOption(GetOptionsObject(options))); }
    toLocaleString() { return TemporalYearMonthToString(getPYM(this), "auto"); }
    toJSON() { return TemporalYearMonthToString(getPYM(this), "auto"); }
    valueOf() { throwValueOf(); }
    toPlainDate(item) {
      var s = getPYM(this);
      if (!isObject(item)) throw typeError("argument must be an object");
      var f = ISODateToFields(s.calendar, s.isoDate, "year-month");
      var input = PrepareCalendarFields(s.calendar, item, ["day"], [], []);
      var merged = CalendarMergeFields(s.calendar, f, input);
      return CreateTemporalDate(CalendarDateFromFields(s.calendar, merged, "constrain"), s.calendar);
    }
  }
  tagged(PlainYearMonth, "Temporal.PlainYearMonth");
  installGetters(PlainYearMonth, YM_GETTERS, dateGetters(PlainYearMonth, function (o) { return getPYM(o); }, function (s) { return s.isoDate; }));
  function CreateTemporalYearMonth(d, calendar) {
    if (!ISOYearMonthWithinLimits(d)) throw rangeError("year-month out of range");
    return new PlainYearMonth(TOKEN, { isoDate: ISODate(d.year, d.month, d.day), calendar: calendar });
  }
  function TemporalYearMonthToString(s, show) {
    var r = PadISOYear(s.isoDate.year) + "-" + pad(s.isoDate.month, 2);
    if (show === "always" || show === "critical" || s.calendar !== "iso8601") r += "-" + pad(s.isoDate.day, 2);
    return r + FormatCalendarAnnotation(s.calendar, show);
  }
  function ToTemporalYearMonth(item, options) {
    if (isObject(item)) {
      if (isPlainYearMonth(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); var s = getPYM(item); return CreateTemporalYearMonth(s.isoDate, s.calendar); }
      var calendar = GetTemporalCalendarIdentifierWithISODefault(item);
      var f = PrepareCalendarFields(calendar, item, ["year", "month", "monthCode"], [], []);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalYearMonth(CalendarYearMonthFromFields(calendar, f, overflow), calendar);
    }
    if (typeof item !== "string") throw typeError("year-month must be a string or an object");
    var r = ParseISODateTime(item, ["yearmonth"]);
    var cal = CanonicalizeCalendar(r.calendar === undefined ? "iso8601" : r.calendar);
    GetTemporalOverflowOption(GetOptionsObject(options));
    var d = ISODate(r.year, r.month, r.day);
    if (!ISOYearMonthWithinLimits(d)) throw rangeError("year-month out of range");
    var fields = ISODateToFields(cal, d, "year-month");
    return CreateTemporalYearMonth(CalendarYearMonthFromFields(cal, fields, "constrain"), cal);
  }
  function AddDurationToYearMonth(op, s, d, options) {
    var dur = getDur(ToTemporalDuration(d));
    if (op === "subtract") dur = getDur(CreateNegatedTemporalDuration(dur));
    var id = ToInternalDurationRecord(dur);
    var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
    var add = id.date;
    if (add.weeks !== 0 || add.days !== 0 || id.time !== 0n) throw rangeError("only years and months can be added to a year-month");
    var f = ISODateToFields(s.calendar, s.isoDate, "year-month");
    f.day = 1;
    var date = CalendarDateFromFields(s.calendar, f, "constrain");
    var added = CalendarDateAdd(s.calendar, date, add, overflow);
    var af = ISODateToFields(s.calendar, added, "year-month");
    return CreateTemporalYearMonth(CalendarYearMonthFromFields(s.calendar, af, overflow), s.calendar);
  }
  function DifferenceTemporalPlainYearMonth(op, s, other, options) {
    var o = getPYM(ToTemporalYearMonth(other));
    if (!CalendarEquals(s.calendar, o.calendar)) throw rangeError("calendars differ");
    var st = GetDifferenceSettings(op, GetOptionsObject(options), "date", ["week", "day"], "month", "year");
    if (CompareISODate(s.isoDate, o.isoDate) === 0) return CreateTemporalDuration(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    var tf = ISODateToFields(s.calendar, s.isoDate, "year-month"); tf.day = 1;
    var td = CalendarDateFromFields(s.calendar, tf, "constrain");
    var of = ISODateToFields(s.calendar, o.isoDate, "year-month"); of.day = 1;
    var od = CalendarDateFromFields(s.calendar, of, "constrain");
    var dd = CalendarDateUntil(s.calendar, td, od, st.largestUnit);
    var d = Internal(AdjustDateDurationRecord(dd, 0, 0), 0n);
    if (st.smallestUnit !== "month" || st.roundingIncrement !== 1) {
      var a = DateTime(td, MidnightTime()), b = DateTime(od, MidnightTime());
      d = RoundRelativeDuration(d, GetUTCEpochNanoseconds(a), GetUTCEpochNanoseconds(b), a, undefined, s.calendar,
        st.largestUnit, st.roundingIncrement, st.smallestUnit, st.roundingMode);
    }
    var r = TemporalDurationFromInternal(d, "day");
    return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
  }

  // ---------------- Temporal.PlainMonthDay ----------------
  class PlainMonthDay {
    #s;
    constructor(isoMonth, isoDay, calendar = undefined, referenceISOYear = undefined) {
      if (isoMonth === TOKEN) { this.#s = isoDay; return; }
      var m = ToIntegerWithTruncation(isoMonth), d = ToIntegerWithTruncation(isoDay);
      var c = toCalendarArg(calendar);
      var ry = referenceISOYear === undefined ? 1972 : ToIntegerWithTruncation(referenceISOYear);
      if (!IsValidISODate(ry, m, d)) throw rangeError("invalid month-day");
      var dd = ISODate(ry, m, d);
      if (!ISODateWithinLimits(dd)) throw rangeError("month-day out of range");
      this.#s = { isoDate: dd, calendar: c };
    }
    static { isPlainMonthDay = function (o) { return isObject(o) && #s in o; }; getPMD = function (o) { return o.#s; }; }
    static from(item, options = undefined) { return ToTemporalMonthDay(item, options); }
    with(temporalMonthDayLike, options = undefined) {
      var s = getPMD(this);
      requirePartial(temporalMonthDayLike);
      var f = ISODateToFields(s.calendar, s.isoDate, "month-day");
      var p = PrepareCalendarFields(s.calendar, temporalMonthDayLike, DATE_FIELDS, [], "partial");
      f = CalendarMergeFields(s.calendar, f, p);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalMonthDay(CalendarMonthDayFromFields(s.calendar, f, overflow), s.calendar);
    }
    equals(other) {
      var s = getPMD(this), o = getPMD(ToTemporalMonthDay(other));
      return CompareISODate(s.isoDate, o.isoDate) === 0 && CalendarEquals(s.calendar, o.calendar);
    }
    toString(options = undefined) { var s = getPMD(this); return TemporalMonthDayToString(s, GetTemporalShowCalendarNameOption(GetOptionsObject(options))); }
    toLocaleString() { return TemporalMonthDayToString(getPMD(this), "auto"); }
    toJSON() { return TemporalMonthDayToString(getPMD(this), "auto"); }
    valueOf() { throwValueOf(); }
    toPlainDate(item) {
      var s = getPMD(this);
      if (!isObject(item)) throw typeError("argument must be an object");
      var f = ISODateToFields(s.calendar, s.isoDate, "month-day");
      var input = PrepareCalendarFields(s.calendar, item, ["year"], [], []);
      var merged = CalendarMergeFields(s.calendar, f, input);
      return CreateTemporalDate(CalendarDateFromFields(s.calendar, merged, "constrain"), s.calendar);
    }
  }
  tagged(PlainMonthDay, "Temporal.PlainMonthDay");
  installGetters(PlainMonthDay, ["calendarId", "monthCode", "day"], dateGetters(PlainMonthDay, function (o) { return getPMD(o); }, function (s) { return s.isoDate; }));
  function CreateTemporalMonthDay(d, calendar) {
    if (!ISODateWithinLimits(d)) throw rangeError("month-day out of range");
    return new PlainMonthDay(TOKEN, { isoDate: ISODate(d.year, d.month, d.day), calendar: calendar });
  }
  function TemporalMonthDayToString(s, show) {
    var r = pad(s.isoDate.month, 2) + "-" + pad(s.isoDate.day, 2);
    if (show === "always" || show === "critical" || s.calendar !== "iso8601") r = PadISOYear(s.isoDate.year) + "-" + r;
    return r + FormatCalendarAnnotation(s.calendar, show);
  }
  function ToTemporalMonthDay(item, options) {
    if (isObject(item)) {
      if (isPlainMonthDay(item)) { GetTemporalOverflowOption(GetOptionsObject(options)); var s = getPMD(item); return CreateTemporalMonthDay(s.isoDate, s.calendar); }
      var calendar = GetTemporalCalendarIdentifierWithISODefault(item);
      var f = PrepareCalendarFields(calendar, item, DATE_FIELDS, [], []);
      var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
      return CreateTemporalMonthDay(CalendarMonthDayFromFields(calendar, f, overflow), calendar);
    }
    if (typeof item !== "string") throw typeError("month-day must be a string or an object");
    var r = ParseISODateTime(item, ["monthday"]);
    var cal = CanonicalizeCalendar(r.calendar === undefined ? "iso8601" : r.calendar);
    GetTemporalOverflowOption(GetOptionsObject(options));
    if (cal === "iso8601") return CreateTemporalMonthDay(ISODate(1972, r.month, r.day), cal);
    var d = ISODate(r.year, r.month, r.day);
    if (!ISODateWithinLimits(d)) throw rangeError("month-day out of range");
    return CreateTemporalMonthDay(CalendarMonthDayFromFields(cal, ISODateToFields(cal, d, "month-day"), "constrain"), cal);
  }

  // ---------------- Temporal.Instant ----------------
  function ToBigIntV(v) {
    var p = isObject(v) ? ToPrimitiveNumberish(v) : v;
    switch (typeof p) {
      case "bigint": return p;
      case "boolean": return p ? 1n : 0n;
      case "string": return Big(p);
      case "undefined": case "symbol": throw typeError("Cannot convert to a BigInt");
      case "number": throw typeError("Cannot convert a Number to a BigInt");
    }
    if (p === null) throw typeError("Cannot convert null to a BigInt");
    throw typeError("Cannot convert to a BigInt");
  }
  function ToPrimitiveNumberish(v) {
    var ex = v[SymToPrimitive];
    if (ex !== undefined && ex !== null) {
      if (!isCallable(ex)) throw typeError("@@toPrimitive is not callable");
      var r = ex.call(v, "number");
      if (isObject(r)) throw typeError("@@toPrimitive returned an object");
      return r;
    }
    var names = ["valueOf", "toString"];
    for (var i = 0; i < 2; i++) {
      var f = v[names[i]];
      if (isCallable(f)) { var r2 = f.call(v); if (!isObject(r2)) return r2; }
    }
    throw typeError("Cannot convert object to primitive value");
  }
  class Instant {
    #s;
    constructor(epochNanoseconds) {
      if (epochNanoseconds === TOKEN) { this.#s = arguments[1]; return; }
      var ns = ToBigIntV(epochNanoseconds);
      if (!IsValidEpochNanoseconds(ns)) throw rangeError("instant out of range");
      this.#s = { ns: ns };
    }
    static { isInstant = function (o) { return isObject(o) && #s in o; }; getInst = function (o) { return o.#s; }; }
    static from(item) { return ToTemporalInstant(item); }
    static fromEpochMilliseconds(epochMilliseconds) {
      var n = ToNumberV(epochMilliseconds);
      if (!NumberIsInteger(n)) throw rangeError("epochMilliseconds must be an integer");
      var ns = Big(n) * 1000000n;
      if (!IsValidEpochNanoseconds(ns)) throw rangeError("instant out of range");
      return CreateTemporalInstant(ns);
    }
    static fromEpochNanoseconds(epochNanoseconds) {
      var ns = ToBigIntV(epochNanoseconds);
      if (!IsValidEpochNanoseconds(ns)) throw rangeError("instant out of range");
      return CreateTemporalInstant(ns);
    }
    static compare(one, two) { var a = getInst(ToTemporalInstant(one)).ns, b = getInst(ToTemporalInstant(two)).ns; return a > b ? 1 : a < b ? -1 : 0; }
    get epochMilliseconds() { return Num(bfdiv(getInst(this).ns, 1000000n)); }
    get epochNanoseconds() { return getInst(this).ns; }
    add(d) { return AddDurationToInstant("add", getInst(this), d); }
    subtract(d) { return AddDurationToInstant("subtract", getInst(this), d); }
    until(other, options = undefined) { return DifferenceTemporalInstant("until", getInst(this), other, options); }
    since(other, options = undefined) { return DifferenceTemporalInstant("since", getInst(this), other, options); }
    round(roundTo) {
      var s = getInst(this);
      var o = roundToObject(roundTo, "smallestUnit");
      var inc = GetRoundingIncrementOption(o);
      var mode = GetRoundingModeOption(o, "halfExpand");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", REQUIRED);
      ValidateTemporalUnitValue(smallest, "time");
      var max = { hour: 24, minute: 1440, second: 86400, millisecond: 86400e3, microsecond: 86400e6, nanosecond: 86400e9 }[smallest];
      ValidateTemporalRoundingIncrement(inc, max, true);
      return CreateTemporalInstant(roundBigToIncrementAsIfPositive(s.ns, unitNs(smallest) * Big(inc), mode));
    }
    equals(other) { return getInst(this).ns === getInst(ToTemporalInstant(other)).ns; }
    toString(options = undefined) {
      var s = getInst(this);
      var o = GetOptionsObject(options);
      var digits = GetTemporalFractionalSecondDigitsOption(o);
      var mode = GetRoundingModeOption(o, "trunc");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
      var tzv = o.timeZone;
      ValidateTemporalUnitValue(smallest, "time");
      if (smallest === "hour") throw rangeError("smallestUnit must not be hour");
      var tz = tzv === undefined ? undefined : ToTemporalTimeZoneIdentifier(tzv);
      var pr = ToSecondsStringPrecisionRecord(smallest, digits);
      var ns = roundBigToIncrementAsIfPositive(s.ns, unitNs(pr.unit) * Big(pr.increment), mode);
      return TemporalInstantToString(ns, tz, pr.precision);
    }
    toLocaleString() { return TemporalInstantToString(getInst(this).ns, undefined, "auto"); }
    toJSON() { return TemporalInstantToString(getInst(this).ns, undefined, "auto"); }
    valueOf() { throwValueOf(); }
    toZonedDateTimeISO(timeZone) {
      var tz = ToTemporalTimeZoneIdentifier(timeZone);
      return CreateTemporalZonedDateTime(getInst(this).ns, tz, "iso8601");
    }
  }
  tagged(Instant, "Temporal.Instant");
  function CreateTemporalInstant(ns) { return new Instant(TOKEN, { ns: ns }); }
  function TemporalInstantToString(ns, tz, precision) {
    var outTz = tz === undefined ? "UTC" : tz;
    var dt = GetISODateTimeFor(outTz, ns);
    var r = ISODateTimeToString(dt, "iso8601", precision, "never");
    if (tz === undefined) return r + "Z";
    return r + FormatDateTimeUTCOffsetRounded(GetOffsetNanosecondsFor(outTz, ns));
  }
  function ToTemporalInstant(item) {
    if (isObject(item)) {
      if (isInstant(item)) return CreateTemporalInstant(getInst(item).ns);
      if (isZonedDateTime(item)) return CreateTemporalInstant(getZDT(item).ns);
      item = ToPrimitiveString(item);
    }
    if (typeof item !== "string") throw typeError("instant must be a string or an object");
    var r = ParseISODateTime(item, ["instant"]);
    var off = r.timeZone.z ? 0 : ParseDateTimeUTCOffset(r.timeZone.offsetString);
    var t = r.time === "start-of-day" ? MidnightTime() : r.time;
    var ns = GetUTCEpochNanoseconds(DateTime(ISODate(r.year, r.month, r.day), t)) - Big(off);
    CheckISODaysRange(epochNsToDateTime(ns).isoDate);
    if (!IsValidEpochNanoseconds(ns)) throw rangeError("instant out of range");
    return CreateTemporalInstant(ns);
  }
  function AddDurationToInstant(op, s, d) {
    var dur = getDur(ToTemporalDuration(d));
    if (op === "subtract") dur = getDur(CreateNegatedTemporalDuration(dur));
    var lu = DefaultTemporalLargestUnitOf(dur);
    if (IsDateUnit(lu)) throw rangeError("an instant cannot add years, months, weeks or days");
    var id = ToInternalDurationRecordWith24HourDays(dur);
    return CreateTemporalInstant(AddInstant(s.ns, id.time));
  }
  function DifferenceTemporalInstant(op, s, other, options) {
    var o = getInst(ToTemporalInstant(other));
    var st = GetDifferenceSettings(op, GetOptionsObject(options), "time", [], "nanosecond", "second");
    var id = DifferenceInstant(s.ns, o.ns, st.roundingIncrement, st.smallestUnit, st.roundingMode);
    var r = TemporalDurationFromInternal(id, st.largestUnit);
    return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
  }

  // ---------------- Temporal.ZonedDateTime ----------------
  class ZonedDateTime {
    #s;
    constructor(epochNanoseconds, timeZone, calendar = undefined) {
      if (epochNanoseconds === TOKEN) { this.#s = timeZone; return; }
      var ns = ToBigIntV(epochNanoseconds);
      if (!IsValidEpochNanoseconds(ns)) throw rangeError("instant out of range");
      if (typeof timeZone !== "string") throw typeError("time zone must be a string");
      var p = ParseTimeZoneIdentifier(timeZone), tz;
      if (p.offsetMinutes === undefined) {
        var rec = GetAvailableNamedTimeZoneIdentifier(p.name);
        if (rec === undefined) throw rangeError("unknown time zone: " + p.name);
        tz = rec.identifier;
      } else tz = FormatOffsetTimeZoneIdentifier(p.offsetMinutes);
      var c = toCalendarArg(calendar);
      this.#s = { ns: ns, timeZone: tz, calendar: c };
    }
    static { isZonedDateTime = function (o) { return isObject(o) && #s in o; }; getZDT = function (o) { return o.#s; }; }
    static from(item, options = undefined) { return ToTemporalZonedDateTime(item, options); }
    static compare(one, two) { var a = getZDT(ToTemporalZonedDateTime(one)).ns, b = getZDT(ToTemporalZonedDateTime(two)).ns; return a > b ? 1 : a < b ? -1 : 0; }
    get timeZoneId() { return getZDT(this).timeZone; }
    get hour() { return zdtDateTime(getZDT(this)).time.hour; }
    get minute() { return zdtDateTime(getZDT(this)).time.minute; }
    get second() { return zdtDateTime(getZDT(this)).time.second; }
    get millisecond() { return zdtDateTime(getZDT(this)).time.millisecond; }
    get microsecond() { return zdtDateTime(getZDT(this)).time.microsecond; }
    get nanosecond() { return zdtDateTime(getZDT(this)).time.nanosecond; }
    get epochMilliseconds() { return Num(bfdiv(getZDT(this).ns, 1000000n)); }
    get epochNanoseconds() { return getZDT(this).ns; }
    get hoursInDay() {
      var s = getZDT(this);
      var today = zdtDateTime(s).isoDate, tomorrow = AddDaysToISODate(today, 1);
      var a = GetStartOfDay(s.timeZone, today), b = GetStartOfDay(s.timeZone, tomorrow);
      return TotalTimeDuration(b - a, "hour");
    }
    get offsetNanoseconds() { var s = getZDT(this); return GetOffsetNanosecondsFor(s.timeZone, s.ns); }
    get offset() { var s = getZDT(this); return FormatUTCOffsetNanoseconds(GetOffsetNanosecondsFor(s.timeZone, s.ns)); }
    with(temporalZonedDateTimeLike, options = undefined) {
      var s = getZDT(this);
      requirePartial(temporalZonedDateTimeLike);
      var offNs = GetOffsetNanosecondsFor(s.timeZone, s.ns);
      var dt = GetISODateTimeFor(s.timeZone, s.ns);
      var f = ISODateToFields(s.calendar, dt.isoDate, "date");
      var t = dt.time;
      f.hour = t.hour; f.minute = t.minute; f.second = t.second; f.millisecond = t.millisecond; f.microsecond = t.microsecond; f.nanosecond = t.nanosecond;
      f.offset = FormatUTCOffsetNanoseconds(offNs);
      var p = PrepareCalendarFields(s.calendar, temporalZonedDateTimeLike, DATE_FIELDS, TIME_FIELDS.concat(["offset"]), "partial");
      f = CalendarMergeFields(s.calendar, f, p);
      var o = GetOptionsObject(options);
      var dis = GetTemporalDisambiguationOption(o);
      var offOpt = GetTemporalOffsetOption(o, "prefer");
      var overflow = GetTemporalOverflowOption(o);
      var res = InterpretTemporalDateTimeFields(s.calendar, f, overflow);
      var newOff = ParseDateTimeUTCOffset(f.offset);
      var ns = InterpretISODateTimeOffset(res.isoDate, res.time, "option", newOff, s.timeZone, dis, offOpt, "match-exactly");
      return CreateTemporalZonedDateTime(ns, s.timeZone, s.calendar);
    }
    withPlainTime(plainTimeLike = undefined) {
      var s = getZDT(this);
      var dt = GetISODateTimeFor(s.timeZone, s.ns), ns;
      if (plainTimeLike === undefined) ns = GetStartOfDay(s.timeZone, dt.isoDate);
      else ns = GetEpochNanosecondsFor(s.timeZone, DateTime(dt.isoDate, getPT(ToTemporalTime(plainTimeLike)).time), "compatible");
      return CreateTemporalZonedDateTime(ns, s.timeZone, s.calendar);
    }
    withTimeZone(timeZoneLike) { var s = getZDT(this); return CreateTemporalZonedDateTime(s.ns, ToTemporalTimeZoneIdentifier(timeZoneLike), s.calendar); }
    withCalendar(calendarLike) { var s = getZDT(this); return CreateTemporalZonedDateTime(s.ns, s.timeZone, ToTemporalCalendarIdentifier(calendarLike)); }
    add(d, options = undefined) { return AddDurationToZonedDateTime("add", getZDT(this), d, options); }
    subtract(d, options = undefined) { return AddDurationToZonedDateTime("subtract", getZDT(this), d, options); }
    until(other, options = undefined) { return DifferenceTemporalZonedDateTime("until", getZDT(this), other, options); }
    since(other, options = undefined) { return DifferenceTemporalZonedDateTime("since", getZDT(this), other, options); }
    round(roundTo) {
      var s = getZDT(this);
      var o = roundToObject(roundTo, "smallestUnit");
      var inc = GetRoundingIncrementOption(o);
      var mode = GetRoundingModeOption(o, "halfExpand");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", REQUIRED);
      ValidateTemporalUnitValue(smallest, "time", ["day"]);
      var max, inclusive;
      if (smallest === "day") { max = 1; inclusive = true; } else { max = MaximumTemporalDurationRoundingIncrement(smallest); inclusive = false; }
      ValidateTemporalRoundingIncrement(inc, max, inclusive);
      if (smallest === "nanosecond" && inc === 1) return CreateTemporalZonedDateTime(s.ns, s.timeZone, s.calendar);
      var dt = GetISODateTimeFor(s.timeZone, s.ns), ns;
      if (smallest === "day") {
        var startNs = GetStartOfDay(s.timeZone, dt.isoDate), endNs = GetStartOfDay(s.timeZone, AddDaysToISODate(dt.isoDate, 1));
        var len = endNs - startNs, progress = s.ns - startNs;
        // the result is a start of day: the date's own or the next one's, even when a backward shift puts this
        // instant after the next date's start (proposal-temporal #3312: midnight occurs twice)
        if (progress >= len) progress = len - 1n;
        ns = startNs + roundBigToIncrement(progress, len, mode);
      } else {
        var rr = RoundISODateTime(dt, inc, smallest, mode);
        var off = GetOffsetNanosecondsFor(s.timeZone, s.ns);
        ns = InterpretISODateTimeOffset(rr.isoDate, rr.time, "option", off, s.timeZone, "compatible", "prefer", "match-exactly");
      }
      return CreateTemporalZonedDateTime(ns, s.timeZone, s.calendar);
    }
    equals(other) {
      var s = getZDT(this), o = getZDT(ToTemporalZonedDateTime(other));
      if (s.ns !== o.ns) return false;
      if (!TimeZoneEquals(s.timeZone, o.timeZone)) return false;
      return CalendarEquals(s.calendar, o.calendar);
    }
    toString(options = undefined) {
      var s = getZDT(this);
      var o = GetOptionsObject(options);
      var showCal = GetTemporalShowCalendarNameOption(o);
      var digits = GetTemporalFractionalSecondDigitsOption(o);
      var showOff = GetTemporalShowOffsetOption(o);
      var mode = GetRoundingModeOption(o, "trunc");
      var smallest = GetTemporalUnitValuedOption(o, "smallestUnit", undefined);
      var showTz = GetTemporalShowTimeZoneNameOption(o);
      ValidateTemporalUnitValue(smallest, "time");
      if (smallest === "hour") throw rangeError("smallestUnit must not be hour");
      var pr = ToSecondsStringPrecisionRecord(smallest, digits);
      return TemporalZonedDateTimeToString(s, pr.precision, showCal, showTz, showOff, pr.increment, pr.unit, mode);
    }
    toLocaleString() { return TemporalZonedDateTimeToString(getZDT(this), "auto", "auto", "auto", "auto"); }
    toJSON() { return TemporalZonedDateTimeToString(getZDT(this), "auto", "auto", "auto", "auto"); }
    valueOf() { throwValueOf(); }
    startOfDay() {
      var s = getZDT(this);
      return CreateTemporalZonedDateTime(GetStartOfDay(s.timeZone, GetISODateTimeFor(s.timeZone, s.ns).isoDate), s.timeZone, s.calendar);
    }
    getTimeZoneTransition(directionParam) {
      var s = getZDT(this);
      if (directionParam === undefined) throw typeError("direction is required");
      var o;
      if (typeof directionParam === "string") { o = ObjectCreate(null); o.direction = directionParam; }
      else o = GetOptionsObject(directionParam);
      var dir = GetDirectionOption(o);
      if (IsOffsetTimeZoneIdentifier(s.timeZone)) return null;
      var t = dir === "next" ? GetNamedTimeZoneNextTransition(s.timeZone, s.ns) : GetNamedTimeZonePreviousTransition(s.timeZone, s.ns);
      return t === null ? null : CreateTemporalZonedDateTime(t, s.timeZone, s.calendar);
    }
    toInstant() { return CreateTemporalInstant(getZDT(this).ns); }
    toPlainDate() { var s = getZDT(this); return CreateTemporalDate(zdtDateTime(s).isoDate, s.calendar); }
    toPlainTime() { return CreateTemporalTime(zdtDateTime(getZDT(this)).time); }
    toPlainDateTime() { var s = getZDT(this); return CreateTemporalDateTime(zdtDateTime(s), s.calendar); }
  }
  tagged(ZonedDateTime, "Temporal.ZonedDateTime");
  installGetters(ZonedDateTime, DATE_GETTERS, dateGetters(ZonedDateTime, function (o) { return getZDT(o); }, function (s) { return zdtDateTime(s).isoDate; }));
  function zdtDateTime(s) { return GetISODateTimeFor(s.timeZone, s.ns); }
  function CreateTemporalZonedDateTime(ns, tz, calendar) { return new ZonedDateTime(TOKEN, { ns: ns, timeZone: tz, calendar: calendar }); }
  function TemporalZonedDateTimeToString(s, precision, showCal, showTz, showOff, increment, unit, mode) {
    if (increment === undefined) increment = 1;
    if (unit === undefined) unit = "nanosecond";
    if (mode === undefined) mode = "trunc";
    var ns = roundBigToIncrementAsIfPositive(s.ns, unitNs(unit) * Big(increment), mode);
    var off = GetOffsetNanosecondsFor(s.timeZone, ns);
    var dt = GetISODateTimeFor(s.timeZone, ns);
    var r = ISODateTimeToString(dt, "iso8601", precision, "never");
    if (showOff !== "never") r += FormatDateTimeUTCOffsetRounded(off);
    if (showTz !== "never") r += "[" + (showTz === "critical" ? "!" : "") + s.timeZone + "]";
    return r + FormatCalendarAnnotation(s.calendar, showCal);
  }
  function ToTemporalZonedDateTime(item, options) {
    var hasZ = false, matchBehaviour = "match-exactly", calendar, tz, offsetString, isoDate, time, dis, offOpt;
    if (isObject(item)) {
      if (isZonedDateTime(item)) {
        var o0 = GetOptionsObject(options);
        GetTemporalDisambiguationOption(o0); GetTemporalOffsetOption(o0, "reject"); GetTemporalOverflowOption(o0);
        var s = getZDT(item);
        return CreateTemporalZonedDateTime(s.ns, s.timeZone, s.calendar);
      }
      calendar = GetTemporalCalendarIdentifierWithISODefault(item);
      var f = PrepareCalendarFields(calendar, item, DATE_FIELDS, TIME_FIELDS.concat(["offset", "timeZone"]), ["timeZone"]);
      tz = f.timeZone; offsetString = f.offset;
      var o = GetOptionsObject(options);
      dis = GetTemporalDisambiguationOption(o);
      offOpt = GetTemporalOffsetOption(o, "reject");
      var overflow = GetTemporalOverflowOption(o);
      var res = InterpretTemporalDateTimeFields(calendar, f, overflow);
      isoDate = res.isoDate; time = res.time;
    } else {
      if (typeof item !== "string") throw typeError("zoned date-time must be a string or an object");
      var r = ParseISODateTime(item, ["zoned"]);
      tz = ToTemporalTimeZoneIdentifier(r.timeZone.tzAnnotation);
      offsetString = r.timeZone.offsetString;
      if (r.timeZone.z) hasZ = true;
      calendar = CanonicalizeCalendar(r.calendar === undefined ? "iso8601" : r.calendar);
      matchBehaviour = "match-minutes";
      if (offsetString !== undefined && hasSubMinute(offsetString)) matchBehaviour = "match-exactly";
      var o2 = GetOptionsObject(options);
      dis = GetTemporalDisambiguationOption(o2);
      offOpt = GetTemporalOffsetOption(o2, "reject");
      GetTemporalOverflowOption(o2);
      isoDate = ISODate(r.year, r.month, r.day);
      time = r.time;
    }
    var ob = hasZ ? "exact" : offsetString === undefined ? "wall" : "option";
    var offNs = ob === "option" ? ParseDateTimeUTCOffset(offsetString) : 0;
    var ns = InterpretISODateTimeOffset(isoDate, time, ob, offNs, tz, dis, offOpt, matchBehaviour);
    return CreateTemporalZonedDateTime(ns, tz, calendar);
  }
  function AddDurationToZonedDateTime(op, s, d, options) {
    var dur = getDur(ToTemporalDuration(d));
    if (op === "subtract") dur = getDur(CreateNegatedTemporalDuration(dur));
    var overflow = GetTemporalOverflowOption(GetOptionsObject(options));
    var id = ToInternalDurationRecord(dur);
    return CreateTemporalZonedDateTime(AddZonedDateTime(s.ns, s.timeZone, s.calendar, id, overflow), s.timeZone, s.calendar);
  }
  function DifferenceTemporalZonedDateTime(op, s, other, options) {
    var o = getZDT(ToTemporalZonedDateTime(other));
    if (!CalendarEquals(s.calendar, o.calendar)) throw rangeError("calendars differ");
    var st = GetDifferenceSettings(op, GetOptionsObject(options), "datetime", [], "nanosecond", "hour");
    var r;
    if (!IsDateUnit(st.largestUnit)) {
      r = TemporalDurationFromInternal(DifferenceInstant(s.ns, o.ns, st.roundingIncrement, st.smallestUnit, st.roundingMode), st.largestUnit);
      return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
    }
    if (!TimeZoneEquals(s.timeZone, o.timeZone)) throw rangeError("time zones differ");
    if (s.ns === o.ns) return CreateTemporalDuration(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
    var id = DifferenceZonedDateTimeWithRounding(s.ns, o.ns, s.timeZone, s.calendar, st.largestUnit, st.roundingIncrement, st.smallestUnit, st.roundingMode);
    r = TemporalDurationFromInternal(id, "hour");
    return op === "since" ? CreateNegatedTemporalDuration(getDur(r)) : r;
  }

  // ---------------- Temporal.Now and the namespace ----------------
  function SystemDateTime(tzLike) {
    var tz = tzLike === undefined ? SystemTimeZoneIdentifier() : ToTemporalTimeZoneIdentifier(tzLike);
    return GetISODateTimeFor(tz, nowNs());
  }
  var Now = {
    instant: function instant() { return CreateTemporalInstant(nowNs()); },
    timeZoneId: function timeZoneId() { return SystemTimeZoneIdentifier(); },
    zonedDateTimeISO: function zonedDateTimeISO(temporalTimeZoneLike = undefined) {
      var tz = temporalTimeZoneLike === undefined ? SystemTimeZoneIdentifier() : ToTemporalTimeZoneIdentifier(temporalTimeZoneLike);
      return CreateTemporalZonedDateTime(nowNs(), tz, "iso8601");
    },
    plainDateTimeISO: function plainDateTimeISO(temporalTimeZoneLike = undefined) { return CreateTemporalDateTime(SystemDateTime(temporalTimeZoneLike), "iso8601"); },
    plainDateISO: function plainDateISO(temporalTimeZoneLike = undefined) { return CreateTemporalDate(SystemDateTime(temporalTimeZoneLike).isoDate, "iso8601"); },
    plainTimeISO: function plainTimeISO(temporalTimeZoneLike = undefined) { return CreateTemporalTime(SystemDateTime(temporalTimeZoneLike).time); }
  };
  var NowObj = {};
  var nowNames = ["instant", "timeZoneId", "zonedDateTimeISO", "plainDateTimeISO", "plainDateISO", "plainTimeISO"];
  for (var ni = 0; ni < nowNames.length; ni++) {
    // method-like: not constructors
    def(NowObj, nowNames[ni], methodOf(Now[nowNames[ni]], nowNames[ni]));
  }
  function methodOf(f, name) {
    var len = f.length;
    var m = { [name](a) { return f.apply(undefined, arguments); } }[name];
    ObjectDefineProperty(m, "length", { value: len, configurable: true });
    return m;
  }
  ObjectDefineProperty(NowObj, SymToStringTag, { value: "Temporal.Now", writable: false, enumerable: false, configurable: true });

  var CLASSES = [Instant, PlainDateTime, Duration, PlainDate, PlainTime, PlainYearMonth, PlainMonthDay, ZonedDateTime];
  for (var ci = 0; ci < CLASSES.length; ci++) markIntrinsic(CLASSES[ci]);
  var Temporal = {};
  def(Temporal, "Instant", Instant);
  def(Temporal, "PlainDateTime", PlainDateTime);
  def(Temporal, "Duration", Duration);
  def(Temporal, "PlainDate", PlainDate);
  def(Temporal, "PlainTime", PlainTime);
  def(Temporal, "PlainYearMonth", PlainYearMonth);
  def(Temporal, "PlainMonthDay", PlainMonthDay);
  def(Temporal, "ZonedDateTime", ZonedDateTime);
  def(Temporal, "Now", NowObj);
  ObjectDefineProperty(Temporal, SymToStringTag, { value: "Temporal", writable: false, enumerable: false, configurable: true });
  def(G, "Temporal", Temporal);

  // what Intl (prelude/intl.js) needs to format Temporal values: the brand, and the instant to format in
  // milliseconds (a plain value as UTC: a date at noon, a time on 1970-01-01)
  function msOfNs(ns) { var q = ns / 1000000n; if (ns < 0n && q * 1000000n !== ns) q -= 1n; return Num(q); }
  return {
    kind(v) {
      if (!isObject(v)) return undefined;
      if (isPlainDate(v)) return "PlainDate";
      if (isPlainYearMonth(v)) return "PlainYearMonth";
      if (isPlainMonthDay(v)) return "PlainMonthDay";
      if (isPlainTime(v)) return "PlainTime";
      if (isPlainDateTime(v)) return "PlainDateTime";
      if (isInstant(v)) return "Instant";
      if (isZonedDateTime(v)) return "ZonedDateTime";
      if (isDuration(v)) return "Duration";
      return undefined;
    },
    calendar(v) { return calendarSlotOf(v); },
    timeZone(v) { return getZDT(v).timeZone; },
    ms(v) {
      if (isPlainDate(v)) return msOfNs(GetUTCEpochNanoseconds(DateTime(getPD(v).isoDate, Time(12, 0, 0, 0, 0, 0))));
      if (isPlainYearMonth(v)) return msOfNs(GetUTCEpochNanoseconds(DateTime(getPYM(v).isoDate, Time(12, 0, 0, 0, 0, 0))));
      if (isPlainMonthDay(v)) return msOfNs(GetUTCEpochNanoseconds(DateTime(getPMD(v).isoDate, Time(12, 0, 0, 0, 0, 0))));
      if (isPlainTime(v)) return msOfNs(GetUTCEpochNanoseconds(DateTime(ISODate(1970, 1, 1), getPT(v).time)));
      if (isPlainDateTime(v)) return msOfNs(GetUTCEpochNanoseconds(getPDT(v).isoDateTime));
      if (isInstant(v)) return msOfNs(getInst(v).ns);
      return msOfNs(getZDT(v).ns);
    }
  };
})

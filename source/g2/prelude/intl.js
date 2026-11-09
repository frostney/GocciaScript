(function (G, intlOp, markIntrinsic) {
  "use strict";
  // Intl (ECMA-402): the objects, their option processing and resolution follow the spec; locale data and the
  // formatting itself come from intlOp (G2.Intl: BCP47, IntlLocaleResolver, IntlICU and the embedded CLDR data).
  // Each object keeps its internal slots in one private field, stamped on by Slots (a constructor whose return
  // override lets a plain function's object get the field).

  // ---------------- primordials ----------------
  var ObjectDefineProperty = Object.defineProperty, ObjectCreate = Object.create, ObjectFreeze = Object.freeze;
  var ObjectGetPrototypeOf = Object.getPrototypeOf;
  var MathFloor = Math.floor, MathMin = Math.min, MathMax = Math.max, MathAbs = Math.abs;
  var Str = String, Num = Number, Bool = Boolean, Sym = Symbol, NumberIsNaN = Number.isNaN;
  var ArrayIsArray = Array.isArray;
  function typeError(m) { return new TypeError(m); }
  function rangeError(m) { return new RangeError(m); }
  function isObject(v) { return v !== null && (typeof v === "object" || typeof v === "function"); }
  function ToObject(v) { if (v === null || v === undefined) throw typeError("Cannot convert undefined or null to object"); return Object(v); }
  function ToNumber(v) { return +v; }
  function ToString(v) { if (typeof v === "symbol") throw typeError("Cannot convert a Symbol value to a string"); return "" + v; }
  function asciiLower(s) {
    var r = "";
    for (var i = 0; i < s.length; i++) {
      var c = s.charCodeAt(i);
      r += c >= 65 && c <= 90 ? Str.fromCharCode(c + 32) : s[i];
    }
    return r;
  }
  // internal records have no prototype, so a setter on Object.prototype never sees their fields
  function rec() { return ObjectCreate(null); }
  // CreateDataPropertyOrThrow, for objects handed to user code
  function cdp(o, k, v) { ObjectDefineProperty(o, k, { value: v, writable: true, enumerable: true, configurable: true }); }
  function def(o, k, v) { ObjectDefineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true }); }
  function getter(o, k, f) {
    ObjectDefineProperty(f, "name", { value: "get " + k, configurable: true });
    ObjectDefineProperty(o, k, { get: f, set: undefined, enumerable: false, configurable: true });
  }
  function tag(o, t) { ObjectDefineProperty(o, Sym.toStringTag, { value: t, writable: false, enumerable: false, configurable: true }); }
  function lengthOf(o) {
    var n = +o.length;
    if (!(n > 0)) return 0;
    n = MathFloor(n);
    return n > 9007199254740991 ? 9007199254740991 : n;
  }

  // the %Intl%.[[FallbackSymbol]] of the legacy constructor behaviour (ChainNumberFormat, ChainDateTimeFormat)
  var FallbackSymbol = Sym("IntlLegacyConstructedSymbol");

  // 7.3.21 OrdinaryHasInstance (the legacy constructor checks may not run @@hasInstance)
  function OrdinaryHasInstance(C, O) {
    if (!isObject(O)) return false;
    var P = C.prototype;
    for (var o = ObjectGetPrototypeOf(O); o !== null; o = ObjectGetPrototypeOf(o)) if (o === P) return true;
    return false;
  }

  // ---------------- internal slots ----------------
  class Base { constructor(o) { return o; } }
  class Slots extends Base {
    #s;
    constructor(o, s) { super(o); this.#s = s; }
    static get(o) { return isObject(o) && #s in o ? o.#s : undefined; }
  }
  function slotsOf(o, kind, method) {
    var s = Slots.get(o);
    if (s === undefined || s.kind !== kind) throw typeError(method + " called on an incompatible receiver");
    return s;
  }
  // 10.1.13 OrdinaryCreateFromConstructor, with the intrinsic prototype as the fallback
  function createFrom(newTarget, fallback) {
    var p = newTarget.prototype;
    if (!isObject(p)) p = fallback;
    return ObjectCreate(p);
  }

  // ---------------- 9.2 locale and parameter negotiation ----------------
  var available = null;
  function availableLocales() {
    if (available === null) {
      var a = intlOp(2), set = ObjectCreate(null);
      for (var i = 0; i < a.length; i++) set[a[i]] = true;
      available = set;
    }
    return available;
  }
  function DefaultLocale() { return intlOp(3); }
  function IsStructurallyValidLanguageTag(t) { return intlOp(0, t); }
  function CanonicalizeUnicodeLocaleId(t) {
    var c = intlOp(1, t);
    if (c === null) throw rangeError("Incorrect locale information provided: " + t);
    return c;
  }
  // 9.2.1 CanonicalizeLocaleList
  function CanonicalizeLocaleList(locales) {
    if (locales === undefined) return [];
    var seen = [], O;
    if (typeof locales === "string" || Slots.get(locales) !== undefined && Slots.get(locales).kind === "Locale") O = [locales];
    else O = ToObject(locales);
    var len = lengthOf(O);
    for (var k = 0; k < len; k++) {
      var Pk = ToString(k);
      if (!(Pk in O)) continue;
      var kValue = O[Pk];
      if (typeof kValue !== "string" && !isObject(kValue)) throw typeError("locale entries must be strings or objects");
      var ls = Slots.get(kValue), t;
      if (ls !== undefined && ls.kind === "Locale") t = ls.locale;
      else t = ToString(kValue);
      if (!IsStructurallyValidLanguageTag(t)) throw rangeError("Incorrect locale information provided: " + t);
      var c = CanonicalizeUnicodeLocaleId(t);
      if (seen.indexOf(c) < 0) seen.push(c);
    }
    return seen;
  }
  // the index of the Unicode locale extension sequence ("-u-...") of a canonical tag, and its end
  function unicodeExtensionRange(locale) {
    var parts = locale.split("-"), pos = 0, start = -1, end = locale.length;
    for (var i = 0; i < parts.length; i++) {
      var p = parts[i];
      if (i > 0 && p.length === 1) {
        if (start >= 0) { end = pos - 1; break; }
        if (p === "x") break;
        if (p === "u") start = pos - 1;
      }
      pos += p.length + 1;
    }
    if (start < 0) return null;
    return [start, end];
  }
  function removeUnicodeExtension(locale) {
    var r = unicodeExtensionRange(locale);
    return r === null ? locale : locale.slice(0, r[0]) + locale.slice(r[1]);
  }
  // 9.2.2 LookupMatchingLocaleByPrefix (BestFit matches the same way)
  function LookupMatchingLocale(availableSet, requested) {
    for (var i = 0; i < requested.length; i++) {
      var locale = requested[i], r = unicodeExtensionRange(locale), extension = r === null ? "" : locale.slice(r[0], r[1]);
      var prefix = removeUnicodeExtension(locale);
      while (prefix !== "") {
        if (availableSet[prefix] === true) return { locale: prefix, extension: extension };
        var pos = prefix.lastIndexOf("-");
        if (pos < 0) break;
        if (pos >= 2 && prefix[pos - 2] === "-") pos -= 2;
        prefix = prefix.slice(0, pos);
      }
    }
    return undefined;
  }
  // 9.2.4 UnicodeExtensionComponents
  function UnicodeExtensionComponents(ext) {
    var attributes = [], keywords = [], keyword = null, parts = ext.split("-");
    for (var i = 2; i < parts.length; i++) {
      var sub = parts[i];
      if (sub.length === 2) {
        if (keyword !== null) keywords.push(keyword);
        keyword = null;
        var dup = false;
        for (var j = 0; j < keywords.length; j++) if (keywords[j].key === sub) dup = true;
        if (!dup) keyword = { key: sub, value: "" };
        else keyword = { key: sub, value: "", dup: true };
      } else if (keyword === null) {
        if (attributes.indexOf(sub) < 0) attributes.push(sub);
      } else keyword.value = keyword.value === "" ? sub : keyword.value + "-" + sub;
    }
    if (keyword !== null) keywords.push(keyword);
    var kw = [];
    for (i = 0; i < keywords.length; i++) if (!keywords[i].dup) kw.push(keywords[i]);
    return { attributes: attributes, keywords: kw };
  }
  // 9.2.5 InsertUnicodeExtensionAndCanonicalize
  function InsertUnicodeExtensionAndCanonicalize(locale, attributes, keywords) {
    var ext = "-u";
    for (var i = 0; i < attributes.length; i++) ext += "-" + attributes[i];
    for (i = 0; i < keywords.length; i++) ext += "-" + keywords[i].key + (keywords[i].value === "" ? "" : "-" + keywords[i].value);
    var px = locale.indexOf("-x-"), l;
    if (px < 0) l = locale + ext; else l = locale.slice(0, px) + ext + locale.slice(px);
    return CanonicalizeUnicodeLocaleId(l);
  }
  // CanonicalizeUValue: the canonical form of a Unicode extension value
  function CanonicalizeUValue(key, value) {
    var c = intlOp(1, "und-u-" + key + "-" + value);
    if (c === null) return value;
    var r = unicodeExtensionRange(c);
    if (r === null) return "";
    var comps = UnicodeExtensionComponents(c.slice(r[0], r[1]));
    for (var i = 0; i < comps.keywords.length; i++) if (comps.keywords[i].key === key) return comps.keywords[i].value;
    return "";
  }
  // 9.2.7 ResolveLocale; localeData(locale, key) gives the supported values, the default first
  function ResolveLocale(availableSet, requested, options, relevantKeys, localeData) {
    var r = LookupMatchingLocale(availableSet, requested);
    if (r === undefined) r = { locale: DefaultLocale(), extension: "" };
    var foundLocale = r.locale, result = rec();
    result.dataLocale = foundLocale;
    var keywords = r.extension !== "" ? UnicodeExtensionComponents(r.extension).keywords : [];
    var supported = [];
    for (var i = 0; i < relevantKeys.length; i++) {
      var key = relevantKeys[i], keyLocaleData = localeData(foundLocale, key), value = keyLocaleData[0], sk = null;
      for (var j = 0; j < keywords.length; j++) {
        if (keywords[j].key !== key) continue;
        var rv = keywords[j].value;
        if (rv !== "") {
          if (keyLocaleData.indexOf(rv) >= 0) { value = rv; sk = { key: key, value: value }; }
        } else if (keyLocaleData.indexOf("true") >= 0) { value = "true"; sk = { key: key, value: "" }; }
        break;
      }
      var ov = options[key];
      if (typeof ov === "string") {
        ov = CanonicalizeUValue(asciiLower(key), ov);
        if (ov === "") ov = "true";
      }
      if (ov !== undefined && ov !== null && ov !== value && keyLocaleData.indexOf(ov) >= 0) { value = ov; sk = null; }
      if (sk !== null) supported.push(sk);
      result[key] = value;
    }
    if (supported.length > 0) foundLocale = InsertUnicodeExtensionAndCanonicalize(foundLocale, [], supported);
    result.locale = foundLocale;
    return result;
  }
  // 9.2.13 GetOption
  function GetOption(options, property, type, values, fallback) {
    var value = options[property];
    if (value === undefined) {
      if (fallback === REQUIRED) throw rangeError(property + " is required");
      return fallback;
    }
    if (type === "boolean") value = Bool(value);
    else if (type === "number") { value = ToNumber(value); if (NumberIsNaN(value)) throw rangeError(property + " is not a number"); }
    else value = ToString(value);
    if (values !== undefined && values.indexOf(value) < 0) throw rangeError("Value " + value + " out of range for " + property);
    return value;
  }
  var REQUIRED = {};
  // 9.2.14 GetBooleanOrStringNumberFormatOption
  function GetBooleanOrStringOption(options, property, stringValues, fallback) {
    var value = options[property];
    if (value === undefined) return fallback;
    if (value === true) return true;
    if (!Bool(value)) return false;
    value = ToString(value);
    if (stringValues.indexOf(value) < 0) throw rangeError("Value " + value + " out of range for " + property);
    return value;
  }
  function GetOptionsObject(options) {
    if (options === undefined) return ObjectCreate(null);
    if (isObject(options)) return options;
    throw typeError("options must be an object");
  }
  function CoerceOptionsToObject(options) {
    if (options === undefined) return ObjectCreate(null);
    return ToObject(options);
  }
  // 9.2.15 DefaultNumberOption, 9.2.16 GetNumberOption
  function DefaultNumberOption(value, min, max, fallback) {
    if (value === undefined) return fallback;
    value = ToNumber(value);
    if (NumberIsNaN(value) || value < min || value > max) throw rangeError("value out of range");
    return MathFloor(value);
  }
  function GetNumberOption(options, property, min, max, fallback) {
    return DefaultNumberOption(options[property], min, max, fallback);
  }
  // the type nonterminal of a Unicode locale extension value: (3*8alphanum) *("-" (3*8alphanum))
  function isUnicodeType(v) {
    if (v === "") return false;
    var parts = v.split("-");
    for (var i = 0; i < parts.length; i++) {
      var p = parts[i];
      if (p.length < 3 || p.length > 8) return false;
      for (var j = 0; j < p.length; j++) {
        var c = p.charCodeAt(j);
        if (!(c >= 48 && c <= 57 || c >= 65 && c <= 90 || c >= 97 && c <= 122)) return false;
      }
    }
    return true;
  }
  // 9.2.8 ResolveOptions; descriptors: [property, key, type, values]
  function ResolveOptions(availableSet, localeData, relevantKeys, descriptors, locales, options, coerce, requireOptions, modify) {
    var requested = CanonicalizeLocaleList(locales);
    if (requireOptions && options === undefined) throw typeError("options are required");
    options = coerce ? CoerceOptionsToObject(options) : GetOptionsObject(options);
    var matcher = GetOption(options, "localeMatcher", "string", ["lookup", "best fit"], "best fit");
    var opt = rec();
    opt.localeMatcher = matcher;
    for (var i = 0; i < descriptors.length; i++) {
      var d = descriptors[i];
      var value = GetOption(options, d[0], d[2] || "string", d[3], undefined);
      if (value !== undefined) {
        value = ToString(value);
        if (!isUnicodeType(value)) throw rangeError("Invalid " + d[0] + ": " + value);
      }
      opt[d[1]] = value;
    }
    if (modify !== undefined) modify(opt);
    var r = ResolveLocale(availableSet, requested, opt, relevantKeys, localeData);
    return { options: options, resolved: r, resolution: opt };
  }
  // 9.2.10 FilterLocales (SupportedLocales)
  function FilterLocales(availableSet, requested, options) {
    options = CoerceOptionsToObject(options);
    GetOption(options, "localeMatcher", "string", ["lookup", "best fit"], "best fit");
    var subset = [];
    for (var i = 0; i < requested.length; i++) {
      if (LookupMatchingLocale(availableSet, [removeUnicodeExtension(requested[i])]) !== undefined) subset.push(requested[i]);
    }
    return subset;
  }
  function supportedLocalesOf(locales, options) {
    var requested = CanonicalizeLocaleList(locales);
    return FilterLocales(availableLocales(), requested, options);
  }
  function noData() { return [null]; }

  // a service constructor: a plain function that throws without new, its prototype not writable
  function makeService(C, name, length, proto) {
    ObjectDefineProperty(C, "length", { value: length, configurable: true });
    ObjectDefineProperty(C, "name", { value: name, configurable: true });
    ObjectDefineProperty(C, "prototype", { value: proto, writable: false, enumerable: false, configurable: false });
    def(proto, "constructor", C);
    def(C, "supportedLocalesOf", { supportedLocalesOf(locales, options = undefined) { return supportedLocalesOf(locales, options); } }.supportedLocalesOf);
    tag(proto, "Intl." + name);
    def(Intl, name, C);
  }
  function method(o, name, f) { def(o, name, f); }
  var CURRENCIES = ["AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD", "AWG", "AZN", "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND", "BOB", "BRL", "BSD", "BTN", "BWP", "BYN", "BZD", "CAD", "CDF", "CHF", "CLP", "CNY", "COP", "CRC", "CUP", "CVE", "CZK", "DJF", "DKK", "DOP", "DZD", "EGP", "ERN", "ETB", "EUR", "FJD", "FKP", "GBP", "GEL", "GHS", "GIP", "GMD", "GNF", "GTQ", "GYD", "HKD", "HNL", "HRK", "HTG", "HUF", "IDR", "ILS", "INR", "IQD", "IRR", "ISK", "JMD", "JOD", "JPY", "KES", "KGS", "KHR", "KMF", "KPW", "KRW", "KWD", "KYD", "KZT", "LAK", "LBP", "LKR", "LRD", "LSL", "LYD", "MAD", "MDL", "MGA", "MKD", "MMK", "MNT", "MOP", "MRU", "MUR", "MVR", "MWK", "MXN", "MYR", "MZN", "NAD", "NGN", "NIO", "NOK", "NPR", "NZD", "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN", "PYG", "QAR", "RON", "RSD", "RUB", "RWF", "SAR", "SBD", "SCR", "SDG", "SEK", "SGD", "SHP", "SLE", "SOS", "SRD", "SSP", "STN", "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP", "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD", "UYU", "UZS", "VES", "VND", "VUV", "WST", "XAF", "XCD", "XOF", "XPF", "YER", "ZAR", "ZMW", "ZWL", "ADP", "AFA", "ALK", "AOK", "AON", "AOR", "ARA", "ARL", "ARM", "ARP", "ATS", "AZM", "BAD", "BAN", "BEC", "BEF", "BEL", "BGL", "BGM", "BGO", "BOL", "BOP", "BOV", "BRB", "BRC", "BRE", "BRN", "BRR", "BRZ", "BUK", "BYB", "BYR", "CHE", "CHW", "CLE", "CLF", "CNH", "CNX", "COU", "CSD", "CSK", "CUC", "CYP", "DDM", "DEM", "ECS", "ECV", "EEK", "ESA", "ESB", "ESP", "FIM", "FRF", "GEK", "GHC", "GNS", "GQE", "GRD", "GWE", "GWP", "HRD", "IEP", "ILP", "ILR", "ISJ", "ITL", "KRH", "KRO", "LTL", "LTT", "LUC", "LUF", "LUL", "LVL", "LVR", "MAF", "MCF", "MDC", "MGF", "MKN", "MLF", "MRO", "MTL", "MTP", "MVP", "MXP", "MXV", "MZE", "MZM", "NIC", "NLG", "PEI", "PES", "PLZ", "PTE", "RHD", "ROL", "RUR", "SDD", "SDP", "SIT", "SKK", "SLL", "SRG", "STD", "SUR", "TJR", "TMM", "TPE", "TRL", "UAK", "UGS", "USN", "USS", "UYI", "UYP", "UYW", "VEB", "VED", "VEF", "VNN", "XAG", "XAU", "XBA", "XBB", "XBC", "XBD", "XCG", "XDR", "XEU", "XFO", "XFU", "XPD", "XPT", "XRE", "XSU", "XTS", "XUA", "XXX", "YDD", "YUD", "YUM", "YUN", "YUR", "ZAL", "ZMK", "ZRN", "ZRZ", "ZWD", "ZWG", "ZWR"];
  var NUMBERING_SYSTEMS = ["adlm", "ahom", "arab", "arabext", "bali", "beng", "bhks", "brah", "cakm", "cham", "deva", "fullwide", "gong", "gonm", "gujr", "guru", "hanidec", "hmng", "java", "kali", "khmr", "knda", "lana", "lanatham", "laoo", "latn", "lepc", "limb", "mathbold", "mathdbl", "mathmono", "mathsanb", "mathsans", "mlym", "modi", "mong", "mroo", "mtei", "mymr", "mymrshan", "mymrtlng", "newa", "nkoo", "olck", "orya", "osma", "rohg", "saur", "shrd", "sind", "sinh", "sora", "sund", "takr", "talu", "tamldec", "telu", "thai", "tibt", "tirh", "vaii", "wara", "wcho", "diak", "gara", "gukh", "hmnp", "kawi", "krai", "mymrepka", "mymrpao", "nagm", "onao", "outlined", "segment", "sunu", "tnsa", "tols"];
  var UNITS = ["acre", "bit", "byte", "celsius", "centimeter", "day", "degree", "fahrenheit", "fluid-ounce", "foot", "gallon", "gigabit", "gigabyte", "gram", "hectare", "hour", "inch", "kilobit", "kilobyte", "kilogram", "kilometer", "liter", "megabit", "megabyte", "meter", "microsecond", "mile", "mile-scandinavian", "milliliter", "millimeter", "millisecond", "minute", "month", "nanosecond", "ounce", "percent", "petabyte", "pound", "second", "stone", "terabit", "terabyte", "week", "yard", "year"];

  // ---------------- the Intl object (8) ----------------
  var Intl = {};
  tag(Intl, "Intl");
  def(Intl, "getCanonicalLocales", { getCanonicalLocales(locales) { return CanonicalizeLocaleList(locales); } }.getCanonicalLocales);
  var CALENDARS = ["buddhist", "chinese", "coptic", "dangi", "ethioaa", "ethiopic", "gregory", "hebrew", "indian",
    "islamic-civil", "islamic-tbla", "islamic-umalqura", "iso8601", "japanese", "persian", "roc"];
  var collationsList = null;
  function availableCollations() {
    if (collationsList === null) {
      var probe = ["en", "ar", "de", "es", "hi", "ko", "ln", "si", "sv", "zh"], set = [];
      for (var i = 0; i < probe.length; i++) {
        var a = intlOp(5, probe[i]);
        for (var j = 0; j < a.length; j++) {
          var c = a[j];
          if (c === "phonebook") c = "phonebk"; else if (c === "traditional") c = "trad"; else if (c === "dictionary") c = "dict";
          if (c !== "standard" && c !== "search" && set.indexOf(c) < 0) set.push(c);
        }
      }
      set.sort();
      collationsList = set;
    }
    return collationsList;
  }
  var timeZonesList = null;
  function availableTimeZones() {
    if (timeZonesList === null) { timeZonesList = intlOp(17); timeZonesList.sort(); }
    return timeZonesList;
  }
  def(Intl, "supportedValuesOf", { supportedValuesOf(key) {
    key = ToString(key);
    var list;
    if (key === "calendar") list = CALENDARS;
    else if (key === "collation") list = availableCollations();
    else if (key === "currency") list = CURRENCIES;
    else if (key === "numberingSystem") list = NUMBERING_SYSTEMS;
    else if (key === "timeZone") list = availableTimeZones();
    else if (key === "unit") list = UNITS;
    else throw rangeError("Invalid key: " + key);
    return list.slice();
  } }.supportedValuesOf);

  // ---------------- 16.1.2 SetNumberFormatDigitOptions ----------------
  var ROUNDING_INCREMENTS = [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000];
  function SetNumberFormatDigitOptions(o, options, mnfdDefault, mxfdDefault, notation) {
    var mnid = GetNumberOption(options, "minimumIntegerDigits", 1, 21, 1);
    var mnfd = options.minimumFractionDigits, mxfd = options.maximumFractionDigits;
    var mnsd = options.minimumSignificantDigits, mxsd = options.maximumSignificantDigits;
    o.minimumIntegerDigits = mnid;
    var roundingIncrement = GetNumberOption(options, "roundingIncrement", 1, 5000, 1);
    if (ROUNDING_INCREMENTS.indexOf(roundingIncrement) < 0) throw rangeError("Invalid roundingIncrement");
    var roundingMode = GetOption(options, "roundingMode", "string", ["ceil", "floor", "expand", "trunc", "halfCeil",
      "halfFloor", "halfExpand", "halfTrunc", "halfEven"], "halfExpand");
    var roundingPriority = GetOption(options, "roundingPriority", "string", ["auto", "morePrecision", "lessPrecision"], "auto");
    var trailingZeroDisplay = GetOption(options, "trailingZeroDisplay", "string", ["auto", "stripIfInteger"], "auto");
    if (roundingIncrement !== 1) mxfdDefault = mnfdDefault;
    o.roundingIncrement = roundingIncrement;
    o.roundingMode = roundingMode;
    o.trailingZeroDisplay = trailingZeroDisplay;
    var hasSd = !(mnsd === undefined && mxsd === undefined), hasFd = !(mnfd === undefined && mxfd === undefined);
    var needSd = true, needFd = true;
    if (roundingPriority === "auto") {
      needSd = hasSd;
      if (needSd || (!hasFd && notation === "compact")) needFd = false;
    }
    if (needSd) {
      if (hasSd) {
        o.minimumSignificantDigits = DefaultNumberOption(mnsd, 1, 21, 1);
        o.maximumSignificantDigits = DefaultNumberOption(mxsd, o.minimumSignificantDigits, 21, 21);
      } else { o.minimumSignificantDigits = 1; o.maximumSignificantDigits = 21; }
    }
    if (needFd) {
      if (hasFd) {
        mnfd = DefaultNumberOption(mnfd, 0, 100, undefined);
        mxfd = DefaultNumberOption(mxfd, 0, 100, undefined);
        if (mnfd === undefined) mnfd = MathMin(mnfdDefault, mxfd);
        else if (mxfd === undefined) mxfd = MathMax(mxfdDefault, mnfd);
        else if (mnfd > mxfd) throw rangeError("minimumFractionDigits is greater than maximumFractionDigits");
        o.minimumFractionDigits = mnfd;
        o.maximumFractionDigits = mxfd;
      } else { o.minimumFractionDigits = mnfdDefault; o.maximumFractionDigits = mxfdDefault; }
    }
    if (!needSd && !needFd) {
      o.minimumFractionDigits = 0; o.maximumFractionDigits = 0;
      o.minimumSignificantDigits = 1; o.maximumSignificantDigits = 2;
      o.roundingType = "more-precision"; o.computedRoundingPriority = "morePrecision";
    } else if (roundingPriority === "morePrecision") { o.roundingType = "more-precision"; o.computedRoundingPriority = "morePrecision"; }
    else if (roundingPriority === "lessPrecision") { o.roundingType = "less-precision"; o.computedRoundingPriority = "lessPrecision"; }
    else if (hasSd) { o.roundingType = "significant-digits"; o.computedRoundingPriority = "auto"; }
    else { o.roundingType = "fraction-digits"; o.computedRoundingPriority = "auto"; }
    if (roundingIncrement !== 1) {
      if (o.roundingType !== "fraction-digits") throw typeError("roundingIncrement needs fraction-digit rounding");
      if (o.maximumFractionDigits !== o.minimumFractionDigits) throw rangeError("roundingIncrement needs equal fraction digits");
    }
  }
  // the digit options of resolvedOptions, in the table's order
  function digitOptions(r, s) {
    cdp(r, "minimumIntegerDigits", s.minimumIntegerDigits);
    if (s.minimumFractionDigits !== undefined) {
      cdp(r, "minimumFractionDigits", s.minimumFractionDigits);
      cdp(r, "maximumFractionDigits", s.maximumFractionDigits);
    }
    if (s.minimumSignificantDigits !== undefined) {
      cdp(r, "minimumSignificantDigits", s.minimumSignificantDigits);
      cdp(r, "maximumSignificantDigits", s.maximumSignificantDigits);
    }
  }

  // ---------------- 17 PluralRules ----------------
  var PluralRulesProto = {};
  function PluralRules(locales = undefined, options = undefined) {
    if (new.target === undefined) throw typeError("Constructor Intl.PluralRules requires 'new'");
    var o = createFrom(new.target, PluralRulesProto);
    var res = ResolveOptions(availableLocales(), noData, [], [], locales, options, true);
    options = res.options;
    var s = rec();
    s.kind = "PluralRules"; s.locale = res.resolved.locale;
    s.type = GetOption(options, "type", "string", ["cardinal", "ordinal"], "cardinal");
    s.notation = GetOption(options, "notation", "string", ["standard", "scientific", "engineering", "compact"], "standard");
    var cd = GetOption(options, "compactDisplay", "string", ["short", "long"], "short");
    if (s.notation === "compact") s.compactDisplay = cd;
    SetNumberFormatDigitOptions(s, options, 0, 3, s.notation);
    new Slots(o, s);
    return o;
  }
  makeService(PluralRules, "PluralRules", 0, PluralRulesProto);
  // the number the plural rules see: rounded as the digit options say
  function pluralValue(s, n) {
    if (!isFinite(n)) return n;
    var r = n;
    if (s.roundingType === "significant-digits" || s.roundingType === "more-precision" && s.maximumSignificantDigits !== undefined && s.minimumFractionDigits === undefined) {
      if (n !== 0) r = +n.toPrecision(s.maximumSignificantDigits);
    } else if (s.maximumFractionDigits !== undefined) {
      r = +n.toFixed(s.maximumFractionDigits);
    }
    return r;
  }
  function selectCategory(s, n) {
    var x = pluralValue(s, n);
    var cat = intlOp(10, s.locale, x, s.type === "ordinal");
    // a visible fraction digit (minimumFractionDigits) makes 1 plural in most locales
    if (s.type === "cardinal" && s.minimumFractionDigits > 0 && cat === "one" && x === MathFloor(x)) {
      var c2 = intlOp(10, s.locale, x + 0.5, false);
      if (c2 !== "one") cat = c2;
    }
    return cat;
  }
  method(PluralRulesProto, "select", { select(value) {
    var s = slotsOf(this, "PluralRules", "Intl.PluralRules.prototype.select");
    return selectCategory(s, ToNumber(value));
  } }.select);
  method(PluralRulesProto, "selectRange", { selectRange(start, end) {
    var s = slotsOf(this, "PluralRules", "Intl.PluralRules.prototype.selectRange");
    if (start === undefined || end === undefined) throw typeError("start and end are required");
    var x = ToNumber(start), y = ToNumber(end);
    if (NumberIsNaN(x) || NumberIsNaN(y)) throw rangeError("start and end must be numbers");
    return selectCategory(s, y);
  } }.selectRange);
  method(PluralRulesProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "PluralRules", "Intl.PluralRules.prototype.resolvedOptions");
    var r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "type", s.type);
    cdp(r, "notation", s.notation);
    if (s.notation === "compact") cdp(r, "compactDisplay", s.compactDisplay);
    digitOptions(r, s);
    var cats = intlOp(11, s.locale, s.type === "ordinal"), order = ["zero", "one", "two", "few", "many", "other"], pc = [];
    for (var i = 0; i < order.length; i++) if (cats.indexOf(order[i]) >= 0) pc.push(order[i]);
    cdp(r, "pluralCategories", pc);
    cdp(r, "roundingIncrement", s.roundingIncrement);
    cdp(r, "roundingMode", s.roundingMode);
    cdp(r, "roundingPriority", s.computedRoundingPriority);
    cdp(r, "trailingZeroDisplay", s.trailingZeroDisplay);
    return r;
  } }.resolvedOptions);

  // ---------------- 14 ListFormat ----------------
  var ListFormatProto = {};
  function ListFormat(locales = undefined, options = undefined) {
    if (new.target === undefined) throw typeError("Constructor Intl.ListFormat requires 'new'");
    var o = createFrom(new.target, ListFormatProto);
    var res = ResolveOptions(availableLocales(), noData, [], [], locales, options, false);
    options = res.options;
    var s = rec();
    s.kind = "ListFormat"; s.locale = res.resolved.locale;
    s.type = GetOption(options, "type", "string", ["conjunction", "disjunction", "unit"], "conjunction");
    s.style = GetOption(options, "style", "string", ["long", "short", "narrow"], "long");
    new Slots(o, s);
    return o;
  }
  makeService(ListFormat, "ListFormat", 0, ListFormatProto);
  // 14.5.4 StringListFromIterable
  function StringListFromIterable(iterable) {
    if (iterable === undefined) return [];
    var list = [], it = iterable[Sym.iterator]();
    var next = it.next;
    while (true) {
      var r = next.call(it);
      if (!isObject(r)) throw typeError("iterator result is not an object");
      if (r.done) return list;
      var v = r.value;
      if (typeof v !== "string") {
        var ret = it.return;
        if (ret !== undefined && ret !== null) ret.call(it);
        throw typeError("list items must be strings");
      }
      list.push(v);
    }
  }
  method(ListFormatProto, "format", { format(list) {
    var s = slotsOf(this, "ListFormat", "Intl.ListFormat.prototype.format");
    var items = StringListFromIterable(list);
    if (items.length === 0) return "";
    var r = intlOp(12, s.locale, items, s.type, s.style);
    return r === undefined ? items.join(", ") : r;
  } }.format);
  method(ListFormatProto, "formatToParts", { formatToParts(list) {
    var s = slotsOf(this, "ListFormat", "Intl.ListFormat.prototype.formatToParts");
    var items = StringListFromIterable(list);
    if (items.length === 0) return [];
    var p = intlOp(13, s.locale, items, s.type, s.style), out = [];
    if (p === undefined) p = [["element", items.join(", ")]];
    for (var i = 0; i < p.length; i++) { var part = {}; cdp(part, "type", p[i][0]); cdp(part, "value", p[i][1]); cdp(out, i, part); }
    return out;
  } }.formatToParts);
  method(ListFormatProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "ListFormat", "Intl.ListFormat.prototype.resolvedOptions");
    var r = {};
    cdp(r, "locale", s.locale); cdp(r, "type", s.type); cdp(r, "style", s.style);
    return r;
  } }.resolvedOptions);


  // ---------------- 16 NumberFormat ----------------
  // 7.1.1.1 OrdinaryToPrimitive with the number hint first, and @@toPrimitive
  function ToPrimitiveNumber(v) {
    if (!isObject(v)) return v;
    var ex = v[Sym.toPrimitive];
    if (ex !== undefined && ex !== null) {
      if (typeof ex !== "function") throw typeError("@@toPrimitive is not a function");
      var r = ex.call(v, "number");
      if (isObject(r)) throw typeError("Cannot convert object to primitive value");
      return r;
    }
    var names = ["valueOf", "toString"];
    for (var i = 0; i < 2; i++) {
      var f = v[names[i]];
      if (typeof f === "function") { var x = f.call(v); if (!isObject(x)) return x; }
    }
    throw typeError("Cannot convert object to primitive value");
  }
  function isWS(c) {
    return c === 9 || c === 10 || c === 11 || c === 12 || c === 13 || c === 32 || c === 160 || c === 0x1680 ||
      (c >= 0x2000 && c <= 0x200a) || c === 0x2028 || c === 0x2029 || c === 0x202f || c === 0x205f || c === 0x3000 || c === 0xfeff;
  }
  function trimWS(s) {
    var a = 0, b = s.length;
    while (a < b && isWS(s.charCodeAt(a))) a++;
    while (b > a && isWS(s.charCodeAt(b - 1))) b--;
    return s.slice(a, b);
  }
  // 16.5.16 ToIntlMathematicalValue: a Number, or { d: decimal string } for an exact value (BigInt, a string)
  function ToIntlMathematicalValue(value) {
    var p = ToPrimitiveNumber(value);
    if (typeof p === "bigint") return { d: "" + p };
    if (typeof p === "string") {
      var s = trimWS(p);
      if (s === "") return 0;
      if (/^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/.test(s)) {
        var n = +s;
        if (!isFinite(n) || +("" + n) !== n || /[eE]/.test(s) || s.replace(/^[+-]/, "").replace(/^0+(?=\d)/, "").length > 15) {
          if (isFinite(n) || /[eE]/.test(s)) return { d: s[0] === "+" ? s.slice(1) : s, n: n };
        }
        return n;
      }
      if (/^0[xX][0-9a-fA-F]+$|^0[oO][0-7]+$|^0[bB][01]+$/.test(s)) {
        var b = BigInt(s);
        return Num(b) === Num(b) && b <= 9007199254740991n ? Num(b) : { d: "" + b };
      }
      return +s;
    }
    return ToNumber(p);
  }
  function isNaNValue(x) { return typeof x === "number" ? NumberIsNaN(x) : false; }
  var SIMPLE_UNITS = UNITS;
  function IsWellFormedUnitIdentifier(u) {
    if (SIMPLE_UNITS.indexOf(u) >= 0) return true;
    var i = u.indexOf("-per-");
    if (i < 0) return false;
    var a = u.slice(0, i), b = u.slice(i + 5);
    return SIMPLE_UNITS.indexOf(a) >= 0 && SIMPLE_UNITS.indexOf(b) >= 0;
  }
  function IsWellFormedCurrencyCode(c) {
    if (c.length !== 3) return false;
    for (var i = 0; i < 3; i++) {
      var k = c.charCodeAt(i);
      if (!(k >= 65 && k <= 90 || k >= 97 && k <= 122)) return false;
    }
    return true;
  }
  function asciiUpper(s) {
    var r = "";
    for (var i = 0; i < s.length; i++) { var c = s.charCodeAt(i); r += c >= 97 && c <= 122 ? Str.fromCharCode(c - 32) : s[i]; }
    return r;
  }
  // 16.1.1.1 SetNumberFormatUnitOptions
  function SetNumberFormatUnitOptions(s, options) {
    var style = GetOption(options, "style", "string", ["decimal", "percent", "currency", "unit"], "decimal");
    s.style = style;
    var currency = GetOption(options, "currency", "string", undefined, undefined);
    if (currency === undefined) { if (style === "currency") throw typeError("currency is required with currency style"); }
    else if (!IsWellFormedCurrencyCode(currency)) throw rangeError("Invalid currency code: " + currency);
    var currencyDisplay = GetOption(options, "currencyDisplay", "string", ["code", "symbol", "narrowSymbol", "name"], "symbol");
    var currencySign = GetOption(options, "currencySign", "string", ["standard", "accounting"], "standard");
    var unit = GetOption(options, "unit", "string", undefined, undefined);
    if (unit === undefined) { if (style === "unit") throw typeError("unit is required with unit style"); }
    else if (!IsWellFormedUnitIdentifier(unit)) throw rangeError("Invalid unit: " + unit);
    var unitDisplay = GetOption(options, "unitDisplay", "string", ["short", "narrow", "long"], "short");
    if (style === "currency") { s.currency = asciiUpper(currency); s.currencyDisplay = currencyDisplay; s.currencySign = currencySign; }
    if (style === "unit") { s.unit = unit; s.unitDisplay = unitDisplay; }
  }
  var nfAvailable = null;
  // a locale's numbering systems: its default first, then every one with simple digits
  function nuData(locale, key) {
    var a = intlOp(6, locale), d = a.length > 0 ? a[0] : "latn", r = [d];
    for (var i = 0; i < NUMBERING_SYSTEMS.length; i++) if (NUMBERING_SYSTEMS[i] !== d) r.push(NUMBERING_SYSTEMS[i]);
    return r;
  }
  var NumberFormatProto = {};
  // 16.1.2 InitializeNumberFormat (with 16.1.1's ChainNumberFormat for a call)
  function NumberFormat(locales = undefined, options = undefined) {
    var nt = new.target === undefined ? NumberFormat : new.target;
    var o = createFrom(nt, NumberFormatProto);
    var s = initNumberFormat(locales, options);
    new Slots(o, s);
    if (new.target === undefined && OrdinaryHasInstance(NumberFormat, this)) {
      ObjectDefineProperty(this, FallbackSymbol, { value: o, writable: false, enumerable: false, configurable: false });
      return this;
    }
    return o;
  }
  function initNumberFormat(locales, options) {
    var res = ResolveOptions(availableLocales(), nuData, ["nu"], [["numberingSystem", "nu", "string", undefined]],
      locales, options, true);
    options = res.options;
    var s = rec();
    s.kind = "NumberFormat";
    s.locale = res.resolved.locale;
    s.numberingSystem = res.resolved.nu;
    SetNumberFormatUnitOptions(s, options);
    var style = s.style;
    var notation = GetOption(options, "notation", "string", ["standard", "scientific", "engineering", "compact"], "standard");
    s.notation = notation;
    var mnfdDefault, mxfdDefault;
    if (style === "currency" && notation === "standard") { mnfdDefault = mxfdDefault = intlOp(22, s.currency); }
    else { mnfdDefault = 0; mxfdDefault = style === "percent" ? 0 : 3; }
    SetNumberFormatDigitOptions(s, options, mnfdDefault, mxfdDefault, notation);
    var compactDisplay = GetOption(options, "compactDisplay", "string", ["short", "long"], "short");
    var defaultUseGrouping = "auto";
    if (notation === "compact") { s.compactDisplay = compactDisplay; defaultUseGrouping = "min2"; }
    var useGrouping = GetBooleanOrStringOption(options, "useGrouping", ["min2", "auto", "always", "true", "false"], defaultUseGrouping);
    if (useGrouping === "true" || useGrouping === "false") useGrouping = defaultUseGrouping;
    if (useGrouping === true) useGrouping = "always";
    s.useGrouping = useGrouping;
    s.signDisplay = GetOption(options, "signDisplay", "string", ["auto", "never", "always", "exceptZero", "negative"], "auto");
    return s;
  }
  makeService(NumberFormat, "NumberFormat", 0, NumberFormatProto);
  // the locale ICU formats with: the numbering system as its nu keyword
  function icuLocale(s) {
    if (s.icuLocale === undefined) {
      var l = removeUnicodeExtension(s.locale);
      var px = l.indexOf("-x-");
      if (px >= 0) l = l.slice(0, px);
      s.icuLocale = s.numberingSystem ? l + "-u-nu-" + s.numberingSystem : l;
    }
    return s.icuLocale;
  }
  function nfOpts(s) {
    if (s.opts === undefined) {
      s.opts = [s.style, s.currency, s.currencyDisplay, s.currencySign, s.unit, s.unitDisplay, s.notation, s.compactDisplay,
        s.signDisplay, s.useGrouping === false ? "false" : s.useGrouping, s.roundingMode, s.trailingZeroDisplay,
        s.minimumIntegerDigits, s.minimumFractionDigits === undefined ? -1 : s.minimumFractionDigits,
        s.maximumFractionDigits === undefined ? -1 : s.maximumFractionDigits,
        s.minimumSignificantDigits === undefined ? -1 : s.minimumSignificantDigits,
        s.maximumSignificantDigits === undefined ? -1 : s.maximumSignificantDigits,
        s.roundingIncrement, s.computedRoundingPriority, s.numberingSystem];
    }
    return s.opts;
  }
  // parenthesized negative currency amounts for currencySign accounting (CLDR: not in German)
  function usesAccounting(s) { return s.style === "currency" && s.currencySign === "accounting" && s.locale.slice(0, 2) !== "de"; }
  function FormatNumeric(s, x) {
    var r = typeof x === "number" ? intlOp(20, icuLocale(s), nfOpts(s), x, false, false) : intlOp(20, icuLocale(s), nfOpts(s), x.d, true, false);
    if (r === undefined) r = "" + (typeof x === "number" ? x : x.d);
    if (usesAccounting(s) && r[0] === "-") r = "(" + r.slice(1) + ")";
    return r;
  }
  // the part types ICU leaves to the caller
  function fixParts(s, x, p) {
    var nan = typeof x === "number" && NumberIsNaN(x), inf = typeof x === "number" && !isFinite(x) && !nan;
    for (var i = 0; i < p.length; i++) {
      if (p[i][0] === "integer" && (nan || inf)) p[i][0] = nan ? "nan" : "infinity";
      else if (s.style === "unit" && p[i][0] === "percentSign") p[i][0] = "unit";
    }
    if (usesAccounting(s)) {
      for (i = 0; i < p.length; i++) if (p[i][0] === "minusSign") break;
      if (i < p.length) {
        p.splice(i, 1);
        p.unshift(["literal", "("]);
        p.push(["literal", ")"]);
      }
    }
    return p;
  }
  function partsToArray(p, withSource) {
    var out = [];
    for (var i = 0; i < p.length; i++) {
      var part = {};
      cdp(part, "type", p[i][0]);
      cdp(part, "value", p[i][1]);
      if (withSource) cdp(part, "source", p[i].length > 2 ? p[i][2] : "shared");
      cdp(out, i, part);
    }
    return out;
  }
  function FormatNumericToParts(s, x) {
    var p = typeof x === "number" ? intlOp(20, icuLocale(s), nfOpts(s), x, false, true) : intlOp(20, icuLocale(s), nfOpts(s), x.d, true, true);
    if (p === undefined) p = [["integer", FormatNumeric(s, x)]];
    return partsToArray(fixParts(s, x, p), false);
  }
  function thisNumberFormat(o, method) {
    var s = Slots.get(o);
    if (s === undefined && OrdinaryHasInstance(NumberFormat, o)) {
      var f = o[FallbackSymbol];
      s = Slots.get(f);
    }
    if (s === undefined || s.kind !== "NumberFormat") throw typeError(method + " called on an incompatible receiver");
    return s;
  }
  getter(NumberFormatProto, "format", { format() {
    var s = thisNumberFormat(this, "Intl.NumberFormat.prototype.format");
    if (s.boundFormat === undefined) {
      var F = (value) => FormatNumeric(s, ToIntlMathematicalValue(value));
      ObjectDefineProperty(F, "name", { value: "", configurable: true });
      s.boundFormat = F;
    }
    return s.boundFormat;
  } }.format);
  method(NumberFormatProto, "formatToParts", { formatToParts(value) {
    var s = slotsOf(this, "NumberFormat", "Intl.NumberFormat.prototype.formatToParts");
    return FormatNumericToParts(s, ToIntlMathematicalValue(value));
  } }.formatToParts);
  function rangeArgs(start, end) {
    if (start === undefined || end === undefined) throw typeError("start and end are required");
    var x = ToIntlMathematicalValue(start), y = ToIntlMathematicalValue(end);
    if (isNaNValue(x) || isNaNValue(y)) throw rangeError("start and end must not be NaN");
    return [x, y];
  }
  function rangeCall(s, x, y, parts) {
    var dec = typeof x !== "number" || typeof y !== "number";
    var a = dec ? (typeof x === "number" ? "" + x : x.d) : x, b = dec ? (typeof y === "number" ? "" + y : y.d) : y;
    return intlOp(21, icuLocale(s), nfOpts(s), a, b, dec, parts);
  }
  method(NumberFormatProto, "formatRange", { formatRange(start, end) {
    var s = slotsOf(this, "NumberFormat", "Intl.NumberFormat.prototype.formatRange");
    var xy = rangeArgs(start, end), r = rangeCall(s, xy[0], xy[1], false);
    return r === undefined ? FormatNumeric(s, xy[0]) + "\u2013" + FormatNumeric(s, xy[1]) : r;
  } }.formatRange);
  method(NumberFormatProto, "formatRangeToParts", { formatRangeToParts(start, end) {
    var s = slotsOf(this, "NumberFormat", "Intl.NumberFormat.prototype.formatRangeToParts");
    var xy = rangeArgs(start, end), p = rangeCall(s, xy[0], xy[1], true);
    if (p === undefined) p = [["integer", FormatNumeric(s, xy[0]), "startRange"], ["literal", "\u2013", "shared"], ["integer", FormatNumeric(s, xy[1]), "endRange"]];
    return partsToArray(p, true);
  } }.formatRangeToParts);
  method(NumberFormatProto, "resolvedOptions", { resolvedOptions() {
    var s = thisNumberFormat(this, "Intl.NumberFormat.prototype.resolvedOptions");
    var r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "numberingSystem", s.numberingSystem);
    cdp(r, "style", s.style);
    if (s.currency !== undefined) { cdp(r, "currency", s.currency); cdp(r, "currencyDisplay", s.currencyDisplay); cdp(r, "currencySign", s.currencySign); }
    if (s.unit !== undefined) { cdp(r, "unit", s.unit); cdp(r, "unitDisplay", s.unitDisplay); }
    digitOptions(r, s);
    cdp(r, "useGrouping", s.useGrouping);
    cdp(r, "notation", s.notation);
    if (s.notation === "compact") cdp(r, "compactDisplay", s.compactDisplay);
    cdp(r, "signDisplay", s.signDisplay);
    cdp(r, "roundingIncrement", s.roundingIncrement);
    cdp(r, "roundingMode", s.roundingMode);
    cdp(r, "roundingPriority", s.computedRoundingPriority);
    cdp(r, "trailingZeroDisplay", s.trailingZeroDisplay);
    return r;
  } }.resolvedOptions);
  // 21.1.3.4 Number.prototype.toLocaleString, 21.2.3.2 BigInt.prototype.toLocaleString
  var NumberValueOf = Number.prototype.valueOf, BigIntValueOf = BigInt.prototype.valueOf;
  def(Number.prototype, "toLocaleString", { toLocaleString(locales = undefined, options = undefined) {
    var x = NumberValueOf.call(this);
    return FormatNumeric(initNumberFormat(locales, options), x);
  } }.toLocaleString);
  def(BigInt.prototype, "toLocaleString", { toLocaleString(locales = undefined, options = undefined) {
    var x = BigIntValueOf.call(this);
    return FormatNumeric(initNumberFormat(locales, options), { d: "" + x });
  } }.toLocaleString);


  def(G, "Intl", Intl);
})

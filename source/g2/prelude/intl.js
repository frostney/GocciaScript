(function (G, intlOp, markIntrinsic, temporal) {
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
  // an Object: typeof says "object" or "function", or "undefined" for an [[IsHTMLDDA]] object (B.3.6)
  function isObject(v) {
    var t = typeof v;
    return (t === "object" && v !== null) || t === "function" || (t === "undefined" && v !== undefined);
  }
  // IsCallable: typeof says "function", or "undefined" for a callable [[IsHTMLDDA]] object (B.3.6)
  function isCallable(f) { var t = typeof f; return t === "function" || (t === "undefined" && f !== undefined); }
  function ToObject(v) { if (v === null || v === undefined) throw typeError("Cannot convert undefined or null to object"); return Object(v); }
  function ToNumber(v) { return +v; }
  // ToString (the string hint first, unlike "" + v)
  function ToString(v) { if (typeof v === "symbol") throw typeError("Cannot convert a Symbol value to a string"); return Str(v); }
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
  // a list append that defines the index (a setter on Object.prototype must not see it)
  function append(list, v) { cdp(list, list.length, v); }
  // internal lists use the primordial Array methods: the user may replace Array.prototype's
  var ArrayProtoIndexOf = Array.prototype.indexOf, ArrayProtoSort = Array.prototype.sort, ArrayProtoJoin = Array.prototype.join;
  var ReflectApply = Reflect.apply;
  function listIndexOf(list, v) { for (var i = 0; i < list.length; i++) if (list[i] === v) return i; return -1; }
  function listSort(list, cmp) { return cmp === undefined ? ReflectApply(ArrayProtoSort, list, []) : ReflectApply(ArrayProtoSort, list, [cmp]); }
  function listJoin(list, sep) { return ReflectApply(ArrayProtoJoin, list, [sep]); }
  function listCopy(list, from, to) {
    var r = [], a = from === undefined ? 0 : from, b = to === undefined ? list.length : to;
    for (var i = a; i < b; i++) append(r, list[i]);
    return r;
  }
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
    if (!isObject(p)) p = markIntrinsic(fallback, newTarget);   // the intrinsic of newTarget's realm
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
  // (a NUL would end the tag at the native boundary)
  function IsStructurallyValidLanguageTag(t) { return t.indexOf("\0") < 0 && intlOp(0, t); }
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
      if (listIndexOf(seen, c) < 0) append(seen, c);
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
        if (keyword !== null) append(keywords, keyword);
        keyword = null;
        var dup = false;
        for (var j = 0; j < keywords.length; j++) if (keywords[j].key === sub) dup = true;
        if (!dup) keyword = { key: sub, value: "" };
        else keyword = { key: sub, value: "", dup: true };
      } else if (keyword === null) {
        if (listIndexOf(attributes, sub) < 0) append(attributes, sub);
      } else keyword.value = keyword.value === "" ? sub : keyword.value + "-" + sub;
    }
    if (keyword !== null) append(keywords, keyword);
    var kw = [];
    for (i = 0; i < keywords.length; i++) if (!keywords[i].dup) append(kw, keywords[i]);
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
          if (listIndexOf(keyLocaleData, rv) >= 0) { value = rv; sk = { key: key, value: value }; }
        } else if (listIndexOf(keyLocaleData, "true") >= 0) { value = "true"; sk = { key: key, value: "" }; }
        break;
      }
      var ov = options[key];
      if (typeof ov === "string") {
        ov = CanonicalizeUValue(asciiLower(key), ov);
        if (ov === "") ov = "true";
      }
      if (ov !== undefined && ov !== null && ov !== value && listIndexOf(keyLocaleData, ov) >= 0) { value = ov; sk = null; }
      if (sk !== null) append(supported, sk);
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
    if (values !== undefined && listIndexOf(values, value) < 0) throw rangeError("Value " + value + " out of range for " + property);
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
    if (listIndexOf(stringValues, value) < 0) throw rangeError("Value " + value + " out of range for " + property);
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
      if (LookupMatchingLocale(availableSet, [removeUnicodeExtension(requested[i])]) !== undefined) append(subset, requested[i]);
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
          if (c !== "standard" && c !== "search" && listIndexOf(set, c) < 0) append(set, c);
        }
      }
      listSort(set);
      collationsList = set;
    }
    return collationsList;
  }
  var timeZonesList = null;
  function availableTimeZones() {
    if (timeZonesList === null) { timeZonesList = intlOp(17); listSort(timeZonesList); }
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
    return listCopy(list);
  } }.supportedValuesOf);

  // ---------------- 16.1.2 SetNumberFormatDigitOptions ----------------
  var ROUNDING_INCREMENTS = [1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 2500, 5000];
  function SetNumberFormatDigitOptions(o, options, mnfdDefault, mxfdDefault, notation) {
    var mnid = GetNumberOption(options, "minimumIntegerDigits", 1, 21, 1);
    var mnfd = options.minimumFractionDigits, mxfd = options.maximumFractionDigits;
    var mnsd = options.minimumSignificantDigits, mxsd = options.maximumSignificantDigits;
    o.minimumIntegerDigits = mnid;
    var roundingIncrement = GetNumberOption(options, "roundingIncrement", 1, 5000, 1);
    if (listIndexOf(ROUNDING_INCREMENTS, roundingIncrement) < 0) throw rangeError("Invalid roundingIncrement");
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
  markIntrinsic(PluralRulesProto);
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
    for (var i = 0; i < order.length; i++) if (listIndexOf(cats, order[i]) >= 0) append(pc, order[i]);
    cdp(r, "pluralCategories", pc);
    cdp(r, "roundingIncrement", s.roundingIncrement);
    cdp(r, "roundingMode", s.roundingMode);
    cdp(r, "roundingPriority", s.computedRoundingPriority);
    cdp(r, "trailingZeroDisplay", s.trailingZeroDisplay);
    return r;
  } }.resolvedOptions);

  // ---------------- 14 ListFormat ----------------
  var ListFormatProto = {};
  markIntrinsic(ListFormatProto);
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
      append(list, v);
    }
  }
  method(ListFormatProto, "format", { format(list) {
    var s = slotsOf(this, "ListFormat", "Intl.ListFormat.prototype.format");
    var items = StringListFromIterable(list);
    if (items.length === 0) return "";
    var r = intlOp(12, s.locale, items, s.type, s.style);
    return r === undefined ? listJoin(items, ", ") : r;
  } }.format);
  method(ListFormatProto, "formatToParts", { formatToParts(list) {
    var s = slotsOf(this, "ListFormat", "Intl.ListFormat.prototype.formatToParts");
    var items = StringListFromIterable(list);
    if (items.length === 0) return [];
    var p = intlOp(13, s.locale, items, s.type, s.style), out = [];
    if (p === undefined) p = [["element", listJoin(items, ", ")]];
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
      if (!isCallable(ex)) throw typeError("@@toPrimitive is not a function");
      var r = ex.call(v, "number");
      if (isObject(r)) throw typeError("Cannot convert object to primitive value");
      return r;
    }
    var names = ["valueOf", "toString"];
    for (var i = 0; i < 2; i++) {
      var f = v[names[i]];
      if (isCallable(f)) { var x = f.call(v); if (!isObject(x)) return x; }
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
    if (listIndexOf(SIMPLE_UNITS, u) >= 0) return true;
    var i = u.indexOf("-per-");
    if (i < 0) return false;
    var a = u.slice(0, i), b = u.slice(i + 5);
    return listIndexOf(SIMPLE_UNITS, a) >= 0 && listIndexOf(SIMPLE_UNITS, b) >= 0;
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
    for (var i = 0; i < NUMBERING_SYSTEMS.length; i++) if (NUMBERING_SYSTEMS[i] !== d) append(r, NUMBERING_SYSTEMS[i]);
    return r;
  }
  var NumberFormatProto = {};
  markIntrinsic(NumberFormatProto);
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
        var q = [["literal", "("]];
        for (var j = 0; j < p.length; j++) if (j !== i) append(q, p[j]);
        append(q, ["literal", ")"]);
        p = q;
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


  // a locale's calendars in BCP 47 names (ICU says gregorian, ethiopic-amete-alem)
  function localeCalendars(locale) {
    var a = intlOp(4, locale), r = [];
    for (var i = 0; i < a.length; i++) {
      var c = a[i];
      if (c === "gregorian") c = "gregory"; else if (c === "ethiopic-amete-alem") c = "ethioaa"; else if (c === "islamicc") c = "islamic-civil";
      if (listIndexOf(r, c) < 0) append(r, c);
    }
    return r;
  }

  // ---------------- 15 Locale ----------------
  function isAlpha(s) { for (var i = 0; i < s.length; i++) { var c = s.charCodeAt(i) | 32; if (c < 97 || c > 122) return false; } return true; }
  function isDigits(s) { for (var i = 0; i < s.length; i++) { var c = s.charCodeAt(i); if (c < 48 || c > 57) return false; } return true; }
  function isAlnum(s) { for (var i = 0; i < s.length; i++) { var c = s.charCodeAt(i); if (!(c >= 48 && c <= 57 || (c | 32) >= 97 && (c | 32) <= 122)) return false; } return true; }
  function isLanguageSubtag(s) { return isAlpha(s) && (s.length >= 2 && s.length <= 3 || s.length >= 5 && s.length <= 8); }
  function isScriptSubtag(s) { return s.length === 4 && isAlpha(s); }
  function isRegionSubtag(s) { return s.length === 2 && isAlpha(s) || s.length === 3 && isDigits(s); }
  function isVariantSubtag(s) {
    return isAlnum(s) && (s.length >= 5 && s.length <= 8 || s.length === 4 && isDigits(s[0]));
  }
  // the unicode_language_id at the start of a canonical tag: { language, script, region, variants, end }
  function parseBaseName(tag) {
    var parts = tag.split("-"), i = 0, r = rec();
    r.language = parts[i++];
    r.script = undefined; r.region = undefined; r.variants = undefined;
    if (i < parts.length && isScriptSubtag(parts[i])) r.script = parts[i++];
    if (i < parts.length && isRegionSubtag(parts[i])) r.region = parts[i++];
    var vs = [];
    while (i < parts.length && parts[i].length > 1 && isVariantSubtag(parts[i])) append(vs, parts[i++]);
    if (vs.length > 0) r.variants = listJoin(vs, "-");
    r.baseName = listJoin(listCopy(parts, 0, i), "-");
    return r;
  }
  function WeekdayToString(fw) {
    var names = ["sun", "mon", "tue", "wed", "thu", "fri", "sat", "sun"];
    if (fw.length === 1 && fw >= "0" && fw <= "7") return names[+fw];
    return fw;
  }
  // 15.1.2 UpdateLanguageId
  function UpdateLanguageId(tag, options) {
    var b = parseBaseName(tag);
    var language = GetOption(options, "language", "string", undefined, b.language);
    if (!isLanguageSubtag(language)) throw rangeError("Invalid language: " + language);
    var script = GetOption(options, "script", "string", undefined, b.script);
    if (script !== undefined && !isScriptSubtag(script)) throw rangeError("Invalid script: " + script);
    var region = GetOption(options, "region", "string", undefined, b.region);
    if (region !== undefined && !isRegionSubtag(region)) throw rangeError("Invalid region: " + region);
    var variants = GetOption(options, "variants", "string", undefined, b.variants);
    if (variants !== undefined) {
      if (variants === "") throw rangeError("Invalid variants");
      var vs = asciiLower(variants).split("-");
      for (var i = 0; i < vs.length; i++) {
        if (!isVariantSubtag(vs[i])) throw rangeError("Invalid variants: " + variants);
        for (var j = 0; j < i; j++) if (vs[j] === vs[i]) throw rangeError("Duplicate variant: " + vs[i]);
      }
    }
    var rest = tag.slice(b.baseName.length);
    var newTag = language;
    if (script !== undefined) newTag += "-" + script;
    if (region !== undefined) newTag += "-" + region;
    if (variants !== undefined) newTag += "-" + variants;
    newTag += rest;
    if (!IsStructurallyValidLanguageTag(newTag)) throw rangeError("Invalid language tag: " + newTag);
    return newTag;
  }
  var LOCALE_KEYS = ["ca", "co", "fw", "hc", "kf", "kn", "nu"];
  // 15.1.3 MakeLocaleRecord
  function MakeLocaleRecord(tag, opt) {
    var r = unicodeExtensionRange(tag), attributes = [], keywords = [], base = tag;
    if (r !== null) {
      var comps = UnicodeExtensionComponents(tag.slice(r[0], r[1]));
      attributes = comps.attributes; keywords = comps.keywords;
      base = tag.slice(0, r[0]) + tag.slice(r[1]);
    }
    var result = rec();
    for (var i = 0; i < LOCALE_KEYS.length; i++) {
      var key = LOCALE_KEYS[i], value = undefined, entry = undefined;
      for (var j = 0; j < keywords.length; j++) if (keywords[j].key === key) { entry = keywords[j]; value = entry.value; }
      var ov = opt[key];
      if (ov !== undefined) {
        value = CanonicalizeUValue(key, ov);
        if (entry !== undefined) entry.value = value; else append(keywords, { key: key, value: value });
      }
      result[key] = value;
    }
    listSort(keywords, function (a, b) { return a.key < b.key ? -1 : a.key > b.key ? 1 : 0; });
    result.locale = attributes.length === 0 && keywords.length === 0 ? CanonicalizeUnicodeLocaleId(base) :
      InsertUnicodeExtensionAndCanonicalize(base, attributes, keywords);
    return result;
  }
  var LocaleProto = {};
  markIntrinsic(LocaleProto);
  function Locale(tag, options = undefined) {
    if (new.target === undefined) throw typeError("Constructor Intl.Locale requires 'new'");
    var o = createFrom(new.target, LocaleProto);
    if (typeof tag !== "string" && !isObject(tag)) throw typeError("First argument to Intl.Locale must be a string or an object");
    var ts = Slots.get(tag);
    if (ts !== undefined && ts.kind === "Locale") tag = ts.locale; else tag = ToString(tag);
    options = CoerceOptionsToObject(options);
    if (!IsStructurallyValidLanguageTag(tag)) throw rangeError("Incorrect locale information provided: " + tag);
    tag = CanonicalizeUnicodeLocaleId(tag);
    tag = UpdateLanguageId(tag, options);
    var opt = rec();
    function typeOpt(name) {
      var v = GetOption(options, name, "string", undefined, undefined);
      if (v !== undefined && !isUnicodeType(v)) throw rangeError("Invalid " + name + ": " + v);
      return v;
    }
    opt.ca = typeOpt("calendar");
    opt.co = typeOpt("collation");
    var fw = GetOption(options, "firstDayOfWeek", "string", undefined, undefined);
    if (fw !== undefined) { fw = WeekdayToString(fw); if (!isUnicodeType(fw)) throw rangeError("Invalid firstDayOfWeek: " + fw); }
    opt.fw = fw;
    opt.hc = GetOption(options, "hourCycle", "string", ["h11", "h12", "h23", "h24"], undefined);
    opt.kf = GetOption(options, "caseFirst", "string", ["upper", "lower", "false"], undefined);
    var kn = GetOption(options, "numeric", "boolean", undefined, undefined);
    if (kn !== undefined) kn = ToString(kn);
    opt.kn = kn;
    opt.nu = typeOpt("numberingSystem");
    var r = MakeLocaleRecord(tag, opt);
    var s = rec();
    s.kind = "Locale";
    s.locale = r.locale;
    s.calendar = r.ca; s.collation = r.co; s.firstDayOfWeek = r.fw; s.hourCycle = r.hc; s.caseFirst = r.kf;
    s.numeric = r.kn === undefined ? false : r.kn === "true" || r.kn === "";
    s.numberingSystem = r.nu;
    new Slots(o, s);
    return o;
  }
  ObjectDefineProperty(Locale, "length", { value: 1, configurable: true });
  ObjectDefineProperty(Locale, "name", { value: "Locale", configurable: true });
  ObjectDefineProperty(Locale, "prototype", { value: LocaleProto, writable: false, enumerable: false, configurable: false });
  def(LocaleProto, "constructor", Locale);
  tag(LocaleProto, "Intl.Locale");
  def(Intl, "Locale", Locale);
  function loc(o, m) { return slotsOf(o, "Locale", "Intl.Locale.prototype." + m); }
  function reattach(s, base) {
    var r = unicodeExtensionRange(s.locale), b = parseBaseName(s.locale);
    return base + s.locale.slice(b.baseName.length);
  }
  method(LocaleProto, "maximize", { maximize() {
    var s = loc(this, "maximize"), b = parseBaseName(s.locale).baseName;
    var m = intlOp(8, b);
    if (m === null || !IsStructurallyValidLanguageTag(m)) m = b;
    return new Locale(reattach(s, m));
  } }.maximize);
  method(LocaleProto, "minimize", { minimize() {
    var s = loc(this, "minimize"), b = parseBaseName(s.locale).baseName;
    var m = intlOp(9, b);
    if (m === null || !IsStructurallyValidLanguageTag(m)) m = b;
    return new Locale(reattach(s, m));
  } }.minimize);
  method(LocaleProto, "toString", { toString() { return loc(this, "toString").locale; } }.toString);
  getter(LocaleProto, "baseName", { baseName() { return parseBaseName(loc(this, "baseName").locale).baseName; } }.baseName);
  getter(LocaleProto, "calendar", { calendar() { return loc(this, "calendar").calendar; } }.calendar);
  getter(LocaleProto, "caseFirst", { caseFirst() { return loc(this, "caseFirst").caseFirst; } }.caseFirst);
  getter(LocaleProto, "collation", { collation() { return loc(this, "collation").collation; } }.collation);
  getter(LocaleProto, "firstDayOfWeek", { firstDayOfWeek() { return loc(this, "firstDayOfWeek").firstDayOfWeek; } }.firstDayOfWeek);
  getter(LocaleProto, "hourCycle", { hourCycle() { return loc(this, "hourCycle").hourCycle; } }.hourCycle);
  getter(LocaleProto, "numeric", { numeric() { return loc(this, "numeric").numeric; } }.numeric);
  getter(LocaleProto, "numberingSystem", { numberingSystem() { return loc(this, "numberingSystem").numberingSystem; } }.numberingSystem);
  getter(LocaleProto, "language", { language() { return parseBaseName(loc(this, "language").locale).language; } }.language);
  getter(LocaleProto, "script", { script() { return parseBaseName(loc(this, "script").locale).script; } }.script);
  getter(LocaleProto, "region", { region() { return parseBaseName(loc(this, "region").locale).region; } }.region);
  getter(LocaleProto, "variants", { variants() { return parseBaseName(loc(this, "variants").locale).variants; } }.variants);
  // the Intl Locale Info proposal
  // the region the locale's preferences come from: the rg keyword, then sd, then its own or likely region
  function regionOf(s) {
    var r = unicodeExtensionRange(s.locale), rg, sd;
    if (r !== null) {
      var kw = UnicodeExtensionComponents(s.locale.slice(r[0], r[1])).keywords;
      for (var i = 0; i < kw.length; i++) { if (kw[i].key === "rg") rg = kw[i].value; else if (kw[i].key === "sd") sd = kw[i].value; }
      if (rg !== undefined && rg.length >= 3 && isAlpha(rg.slice(0, 2))) return asciiUpper(rg.slice(0, 2));
    }
    var b = parseBaseName(s.locale);
    if (b.region !== undefined) return b.region;
    if (sd !== undefined && sd.length >= 3 && isAlpha(sd.slice(0, 2))) return asciiUpper(sd.slice(0, 2));
    var m = intlOp(8, b.baseName);
    return m === null ? undefined : parseBaseName(m).region;
  }
  method(LocaleProto, "getCalendars", { getCalendars() {
    var s = loc(this, "getCalendars");
    if (s.calendar !== undefined) return [s.calendar];
    var rg = regionOf(s), b = parseBaseName(s.locale);
    var a = localeCalendars(rg !== undefined ? b.language + "-" + rg : s.locale);
    return a.length > 0 ? a : ["gregory"];
  } }.getCalendars);
  method(LocaleProto, "getCollations", { getCollations() {
    var s = loc(this, "getCollations");
    if (s.collation !== undefined) return [s.collation];
    var a = intlOp(5, s.locale), r = [];
    for (var i = 0; i < a.length; i++) {
      var c = a[i];
      if (c === "phonebook") c = "phonebk"; else if (c === "traditional") c = "trad"; else if (c === "dictionary") c = "dict";
      if (c !== "standard" && c !== "search" && listIndexOf(r, c) < 0) append(r, c);
    }
    listSort(r);
    return r.length > 0 ? r : ["emoji", "eor"];
  } }.getCollations);
  method(LocaleProto, "getHourCycles", { getHourCycles() {
    var s = loc(this, "getHourCycles");
    if (s.hourCycle !== undefined) return [s.hourCycle];
    var rg = regionOf(s), a = intlOp(7, rg !== undefined ? parseBaseName(s.locale).language + "-" + rg : s.locale, rg || "");
    return a.length > 0 ? a : ["h23"];
  } }.getHourCycles);
  method(LocaleProto, "getNumberingSystems", { getNumberingSystems() {
    var s = loc(this, "getNumberingSystems");
    if (s.numberingSystem !== undefined) return [s.numberingSystem];
    var a = intlOp(6, s.locale);
    return a.length > 0 ? [a[0]] : ["latn"];
  } }.getNumberingSystems);
  method(LocaleProto, "getTimeZones", { getTimeZones() {
    var s = loc(this, "getTimeZones"), b = parseBaseName(s.locale);
    if (b.region === undefined) return undefined;
    var a = intlOp(23, b.region);
    if (a === undefined) return undefined;
    listSort(a);
    return a;
  } }.getTimeZones);
  // the direction of the locale's (likely) script; undefined for an unknown or unregistered one
  var RTL_SCRIPTS = ["Adlm", "Arab", "Aran", "Armi", "Avst", "Chrs", "Cprt", "Elym", "Hatr", "Hebr", "Hung", "Khar", "Lydi",
    "Mand", "Mani", "Mend", "Merc", "Mero", "Narb", "Nbat", "Nkoo", "Orkh", "Ougr", "Palm", "Phli", "Phlp", "Phnx", "Prti",
    "Rohg", "Samr", "Sarb", "Sogd", "Sogo", "Syrc", "Thaa", "Yezi"];
  // the scripts Unicode's script metadata gives a left-to-right direction
  var LTR_SCRIPTS = ("Latn Cyrl Grek Armn Geor Deva Beng Guru Gujr Orya Taml Telu Knda Mlym Sinh Thai Laoo Tibt Mymr Khmr " +
    "Mong Hani Hans Hant Hira Kana Jpan Hang Kore Hrkt Jamo Ethi Cher Cans Ogam Runr Yiii Tglg Hano Buhd Tagb Limb Tale Talu " +
    "Bugi Bali Sund Lepc Olck Vaii Saur Kali Rjng Cham Lana Tavt Java Bamu Mtei Lisu Copt Glag Tfng Xpeo Ugar Goth Ital Dsrt " +
    "Shaw Osma Osge Linb Lina Xsux Egyp Brah Kthi Mroo Shrd Sora Takr Chak Plrd Bopo Hmng Hmnp Tirh Khoj Sind Mahj Gran Modi " +
    "Sidd Wara Pauc Ahom Mult Bhks Marc Newa Tang Gonm Gong Soyo Zanb Dogr Maka Medf Nand Wcho Kits Diak Bass Dupl Elba Lyci " +
    "Cari Perm Nkgb Nshu Kawi Nagm Toto Tnsa Vith Cpmn Gara Gukh Krai Onao Sunu Todr Tutg Sgnw Leke Bhks Mymr Khar").split(" ");
  function textDirection(s) {
    var b = parseBaseName(s.locale), script = b.script;
    if (script === undefined) {
      var m = intlOp(8, b.baseName);
      if (m !== null) script = parseBaseName(m).script;
    }
    if (script === undefined) return undefined;
    if (listIndexOf(RTL_SCRIPTS, script) >= 0) return "rtl";
    return listIndexOf(LTR_SCRIPTS, script) >= 0 ? "ltr" : undefined;
  }
  method(LocaleProto, "getTextInfo", { getTextInfo() {
    var s = loc(this, "getTextInfo"), r = {};
    cdp(r, "direction", textDirection(s));
    return r;
  } }.getTextInfo);
  method(LocaleProto, "getWeekInfo", { getWeekInfo() {
    var s = loc(this, "getWeekInfo"), w = intlOp(25, regionOf(s) || "001") || [1, 6, 7, 1], r = {};
    var first = w[0];
    if (s.firstDayOfWeek !== undefined) {
      var days = ["mon", "tue", "wed", "thu", "fri", "sat", "sun"], k = listIndexOf(days, s.firstDayOfWeek);
      if (k >= 0) first = k + 1;
    }
    var weekend = [];
    for (var d = w[1]; ; d = d % 7 + 1) { append(weekend, d); if (d === w[2]) break; }
    listSort(weekend, function (a, b) { return a - b; });
    cdp(r, "firstDay", first);
    cdp(r, "weekend", weekend);
    return r;
  } }.getWeekInfo);

  // ---------------- 11 DateTimeFormat ----------------
  var DTF_COMPONENTS = [
    ["weekday", ["narrow", "short", "long"]], ["era", ["narrow", "short", "long"]], ["year", ["2-digit", "numeric"]],
    ["month", ["2-digit", "numeric", "narrow", "short", "long"]], ["day", ["2-digit", "numeric"]],
    ["dayPeriod", ["narrow", "short", "long"]], ["hour", ["2-digit", "numeric"]], ["minute", ["2-digit", "numeric"]],
    ["second", ["2-digit", "numeric"]], ["fractionalSecondDigits", null],
    ["timeZoneName", ["short", "long", "shortOffset", "longOffset", "shortGeneric", "longGeneric"]]];
  function caData(locale, key) {
    var a = localeCalendars(locale), d = a.length > 0 ? a[0] : "gregory", r = [d];
    for (var i = 0; i < CALENDARS.length; i++) if (CALENDARS[i] !== d) append(r, CALENDARS[i]);
    return r;
  }
  function hcData(locale, key) {
    var a = intlOp(7, locale, ""), r = [null];
    var all = ["h11", "h12", "h23", "h24"];
    for (var i = 0; i < all.length; i++) append(r, all[i]);
    return r;
  }
  function dtfData(locale, key) {
    if (key === "ca") return caData(locale);
    if (key === "nu") return nuData(locale);
    return hcData(locale);
  }
  function localeHourCycle(locale) {
    var b = parseBaseName(locale), rg = b.region;
    if (rg === undefined) { var m = intlOp(8, b.baseName); if (m !== null) rg = parseBaseName(m).region; }
    var a = intlOp(7, locale, rg || "");
    return a.length > 0 ? a[0] : "h23";
  }
  function hourCycle12(locale) { return locale.slice(0, 2) === "ja" ? "h11" : "h12"; }
  function hourCycle24(locale) { return "h23"; }
  // an offset time zone (+HH, +HHMM, +HH:MM) as +-HH:MM, or undefined
  function parseOffsetZone(tz) {
    var m = /^([+\-\u2212])(\d{2})(?::?(\d{2}))?$/.exec(tz);
    if (m === null) return undefined;
    var h = +m[2], mi = m[3] === undefined ? 0 : +m[3];
    if (h > 23 || mi > 59) return undefined;
    var sign = m[1] === "+" ? "+" : "-";
    if (h === 0 && mi === 0) sign = "+";
    return sign + (h < 10 ? "0" : "") + h + ":" + (mi < 10 ? "0" : "") + mi;
  }
  function DefaultTimeZone() { var z = intlOp(27); return z ? z : "UTC"; }
  var DateTimeFormatProto = {};
  markIntrinsic(DateTimeFormatProto);
  // Temporal: GetDateTimeFormat(formats, matcher, options, required, defaults, inherit) as a components record
  // (ICU picks the pattern), or null
  var DT_REQUIRED = { date: ["weekday", "year", "month", "day"], time: ["dayPeriod", "hour", "minute", "second", "fractionalSecondDigits"],
    "year-month": ["year", "month"], "month-day": ["month", "day"],
    any: ["weekday", "year", "month", "day", "dayPeriod", "hour", "minute", "second", "fractionalSecondDigits"] };
  var DT_DEFAULTS = { date: ["year", "month", "day"], time: ["hour", "minute", "second"], "year-month": ["year", "month"],
    "month-day": ["month", "day"], "zoned-date-time": ["year", "month", "day", "hour", "minute", "second"],
    all: ["year", "month", "day", "hour", "minute", "second"] };
  function GetDateTimeFormat(options, required, defaults, inheritAll) {
    var req = DT_REQUIRED[required], f = rec(), i;
    if (inheritAll) { for (var k in options) f[k] = options[k]; }
    else if (required === "date" || required === "year-month" || required === "any") f.era = options.era;
    var any = false, all9 = DT_REQUIRED.any;
    for (i = 0; i < all9.length; i++) if (options[all9[i]] !== undefined) any = true;
    var need = true;
    for (i = 0; i < req.length; i++) if (options[req[i]] !== undefined) { f[req[i]] = options[req[i]]; need = false; }
    if (need) {
      if (any && !inheritAll) return null;
      var d = DT_DEFAULTS[defaults];
      for (i = 0; i < d.length; i++) f[d[i]] = "numeric";
      if (defaults === "zoned-date-time" && f.timeZoneName === undefined) f.timeZoneName = "short";
    }
    return f;
  }
  // the Temporal formats of dateStyle / timeStyle (AdjustDateTimeStyleFormat): a style, or the fields it allows
  function monthForStyle(st) { return st === "full" || st === "long" ? "long" : st === "medium" ? "short" : "numeric"; }
  function styleTemporalFormats(s) {
    var ds = s.dateStyle, ts = s.timeStyle, pts = ts === "full" || ts === "long" ? "medium" : ts;
    function style(d, t) { var f = rec(); f.dateStyle = d; f.timeStyle = t; return f; }
    if (ds !== undefined) {
      s.tPlainDate = style(ds, undefined);
      var ym = rec(); ym.year = "numeric"; ym.month = monthForStyle(ds); s.tPlainYearMonth = ym;
      var md = rec(); md.month = monthForStyle(ds); md.day = "numeric"; s.tPlainMonthDay = md;
    } else { s.tPlainDate = null; s.tPlainYearMonth = null; s.tPlainMonthDay = null; }
    s.tPlainTime = ts !== undefined ? style(undefined, pts) : null;
    s.tPlainDateTime = style(ds, pts);
    s.tInstant = style(ds, ts);
  }
  // 11.1.2 CreateDateTimeFormat (Temporal's required and defaults)
  function CreateDateTimeFormat(locales, options, required, defaults, toLocaleStringTimeZone) {
    var hour12;
    var res = ResolveOptions(availableLocales(), dtfData, ["ca", "hc", "nu"],
      [["calendar", "ca", "string", undefined], ["numberingSystem", "nu", "string", undefined],
       ["hour12", "hour12", "boolean", undefined], ["hourCycle", "hc", "string", ["h11", "h12", "h23", "h24"]]],
      locales, options, true, false, function (opt) {
        hour12 = opt.hour12;
        opt.hour12 = undefined;
        if (hour12 !== undefined) opt.hc = null;
      });
    options = res.options;
    var r = res.resolved, s = rec();
    s.kind = "DateTimeFormat";
    s.locale = r.locale;
    s.calendar = r.ca === "islamic" ? "islamic-tbla" : r.ca;
    s.numberingSystem = r.nu;
    var dataLocale = r.dataLocale, hc;
    if (hour12 === "true" || hour12 === true) hc = hourCycle12(dataLocale);
    else if (hour12 === "false" || hour12 === false) hc = hourCycle24(dataLocale);
    else hc = r.hc;
    var timeZone = options.timeZone;
    if (timeZone === undefined) timeZone = toLocaleStringTimeZone !== undefined ? toLocaleStringTimeZone : DefaultTimeZone();
    else {
      if (toLocaleStringTimeZone !== undefined) throw typeError("timeZone is not allowed in ZonedDateTime.prototype.toLocaleString");
      timeZone = ToString(timeZone);
      var off = parseOffsetZone(timeZone);
      if (off !== undefined) timeZone = off;
      else {
        var id = timeZone.indexOf("\0") >= 0 ? null : intlOp(26, timeZone);
        if (id === null) throw rangeError("Invalid time zone specified: " + timeZone);
        timeZone = id;
      }
    }
    s.timeZone = timeZone;
    var fmt = rec(), explicit = false;
    for (var i = 0; i < DTF_COMPONENTS.length; i++) {
      var prop = DTF_COMPONENTS[i][0], v;
      if (prop === "fractionalSecondDigits") v = GetNumberOption(options, prop, 1, 3, undefined);
      else v = GetOption(options, prop, "string", DTF_COMPONENTS[i][1], undefined);
      fmt[prop] = v;
      if (v !== undefined) explicit = true;
    }
    GetOption(options, "formatMatcher", "string", ["basic", "best fit"], "best fit");
    s.dateStyle = GetOption(options, "dateStyle", "string", ["full", "long", "medium", "short"], undefined);
    s.timeStyle = GetOption(options, "timeStyle", "string", ["full", "long", "medium", "short"], undefined);
    var raw = rec();
    for (var rk in fmt) raw[rk] = fmt[rk];
    if (s.dateStyle !== undefined || s.timeStyle !== undefined) {
      if (explicit) throw typeError("dateStyle and timeStyle may not be used with other date-time components");
      if (required === "date" && s.timeStyle !== undefined) throw typeError("timeStyle is not allowed here");
      if (required === "time" && s.dateStyle !== undefined) throw typeError("dateStyle is not allowed here");
      fmt = rec();
      if (s.timeStyle !== undefined) fmt.hour = "numeric";
      styleTemporalFormats(s);
    } else {
      var need = true;
      if (required === "date" || required === "any") {
        if (fmt.weekday !== undefined || fmt.year !== undefined || fmt.month !== undefined || fmt.day !== undefined) need = false;
      }
      if (required === "time" || required === "any") {
        if (fmt.dayPeriod !== undefined || fmt.hour !== undefined || fmt.minute !== undefined || fmt.second !== undefined ||
          fmt.fractionalSecondDigits !== undefined) need = false;
      }
      if (need && (defaults === "date" || defaults === "all")) { fmt.year = "numeric"; fmt.month = "numeric"; fmt.day = "numeric"; }
      if (need && (defaults === "time" || defaults === "all")) { fmt.hour = "numeric"; fmt.minute = "numeric"; fmt.second = "numeric"; }
      s.tPlainDate = GetDateTimeFormat(raw, "date", "date", false);
      s.tPlainYearMonth = GetDateTimeFormat(raw, "year-month", "year-month", false);
      s.tPlainMonthDay = GetDateTimeFormat(raw, "month-day", "month-day", false);
      s.tPlainTime = GetDateTimeFormat(raw, "time", "time", false);
      s.tPlainDateTime = GetDateTimeFormat(raw, "any", "all", false);
      s.tInstant = GetDateTimeFormat(raw, "any", toLocaleStringTimeZone !== undefined ? "zoned-date-time" : "all", true);
    }
    s.format = fmt;
    if (fmt.hour !== undefined) {
      if (hc === null || hc === undefined) hc = localeHourCycle(dataLocale);
      s.hourCycle = hc;
    } else s.hourCycle = undefined;
    s.hour12Option = hour12;
    return s;
  }
  function DateTimeFormat(locales = undefined, options = undefined) {
    var nt = new.target === undefined ? DateTimeFormat : new.target;
    var o = createFrom(nt, DateTimeFormatProto);
    new Slots(o, CreateDateTimeFormat(locales, options, "any", "date"));
    if (new.target === undefined && OrdinaryHasInstance(DateTimeFormat, this)) {
      ObjectDefineProperty(this, FallbackSymbol, { value: o, writable: false, enumerable: false, configurable: false });
      return this;
    }
    return o;
  }
  makeService(DateTimeFormat, "DateTimeFormat", 0, DateTimeFormatProto);
  // the native's options for a format record: a plain value formats in UTC, without a time zone name
  function optsFor(s, f, plain) {
    var tz = plain ? "UTC" : s.timeZone, ds, ts;
    if (tz[0] === "+" || tz[0] === "-") tz = "GMT" + tz;
    if (f === s.format) { ds = s.dateStyle; ts = s.timeStyle; } else { ds = f.dateStyle; ts = f.timeStyle; }
    var hasHour = f.hour !== undefined || ts !== undefined, hc = hasHour ? (s.hourCycle !== undefined ? s.hourCycle : localeHourCycle(s.locale)) : undefined;
    return [ds, ts, s.calendar, s.numberingSystem, tz,
      hc === undefined ? -1 : hc === "h11" || hc === "h12" ? 1 : 0, hc,
      f.weekday, f.era, f.year, f.month, f.day, f.dayPeriod, f.hour, f.minute, f.second,
      f.fractionalSecondDigits === undefined ? -1 : f.fractionalSecondDigits, plain ? undefined : f.timeZoneName];
  }
  function dtfOpts(s) {
    if (s.opts === undefined) s.opts = optsFor(s, s.format, false);
    return s.opts;
  }
  function dtfLocale(s) {
    if (s.icuLocale === undefined) {
      var l = removeUnicodeExtension(s.locale), px = l.indexOf("-x-");
      if (px >= 0) l = l.slice(0, px);
      s.icuLocale = l + "-u-ca-" + s.calendar + "-nu-" + s.numberingSystem;
    }
    return s.icuLocale;
  }
  var DateNow = Date.now, DateValueOf = Date.prototype.valueOf;
  function TimeClip(t) {
    if (!isFinite(t) || MathAbs(t) > 8.64e15) return NaN;
    return t < 0 ? -MathFloor(-t) + 0 : MathFloor(t) + 0;
  }
  function temporalKind(v) { return temporal !== undefined && isObject(v) ? temporal.kind(v) : undefined; }
  // ToDateTimeFormattable: a Temporal object stays one, anything else becomes a Number
  function dateArg(date) {
    if (date === undefined) return DateNow();
    var k = temporalKind(date);
    if (k !== undefined && k !== "Duration") return date;
    return ToNumber(date);
  }
  // HandleDateTimeValue: { ms, format, plain }
  function HandleDateTimeValue(s, x) {
    var r = rec();
    if (typeof x === "number") {
      x = TimeClip(x);
      if (NumberIsNaN(x)) throw rangeError("Invalid time value");
      r.ms = x; r.format = s.format; r.plain = false;
      return r;
    }
    var k = temporal.kind(x), cal, f;
    if (k === "ZonedDateTime") throw typeError("Temporal.ZonedDateTime cannot be formatted by Intl.DateTimeFormat; use toLocaleString");
    if (k === "PlainDate" || k === "PlainDateTime") {
      cal = temporal.calendar(x);
      if (cal !== "iso8601" && cal !== s.calendar) throw rangeError("calendar " + cal + " does not match the formatter's " + s.calendar);
    } else if (k === "PlainYearMonth" || k === "PlainMonthDay") {
      cal = temporal.calendar(x);
      if (cal !== s.calendar) throw rangeError("calendar " + cal + " does not match the formatter's " + s.calendar);
    }
    f = k === "PlainDate" ? s.tPlainDate : k === "PlainYearMonth" ? s.tPlainYearMonth : k === "PlainMonthDay" ? s.tPlainMonthDay :
      k === "PlainTime" ? s.tPlainTime : k === "PlainDateTime" ? s.tPlainDateTime : s.tInstant;
    if (f === null || f === undefined) throw typeError("the formatter's options do not apply to Temporal." + k);
    r.ms = temporal.ms(x); r.format = f; r.plain = k !== "Instant";
    return r;
  }
  function FormatDateTime(s, x) {
    var h = HandleDateTimeValue(s, x);
    var r = intlOp(30, dtfLocale(s), h.format === s.format && !h.plain ? dtfOpts(s) : optsFor(s, h.format, h.plain), h.ms, false);
    return r === undefined ? new Date(h.ms).toString() : r;
  }
  function FormatDateTimeToParts(s, x) {
    var h = HandleDateTimeValue(s, x);
    var p = intlOp(30, dtfLocale(s), h.format === s.format && !h.plain ? dtfOpts(s) : optsFor(s, h.format, h.plain), h.ms, true);
    if (p === undefined) p = [["literal", FormatDateTime(s, x)]];
    return partsToArray(p, false);
  }
  function thisDateTimeFormat(o, method) {
    var s = Slots.get(o);
    if (s === undefined && OrdinaryHasInstance(DateTimeFormat, o)) s = Slots.get(o[FallbackSymbol]);
    if (s === undefined || s.kind !== "DateTimeFormat") throw typeError(method + " called on an incompatible receiver");
    return s;
  }
  getter(DateTimeFormatProto, "format", { format() {
    var s = thisDateTimeFormat(this, "Intl.DateTimeFormat.prototype.format");
    if (s.boundFormat === undefined) {
      var F = (date) => FormatDateTime(s, dateArg(date));
      ObjectDefineProperty(F, "name", { value: "", configurable: true });
      s.boundFormat = F;
    }
    return s.boundFormat;
  } }.format);
  method(DateTimeFormatProto, "formatToParts", { formatToParts(date) {
    var s = slotsOf(this, "DateTimeFormat", "Intl.DateTimeFormat.prototype.formatToParts");
    return FormatDateTimeToParts(s, dateArg(date));
  } }.formatToParts);
  // PartitionDateTimeRangePattern's checks: both Numbers or both the same Temporal type
  function dtRange(s, start, end) {
    if (start === undefined || end === undefined) throw typeError("startDate and endDate are required");
    var x = dateArg(start), y = dateArg(end);
    if ((typeof x === "number") !== (typeof y === "number") || typeof x !== "number" && temporal.kind(x) !== temporal.kind(y))
      throw typeError("startDate and endDate must be of the same type");
    var hx = HandleDateTimeValue(s, x), hy = HandleDateTimeValue(s, y);
    return [hx, hy];
  }
  function rangeOpts(s, h) { return h.format === s.format && !h.plain ? dtfOpts(s) : optsFor(s, h.format, h.plain); }
  method(DateTimeFormatProto, "formatRange", { formatRange(startDate, endDate) {
    var s = slotsOf(this, "DateTimeFormat", "Intl.DateTimeFormat.prototype.formatRange");
    var h = dtRange(s, startDate, endDate), r = intlOp(31, dtfLocale(s), rangeOpts(s, h[0]), h[0].ms, h[1].ms, false);
    return r === undefined ? "" + h[0].ms + " \u2013 " + h[1].ms : r;
  } }.formatRange);
  method(DateTimeFormatProto, "formatRangeToParts", { formatRangeToParts(startDate, endDate) {
    var s = slotsOf(this, "DateTimeFormat", "Intl.DateTimeFormat.prototype.formatRangeToParts");
    var h = dtRange(s, startDate, endDate), p = intlOp(31, dtfLocale(s), rangeOpts(s, h[0]), h[0].ms, h[1].ms, true);
    if (p === undefined) p = [["literal", "" + h[0].ms, "startRange"], ["literal", " \u2013 ", "shared"], ["literal", "" + h[1].ms, "endRange"]];
    return partsToArray(p, true);
  } }.formatRangeToParts);
  method(DateTimeFormatProto, "resolvedOptions", { resolvedOptions() {
    var s = thisDateTimeFormat(this, "Intl.DateTimeFormat.prototype.resolvedOptions");
    var r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "calendar", s.calendar);
    cdp(r, "numberingSystem", s.numberingSystem);
    cdp(r, "timeZone", s.timeZone);
    if (s.hourCycle !== undefined) {
      cdp(r, "hourCycle", s.hourCycle);
      cdp(r, "hour12", s.hourCycle === "h11" || s.hourCycle === "h12");
    }
    if (s.dateStyle === undefined && s.timeStyle === undefined) {
      for (var i = 0; i < DTF_COMPONENTS.length; i++) {
        var p = DTF_COMPONENTS[i][0], v = s.format[p];
        if (v !== undefined) cdp(r, p, v);
      }
    }
    if (s.dateStyle !== undefined) cdp(r, "dateStyle", s.dateStyle);
    if (s.timeStyle !== undefined) cdp(r, "timeStyle", s.timeStyle);
    return r;
  } }.resolvedOptions);
  // 21.4.4.38-40 Date.prototype.toLocaleString, toLocaleDateString, toLocaleTimeString
  function dateLocale(d, locales, options, required, defaults) {
    var x = DateValueOf.call(d);
    if (NumberIsNaN(x)) return "Invalid Date";
    return FormatDateTime(CreateDateTimeFormat(locales, options, required, defaults), x);
  }
  def(Date.prototype, "toLocaleString", { toLocaleString(locales = undefined, options = undefined) { return dateLocale(this, locales, options, "any", "all"); } }.toLocaleString);
  def(Date.prototype, "toLocaleDateString", { toLocaleDateString(locales = undefined, options = undefined) { return dateLocale(this, locales, options, "date", "date"); } }.toLocaleDateString);
  def(Date.prototype, "toLocaleTimeString", { toLocaleTimeString(locales = undefined, options = undefined) { return dateLocale(this, locales, options, "time", "time"); } }.toLocaleTimeString);
  // the Temporal prototypes' toLocaleString (the Temporal proposal's ECMA-402 amendments)
  if (temporal !== undefined) {
    var TP = G.Temporal;
    var tls = function (cls, kind, required, defaults) {
      def(TP[cls].prototype, "toLocaleString", { toLocaleString(locales = undefined, options = undefined) {
        if (temporal.kind(this) !== kind) throw typeError("Temporal." + cls + ".prototype.toLocaleString called on an incompatible receiver");
        return FormatDateTime(CreateDateTimeFormat(locales, options, required, defaults), this);
      } }.toLocaleString);
    };
    tls("Instant", "Instant", "any", "all");
    tls("PlainDate", "PlainDate", "date", "date");
    tls("PlainDateTime", "PlainDateTime", "any", "all");
    tls("PlainMonthDay", "PlainMonthDay", "date", "date");
    tls("PlainTime", "PlainTime", "time", "time");
    tls("PlainYearMonth", "PlainYearMonth", "date", "date");
    def(TP.ZonedDateTime.prototype, "toLocaleString", { toLocaleString(locales = undefined, options = undefined) {
      if (temporal.kind(this) !== "ZonedDateTime") throw typeError("Temporal.ZonedDateTime.prototype.toLocaleString called on an incompatible receiver");
      var tz = temporal.timeZone(this);
      var s = CreateDateTimeFormat(locales, options, "any", "all", tz);
      var cal = temporal.calendar(this);
      if (cal !== "iso8601" && cal !== s.calendar) throw rangeError("calendar " + cal + " does not match the formatter's " + s.calendar);
      var r = intlOp(30, dtfLocale(s), optsFor(s, s.tInstant, false), temporal.ms(this), false);
      return r === undefined ? "" + temporal.ms(this) : r;
    } }.toLocaleString);
  }

  // ---------------- 10 Collator ----------------
  function coData(locale, key) {
    if (key === "kn") return ["false", "true"];
    if (key === "kf") return ["false", "lower", "upper"];
    var a = intlOp(5, locale), r = [null];
    for (var i = 0; i < a.length; i++) {
      var c = a[i];
      if (c === "phonebook") c = "phonebk"; else if (c === "traditional") c = "trad"; else if (c === "dictionary") c = "dict";
      if (c !== "standard" && c !== "search" && listIndexOf(r, c) < 0) append(r, c);
    }
    return r;
  }
  var CollatorProto = {};
  markIntrinsic(CollatorProto);
  function Collator(locales = undefined, options = undefined) {
    var nt = new.target === undefined ? Collator : new.target;
    var o = createFrom(nt, CollatorProto);
    var requested = CanonicalizeLocaleList(locales);
    options = CoerceOptionsToObject(options);
    var s = rec();
    s.kind = "Collator";
    s.usage = GetOption(options, "usage", "string", ["sort", "search"], "sort");
    var res = ResolveOptions(availableLocales(), coData, ["co", "kf", "kn"],
      [["collation", "co", "string", undefined], ["numeric", "kn", "boolean", undefined], ["caseFirst", "kf", "string", ["upper", "lower", "false"]]],
      listCopy(requested), options, false);
    var r = res.resolved;
    s.locale = r.locale;
    s.collation = r.co === null || r.co === undefined ? "default" : r.co;
    s.numeric = r.kn === "true";
    s.caseFirst = r.kf === undefined || r.kf === null ? "false" : r.kf;
    s.sensitivity = GetOption(options, "sensitivity", "string", ["base", "accent", "case", "variant"], "variant");
    s.ignorePunctuation = GetOption(options, "ignorePunctuation", "boolean", undefined, r.dataLocale.slice(0, 2) === "th");
    new Slots(o, s);
    return o;
  }
  makeService(Collator, "Collator", 0, CollatorProto);
  function collatorLocale(s) {
    if (s.icuLocale === undefined) {
      var l = removeUnicodeExtension(s.locale), px = l.indexOf("-x-");
      if (px >= 0) l = l.slice(0, px);
      s.icuLocale = s.collation !== "default" ? l + "-u-co-" + s.collation : l;
    }
    return s.icuLocale;
  }
  function CompareStrings(s, x, y) {
    return intlOp(40, collatorLocale(s), x, y, s.sensitivity, s.ignorePunctuation, s.numeric, s.caseFirst);
  }
  getter(CollatorProto, "compare", { compare() {
    var s = slotsOf(this, "Collator", "Intl.Collator.prototype.compare");
    if (s.boundCompare === undefined) {
      var F = (x, y) => CompareStrings(s, ToString(x), ToString(y));
      ObjectDefineProperty(F, "name", { value: "", configurable: true });
      s.boundCompare = F;
    }
    return s.boundCompare;
  } }.compare);
  method(CollatorProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "Collator", "Intl.Collator.prototype.resolvedOptions"), r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "usage", s.usage);
    cdp(r, "sensitivity", s.sensitivity);
    cdp(r, "ignorePunctuation", s.ignorePunctuation);
    cdp(r, "collation", s.collation);
    cdp(r, "numeric", s.numeric);
    cdp(r, "caseFirst", s.caseFirst);
    return r;
  } }.resolvedOptions);
  // 19.1.2 / 19.1.3 String.prototype.toLocaleLowerCase / toLocaleUpperCase: TransformCase. Only az, lt and tr have
  // language-sensitive case mappings (LookupMatchingLocaleByPrefix); the rest map as the root locale does.
  function transformCase(S, locales, upper) {
    var requested = CanonicalizeLocaleList(locales);
    var requestedLocale = requested.length > 0 ? requested[0] : DefaultLocale();
    var lang = removeUnicodeExtension(requestedLocale).split("-")[0];
    if (lang !== "az" && lang !== "lt" && lang !== "tr") lang = "";
    return intlOp(60, lang, S, upper);
  }
  def(String.prototype, "toLocaleLowerCase", { toLocaleLowerCase(locales = undefined) {
    if (this === null || this === undefined) throw typeError("String.prototype.toLocaleLowerCase called on null or undefined");
    return transformCase(Str(this), locales, false);
  } }.toLocaleLowerCase);
  def(String.prototype, "toLocaleUpperCase", { toLocaleUpperCase(locales = undefined) {
    if (this === null || this === undefined) throw typeError("String.prototype.toLocaleUpperCase called on null or undefined");
    return transformCase(Str(this), locales, true);
  } }.toLocaleUpperCase);

  // 22.1.3.12 String.prototype.localeCompare (ECMA-402 19.1.1)
  def(String.prototype, "localeCompare", { localeCompare(that, locales = undefined, options = undefined) {
    if (this === null || this === undefined) throw typeError("String.prototype.localeCompare called on null or undefined");
    var S = ToString(this), T = ToString(that);
    var c = new Collator(locales, options);
    return CompareStrings(Slots.get(c), S, T);
  } }.localeCompare);

  // ---------------- 18 RelativeTimeFormat ----------------
  var RelativeTimeFormatProto = {};
  markIntrinsic(RelativeTimeFormatProto);
  function RelativeTimeFormat(locales = undefined, options = undefined) {
    if (new.target === undefined) throw typeError("Constructor Intl.RelativeTimeFormat requires 'new'");
    var o = createFrom(new.target, RelativeTimeFormatProto);
    var res = ResolveOptions(availableLocales(), nuData, ["nu"], [["numberingSystem", "nu", "string", undefined]], locales, options, true);
    options = res.options;
    var s = rec();
    s.kind = "RelativeTimeFormat";
    s.locale = res.resolved.locale;
    s.numberingSystem = res.resolved.nu;
    s.style = GetOption(options, "style", "string", ["long", "short", "narrow"], "long");
    s.numeric = GetOption(options, "numeric", "string", ["always", "auto"], "always");
    new Slots(o, s);
    return o;
  }
  makeService(RelativeTimeFormat, "RelativeTimeFormat", 0, RelativeTimeFormatProto);
  var REL_UNITS = ["second", "minute", "hour", "day", "week", "month", "quarter", "year"];
  // 18.5.1 SingularRelativeTimeUnit
  function SingularRelativeTimeUnit(unit) {
    for (var i = 0; i < REL_UNITS.length; i++) if (unit === REL_UNITS[i] || unit === REL_UNITS[i] + "s") return REL_UNITS[i];
    throw rangeError("Invalid unit argument: " + unit);
  }
  function rtfArgs(value, unit) {
    value = ToNumber(value);
    unit = ToString(unit);
    if (!isFinite(value)) throw rangeError("Invalid value: " + value);
    return [value, SingularRelativeTimeUnit(unit)];
  }
  function rtfLocale(s) {
    if (s.icuLocale === undefined) {
      var l = removeUnicodeExtension(s.locale), px = l.indexOf("-x-");
      if (px >= 0) l = l.slice(0, px);
      s.icuLocale = l + "-u-nu-" + s.numberingSystem;
    }
    return s.icuLocale;
  }
  // the unit's name for a plural category: English and Polish (as Goccia's Intl), else the CLDR pattern
  var EN_UNITS = {
    long: { second: ["second", "seconds"], minute: ["minute", "minutes"], hour: ["hour", "hours"], day: ["day", "days"],
      week: ["week", "weeks"], month: ["month", "months"], quarter: ["quarter", "quarters"], year: ["year", "years"] },
    short: { second: ["sec.", "sec."], minute: ["min.", "min."], hour: ["hr.", "hr."], day: ["day", "days"],
      week: ["wk.", "wk."], month: ["mo.", "mo."], quarter: ["qtr.", "qtrs."], year: ["yr.", "yr."] },
    narrow: { second: ["s", "s"], minute: ["m", "m"], hour: ["h", "h"], day: ["d", "d"], week: ["w", "w"],
      month: ["mo", "mo"], quarter: ["q", "q"], year: ["y", "y"] }
  };
  function plUnit(unit, style, cat) {
    var c = cat === "one" ? 0 : cat === "few" ? 1 : cat === "other" ? 3 : 2;
    var T = {
      second: style === "long" ? ["sekund\u0119", "sekundy", "sekund", "sekundy"] : style === "short" ? ["sek.", "sek.", "sek.", "sek."] : ["s", "s", "s", "s"],
      minute: style === "long" ? ["minut\u0119", "minuty", "minut", "minuty"] : ["min", "min", "min", "min"],
      hour: style === "long" ? ["godzin\u0119", "godziny", "godzin", "godziny"] : style === "short" ? ["godz.", "godz.", "godz.", "godz."] : ["g.", "g.", "g.", "g."],
      day: ["dzie\u0144", "dni", "dni", "dnia"],
      week: style === "long" ? ["tydzie\u0144", "tygodnie", "tygodni", "tygodnia"] : ["tydz.", "tyg.", "tyg.", "tyg."],
      month: style === "long" ? ["miesi\u0105c", "miesi\u0105ce", "miesi\u0119cy", "miesi\u0105ca"] : ["mies.", "mies.", "mies.", "mies."],
      quarter: style === "long" ? ["kwarta\u0142", "kwarta\u0142y", "kwarta\u0142\u00f3w", "kwarta\u0142u"] : ["kw.", "kw.", "kw.", "kw."],
      year: ["rok", "lata", "lat", "roku"]
    };
    return T[unit][c];
  }
  var EN_AUTO = {
    second: [null, "now", null], minute: [null, "this minute", null], hour: [null, "this hour", null],
    day: ["yesterday", "today", "tomorrow"], week: ["last week", "this week", "next week"],
    month: ["last month", "this month", "next month"], quarter: ["last quarter", "this quarter", "next quarter"],
    year: ["last year", "this year", "next year"]
  };
  function rtfNumberFormat(s) {
    if (s.nf === undefined) s.nf = initNumberFormat(removeUnicodeExtension(s.locale), { numberingSystem: s.numberingSystem });
    return s.nf;
  }
  // the parts of value in unit: [type, value, unit] triples (unit set on the number's parts)
  function relativeParts(s, value, unit) {
    var lang = s.locale.slice(0, 2), past = value < 0 || (value === 0 && 1 / value < 0);
    if (s.numeric === "auto" && lang === "en" && (value === -1 || value === 0 || value === 1)) {
      var name = EN_AUTO[unit][value + 1];
      if (name !== null) return [["literal", name]];
    }
    var nf = rtfNumberFormat(s), x = MathAbs(value), numParts = FormatNumericToParts(nf, x), out = [];
    if (lang === "en" || lang === "pl") {
      var cat = intlOp(10, s.locale, x, false), word;
      if (lang === "en") word = EN_UNITS[s.style][unit][cat === "one" ? 0 : 1];
      else word = plUnit(unit, s.style, cat);
      if (!past) append(out, ["literal", lang === "pl" ? "za " : "in "]);
      for (var i = 0; i < numParts.length; i++) append(out, [numParts[i].type, numParts[i].value, unit]);
      append(out, ["literal", " " + word + (past ? (lang === "pl" ? " temu" : " ago") : "")]);
      return out;
    }
    var pat = intlOp(41, s.locale, unit), num = "";
    for (i = 0; i < numParts.length; i++) num += numParts[i].value;
    var t = pat === undefined ? "{0}" : past ? (pat[1] || pat[0]) : (pat[0] || pat[1]);
    var k = t.indexOf("{0}");
    if (k < 0) return [["literal", t]];
    if (k > 0) append(out, ["literal", t.slice(0, k)]);
    for (i = 0; i < numParts.length; i++) append(out, [numParts[i].type, numParts[i].value, unit]);
    if (k + 3 < t.length) append(out, ["literal", t.slice(k + 3)]);
    return out;
  }
  method(RelativeTimeFormatProto, "format", { format(value, unit) {
    var s = slotsOf(this, "RelativeTimeFormat", "Intl.RelativeTimeFormat.prototype.format");
    var a = rtfArgs(value, unit), p = relativeParts(s, a[0], a[1]), r = "";
    for (var i = 0; i < p.length; i++) r += p[i][1];
    return r;
  } }.format);
  method(RelativeTimeFormatProto, "formatToParts", { formatToParts(value, unit) {
    var s = slotsOf(this, "RelativeTimeFormat", "Intl.RelativeTimeFormat.prototype.formatToParts");
    var a = rtfArgs(value, unit), p = relativeParts(s, a[0], a[1]), out = [];
    for (var i = 0; i < p.length; i++) {
      var part = {};
      cdp(part, "type", p[i][0]);
      cdp(part, "value", p[i][1]);
      if (p[i].length > 2) cdp(part, "unit", p[i][2]);
      cdp(out, i, part);
    }
    return out;
  } }.formatToParts);
  method(RelativeTimeFormatProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "RelativeTimeFormat", "Intl.RelativeTimeFormat.prototype.resolvedOptions"), r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "style", s.style);
    cdp(r, "numeric", s.numeric);
    cdp(r, "numberingSystem", s.numberingSystem);
    return r;
  } }.resolvedOptions);

  // ---------------- 12 DisplayNames ----------------
  var DisplayNamesProto = {};
  markIntrinsic(DisplayNamesProto);
  function DisplayNames(locales, options) {
    if (new.target === undefined) throw typeError("Constructor Intl.DisplayNames requires 'new'");
    var o = createFrom(new.target, DisplayNamesProto);
    var res = ResolveOptions(availableLocales(), noData, [], [], locales, options, false, true);
    options = res.options;
    var s = rec();
    s.kind = "DisplayNames";
    s.style = GetOption(options, "style", "string", ["narrow", "short", "long"], "long");
    s.type = GetOption(options, "type", "string", ["language", "region", "script", "currency", "calendar", "dateTimeField"], undefined);
    if (s.type === undefined) throw typeError("type is required");
    s.fallback = GetOption(options, "fallback", "string", ["code", "none"], "code");
    s.locale = res.resolved.locale;
    var ld = GetOption(options, "languageDisplay", "string", ["dialect", "standard"], "dialect");
    if (s.type === "language") s.languageDisplay = ld;
    new Slots(o, s);
    return o;
  }
  makeService(DisplayNames, "DisplayNames", 2, DisplayNamesProto);
  var DATE_TIME_FIELDS = ["era", "year", "quarter", "month", "weekOfYear", "weekday", "day", "dayPeriod", "hour", "minute",
    "second", "timeZoneName"];
  // 12.5.1 CanonicalCodeForDisplayNames
  function CanonicalCodeForDisplayNames(type, code) {
    if (type === "language") {
      var parts = code.split("-");
      for (var i = 0; i < parts.length; i++) if (parts[i].length === 1) throw rangeError("Invalid language code: " + code);
      if (!IsStructurallyValidLanguageTag(code)) throw rangeError("Invalid language code: " + code);
      var b = parseBaseName(code);
      if (!isLanguageSubtag(b.language) || b.baseName !== code) throw rangeError("Invalid language code: " + code);
      return CanonicalizeUnicodeLocaleId(code);
    }
    if (type === "region") {
      if (!isRegionSubtag(code)) throw rangeError("Invalid region code: " + code);
      return asciiUpper(code);
    }
    if (type === "script") {
      if (!isScriptSubtag(code)) throw rangeError("Invalid script code: " + code);
      return asciiUpper(code[0]) + asciiLower(code.slice(1));
    }
    if (type === "calendar") {
      if (!isUnicodeType(code) || code.indexOf("_") >= 0) throw rangeError("Invalid calendar code: " + code);
      return asciiLower(code);
    }
    if (type === "dateTimeField") {
      if (listIndexOf(DATE_TIME_FIELDS, code) < 0) throw rangeError("Invalid dateTimeField code: " + code);
      return code;
    }
    if (!IsWellFormedCurrencyCode(code)) throw rangeError("Invalid currency code: " + code);
    return asciiUpper(code);
  }
  method(DisplayNamesProto, "of", { of(code) {
    var s = slotsOf(this, "DisplayNames", "Intl.DisplayNames.prototype.of");
    code = ToString(code);
    code = CanonicalCodeForDisplayNames(s.type, code);
    var name = intlOp(16, s.locale, code, s.type, s.style);
    if (name !== undefined) return name;
    return s.fallback === "code" ? code : undefined;
  } }.of);
  method(DisplayNamesProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "DisplayNames", "Intl.DisplayNames.prototype.resolvedOptions"), r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "style", s.style);
    cdp(r, "type", s.type);
    cdp(r, "fallback", s.fallback);
    if (s.type === "language") cdp(r, "languageDisplay", s.languageDisplay);
    return r;
  } }.resolvedOptions);

  // ---------------- 19 Segmenter ----------------
  var SegmenterProto = {};
  markIntrinsic(SegmenterProto);
  function Segmenter(locales = undefined, options = undefined) {
    if (new.target === undefined) throw typeError("Constructor Intl.Segmenter requires 'new'");
    var o = createFrom(new.target, SegmenterProto);
    var res = ResolveOptions(availableLocales(), noData, [], [], locales, options, false);
    options = res.options;
    var s = rec();
    s.kind = "Segmenter";
    s.locale = res.resolved.locale;
    s.granularity = GetOption(options, "granularity", "string", ["grapheme", "word", "sentence"], "grapheme");
    new Slots(o, s);
    return o;
  }
  makeService(Segmenter, "Segmenter", 0, SegmenterProto);
  // the segment boundaries of a string: starts and word-likeness, computed once per Segments object
  function segmentTable(seg, str) {
    var b = str === "" ? [] : intlOp(50, seg.locale, seg.granularity, str), starts = [0], wl = [];
    if (b === undefined) {
      // no ICU: code points
      b = [];
      for (var i = 0; i < str.length; i++) {
        var c = str.charCodeAt(i);
        if (c >= 0xd800 && c <= 0xdbff && i + 1 < str.length) i++;
        append(b, i + 1); append(b, false);
      }
    }
    for (var k = 0; k < b.length; k += 2) {
      if (b[k] <= starts[starts.length - 1]) continue;
      append(wl, b[k + 1]);
      append(starts, b[k]);
    }
    if (starts[starts.length - 1] !== str.length && str.length > 0) { append(wl, false); append(starts, str.length); }
    return { starts: starts, wordLike: wl };
  }
  // 19.7.1 CreateSegmentDataObject
  function segmentData(seg, str, t, i) {
    var r = {}, start = t.starts[i], end = t.starts[i + 1];
    cdp(r, "segment", str.slice(start, end));
    cdp(r, "index", start);
    cdp(r, "input", str);
    if (seg.granularity === "word") cdp(r, "isWordLike", t.wordLike[i]);
    return r;
  }
  function segmentIndexAt(t, n) {
    var lo = 0, hi = t.starts.length - 2;
    while (lo < hi) {
      var mid = (lo + hi + 1) >> 1;
      if (t.starts[mid] <= n) lo = mid; else hi = mid - 1;
    }
    return lo;
  }
  var SegmentsProto = {};
  markIntrinsic(SegmentsProto);
  method(SegmenterProto, "segment", { segment(string) {
    var s = slotsOf(this, "Segmenter", "Intl.Segmenter.prototype.segment");
    var str = ToString(string), o = ObjectCreate(SegmentsProto), ss = rec();
    ss.kind = "Segments"; ss.segmenter = s; ss.string = str; ss.table = undefined;
    new Slots(o, ss);
    return o;
  } }.segment);
  function tableOf(ss) { if (ss.table === undefined) ss.table = segmentTable(ss.segmenter, ss.string); return ss.table; }
  method(SegmentsProto, "containing", { containing(index) {
    var ss = slotsOf(this, "Segments", "%Segments.prototype%.containing");
    var n = ToNumber(index);
    n = NumberIsNaN(n) ? 0 : n < 0 ? -MathFloor(-n) : MathFloor(n);
    if (n < 0 || n >= ss.string.length) return undefined;
    var t = tableOf(ss);
    return segmentData(ss.segmenter, ss.string, t, segmentIndexAt(t, n));
  } }.containing);
  var IteratorPrototype = ObjectGetPrototypeOf(ObjectGetPrototypeOf([][Sym.iterator]()));
  var SegmentIteratorProto = ObjectCreate(IteratorPrototype);
  tag(SegmentIteratorProto, "Segmenter String Iterator");
  def(SegmentsProto, Sym.iterator, { [Sym.iterator]() {
    var ss = slotsOf(this, "Segments", "%Segments.prototype%[@@iterator]");
    var it = ObjectCreate(SegmentIteratorProto), si = rec();
    si.kind = "SegmentIterator"; si.segments = ss; si.position = 0;
    new Slots(it, si);
    return it;
  } }[Sym.iterator]);
  method(SegmentIteratorProto, "next", { next() {
    var si = slotsOf(this, "SegmentIterator", "%SegmentIterator.prototype%.next"), ss = si.segments, r = {};
    if (si.position >= ss.string.length) { cdp(r, "value", undefined); cdp(r, "done", true); return r; }
    var t = tableOf(ss), i = segmentIndexAt(t, si.position);
    si.position = t.starts[i + 1];
    cdp(r, "value", segmentData(ss.segmenter, ss.string, t, i));
    cdp(r, "done", false);
    return r;
  } }.next);
  method(SegmenterProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "Segmenter", "Intl.Segmenter.prototype.resolvedOptions"), r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "granularity", s.granularity);
    return r;
  } }.resolvedOptions);

  // ---------------- 13 DurationFormat ----------------
  // [unit, styles, digitalBase, NumberFormat unit]
  var DURATION_UNITS = [
    ["years", ["long", "short", "narrow"], "short", "year"], ["months", ["long", "short", "narrow"], "short", "month"],
    ["weeks", ["long", "short", "narrow"], "short", "week"], ["days", ["long", "short", "narrow"], "short", "day"],
    ["hours", ["long", "short", "narrow", "numeric", "2-digit"], "numeric", "hour"],
    ["minutes", ["long", "short", "narrow", "numeric", "2-digit"], "numeric", "minute"],
    ["seconds", ["long", "short", "narrow", "numeric", "2-digit"], "numeric", "second"],
    ["milliseconds", ["long", "short", "narrow", "numeric"], "numeric", "millisecond"],
    ["microseconds", ["long", "short", "narrow", "numeric"], "numeric", "microsecond"],
    ["nanoseconds", ["long", "short", "narrow", "numeric"], "numeric", "nanosecond"]];
  var TemporalDurationFrom = G.Temporal !== undefined ? G.Temporal.Duration.from : undefined;
  var durationGetters = rec();
  if (G.Temporal !== undefined) {
    for (var dui = 0; dui < DURATION_UNITS.length; dui++) {
      var duName = DURATION_UNITS[dui][0];
      durationGetters[duName] = Object.getOwnPropertyDescriptor(G.Temporal.Duration.prototype, duName).get;
    }
    durationGetters.sign = Object.getOwnPropertyDescriptor(G.Temporal.Duration.prototype, "sign").get;
  }
  // 13.5.6 GetDurationUnitOptions
  function GetDurationUnitOptions(unit, options, baseStyle, styles, digitalBase, prevStyle, twoDigitHours) {
    var style = GetOption(options, unit, "string", styles, undefined), displayDefault = "always";
    var hms = unit === "hours" || unit === "minutes" || unit === "seconds";
    if (style === undefined) {
      if (baseStyle === "digital") { if (!hms) displayDefault = "auto"; style = digitalBase; }
      else if (prevStyle === "fractional" || prevStyle === "numeric" || prevStyle === "2-digit") {
        if (unit !== "minutes" && unit !== "seconds") displayDefault = "auto";
        style = "numeric";
      } else { displayDefault = "auto"; style = baseStyle; }
    }
    if (style === "numeric" && (unit === "milliseconds" || unit === "microseconds" || unit === "nanoseconds")) {
      style = "fractional"; displayDefault = "auto";
    }
    var display = GetOption(options, unit + "Display", "string", ["auto", "always"], displayDefault);
    if (display === "always" && style === "fractional") throw rangeError(unit + "Display cannot be always with a fractional style");
    if (prevStyle === "fractional" && style !== "fractional") throw rangeError(unit + " must be fractional after a fractional unit");
    if (prevStyle === "numeric" || prevStyle === "2-digit") {
      if (style !== "fractional" && style !== "numeric" && style !== "2-digit") throw rangeError(unit + " must be numeric after a numeric unit");
      if (unit === "minutes" || unit === "seconds") style = "2-digit";
    }
    if (unit === "hours" && twoDigitHours) style = "2-digit";
    var r = rec(); r.style = style; r.display = display;
    return r;
  }
  var DurationFormatProto = {};
  markIntrinsic(DurationFormatProto);
  function DurationFormat(locales = undefined, options = undefined) {
    if (new.target === undefined) throw typeError("Constructor Intl.DurationFormat requires 'new'");
    var o = createFrom(new.target, DurationFormatProto);
    var res = ResolveOptions(availableLocales(), nuData, ["nu"], [["numberingSystem", "nu", "string", undefined]], locales, options, false);
    options = res.options;
    var s = rec();
    s.kind = "DurationFormat";
    s.locale = res.resolved.locale;
    s.numberingSystem = res.resolved.nu;
    s.style = GetOption(options, "style", "string", ["long", "short", "narrow", "digital"], "short");
    var prev = "";
    s.units = rec();
    for (var i = 0; i < DURATION_UNITS.length; i++) {
      var u = DURATION_UNITS[i];
      var uo = GetDurationUnitOptions(u[0], options, s.style, u[1], u[2], prev, false);
      s.units[u[0]] = uo;
      if (u[0] === "hours" || u[0] === "minutes" || u[0] === "seconds" || u[0] === "milliseconds" || u[0] === "microseconds") prev = uo.style;
    }
    s.fractionalDigits = GetNumberOption(options, "fractionalDigits", 0, 9, undefined);
    new Slots(o, s);
    return o;
  }
  makeService(DurationFormat, "DurationFormat", 0, DurationFormatProto);
  function durationRecord(d) {
    if (TemporalDurationFrom === undefined) throw typeError("Temporal is not available");
    var td = TemporalDurationFrom(d), r = rec();
    for (var i = 0; i < DURATION_UNITS.length; i++) r[DURATION_UNITS[i][0]] = durationGetters[DURATION_UNITS[i][0]].call(td);
    r.sign = durationGetters.sign.call(td);
    return r;
  }
  // a decimal string of seconds-like value + the fractional units: exact (BigInt nanoseconds)
  function withFraction(s, d, unit) {
    var scale = { seconds: [1000000000n, ["milliseconds", 1000000n], ["microseconds", 1000n], ["nanoseconds", 1n]],
      milliseconds: [1000000n, ["microseconds", 1000n], ["nanoseconds", 1n]], microseconds: [1000n, ["nanoseconds", 1n]] }[unit];
    var total = BigInt(d[unit]) * scale[0], digits = 0;
    for (var i = 1; i < scale.length; i++) {
      if (s.units[scale[i][0]].style === "fractional") total += BigInt(d[scale[i][0]]) * scale[i][1];
    }
    var neg = total < 0n;
    if (neg) total = -total;
    var str = "" + total, places = ("" + scale[0]).length - 1;
    while (str.length <= places) str = "0" + str;
    var dec = str.slice(0, str.length - places) + "." + str.slice(str.length - places);
    return { d: (neg ? "-" : "") + dec };
  }
  function NextUnitFractional(s, unit) {
    if (unit === "seconds") return s.units.milliseconds.style === "fractional";
    if (unit === "milliseconds") return s.units.microseconds.style === "fractional";
    if (unit === "microseconds") return s.units.nanoseconds.style === "fractional";
    return false;
  }
  function durationNF(s, opts) {
    var o = {};
    for (var k in opts) cdp(o, k, opts[k]);
    return initNumberFormat(s.locale, o);
  }
  function unitParts(s, nfopts, value, unit) {
    var p = FormatNumericToParts(durationNF(s, nfopts), value), out = [];
    for (var i = 0; i < p.length; i++) append(out, [p[i].type, p[i].value, unit]);
    return out;
  }
  function negZero(v) { return typeof v === "number" ? -0 : { d: "-0" }; }
  function isZero(v) { return typeof v === "number" ? v === 0 : /^-?0*\.?0*$/.test(v.d); }
  function FormatNumericUnits(s, d, first, signDisplayed) {
    var out = [], hv = d.hours, mv = d.minutes, sv = d.seconds;
    if (d.milliseconds !== 0 || d.microseconds !== 0 || d.nanoseconds !== 0) sv = withFraction(s, d, "seconds");
    var hf = first === "hours" && (hv !== 0 || s.units.hours.display === "always");
    var sf = !isZero(typeof sv === "number" ? sv : sv) || s.units.seconds.display === "always";
    var mf = false;
    if (first === "hours" || first === "minutes") {
      if (hf && sf) mf = true;
      else if (mv !== 0 || s.units.minutes.display === "always") mf = true;
    }
    function opts(unit, sd) {
      var o = rec();
      o.numberingSystem = s.numberingSystem;
      if (s.units[unit].style === "2-digit") o.minimumIntegerDigits = 2;
      if (!sd) o.signDisplay = "never";
      o.useGrouping = false;
      return o;
    }
    if (hf) {
      if (signDisplayed && hv === 0 && d.sign === -1) hv = -0;
      var p = unitParts(s, opts("hours", signDisplayed), hv, "hour");
      for (var i = 0; i < p.length; i++) append(out, p[i]);
      signDisplayed = false;
    }
    if (mf) {
      if (signDisplayed && mv === 0 && d.sign === -1) mv = -0;
      if (hf) append(out, ["literal", ":"]);
      p = unitParts(s, opts("minutes", signDisplayed), mv, "minute");
      for (i = 0; i < p.length; i++) append(out, p[i]);
      signDisplayed = false;
    }
    if (sf) {
      if (mf) append(out, ["literal", ":"]);
      var so = opts("seconds", signDisplayed);
      if (s.fractionalDigits === undefined) { so.maximumFractionDigits = 9; so.minimumFractionDigits = 0; }
      else { so.maximumFractionDigits = s.fractionalDigits; so.minimumFractionDigits = s.fractionalDigits; }
      so.roundingMode = "trunc";
      if (typeof sv === "number" && signDisplayed && sv === 0 && d.sign === -1) sv = -0;
      p = unitParts(s, so, sv, "second");
      for (i = 0; i < p.length; i++) append(out, p[i]);
    }
    return out;
  }
  // 13.5.15 PartitionDurationFormatPattern
  function PartitionDurationFormatPattern(s, d) {
    var result = [], signDisplayed = true, numericFound = false;
    for (var i = 0; i < DURATION_UNITS.length && !numericFound; i++) {
      var unit = DURATION_UNITS[i][0], nfUnit = DURATION_UNITS[i][3], uo = s.units[unit], value = d[unit];
      if (uo.style === "numeric" || uo.style === "2-digit") {
        var np = FormatNumericUnits(s, d, unit, signDisplayed);
        if (np.length > 0) append(result, np);
        numericFound = true;
      } else {
        var o = rec();
        if (NextUnitFractional(s, unit)) {
          value = withFraction(s, d, unit);
          if (s.fractionalDigits === undefined) { o.maximumFractionDigits = 9; o.minimumFractionDigits = 0; }
          else { o.maximumFractionDigits = s.fractionalDigits; o.minimumFractionDigits = s.fractionalDigits; }
          o.roundingMode = "trunc";
          numericFound = true;
        }
        if (uo.display === "always" || !isZero(typeof value === "number" ? value : value)) {
          o.numberingSystem = s.numberingSystem;
          if (signDisplayed) {
            signDisplayed = false;
            if (isZero(typeof value === "number" ? value : value) && d.sign === -1) value = negZero(value);
          } else o.signDisplay = "never";
          o.style = "unit"; o.unit = nfUnit; o.unitDisplay = uo.style;
          append(result, unitParts(s, o, value, nfUnit));
        }
      }
    }
    // 13.5.14 ListFormatParts
    var ls = s.style === "digital" ? "short" : s.style, strings = [];
    for (i = 0; i < result.length; i++) {
      var str = "";
      for (var j = 0; j < result[i].length; j++) str += result[i][j][1];
      append(strings, str);
    }
    if (strings.length === 0) return [];
    var lp = intlOp(13, removeUnicodeExtension(s.locale), strings, "unit", ls), out = [], k = 0;
    if (lp === undefined) { lp = []; for (i = 0; i < strings.length; i++) { if (i > 0) append(lp, ["literal", ", "]); append(lp, ["element", strings[i]]); } }
    for (i = 0; i < lp.length; i++) {
      if (lp[i][0] === "element") { var parts = result[k++]; for (j = 0; j < parts.length; j++) append(out, parts[j]); }
      else append(out, ["literal", lp[i][1]]);
    }
    return out;
  }
  method(DurationFormatProto, "format", { format(duration) {
    var s = slotsOf(this, "DurationFormat", "Intl.DurationFormat.prototype.format");
    var p = PartitionDurationFormatPattern(s, durationRecord(duration)), r = "";
    for (var i = 0; i < p.length; i++) r += p[i][1];
    return r;
  } }.format);
  method(DurationFormatProto, "formatToParts", { formatToParts(duration) {
    var s = slotsOf(this, "DurationFormat", "Intl.DurationFormat.prototype.formatToParts");
    var p = PartitionDurationFormatPattern(s, durationRecord(duration)), out = [];
    for (var i = 0; i < p.length; i++) {
      var part = {};
      cdp(part, "type", p[i][0]);
      cdp(part, "value", p[i][1]);
      if (p[i].length > 2 && p[i][2] !== undefined) cdp(part, "unit", p[i][2]);
      cdp(out, i, part);
    }
    return out;
  } }.formatToParts);
  method(DurationFormatProto, "resolvedOptions", { resolvedOptions() {
    var s = slotsOf(this, "DurationFormat", "Intl.DurationFormat.prototype.resolvedOptions"), r = {};
    cdp(r, "locale", s.locale);
    cdp(r, "numberingSystem", s.numberingSystem);
    cdp(r, "style", s.style);
    for (var i = 0; i < DURATION_UNITS.length; i++) {
      var u = DURATION_UNITS[i][0], uo = s.units[u], st = uo.style;
      if (st === "fractional") st = "numeric";
      cdp(r, u, st);
      cdp(r, u + "Display", uo.display);
    }
    if (s.fractionalDigits !== undefined) cdp(r, "fractionalDigits", s.fractionalDigits);
    return r;
  } }.resolvedOptions);

  if (temporal !== undefined) {
    def(G.Temporal.Duration.prototype, "toLocaleString", { toLocaleString(locales = undefined, options = undefined) {
      if (temporal.kind(this) !== "Duration") throw typeError("Temporal.Duration.prototype.toLocaleString called on an incompatible receiver");
      var df = new DurationFormat(locales, options), p = PartitionDurationFormatPattern(Slots.get(df), durationRecord(this)), r = "";
      for (var i = 0; i < p.length; i++) r += p[i][1];
      return r;
    } }.toLocaleString);
  }

  def(G, "Intl", Intl);
})

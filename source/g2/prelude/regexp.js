(function (G, N) {
  "use strict";
  // 22.2 RegExp by the spec's steps: the constructor's general path, RegExpExec, the Symbol.match / matchAll /
  // replace / search / split methods, the flags getter, toString, test, the RegExp String Iterator,
  // RegExp.escape, and the String.prototype methods that dispatch on their argument. G2.RegExpV keeps native
  // fast paths for an unmodified RegExp and calls these otherwise. N holds the natives: exec
  // (RegExpBuiltinExec), isRE, alloc, init, source, flags.
  var apply = Reflect.apply, construct = Reflect.construct, defineProperty = Object.defineProperty;
  var getProto = Object.getPrototypeOf, setProto = Object.setPrototypeOf;
  var TypeErr = TypeError, Str = String, MathMin = Math.min, MathMax = Math.max, MathTrunc = Math.trunc;
  var SymMatch = Symbol.match, SymMatchAll = Symbol.matchAll, SymReplace = Symbol.replace;
  var SymSearch = Symbol.search, SymSplit = Symbol.split, SymSpecies = Symbol.species;
  var SymToStringTag = Symbol.toStringTag, SymIterator = Symbol.iterator;
  var nativeExec = N.exec, isRE = N.isRE, alloc = N.alloc, initRE = N.init;
  var RP = N.proto, RegExpCtor = N.ctor;
  var strIndexOf = Str.prototype.indexOf, strSlice = Str.prototype.slice, charCodeAt = Str.prototype.charCodeAt;
  var strReplacePlain = N.strReplace, strReplaceAllPlain = N.strReplaceAll, strSplitPlain = N.strSplit;
  var objectIs = Object.is;

  // an Object: typeof says "object" or "function", or "undefined" for an [[IsHTMLDDA]] object (B.3.6)
  function isObject(v) {
    var t = typeof v;
    return (t === "object" && v !== null) || t === "function" || (t === "undefined" && v !== undefined);
  }
  // IsCallable: typeof says "function", or "undefined" for a callable [[IsHTMLDDA]] object (B.3.6)
  function isCallable(f) { var t = typeof f; return t === "function" || (t === "undefined" && f !== undefined); }
  // an internal List: an array without a prototype, so filling it runs no Array.prototype index accessor
  function list() { return setProto([], null); }
  function def(o, k, v) { defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true }); }
  function toStr(v) { return `${v}`; }
  function toIntegerOrInfinity(v) {
    var n = +v;
    if (n !== n || n === 0) return 0;
    return MathTrunc(n);
  }
  function toLength(v) {
    var n = toIntegerOrInfinity(v);
    if (n <= 0) return 0;
    return n > 9007199254740991 ? 9007199254740991 : n;
  }
  function has(s, c) { return apply(strIndexOf, s, [c]) !== -1; }
  function sub(s, a, b) { return apply(strSlice, s, [a, b]); }
  function requireCoercible(v, name) {
    if (v === undefined || v === null) throw new TypeErr("String.prototype." + name + " called on null or undefined");
  }
  function getMethod(v, k) {
    var f = v[k];
    if (f === undefined || f === null) return undefined;
    if (!isCallable(f)) throw new TypeErr("the method is not callable");
    return f;
  }
  function isConstructor(f) {
    if (!isCallable(f)) return false;
    try { construct(Str, [], f); return true; } catch (e) { return false; }
  }
  // 7.3.22 SpeciesConstructor
  function speciesConstructor(O, dflt) {
    var C = O.constructor;
    if (C === undefined) return dflt;
    if (!isObject(C)) throw new TypeErr("object.constructor is not an object");
    var S = C[SymSpecies];
    if (S === undefined || S === null) return dflt;
    if (isConstructor(S)) return S;
    throw new TypeErr("object.constructor[Symbol.species] is not a constructor");
  }
  // 7.2.8 IsRegExp
  function isRegExp(v) {
    if (!isObject(v)) return false;
    var m = v[SymMatch];
    if (m !== undefined) return !!m;
    return isRE(v);
  }
  // 22.2.7.3 AdvanceStringIndex
  function advance(S, index, unicode) {
    if (!unicode || index + 1 >= S.length) return index + 1;
    var c = apply(charCodeAt, S, [index]);
    if (c >= 0xD800 && c <= 0xDBFF) {
      var d = apply(charCodeAt, S, [index + 1]);
      if (d >= 0xDC00 && d <= 0xDFFF) return index + 2;
    }
    return index + 1;
  }
  // 22.2.7.1 RegExpExec
  function regExpExec(R, S) {
    var exec = R.exec;
    if (isCallable(exec)) {
      var result = apply(exec, R, [S]);
      if (result !== null && !isObject(result)) throw new TypeErr("RegExp exec method returned something other than an Object or null");
      return result;
    }
    if (!isRE(R)) throw new TypeErr("RegExp.prototype.exec called on an incompatible receiver");
    return nativeExec(R, S);
  }
  // 22.2.3.1 RegExpCreate
  function regExpCreate(P, F) {
    return initialize(alloc(RP, true), P, F);
  }
  // 22.2.3.3 RegExpInitialize
  function initialize(O, P, F) {
    P = P === undefined ? "" : toStr(P);
    F = F === undefined ? "" : toStr(F);
    initRE(O, P, F);
    O.lastIndex = 0;
    return O;
  }

  // 22.2.4.1 RegExp ( pattern, flags ): the native constructor calls this with its new.target
  function regExpConstruct(pattern, flags, newTarget) {
    var patternIsRegExp = isRegExp(pattern);
    if (newTarget === undefined) {
      newTarget = RegExpCtor;
      if (patternIsRegExp && flags === undefined) {
        if (pattern.constructor === newTarget) return pattern;
      }
    }
    var P, F;
    if (isRE(pattern)) {
      P = N.source(pattern, false);
      F = flags === undefined ? N.flags(pattern) : flags;
    } else if (patternIsRegExp) {
      P = pattern.source;
      F = flags === undefined ? pattern.flags : flags;
    } else {
      P = pattern;
      F = flags;
    }
    var proto = newTarget.prototype;
    if (!isObject(proto)) proto = RP;
    return initialize(alloc(proto, newTarget === RegExpCtor), P, F);
  }

  // B.2.4.1 RegExp.prototype.compile ( pattern, flags )
  function compile(pattern, flags) {
    var O = this;
    if (!isRE(O)) throw new TypeErr("RegExp.prototype.compile called on an incompatible receiver");
    if (!N.legacy(O)) throw new TypeErr("RegExp.prototype.compile called on a RegExp subclass instance");
    var P, F;
    if (isRE(pattern)) {
      if (flags !== undefined) throw new TypeErr("RegExp.prototype.compile: flags must be undefined when the pattern is a RegExp");
      P = N.source(pattern, false);
      F = N.flags(pattern);
    } else {
      P = pattern;
      F = flags;
    }
    return initialize(O, P, F);
  }

  // 22.2.6.4 get RegExp.prototype.flags
  var flagsGetter = {
    get flags() {
      var R = this;
      if (!isObject(R)) throw new TypeErr("RegExp.prototype.flags getter called on a non-object");
      var r = "";
      if (R.hasIndices) r += "d";
      if (R.global) r += "g";
      if (R.ignoreCase) r += "i";
      if (R.multiline) r += "m";
      if (R.dotAll) r += "s";
      if (R.unicode) r += "u";
      if (R.unicodeSets) r += "v";
      if (R.sticky) r += "y";
      return r;
    }
  };

  var methods = {
    // 22.2.6.17 RegExp.prototype.toString ( )
    toString() {
      var R = this;
      if (!isObject(R)) throw new TypeErr("RegExp.prototype.toString called on a non-object");
      return "/" + toStr(R.source) + "/" + toStr(R.flags);
    },
    // 22.2.6.16 RegExp.prototype.test ( S )
    test(S) {
      var R = this;
      if (!isObject(R)) throw new TypeErr("RegExp.prototype.test called on a non-object");
      return regExpExec(R, toStr(S)) !== null;
    },
    // 22.2.6.8 RegExp.prototype [ %Symbol.match% ] ( string )
    [SymMatch](string) {
      var rx = this;
      if (!isObject(rx)) throw new TypeErr("RegExp.prototype[Symbol.match] called on a non-object");
      var S = toStr(string);
      var flags = toStr(rx.flags);
      if (!has(flags, "g")) return regExpExec(rx, S);
      var fullUnicode = has(flags, "u") || has(flags, "v");
      rx.lastIndex = 0;
      var A = [], n = 0;
      for (;;) {
        var result = regExpExec(rx, S);
        if (result === null) return n === 0 ? null : A;
        var matchStr = toStr(result[0]);
        defineProperty(A, n, { value: matchStr, writable: true, enumerable: true, configurable: true });
        if (matchStr === "") rx.lastIndex = advance(S, toLength(rx.lastIndex), fullUnicode);
        n++;
      }
    },
    // 22.2.6.9 RegExp.prototype [ %Symbol.matchAll% ] ( string )
    [SymMatchAll](string) {
      var R = this;
      if (!isObject(R)) throw new TypeErr("RegExp.prototype[Symbol.matchAll] called on a non-object");
      var S = toStr(string);
      var C = speciesConstructor(R, RegExpCtor);
      var flags = toStr(R.flags);
      var matcher = construct(C, [R, flags]);
      matcher.lastIndex = toLength(R.lastIndex);
      return new RegExpStringIterator(matcher, S, has(flags, "g"), has(flags, "u") || has(flags, "v"));
    },
    // 22.2.6.11 RegExp.prototype [ %Symbol.replace% ] ( string, replaceValue )
    [SymReplace](string, replaceValue) {
      var rx = this;
      if (!isObject(rx)) throw new TypeErr("RegExp.prototype[Symbol.replace] called on a non-object");
      var S = toStr(string), lengthS = S.length;
      var functionalReplace = isCallable(replaceValue);
      if (!functionalReplace) replaceValue = toStr(replaceValue);
      var flags = toStr(rx.flags);
      var global = has(flags, "g");
      if (global) rx.lastIndex = 0;
      var results = list(), nr = 0;
      for (;;) {
        var result = regExpExec(rx, S);
        if (result === null) break;
        results[nr++] = result;
        if (!global) break;
        var matchStr = toStr(result[0]);
        if (matchStr === "") rx.lastIndex = advance(S, toLength(rx.lastIndex), has(flags, "u") || has(flags, "v"));
      }
      var acc = "", nextSourcePosition = 0;
      for (var i = 0; i < nr; i++) {
        result = results[i];
        var resultLength = toLength(result.length);
        var nCaptures = MathMax(resultLength - 1, 0);
        var matched = toStr(result[0]);
        var matchLength = matched.length;
        var position = toIntegerOrInfinity(result.index);
        position = MathMax(MathMin(position, lengthS), 0);
        var captures = list();
        for (var n = 1; n <= nCaptures; n++) {
          var capN = result[n];
          if (capN !== undefined) capN = toStr(capN);
          captures[n - 1] = capN;
        }
        var namedCaptures = result.groups;
        var replacement;
        if (functionalReplace) {
          var args = list();
          args[0] = matched;
          for (n = 0; n < nCaptures; n++) args[n + 1] = captures[n];
          args[nCaptures + 1] = position;
          args[nCaptures + 2] = S;
          if (namedCaptures !== undefined) args[nCaptures + 3] = namedCaptures;
          replacement = toStr(apply(replaceValue, undefined, args));
        } else {
          if (namedCaptures !== undefined) {
            if (namedCaptures === null) throw new TypeErr("the groups of a RegExp result cannot be converted to an object");
            namedCaptures = Object(namedCaptures);
          }
          replacement = getSubstitution(matched, S, position, captures, namedCaptures, replaceValue);
        }
        if (position >= nextSourcePosition) {
          acc = acc + sub(S, nextSourcePosition, position) + replacement;
          nextSourcePosition = position + matchLength;
        }
      }
      if (nextSourcePosition >= lengthS) return acc;
      return acc + sub(S, nextSourcePosition);
    },
    // 22.2.6.12 RegExp.prototype [ %Symbol.search% ] ( string )
    [SymSearch](string) {
      var rx = this;
      if (!isObject(rx)) throw new TypeErr("RegExp.prototype[Symbol.search] called on a non-object");
      var S = toStr(string);
      var previousLastIndex = rx.lastIndex;
      if (!objectIs(previousLastIndex, 0)) rx.lastIndex = 0;
      var result = regExpExec(rx, S);
      var currentLastIndex = rx.lastIndex;
      if (!objectIs(currentLastIndex, previousLastIndex)) rx.lastIndex = previousLastIndex;
      if (result === null) return -1;
      return result.index;
    },
    // 22.2.6.14 RegExp.prototype [ %Symbol.split% ] ( string, limit )
    [SymSplit](string, limit) {
      var rx = this;
      if (!isObject(rx)) throw new TypeErr("RegExp.prototype[Symbol.split] called on a non-object");
      var S = toStr(string);
      var C = speciesConstructor(rx, RegExpCtor);
      var flags = toStr(rx.flags);
      var unicodeMatching = has(flags, "u") || has(flags, "v");
      var newFlags = has(flags, "y") ? flags : flags + "y";
      var splitter = construct(C, [rx, newFlags]);
      var A = [], lengthA = 0;
      var lim = limit === undefined ? 4294967295 : limit >>> 0;
      if (lim === 0) return A;
      if (S === "") {
        if (regExpExec(splitter, S) !== null) return A;
        defineProperty(A, 0, { value: S, writable: true, enumerable: true, configurable: true });
        return A;
      }
      var size = S.length, p = 0, q = 0;
      while (q < size) {
        splitter.lastIndex = q;
        var z = regExpExec(splitter, S);
        if (z === null) { q = advance(S, q, unicodeMatching); continue; }
        var e = MathMin(toLength(splitter.lastIndex), size);
        if (e === p) { q = advance(S, q, unicodeMatching); continue; }
        defineProperty(A, lengthA, { value: sub(S, p, q), writable: true, enumerable: true, configurable: true });
        if (++lengthA === lim) return A;
        p = e;
        var numberOfCaptures = MathMax(toLength(z.length) - 1, 0);
        for (var i = 1; i <= numberOfCaptures; i++) {
          defineProperty(A, lengthA, { value: z[i], writable: true, enumerable: true, configurable: true });
          if (++lengthA === lim) return A;
        }
        q = p;
      }
      defineProperty(A, lengthA, { value: sub(S, p, size), writable: true, enumerable: true, configurable: true });
      return A;
    }
  };

  // 22.1.3.19.1 GetSubstitution ( matched, str, position, captures, namedCaptures, replacementTemplate )
  function getSubstitution(matched, str, position, captures, namedCaptures, template) {
    var result = "", stringLength = str.length, m = captures.length, i = 0, tl = template.length;
    while (i < tl) {
      var c = apply(charCodeAt, template, [i]);
      if (c !== 0x24 || i + 1 >= tl) { result += template[i]; i++; continue; }
      var d = apply(charCodeAt, template, [i + 1]);
      if (d === 0x24) { result += "$"; i += 2; continue; }
      if (d === 0x26) { result += matched; i += 2; continue; }
      if (d === 0x60) { result += sub(str, 0, position); i += 2; continue; }
      if (d === 0x27) { result += sub(str, MathMin(position + matched.length, stringLength)); i += 2; continue; }
      if (d >= 0x30 && d <= 0x39) {
        var digitCount = 1, index = d - 0x30;
        if (i + 2 < tl) {
          var e = apply(charCodeAt, template, [i + 2]);
          if (e >= 0x30 && e <= 0x39) { digitCount = 2; index = index * 10 + (e - 0x30); }
        }
        if (index > m && digitCount === 2) { digitCount = 1; index = d - 0x30; }
        if (index >= 1 && index <= m) {
          var capture = captures[index - 1];
          if (capture !== undefined) result += capture;
        } else result += sub(template, i, i + 1 + digitCount);
        i += 1 + digitCount;
        continue;
      }
      if (d === 0x3C) {
        var gtPos = apply(strIndexOf, template, [">", i]);
        if (gtPos === -1 || namedCaptures === undefined) { result += "$<"; i += 2; continue; }
        var groupName = sub(template, i + 2, gtPos);
        var cap = namedCaptures[groupName];
        if (cap !== undefined) result += toStr(cap);
        i = gtPos + 1;
        continue;
      }
      result += "$";
      i++;
    }
    return result;
  }

  // 22.2.9 RegExp String Iterator Objects
  var IteratorProto = getProto(getProto([][SymIterator]()));
  class RegExpStringIterator {
    #R; #S; #global; #unicode; #done = false;
    constructor(R, S, global, unicode) { this.#R = R; this.#S = S; this.#global = global; this.#unicode = unicode; }
    static #check(O) {
      if (!isObject(O) || !(#R in O)) throw new TypeErr("%RegExpStringIteratorPrototype%.next called on an incompatible receiver");
    }
    next() {
      RegExpStringIterator.#check(this);
      if (this.#done) return { value: undefined, done: true };
      var R = this.#R, S = this.#S;
      var match = regExpExec(R, S);
      if (match === null) { this.#done = true; return { value: undefined, done: true }; }
      if (this.#global) {
        if (toStr(match[0]) === "") R.lastIndex = advance(S, toLength(R.lastIndex), this.#unicode);
        return { value: match, done: false };
      }
      this.#done = true;
      return { value: match, done: false };
    }
  }
  var RSIP = RegExpStringIterator.prototype;
  delete RSIP.constructor;
  setProto(RSIP, IteratorProto);
  defineProperty(RSIP, SymToStringTag, { value: "RegExp String Iterator", writable: false, enumerable: false, configurable: true });

  // 22.2.5.1 RegExp.escape ( string )
  var HEX = "0123456789abcdef";
  function hex2(n) { return HEX[n >> 4] + HEX[n & 15]; }
  function uEscape(n) { return "\\u" + HEX[(n >> 12) & 15] + HEX[(n >> 8) & 15] + HEX[(n >> 4) & 15] + HEX[n & 15]; }
  function isWhiteOrLT(c) {
    return c === 0x09 || c === 0x0B || c === 0x0C || c === 0x20 || c === 0xA0 || c === 0xFEFF || c === 0x1680 ||
      (c >= 0x2000 && c <= 0x200A) || c === 0x202F || c === 0x205F || c === 0x3000 ||
      c === 0x0A || c === 0x0D || c === 0x2028 || c === 0x2029;
  }
  function escape(string) {
    if (typeof string !== "string") throw new TypeErr("RegExp.escape requires a string");
    var escaped = "", n = string.length;
    for (var i = 0; i < n; i++) {
      var c = apply(charCodeAt, string, [i]);
      if (escaped === "" && ((c >= 0x30 && c <= 0x39) || (c >= 0x41 && c <= 0x5A) || (c >= 0x61 && c <= 0x7A))) {
        escaped = "\\x" + hex2(c);
        continue;
      }
      if (c >= 0xD800 && c <= 0xDBFF && i + 1 < n) {
        var d = apply(charCodeAt, string, [i + 1]);
        if (d >= 0xDC00 && d <= 0xDFFF) { escaped += string[i] + string[i + 1]; i++; continue; }
      }
      if ("^$\\.*+?()[]{}|/".indexOf(string[i]) !== -1) { escaped += "\\" + string[i]; continue; }
      if (c === 0x09) { escaped += "\\t"; continue; }
      if (c === 0x0A) { escaped += "\\n"; continue; }
      if (c === 0x0B) { escaped += "\\v"; continue; }
      if (c === 0x0C) { escaped += "\\f"; continue; }
      if (c === 0x0D) { escaped += "\\r"; continue; }
      if (",-=<>#&!%:;@~'`\"".indexOf(string[i]) !== -1 || isWhiteOrLT(c) || (c >= 0xD800 && c <= 0xDFFF)) {
        escaped += c <= 0xFF ? "\\x" + hex2(c) : uEscape(c);
        continue;
      }
      escaped += string[i];
    }
    return escaped;
  }

  // 22.1.3 String.prototype methods taking a RegExp or a pattern
  var strMethods = {
    match(regexp) {
      requireCoercible(this, "match");
      if (isObject(regexp)) {
        var matcher = getMethod(regexp, SymMatch);
        if (matcher !== undefined) return apply(matcher, regexp, [this]);
      }
      var S = toStr(this);
      var rx = regExpCreate(regexp, undefined);
      return rx[SymMatch](S);
    },
    matchAll(regexp) {
      requireCoercible(this, "matchAll");
      if (isObject(regexp)) {
        if (isRegExp(regexp)) {
          var flags = regexp.flags;
          requireCoercible(flags, "matchAll");
          if (!has(toStr(flags), "g")) throw new TypeErr("String.prototype.matchAll called with a non-global RegExp");
        }
        var matcher = getMethod(regexp, SymMatchAll);
        if (matcher !== undefined) return apply(matcher, regexp, [this]);
      }
      var S = toStr(this);
      var rx = regExpCreate(regexp, "g");
      return rx[SymMatchAll](S);
    },
    replace(searchValue, replaceValue) {
      requireCoercible(this, "replace");
      if (isObject(searchValue)) {
        var replacer = getMethod(searchValue, SymReplace);
        if (replacer !== undefined) return apply(replacer, searchValue, [this, replaceValue]);
      }
      return apply(strReplacePlain, toStr(this), [toStr(searchValue), replaceValue]);
    },
    replaceAll(searchValue, replaceValue) {
      requireCoercible(this, "replaceAll");
      if (isObject(searchValue)) {
        if (isRegExp(searchValue)) {
          var flags = searchValue.flags;
          requireCoercible(flags, "replaceAll");
          if (!has(toStr(flags), "g")) throw new TypeErr("String.prototype.replaceAll called with a non-global RegExp");
        }
        var replacer = getMethod(searchValue, SymReplace);
        if (replacer !== undefined) return apply(replacer, searchValue, [this, replaceValue]);
      }
      return apply(strReplaceAllPlain, toStr(this), [toStr(searchValue), replaceValue]);
    },
    search(regexp) {
      requireCoercible(this, "search");
      if (isObject(regexp)) {
        var searcher = getMethod(regexp, SymSearch);
        if (searcher !== undefined) return apply(searcher, regexp, [this]);
      }
      var S = toStr(this);
      var rx = regExpCreate(regexp, undefined);
      return rx[SymSearch](S);
    },
    split(separator, limit) {
      requireCoercible(this, "split");
      if (isObject(separator)) {
        var splitter = getMethod(separator, SymSplit);
        if (splitter !== undefined) return apply(splitter, separator, [this, limit]);
      }
      return apply(strSplitPlain, toStr(this), [separator, limit]);
    }
  };

  def(RP, "compile", compile);
  defineProperty(RP, "flags", { get: Object.getOwnPropertyDescriptor(flagsGetter, "flags").get, set: undefined, enumerable: false, configurable: true });
  def(RP, "toString", methods.toString);
  var keys = [SymMatch, SymMatchAll, SymReplace, SymSearch, SymSplit];
  for (var k = 0; k < keys.length; k++) def(RP, keys[k], methods[keys[k]]);
  // a built-in function, not a constructor: a method's function object (no [[Construct]], no prototype)
  def(RegExpCtor, "escape", { escape(string) { return escape(string); } }.escape);

  // B.2.2.2-B.2.2.14 the String.prototype HTML methods, B.2.2.15-16 trimLeft / trimRight
  var SP = Str.prototype;
  function createHTML(string, tag, attribute, value, name) {
    requireCoercible(string, name);
    var S = toStr(string);
    var p1 = "<" + tag;
    if (attribute !== "") {
      var V = toStr(value), escapedV = "";
      for (var i = 0; i < V.length; i++) escapedV += V[i] === '"' ? "&quot;" : V[i];
      p1 = p1 + " " + attribute + '="' + escapedV + '"';
    }
    return p1 + ">" + S + "</" + tag + ">";
  }
  var html = {
    anchor(name) { return createHTML(this, "a", "name", name, "anchor"); },
    big() { return createHTML(this, "big", "", "", "big"); },
    blink() { return createHTML(this, "blink", "", "", "blink"); },
    bold() { return createHTML(this, "b", "", "", "bold"); },
    fixed() { return createHTML(this, "tt", "", "", "fixed"); },
    fontcolor(color) { return createHTML(this, "font", "color", color, "fontcolor"); },
    fontsize(size) { return createHTML(this, "font", "size", size, "fontsize"); },
    italics() { return createHTML(this, "i", "", "", "italics"); },
    link(url) { return createHTML(this, "a", "href", url, "link"); },
    small() { return createHTML(this, "small", "", "", "small"); },
    strike() { return createHTML(this, "strike", "", "", "strike"); },
    sub() { return createHTML(this, "sub", "", "", "sub"); },
    sup() { return createHTML(this, "sup", "", "", "sup"); }
  };
  var htmlNames = ["anchor", "big", "blink", "bold", "fixed", "fontcolor", "fontsize", "italics", "link", "small",
    "strike", "sub", "sup"];
  for (k = 0; k < htmlNames.length; k++) def(SP, htmlNames[k], html[htmlNames[k]]);
  def(SP, "trimLeft", SP.trimStart);
  def(SP, "trimRight", SP.trimEnd);
  return {
    construct: regExpConstruct, test: methods.test, match: strMethods.match, matchAll: strMethods.matchAll,
    replace: strMethods.replace, replaceAll: strMethods.replaceAll, search: strMethods.search, split: strMethods.split
  };
})

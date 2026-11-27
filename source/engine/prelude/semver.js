// goccia:semver for the CLI host: node-semver's main export over Goccia.Semver (SemVer 2.0.0), as
// Goccia.Builtins.Semver binds it. The parsing and comparing are the core's, reached through Goccia.HostAPI's
// n.semver(op, ...) dispatcher, which answers in JSON (a parse error throws: a TypeError or an Error, as the core
// raises it). Called once per realm with the global object, the host's natives (n) and the preludes' shared
// internals; registers the module.
(function (G, n, shared) {
  'use strict';
  const isObject = (v) => v !== null && (typeof v === 'object' || typeof v === 'function');
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not reach into the module
  const { TypeError, String } = G;
  const JSONParse = JSON.parse, JSONStringify = JSON.stringify, ArrayIsArray = Array.isArray, ObjectCreate = Object.create;
  const ReflectApply = Reflect.apply, defineProperty = Object.defineProperty, semverOp = n.semver;
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { value: v, writable: true, enumerable: true, configurable: true }); };
  // the dispatcher's answer for op and the arguments after it (spreading them would look up the array iterator)
  const call = function (op) { return JSONParse(ReflectApply(semverOp, n, arguments)); };

  // options: a boolean (loose) or an object; as a bit set for the dispatcher
  function optionsOf(v) {
    const o = { loose: false, includePrerelease: false, rtl: false };
    if (v === undefined || v === null) return o;
    if (!isObject(v)) { if (v) o.loose = true; return o; }
    if (v.loose !== undefined) o.loose = !!v.loose;
    if (v.includePrerelease !== undefined) o.includePrerelease = !!v.includePrerelease;
    if (v.rtl !== undefined) o.rtl = !!v.rtl;
    return o;
  }
  const bits = (o) => (o.loose ? 1 : 0) | (o.includePrerelease ? 2 : 0) | (o.rtl ? 4 : 0);
  const optionsObject = (o) => ({ loose: o.loose, includePrerelease: o.includePrerelease, rtl: o.rtl });
  function str(args, i, name) {
    if (args.length <= i) throw new TypeError(`${name} requires a string argument`);
    return String(args[i]);
  }
  const optsAt = (args, i) => (args.length > i ? optionsOf(args[i]) : optionsOf(undefined));
  function list(v) {
    if (!ArrayIsArray(v)) throw new TypeError('Expected an array of versions');
    const r = [];
    for (let i = 0; i < v.length; i++) append(r, String(v[i]));
    return JSONStringify(r);
  }

  // ---------------- the classes ----------------
  function writeSemver(target, r) {
    target.options = optionsObject(r.options);
    target.loose = r.options.loose;
    target.includePrerelease = r.options.includePrerelease;
    target.raw = r.raw;
    target.version = r.version;
    target.major = r.major;
    target.minor = r.minor;
    target.patch = r.patch;
    target.prerelease = r.prerelease;
    target.build = r.build;
    return target;
  }
  const semverObject = (r) => writeSemver(ObjectCreate(SemVer.prototype), r);
  function comparatorObject(c) {
    const o = ObjectCreate(Comparator.prototype);
    o.options = optionsObject(c.options);
    o.loose = c.options.loose;
    o.operator = c.operator;
    o.semver = c.any ? null : semverObject(c.semver);
    o.value = c.value;
    return o;
  }
  function rangeObject(r) {
    const o = ObjectCreate(Range.prototype);
    o.options = optionsObject(r.options);
    o.loose = r.options.loose;
    o.includePrerelease = r.options.includePrerelease;
    o.raw = r.raw;
    o.range = r.range;
    o.set = [];
    for (let i = 0; i < r.set.length; i++) {
      const cs = r.set[i], row = [];
      for (let j = 0; j < cs.length; j++) append(row, comparatorObject(cs[j]));
      append(o.set, row);
    }
    return o;
  }
  // a version, comparator or range argument: one of these objects (its raw text and options), else text
  function readSemver(v, opts) {
    if (isObject(v)) {
      const text = v.raw !== undefined ? v.raw : v.version;
      return call('semver', String(text), bits(optionsOf(v.options)));
    }
    return call('semver', String(v), bits(opts));
  }
  function readComparator(v, opts) {
    if (isObject(v)) return { value: v.value === undefined ? '' : String(v.value), opts: optionsOf(v.options) };
    return { value: String(v), opts };
  }
  function readRange(v, opts) {
    if (isObject(v)) {
      const text = v.raw !== undefined ? v.raw : v.range;
      return call('range', String(text), bits(optionsOf(v.options)));
    }
    return call('range', String(v), bits(opts));
  }

  function SemVer(version, options) {
    return semverObject(call('semver', str(arguments, 0, 'SemVer'), bits(optsAt(arguments, 1))));
  }
  function Comparator(comparator, options) {
    return comparatorObject(call('comparator', str(arguments, 0, 'Comparator'), bits(optsAt(arguments, 1))));
  }
  function Range(range, options) {
    return rangeObject(call('range', str(arguments, 0, 'Range'), bits(optsAt(arguments, 1))));
  }
  const def = (proto, name, fn, length) => {
    Object.defineProperty(fn, 'name', { value: name, configurable: true });
    Object.defineProperty(fn, 'length', { value: length, configurable: true });
    Object.defineProperty(proto, name, { value: fn, writable: true, enumerable: false, configurable: true });
  };
  def(SemVer.prototype, 'toString', function () { return readSemver(this, optionsOf()).version; }, 0);
  def(SemVer.prototype, 'format', function () { return readSemver(this, optionsOf()).version; }, 0);
  def(SemVer.prototype, 'compare', function (other) {
    const a = readSemver(this, optionsOf()), b = readSemver(other, a.options);
    return call('compare', a.version, b.version, bits(a.options));
  }, 1);
  def(SemVer.prototype, 'compareMain', function (other) {
    const a = readSemver(this, optionsOf()), b = readSemver(other, a.options);
    const parts = ['major', 'minor', 'patch'];
    for (let i = 0; i < parts.length; i++) { const k = parts[i]; if (a[k] < b[k]) return -1; if (a[k] > b[k]) return 1; }
    return 0;
  }, 1);
  def(SemVer.prototype, 'comparePre', function (other) {
    const a = readSemver(this, optionsOf()), b = readSemver(other, a.options);
    // SemVer 2.0.0 11.3-11.4: a release outranks its prereleases; identifiers compare in order, numeric below
    // alphanumeric, a shorter prefix below
    const p = a.prerelease, q = b.prerelease;
    if (p.length > 0 && q.length === 0) return -1;
    if (p.length === 0 && q.length > 0) return 1;
    for (let i = 0; ; i++) {
      if (i >= p.length && i >= q.length) return 0;
      if (i >= q.length) return 1;
      if (i >= p.length) return -1;
      const x = p[i], y = q[i];
      if (x === y) continue;
      const xn = typeof x === 'number', yn = typeof y === 'number';
      if (xn && !yn) return -1;
      if (!xn && yn) return 1;
      return x < y ? -1 : 1;
    }
  }, 1);
  def(SemVer.prototype, 'compareBuild', function (other) {
    const a = readSemver(this, optionsOf()), b = readSemver(other, a.options);
    return call('compareBuild', a.version, b.version, bits(a.options));
  }, 1);
  def(SemVer.prototype, 'inc', function (release, identifier) {
    const a = readSemver(this, optionsOf());
    const r = call('incStrict', a.version, String(release), arguments.length > 1 ? String(identifier) : '', bits(a.options));
    writeSemver(this, call('semver', r, bits(a.options)));
    return this;
  }, 1);
  def(Comparator.prototype, 'toString', function () { return readComparator(this, optionsOf()).value; }, 0);
  def(Comparator.prototype, 'test', function (version) {
    const c = readComparator(this, optionsOf());
    return call('comparatorTest', c.value, bits(c.opts), isObject(version) ? String(version.raw !== undefined ? version.raw : version.version) : String(version));
  }, 1);
  def(Comparator.prototype, 'intersects', function (other, options) {
    const c = readComparator(this, optionsOf());
    const o = optsAt(arguments, 1);
    const d = readComparator(other, o);
    return call('comparatorIntersects', c.value, bits(c.opts), d.value, bits(d.opts), bits(o));
  }, 1);
  def(Range.prototype, 'toString', function () { return readRange(this, optionsOf()).range; }, 0);
  def(Range.prototype, 'format', function () { return readRange(this, optionsOf()).range; }, 0);
  def(Range.prototype, 'test', function (version) {
    const r = readRange(this, optionsOf());
    return call('satisfies', String(version), r.raw, bits(r.options));
  }, 1);
  def(Range.prototype, 'intersects', function (other, options) {
    const r = readRange(this, optionsOf());
    const o = optsAt(arguments, 1);
    const s = readRange(other, o);
    return call('intersects', r.raw, s.raw, bits(o));
  }, 1);

  // ---------------- the functions ----------------
  const nullable = (v) => (v === '' ? null : v);
  const fns = {
    valid(v, o) { return nullable(call('valid', str(arguments, 0, 'valid'), bits(optsAt(arguments, 1)))); },
    clean(v, o) { return nullable(call('clean', str(arguments, 0, 'clean'), bits(optsAt(arguments, 1)))); },
    parse(v, o) { const r = call('parse', str(arguments, 0, 'parse'), bits(optsAt(arguments, 1))); return r === null ? null : semverObject(r); },
    inc(v, release) {
      const version = str(arguments, 0, 'inc'), rel = str(arguments, 1, 'inc');
      let identifier = '', opts = optionsOf();
      if (arguments.length > 2) { if (isObject(arguments[2])) opts = optionsOf(arguments[2]); else identifier = String(arguments[2]); }
      if (identifier === '' && arguments.length > 3) identifier = String(arguments[3]);
      return call('inc', version, rel, identifier, bits(opts));
    },
    prerelease(v, o) { return call('prerelease', str(arguments, 0, 'prerelease'), bits(optsAt(arguments, 1))); },
    major(v, o) { return call('major', str(arguments, 0, 'major'), bits(optsAt(arguments, 1))); },
    minor(v, o) { return call('minor', str(arguments, 0, 'minor'), bits(optsAt(arguments, 1))); },
    patch(v, o) { return call('patch', str(arguments, 0, 'patch'), bits(optsAt(arguments, 1))); },
    intersects(a, b) { return call('intersects', str(arguments, 0, 'intersects'), str(arguments, 1, 'intersects'), bits(optsAt(arguments, 2))); },
    gt(a, b) { return call('compare', str(arguments, 0, 'gt'), str(arguments, 1, 'gt'), bits(optsAt(arguments, 2))) > 0; },
    gte(a, b) { return call('compare', str(arguments, 0, 'gte'), str(arguments, 1, 'gte'), bits(optsAt(arguments, 2))) >= 0; },
    lt(a, b) { return call('compare', str(arguments, 0, 'lt'), str(arguments, 1, 'lt'), bits(optsAt(arguments, 2))) < 0; },
    lte(a, b) { return call('compare', str(arguments, 0, 'lte'), str(arguments, 1, 'lte'), bits(optsAt(arguments, 2))) <= 0; },
    eq(a, b) { return call('compare', str(arguments, 0, 'eq'), str(arguments, 1, 'eq'), bits(optsAt(arguments, 2))) === 0; },
    neq(a, b) { return call('compare', str(arguments, 0, 'neq'), str(arguments, 1, 'neq'), bits(optsAt(arguments, 2))) !== 0; },
    cmp(a, op, b) { return call('cmp', str(arguments, 0, 'cmp'), str(arguments, 1, 'cmp'), str(arguments, 2, 'cmp'), bits(optsAt(arguments, 3))); },
    compare(a, b) { return call('compare', str(arguments, 0, 'compare'), str(arguments, 1, 'compare'), bits(optsAt(arguments, 2))); },
    rcompare(a, b) { return call('compare', str(arguments, 1, 'rcompare'), str(arguments, 0, 'rcompare'), bits(optsAt(arguments, 2))); },
    compareBuild(a, b) { return call('compareBuild', str(arguments, 0, 'compareBuild'), str(arguments, 1, 'compareBuild'), bits(optsAt(arguments, 2))); },
    compareLoose(a, b) { return call('compareLoose', str(arguments, 0, 'compareLoose'), str(arguments, 1, 'compareLoose')); },
    diff(a, b) { return nullable(call('diff', str(arguments, 0, 'diff'), str(arguments, 1, 'diff'), bits(optsAt(arguments, 2)))); },
    sort(list_) { return call('sort', list(list_), bits(optsAt(arguments, 1)), 1); },
    rsort(list_) { return call('sort', list(list_), bits(optsAt(arguments, 1)), -1); },
    validRange(r, o) { return nullable(call('validRange', str(arguments, 0, 'validRange'), bits(optsAt(arguments, 1)))); },
    satisfies(v, r) { return call('satisfies', str(arguments, 0, 'satisfies'), str(arguments, 1, 'satisfies'), bits(optsAt(arguments, 2))); },
    maxSatisfying(vs, r) { return nullable(call('maxSatisfying', list(vs), str(arguments, 1, 'maxSatisfying'), bits(optsAt(arguments, 2)))); },
    minSatisfying(vs, r) { return nullable(call('minSatisfying', list(vs), str(arguments, 1, 'minSatisfying'), bits(optsAt(arguments, 2)))); },
    minVersion(r, o) { const v = call('minVersion', str(arguments, 0, 'minVersion'), bits(optsAt(arguments, 1))); return v === null ? null : semverObject(v); },
    gtr(v, r) { return call('gtr', str(arguments, 0, 'gtr'), str(arguments, 1, 'gtr'), bits(optsAt(arguments, 2))); },
    ltr(v, r) { return call('ltr', str(arguments, 0, 'ltr'), str(arguments, 1, 'ltr'), bits(optsAt(arguments, 2))); },
    outside(v, r, hilo) { return call('outside', str(arguments, 0, 'outside'), str(arguments, 1, 'outside'), str(arguments, 2, 'outside'), bits(optsAt(arguments, 3))); },
    simplifyRange(vs, r) { return call('simplifyRange', list(vs), str(arguments, 1, 'simplifyRange'), bits(optsAt(arguments, 2))); },
    subset(a, b) { return call('subset', str(arguments, 0, 'subset'), str(arguments, 1, 'subset'), bits(optsAt(arguments, 2))); },
    toComparators(r, o) { return call('toComparators', str(arguments, 0, 'toComparators'), bits(optsAt(arguments, 1))); },
    coerce(v, o) { const r = call('coerce', str(arguments, 0, 'coerce'), bits(optsAt(arguments, 1))); return r === null ? null : semverObject(r); }
  };
  const lengths = { inc: 2, intersects: 2, gt: 2, gte: 2, lt: 2, lte: 2, eq: 2, neq: 2, cmp: 3, compare: 2, rcompare: 2,
    compareBuild: 2, compareLoose: 2, diff: 2, satisfies: 2, maxSatisfying: 2, minSatisfying: 2, gtr: 2, ltr: 2, outside: 3,
    simplifyRange: 2, subset: 2 };
  for (const k of Object.keys(fns)) Object.defineProperty(fns[k], 'length', { value: lengths[k] !== undefined ? lengths[k] : 1, configurable: true });

  const pick = (names) => { const o = {}; for (const k of names) o[k] = fns[k]; return o; };
  const ns = {
    SemVer, Comparator, Range,
    SEMVER_SPEC_VERSION: '2.0.0',
    RELEASE_TYPES: ['major', 'premajor', 'minor', 'preminor', 'patch', 'prepatch', 'prerelease'],
    ...fns,
    classes: { Comparator, Range, SemVer, comparator: Comparator, range: Range, semver: SemVer },
    functions: pick(['clean', 'cmp', 'coerce', 'compare', 'compareBuild', 'compareLoose', 'diff', 'eq', 'gt', 'gte', 'inc',
      'lt', 'lte', 'major', 'minor', 'neq', 'parse', 'patch', 'prerelease', 'rcompare', 'rsort', 'satisfies', 'sort', 'valid']),
    ranges: pick(['gtr', 'intersects', 'ltr', 'maxSatisfying', 'minSatisfying', 'minVersion', 'outside', 'simplifyRange',
      'subset', 'toComparators', 'validRange'])
  };
  shared.hostModules['goccia:semver'] = () => ns;
})

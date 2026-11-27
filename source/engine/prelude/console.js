// console for the CLI host: every method writes one line through n.writeLine with the method's name, which picks the
// stream (warn, error, info and debug behind a "Warning: ", "Error: ", "Info: " or "Debug: " prefix), indented two
// spaces per open group. Values render in util.inspect's shapes to the depth n.inspectDepth() gives (5 unless
// --inspect-depth sets it): strings quoted when nested, -0 kept, functions, classes and errors in brackets. Called
// once per realm with the global object, the host's natives (n) and the preludes' shared internals; replaces the
// conformance host's console and sets shared.inspect.
(function (G, n, shared) {
  'use strict';
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const getPrototypeOf = Object.getPrototypeOf;
  const apply = Reflect.apply;
  const isArray = Array.isArray;
  const defineProperty = Object.defineProperty, ObjectKeys = Object.keys, ObjectIs = Object.is;
  const hidden = (o, k, v) => defineProperty(o, k, { __proto__: null, value: v, writable: true, enumerable: false, configurable: true });
  // what the functions below call, captured before any script runs: a script that replaces a prototype method or a
  // global does not change how console renders
  const StringCtor = String, MathRound = Math.round, ErrorIsError = Error.isError, SymbolToStringTag = Symbol.toStringTag;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeSplit = uncurryThis(String.prototype.split), StringPrototypeIncludes = uncurryThis(String.prototype.includes);
  const StringPrototypeStartsWith = uncurryThis(String.prototype.startsWith), StringPrototypeRepeat = uncurryThis(String.prototype.repeat);
  const ArrayPrototypeJoin = uncurryThis(Array.prototype.join), ArrayPrototypeIncludes = uncurryThis(Array.prototype.includes);
  const ObjectPrototypeHasOwnProperty = uncurryThis(Object.prototype.hasOwnProperty);
  const ObjectPrototypePropertyIsEnumerable = uncurryThis(Object.prototype.propertyIsEnumerable);
  const MapPrototypeGet = uncurryThis(Map.prototype.get), MapPrototypeSet = uncurryThis(Map.prototype.set);
  const MapPrototypeHas = uncurryThis(Map.prototype.has), MapPrototypeDelete = uncurryThis(Map.prototype.delete);
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true }); };
  // a new list of f applied to each element (Array.prototype.map would look up the constructor and Symbol.species)
  const mapList = (list, f) => { const r = []; for (let i = 0; i < list.length; i++) append(r, f(list[i])); return r; };
  // the inspection depth, read from the host when a value's rendering starts
  let DEPTH = 5;
  const brand = (fn) => (v) => { try { apply(fn, v, []); return true; } catch { return false; } };
  const isMap = brand(getOwnPropertyDescriptor(Map.prototype, 'size').get);
  const isSet = brand(getOwnPropertyDescriptor(Set.prototype, 'size').get);
  const mapForEach = Map.prototype.forEach, setForEach = Set.prototype.forEach;
  const fnToString = Function.prototype.toString;
  const symToString = Symbol.prototype.toString;

  function join(prefix, open, close, entries, multi) {
    let s = prefix !== '' ? prefix + ' ' : '';
    s += open;
    if (multi) {
      for (let i = 0; i < entries.length; i++) s += (i > 0 ? ',' : '') + '\n  ' + ArrayPrototypeJoin(StringPrototypeSplit(entries[i], '\n'), '\n  ');
      s += '\n';
    } else {
      for (let i = 0; i < entries.length; i++) s += (i > 0 ? ', ' : ' ') + entries[i];
      s += ' ';
    }
    return s + close;
  }
  const quote = (s) => (!StringPrototypeIncludes(s, "'") ? "'" + s + "'" : !StringPrototypeIncludes(s, '"') ? '"' + s + '"' : '`' + s + '`');
  const stringProp = (o, k) => { const v = o[k]; return typeof v === 'string' ? v : ''; };

  // { text, multi }
  function properties(o, depth, consoleForm, skip) {
    const out = [];
    let multi = false;
    const keys = ObjectKeys(o);
    for (let i = 0; i < keys.length; i++) {
      const k = keys[i];
      if (ArrayPrototypeIncludes(skip, k)) continue;
      const r = render(o[k], true, depth + 1, consoleForm);
      multi = multi || r.multi;
      append(out, k + ': ' + r.text);
    }
    return { entries: out, multi };
  }
  function functionBase(f) {
    let src = '';
    try { src = apply(fnToString, f, []); } catch {}
    if (StringPrototypeStartsWith(src, 'class')) {
      let name = getOwnPropertyDescriptor(f, 'name') !== undefined ? stringProp(f, 'name') : '';
      if (name === '') name = '(anonymous)';
      const parent = getPrototypeOf(f);
      const superName = parent !== null && typeof parent === 'function' ? stringProp(parent, 'name') : '';
      return '[class ' + name + (superName !== '' ? ' extends ' + superName : '') + ']';
    }
    let kind = 'Function';
    const tag = f[SymbolToStringTag];
    if (tag === 'AsyncFunction' || tag === 'GeneratorFunction' || tag === 'AsyncGeneratorFunction') kind = tag;
    const name = stringProp(f, 'name');
    return name === '' ? '[' + kind + ' (anonymous)]' : '[' + kind + ': ' + name + ']';
  }
  function renderError(e, depth, consoleForm) {
    let stack = stringProp(e, 'stack');
    if (stack === '') {
      const name = e.name === undefined ? 'Error' : stringProp(e, 'name');
      const message = stringProp(e, 'message');
      stack = name === '' ? message : message === '' ? name : name + ': ' + message;
    }
    if (!StringPrototypeIncludes(stack, '\n    at')) stack = '[' + stack + ']';
    let multi = StringPrototypeIncludes(stack, '\n');
    if (depth >= DEPTH) return { text: stack, multi };
    const skip = [];
    const own = ['name', 'message', 'stack'];
    for (let i = 0; i < own.length; i++) {
      const k = own[i];
      if (ObjectPrototypeHasOwnProperty(e, k)) {
        const v = stringProp(e, k);
        if (v !== '' && StringPrototypeIncludes(stack, v)) append(skip, k);
      }
    }
    const p = properties(e, depth, consoleForm, skip);
    multi = multi || p.multi;
    if ('cause' in e && !ObjectPrototypePropertyIsEnumerable(e, 'cause')) {
      const r = render(e.cause, true, depth + 1, consoleForm);
      multi = multi || r.multi;
      append(p.entries, '[cause]: ' + r.text);
    }
    if (isArray(e.errors) && !ObjectPrototypePropertyIsEnumerable(e, 'errors')) {
      const r = render(e.errors, true, depth + 1, consoleForm);
      multi = multi || r.multi;
      append(p.entries, '[errors]: ' + r.text);
    }
    return { text: p.entries.length > 0 ? join(stack, '{', '}', p.entries, multi) : stack, multi };
  }
  function render(v, nested, depth, consoleForm) {
    const t = typeof v;
    if (t === 'symbol') return { text: apply(symToString, v, []), multi: false };
    if (t === 'string') return { text: nested ? quote(v) : v, multi: false };
    if (t === 'number') return { text: consoleForm && ObjectIs(v, -0) ? '-0' : StringCtor(v), multi: false };
    if (t === 'bigint') return { text: StringCtor(v) + 'n', multi: false };
    if (v === null || (t !== 'object' && t !== 'function')) return { text: StringCtor(v), multi: false };
    if (isArray(v)) {
      if (depth >= DEPTH) return { text: '[Array]', multi: false };
      if (v.length === 0) return { text: '[]', multi: false };
      let multi = false;
      const entries = [];
      for (let i = 0; i < v.length; i++) { const r = render(v[i], true, depth + 1, consoleForm); multi = multi || r.multi; append(entries, r.text); }
      return { text: join('', '[', ']', entries, multi), multi };
    }
    if (isSet(v)) {
      if (depth >= DEPTH) return { text: '[Set]', multi: false };
      const items = [];
      apply(setForEach, v, [(x) => append(items, x)]);
      if (items.length === 0) return { text: 'Set(0) {}', multi: false };
      let multi = false;
      const entries = mapList(items, (x) => { const r = render(x, true, depth + 1, consoleForm); multi = multi || r.multi; return r.text; });
      return { text: join('Set(' + items.length + ')', '{', '}', entries, multi), multi };
    }
    if (isMap(v)) {
      if (depth >= DEPTH) return { text: '[Map]', multi: false };
      const pairs = [];
      apply(mapForEach, v, [(val, key) => append(pairs, [key, val])]);
      if (pairs.length === 0) return { text: 'Map(0) {}', multi: false };
      let multi = false;
      const entries = mapList(pairs, (pair) => {
        const key = pair[0], val = pair[1];
        const a = render(key, true, depth + 1, consoleForm), b = render(val, true, depth + 1, consoleForm);
        multi = multi || a.multi || b.multi;
        return a.text + ' => ' + b.text;
      });
      return { text: join('Map(' + pairs.length + ')', '{', '}', entries, multi), multi };
    }
    if (consoleForm && t === 'function') {
      const base = functionBase(v);
      if (depth >= DEPTH) return { text: base, multi: false };
      const p = properties(v, depth, consoleForm, []);
      return { text: p.entries.length > 0 ? join(base, '{', '}', p.entries, p.multi) : base, multi: p.multi };
    }
    if (consoleForm && ErrorIsError(v)) return renderError(v, depth, consoleForm);
    if (depth >= DEPTH) return { text: '[Object]', multi: false };
    const p = properties(v, depth, consoleForm, []);
    return { text: p.entries.length === 0 ? '{}' : join('', '{', '}', p.entries, p.multi), multi: p.multi };
  }
  const inspect = (v) => { DEPTH = n.inspectDepth(); return render(v, false, 0, true).text; };
  shared.inspect = inspect;
  shared.display = (v) => { DEPTH = n.inspectDepth(); return render(v, false, 0, false).text; };

  // ---------------- console ----------------
  let groupDepth = 0;
  const counters = new Map(), timers = new Map();
  const prefix = () => StringPrototypeRepeat('  ', groupDepth);
  // each line with the console method that wrote it: a host that captures output (GocciaRunner's JSON report)
  // splits the streams by method
  function emit(method, line) {
    n.writeLine(groupDepth > 0 ? ArrayPrototypeJoin(StringPrototypeSplit(line, '\n'), '\n' + prefix()) : line, method);
  }
  const format = (args) => ArrayPrototypeJoin(mapList(args, inspect), ' ');
  const labelOf = (args) => (args.length >= 1 && args[0] !== undefined ? StringCtor(args[0]) : 'default');
  const elapsed = (start) => {
    const ms = n.hrNow() - start;
    return StringCtor(MathRound(ms * 1000) / 1000);
  };
  const console = {
    log(...args) { emit('log', prefix() + format(args)); },
    warn(...args) { emit('warn', prefix() + 'Warning: ' + format(args)); },
    error(...args) { emit('error', prefix() + 'Error: ' + format(args)); },
    info(...args) { emit('info', prefix() + 'Info: ' + format(args)); },
    debug(...args) { emit('debug', prefix() + 'Debug: ' + format(args)); },
    dir(...args) { if (args.length >= 1) emit('dir', prefix() + inspect(args[0])); },
    assert(...args) {
      if (args.length === 0) { emit('assert', prefix() + 'Assertion failed'); return; }
      if (!args[0]) { let rest = ''; for (let i = 1; i < args.length; i++) rest += ' ' + inspect(args[i]); emit('assert', prefix() + 'Assertion failed:' + rest); }
    },
    count(...args) {
      const label = labelOf(args);
      const c = (MapPrototypeGet(counters, label) || 0) + 1;
      MapPrototypeSet(counters, label, c);
      emit('count', prefix() + label + ': ' + c);
    },
    countReset(...args) { MapPrototypeSet(counters, labelOf(args), 0); },
    time(...args) { MapPrototypeSet(timers, labelOf(args), n.hrNow()); },
    timeEnd(...args) {
      const label = labelOf(args);
      if (MapPrototypeHas(timers, label)) { emit('timeEnd', prefix() + label + ': ' + elapsed(MapPrototypeGet(timers, label)) + 'ms'); MapPrototypeDelete(timers, label); }
      else emit('timeEnd', prefix() + "Timer '" + label + "' does not exist");
    },
    timeLog(...args) {
      const label = labelOf(args);
      if (MapPrototypeHas(timers, label)) emit('timeLog', prefix() + label + ': ' + elapsed(MapPrototypeGet(timers, label)) + 'ms');
      else emit('timeLog', prefix() + "Timer '" + label + "' does not exist");
    },
    clear() {},
    group(...args) { if (args.length > 0) emit('group', prefix() + format(args)); groupDepth++; },
    groupCollapsed(...args) { if (args.length > 0) emit('groupCollapsed', prefix() + format(args)); groupDepth++; },
    groupEnd() { if (groupDepth > 0) groupDepth--; },
    trace(...args) { emit('trace', prefix() + 'Trace' + (args.length > 0 ? ': ' + format(args) : '')); },
    table(...args) { if (args.length >= 1) emit('table', prefix() + inspect(args[0])); }
  };
  hidden(G, 'console', console);
})

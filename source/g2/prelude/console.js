// console for the CLI host on G2, as Goccia.Builtins.Console has it: every method writes one line to standard output
// (warn, error, info and debug behind a "Warning: ", "Error: ", "Info: " or "Debug: " prefix), indented two spaces
// per open group, its values rendered by Goccia.Values.Formatting's console form (util.inspect's shapes to a depth of
// 5: strings quoted when nested, -0 kept, functions, classes and errors in brackets). Called once per realm with the
// global object, the host's natives (n) and the preludes' shared internals; replaces the conformance host's console
// and sets shared.inspect.
(function (G, n, shared) {
  'use strict';
  const getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  const getPrototypeOf = Object.getPrototypeOf;
  const apply = Reflect.apply;
  const isArray = Array.isArray;
  const hidden = (o, k, v) => Object.defineProperty(o, k, { value: v, writable: true, enumerable: false, configurable: true });
  const DEPTH = 5;
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
      for (let i = 0; i < entries.length; i++) s += (i > 0 ? ',' : '') + '\n  ' + entries[i].split('\n').join('\n  ');
      s += '\n';
    } else {
      for (let i = 0; i < entries.length; i++) s += (i > 0 ? ', ' : ' ') + entries[i];
      s += ' ';
    }
    return s + close;
  }
  const quote = (s) => (!s.includes("'") ? "'" + s + "'" : !s.includes('"') ? '"' + s + '"' : '`' + s + '`');
  const stringProp = (o, k) => { const v = o[k]; return typeof v === 'string' ? v : ''; };

  // { text, multi }
  function properties(o, depth, consoleForm, skip) {
    const out = [];
    let multi = false;
    for (const k of Object.keys(o)) {
      if (skip.includes(k)) continue;
      const r = render(o[k], true, depth + 1, consoleForm);
      multi = multi || r.multi;
      out.push(k + ': ' + r.text);
    }
    return { entries: out, multi };
  }
  function functionBase(f) {
    let src = '';
    try { src = apply(fnToString, f, []); } catch {}
    if (src.startsWith('class')) {
      let name = getOwnPropertyDescriptor(f, 'name') !== undefined ? stringProp(f, 'name') : '';
      if (name === '') name = '(anonymous)';
      const parent = getPrototypeOf(f);
      const superName = parent !== null && typeof parent === 'function' ? stringProp(parent, 'name') : '';
      return '[class ' + name + (superName !== '' ? ' extends ' + superName : '') + ']';
    }
    let kind = 'Function';
    const tag = f[Symbol.toStringTag];
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
    if (!stack.includes('\n    at')) stack = '[' + stack + ']';
    let multi = stack.includes('\n');
    if (depth >= DEPTH) return { text: stack, multi };
    const skip = [];
    for (const k of ['name', 'message', 'stack']) {
      if (Object.prototype.hasOwnProperty.call(e, k)) {
        const v = stringProp(e, k);
        if (v !== '' && stack.includes(v)) skip.push(k);
      }
    }
    const p = properties(e, depth, consoleForm, skip);
    multi = multi || p.multi;
    if ('cause' in e && !Object.prototype.propertyIsEnumerable.call(e, 'cause')) {
      const r = render(e.cause, true, depth + 1, consoleForm);
      multi = multi || r.multi;
      p.entries.push('[cause]: ' + r.text);
    }
    if (isArray(e.errors) && !Object.prototype.propertyIsEnumerable.call(e, 'errors')) {
      const r = render(e.errors, true, depth + 1, consoleForm);
      multi = multi || r.multi;
      p.entries.push('[errors]: ' + r.text);
    }
    return { text: p.entries.length > 0 ? join(stack, '{', '}', p.entries, multi) : stack, multi };
  }
  function render(v, nested, depth, consoleForm) {
    const t = typeof v;
    if (t === 'symbol') return { text: apply(symToString, v, []), multi: false };
    if (t === 'string') return { text: nested ? quote(v) : v, multi: false };
    if (t === 'number') return { text: consoleForm && Object.is(v, -0) ? '-0' : String(v), multi: false };
    if (t === 'bigint') return { text: String(v) + 'n', multi: false };
    if (v === null || (t !== 'object' && t !== 'function')) return { text: String(v), multi: false };
    if (isArray(v)) {
      if (depth >= DEPTH) return { text: '[Array]', multi: false };
      if (v.length === 0) return { text: '[]', multi: false };
      let multi = false;
      const entries = [];
      for (let i = 0; i < v.length; i++) { const r = render(v[i], true, depth + 1, consoleForm); multi = multi || r.multi; entries.push(r.text); }
      return { text: join('', '[', ']', entries, multi), multi };
    }
    if (isSet(v)) {
      if (depth >= DEPTH) return { text: '[Set]', multi: false };
      const items = [];
      apply(setForEach, v, [(x) => items.push(x)]);
      if (items.length === 0) return { text: 'Set(0) {}', multi: false };
      let multi = false;
      const entries = items.map((x) => { const r = render(x, true, depth + 1, consoleForm); multi = multi || r.multi; return r.text; });
      return { text: join('Set(' + items.length + ')', '{', '}', entries, multi), multi };
    }
    if (isMap(v)) {
      if (depth >= DEPTH) return { text: '[Map]', multi: false };
      const pairs = [];
      apply(mapForEach, v, [(val, key) => pairs.push([key, val])]);
      if (pairs.length === 0) return { text: 'Map(0) {}', multi: false };
      let multi = false;
      const entries = pairs.map(([key, val]) => {
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
    if (consoleForm && Error.isError(v)) return renderError(v, depth, consoleForm);
    if (depth >= DEPTH) return { text: '[Object]', multi: false };
    const p = properties(v, depth, consoleForm, []);
    return { text: p.entries.length === 0 ? '{}' : join('', '{', '}', p.entries, p.multi), multi: p.multi };
  }
  const inspect = (v) => render(v, false, 0, true).text;
  shared.inspect = inspect;
  shared.display = (v) => render(v, false, 0, false).text;

  // ---------------- console ----------------
  let groupDepth = 0;
  const counters = new Map(), timers = new Map();
  const prefix = () => '  '.repeat(groupDepth);
  function emit(line) {
    n.writeLine(groupDepth > 0 ? line.split('\n').join('\n' + prefix()) : line);
  }
  const format = (args) => args.map(inspect).join(' ');
  const labelOf = (args) => (args.length >= 1 && args[0] !== undefined ? String(args[0]) : 'default');
  const elapsed = (start) => {
    const ms = n.hrNow() - start;
    return (Math.round(ms * 1000) / 1000).toString();
  };
  const console = {
    log(...args) { emit(prefix() + format(args)); },
    warn(...args) { emit(prefix() + 'Warning: ' + format(args)); },
    error(...args) { emit(prefix() + 'Error: ' + format(args)); },
    info(...args) { emit(prefix() + 'Info: ' + format(args)); },
    debug(...args) { emit(prefix() + 'Debug: ' + format(args)); },
    dir(...args) { if (args.length >= 1) emit(prefix() + inspect(args[0])); },
    assert(...args) {
      if (args.length === 0) { emit(prefix() + 'Assertion failed'); return; }
      if (!args[0]) emit(prefix() + 'Assertion failed:' + args.slice(1).map((a) => ' ' + inspect(a)).join(''));
    },
    count(...args) {
      const label = labelOf(args);
      const c = (counters.get(label) || 0) + 1;
      counters.set(label, c);
      emit(prefix() + label + ': ' + c);
    },
    countReset(...args) { counters.set(labelOf(args), 0); },
    time(...args) { timers.set(labelOf(args), n.hrNow()); },
    timeEnd(...args) {
      const label = labelOf(args);
      if (timers.has(label)) { emit(prefix() + label + ': ' + elapsed(timers.get(label)) + 'ms'); timers.delete(label); }
      else emit(prefix() + "Timer '" + label + "' does not exist");
    },
    timeLog(...args) {
      const label = labelOf(args);
      if (timers.has(label)) emit(prefix() + label + ': ' + elapsed(timers.get(label)) + 'ms');
      else emit(prefix() + "Timer '" + label + "' does not exist");
    },
    clear() {},
    group(...args) { if (args.length > 0) emit(prefix() + format(args)); groupDepth++; },
    groupCollapsed(...args) { if (args.length > 0) emit(prefix() + format(args)); groupDepth++; },
    groupEnd() { if (groupDepth > 0) groupDepth--; },
    trace(...args) { emit(prefix() + 'Trace' + (args.length > 0 ? ': ' + format(args) : '')); },
    table(...args) { if (args.length >= 1) emit(prefix() + inspect(args[0])); }
  };
  hidden(G, 'console', console);
})

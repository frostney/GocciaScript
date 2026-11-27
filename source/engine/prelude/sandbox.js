// GocciaRunner's sandbox mode (ADR 0122): the fs, goccia and goccia:search host modules over the thread's
// sandbox virtual file system, as GocciaRunner's sandbox extension (Goccia.RuntimeExtensions.Sandbox) has them.
// Goccia.Sandbox's natives (n.sb*) do the work; this validates the arguments with that extension's messages, defers
// the callback and promise forms to a microtask, and shapes the results. Installed only in sandbox mode, with the
// global object, the host's natives (n) and the preludes' shared internals.
(function (G, n, shared) {
  'use strict';
  const TypeErrorC = TypeError, PromiseC = Promise, DateC = Date, Uint8ArrayC = Uint8Array;
  const isArray = Array.isArray, defineProperty = Object.defineProperty, create = Object.create;
  const ownKeys = Reflect.ownKeys, apply = Reflect.apply, getOwnDescriptor = Object.getOwnPropertyDescriptor;
  const queue = G.queueMicrotask, parseJSON = JSON.parse, StringC = String;
  const lower = String.prototype.toLowerCase;
  const statsMap = new WeakMap(), statsGet = WeakMap.prototype.get, statsSet = WeakMap.prototype.set;
  const asyncIterator = Symbol.asyncIterator;
  const MathTrunc = Math.trunc, MathMin = Math.min, MathFloor = Math.floor;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const StringPrototypeCharCodeAt = uncurryThis(String.prototype.charCodeAt), StringPrototypeSlice = uncurryThis(String.prototype.slice);
  const StringPrototypeIndexOf = uncurryThis(String.prototype.indexOf), StringPrototypeSplit = uncurryThis(String.prototype.split);
  const ArrayPrototypeJoin = uncurryThis(Array.prototype.join);

  const missing = (v) => v === undefined || v === null;
  const isObject = (v) => (typeof v === 'object' && v !== null) || typeof v === 'function';
  const lowerCase = (s) => apply(lower, s, []);
  const push = (a, v) => defineProperty(a, a.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true });
  // a function as the extension's natives are: the name, the length, no prototype, not a constructor
  const named = (name, length, impl) => {
    // the arguments as own properties of an object without a prototype: reading one the call did not pass
    // never reaches a property Array.prototype holds
    const f = { [name](...args) {
      const a = { __proto__: null, length: args.length };
      for (let i = 0; i < args.length; i++) a[i] = args[i];
      return impl(a, this);
    } }[name];
    defineProperty(f, 'length', { __proto__: null, value: length, configurable: true });
    return f;
  };
  // Pascal's Trim: the code units up to a space off both ends
  const trim = (s) => {
    let a = 0, b = s.length;
    while (a < b && StringPrototypeCharCodeAt(s, a) <= 32) a++;
    while (b > a && StringPrototypeCharCodeAt(s, b - 1) <= 32) b--;
    return StringPrototypeSlice(s, a, b);
  };

  // ---- argument checks ----
  const requireStringArg = (v, method) => {
    if (missing(v)) throw new TypeErrorC(method + ' requires a path string');
    return StringC(v);
  };
  const requireAsyncPath = (v, method) => {
    if (typeof v !== 'string') throw new TypeErrorC(method + ' requires a path string');
    return v;
  };
  const requireCallback = (v, method) => {
    if (typeof v !== 'function') throw new TypeErrorC(method + ' requires a callback function');
    return v;
  };
  const validateCount = (args, max, method) => {
    if (args.length > max) throw new TypeErrorC(method + ' received unsupported arguments');
  };
  const validateNoOptions = (v, method) => {
    if (!missing(v)) throw new TypeErrorC(method + ' options are not supported');
  };
  const isUtf8 = (s) => { s = lowerCase(s); return s === 'utf8' || s === 'utf-8'; };
  const wantsText = (v) => {
    if (typeof v === 'string') return isUtf8(v);
    if (isObject(v)) {
      const encoding = v.encoding;
      if (typeof encoding === 'string') return isUtf8(encoding);
    }
    return false;
  };
  const parseReadOptions = (v, method) => {
    let text = false;
    if (missing(v)) return false;
    if (typeof v === 'string') {
      if (!isUtf8(v)) throw new TypeErrorC(method + ' only supports utf8 encoding');
      return true;
    }
    if (!isObject(v)) throw new TypeErrorC(method + ' options must be an encoding string or object');
    const encoding = v.encoding;
    if (!missing(encoding)) {
      if (typeof encoding !== 'string') throw new TypeErrorC(method + ' option "encoding" must be a string');
      if (!isUtf8(encoding)) throw new TypeErrorC(method + ' only supports utf8 encoding');
      text = true;
    }
    if (!missing(v.flag) || !missing(v.signal)) throw new TypeErrorC(method + ' received unsupported options');
    return text;
  };
  const optionRecursive = (v) => {
    if (typeof v === 'boolean') return v;
    if (isObject(v)) return v.recursive === true;
    return false;
  };
  const validateRecursiveOptions = (v, method) => {
    if (missing(v)) return false;
    if (!isObject(v)) throw new TypeErrorC(method + ' options must be an object');
    let recursive = false;
    const r = v.recursive;
    if (!missing(r)) {
      if (typeof r !== 'boolean') throw new TypeErrorC(method + ' option "recursive" must be a boolean');
      recursive = r;
    }
    if (!missing(v.mode)) throw new TypeErrorC(method + ' received unsupported options');
    const force = v.force;
    if (!missing(force)) {
      if (typeof force !== 'boolean') throw new TypeErrorC(method + ' option "force" must be a boolean');
      if (force) throw new TypeErrorC(method + ' received unsupported options');
    }
    return recursive;
  };
  const validateCopyMode = (v, method) => {
    if (missing(v)) return;
    if (typeof v !== 'number' || v !== 0) throw new TypeErrorC(method + ' only supports copy mode 0');
  };

  // ---- Stats ----
  const roundMs = (ms) => {
    const f = ms - MathTrunc(ms);
    let r = ms - f;
    if (f >= 0.5) r += 1;
    else if (f < -0.5) r -= 1;
    return r;
  };
  const statsOf = (receiver) => {
    const s = isObject(receiver) ? apply(statsGet, statsMap, [receiver]) : undefined;
    if (s === undefined) throw new TypeErrorC('Stats method called on incompatible receiver');
    return s;
  };
  const statsProto = create(Object.prototype);
  const dateGetter = (name, index) => {
    const get = getOwnDescriptor({ get [name]() { return new DateC(roundMs(statsOf(this)[index])); } }, name).get;
    defineProperty(statsProto, name, { __proto__: null, get, enumerable: true, configurable: true });
  };
  dateGetter('atime', 4);
  dateGetter('mtime', 5);
  dateGetter('ctime', 6);
  dateGetter('birthtime', 7);
  const statsMethod = (name, impl) => defineProperty(statsProto, name,
    { __proto__: null, value: named(name, 0, (args, self) => impl(statsOf(self))), writable: true, configurable: true });
  statsMethod('isFile', (s) => s[8]);
  statsMethod('isDirectory', (s) => s[9]);
  statsMethod('isSymbolicLink', () => false);
  // [path, name, type, size, atimeMs, mtimeMs, ctimeMs, birthtimeMs, isFile, isDirectory] (n.sbStat)
  const makeStats = (s) => {
    const stats = { __proto__: statsProto, path: s[0], name: s[1], type: s[2], size: s[3], atimeMs: s[4],
      mtimeMs: s[5], ctimeMs: s[6], birthtimeMs: s[7] };
    apply(statsSet, statsMap, [stats, s]);
    return stats;
  };

  // ---- the operations ----
  const readValue = (path, text) => {
    const r = n.sbRead(path, text);
    return text ? r : new Uint8ArrayC(r);
  };
  const ops = {
    readFile: (a) => readValue(a.path, a.text),
    writeFile: (a) => n.sbWrite(a.path, a.data, false),
    appendFile: (a) => n.sbWrite(a.path, a.data, true),
    mkdir: (a) => n.sbMkdir(a.path, a.recursive),
    readdir: (a) => n.sbReaddir(a.path),
    stat: (a) => makeStats(n.sbStat(a.path)),
    exists: (a) => n.sbExists(a.path),
    rm: (a) => n.sbRm(a.path, a.recursive),
    rename: (a) => n.sbMove(a.path, a.dest, false),
    copyFile: (a) => n.sbMove(a.path, a.dest, true),
    glob: (a) => n.sbGlob(a.method, a.patterns, a.excludes, a.cwd, true),
  };
  // how a callback hears of the outcome: (err), (null), (null, value), mkdir's (null[, path]), exists' (present)
  const ERROR_ONLY = 0, VALUE = 1, MKDIR = 2, EXISTS = 3;
  const callBack = (callback, args) => apply(callback, undefined, args);
  const enqueueCallback = (op, a, completion, callback) => {
    queue(() => {
      let value;
      try {
        value = ops[op](a);
      } catch (e) {
        callBack(callback, completion === EXISTS ? [false] : [e]);
        return;
      }
      if (completion === VALUE) callBack(callback, [null, value]);
      else if (completion === MKDIR) callBack(callback, a.recursive ? [null, value] : [null]);
      else if (completion === EXISTS) callBack(callback, [value]);
      else callBack(callback, [null]);
    });
  };
  // a promise settled by the operation in a microtask; an argument refused rejects it
  const promised = (prepare) => new PromiseC((resolve, reject) => {
    const prepared = prepare();
    const op = prepared[0], a = prepared[1];
    queue(() => {
      let value;
      try {
        value = ops[op](a);
      } catch (e) {
        reject(e);
        return;
      }
      resolve(value);
    });
  });

  // ---- glob and search options ----
  const patternList = (v, method, what) => {
    if (typeof v === 'string') return [v];
    if (!isArray(v)) throw new TypeErrorC(method + ' ' + what + ' must be a string or an array of strings');
    const out = [];
    const len = v.length;
    for (let i = 0; i < len; i++) {
      const e = v[i];
      if (typeof e !== 'string') throw new TypeErrorC(method + ' ' + what + ' must be a string or an array of strings');
      push(out, e);
    }
    return out;
  };
  const searchOptions = (v, names, method) => {
    if (missing(v)) return null;
    if (!isObject(v) || isArray(v) || typeof v === 'function') throw new TypeErrorC(method + ' options must be an object');
    const keys = ownKeys(v);
    for (let k = 0; k < keys.length; k++) {
      const key = keys[k];
      if (typeof key !== 'string') continue;
      let known = false;
      for (let i = 0; i < names.length; i++) if (names[i] === key) known = true;
      if (!known) throw new TypeErrorC(method + ' does not accept option "' + key + '"');
    }
    return v;
  };
  const searchCwd = (o, method) => {
    if (o === null) return n.sbCwd();
    const v = o.cwd;
    if (missing(v)) return n.sbCwd();
    if (typeof v !== 'string') throw new TypeErrorC(method + ' option "cwd" must be a string');
    return v;
  };
  const searchExcludes = (o, method) => {
    if (o === null) return [];
    const v = o.exclude;
    if (missing(v)) return [];
    if (!isArray(v)) throw new TypeErrorC(method + ' option "exclude" must be an array of strings');
    return patternList(v, method, 'option "exclude"');
  };
  const searchBoolean = (o, name, method) => {
    if (o === null) return false;
    const v = o[name];
    if (missing(v)) return false;
    if (typeof v !== 'boolean') throw new TypeErrorC(method + ' option "' + name + '" must be a boolean');
    return v;
  };
  const MAX_SAFE_INTEGER = 9007199254740991, MAX_INT = 2147483647;
  const searchInteger = (o, name, method, fallback, minimum) => {
    if (o === null) return fallback;
    const v = o[name];
    if (missing(v)) return fallback;
    if (typeof v !== 'number') throw new TypeErrorC(method + ' option "' + name + '" must be an integer');
    if (v !== v || v === Infinity || v === -Infinity || v !== MathTrunc(v) || v < minimum || v > MAX_SAFE_INTEGER)
      throw new TypeErrorC(method + ' option "' + name + '" must be an integer of at least ' + minimum);
    return v;
  };
  const searchSelection = (o, method) => {
    const cwd = searchCwd(o, method);
    const excludes = searchExcludes(o, method);
    let hasGlob = false, globs = [];
    if (o !== null) {
      const v = o.glob;
      if (!missing(v)) {
        hasGlob = true;
        globs = patternList(v, method, 'option "glob"');
      }
    }
    return { cwd, excludes, hasGlob, globs };
  };
  const requireSearchText = (v, method, what) => {
    if (typeof v !== 'string') throw new TypeErrorC(method + ' requires a ' + what + ' string');
    if (v === '') throw new TypeErrorC(method + ' requires a non-empty ' + what);
    return v;
  };
  const GLOB_OPTIONS = ['cwd', 'exclude'];
  const parseGlob = (pattern, options, method) => {
    const patterns = patternList(pattern, method, 'pattern');
    const o = searchOptions(options, GLOB_OPTIONS, method);
    const cwd = searchCwd(o, method);
    const excludes = searchExcludes(o, method);
    // the patterns compile before any work, a refused one the call's TypeError
    n.sbGlob(method, patterns, excludes, cwd, false);
    return { method, patterns, excludes, cwd };
  };

  // ---- fs ----
  const fs = {};
  const promises = {};
  const def = (target, name, length, impl) => {
    defineProperty(target, name, { __proto__: null, value: named(name, length, impl), writable: true, enumerable: true,
      configurable: true });
  };
  def(fs, 'readFileSync', 2, (a) => readValue(requireStringArg(a[0], 'fs.readFileSync'), wantsText(a[1])));
  def(fs, 'writeFileSync', 2, (a) => {
    const path = requireStringArg(a[0], 'fs.writeFileSync');
    n.sbWrite(path, n.sbBytes('fs.writeFileSync', a[1]), false);
  });
  def(fs, 'appendFileSync', 2, (a) => {
    const path = requireStringArg(a[0], 'fs.appendFileSync');
    n.sbWrite(path, n.sbBytes('fs.appendFileSync', a[1]), true);
  });
  def(fs, 'mkdirSync', 2, (a) => {
    const path = requireStringArg(a[0], 'fs.mkdirSync');
    n.sbMkdir(path, optionRecursive(a[1]));
  });
  def(fs, 'readdirSync', 1, (a) => n.sbReaddir(requireStringArg(a[0], 'fs.readdirSync')));
  def(fs, 'statSync', 1, (a) => makeStats(n.sbStat(requireStringArg(a[0], 'fs.statSync'))));
  def(fs, 'existsSync', 1, (a) => n.sbExists(requireStringArg(a[0], 'fs.existsSync')));
  def(fs, 'rmSync', 2, (a) => {
    const path = requireStringArg(a[0], 'fs.rmSync');
    n.sbRm(path, optionRecursive(a[1]));
  });
  def(fs, 'renameSync', 2, (a) => {
    const path = requireStringArg(a[0], 'fs.renameSync');
    n.sbMove(path, requireStringArg(a[1], 'fs.renameSync'), false);
  });
  def(fs, 'copyFileSync', 2, (a) => {
    const path = requireStringArg(a[0], 'fs.copyFileSync');
    n.sbMove(path, requireStringArg(a[1], 'fs.copyFileSync'), true);
  });
  def(fs, 'globSync', 2, (a) => {
    validateCount(a, 2, 'fs.globSync');
    const g = parseGlob(a[0], a[1], 'fs.globSync');
    return n.sbGlob(g.method, g.patterns, g.excludes, g.cwd, true);
  });
  // the callback forms: (path, [options,] callback)
  const optionsAndCallback = (a, at, method) => a.length >= at + 2
    ? { options: a[at], callback: requireCallback(a[at + 1], method) }
    : { options: undefined, callback: requireCallback(a[at], method) };
  def(fs, 'readFile', 3, (a) => {
    validateCount(a, 3, 'fs.readFile');
    const path = requireAsyncPath(a[0], 'fs.readFile');
    const oc = optionsAndCallback(a, 1, 'fs.readFile');
    enqueueCallback('readFile', { path, text: parseReadOptions(oc.options, 'fs.readFile') }, VALUE, oc.callback);
  });
  const writeForm = (name, a) => {
    const method = 'fs.' + name;
    validateCount(a, 4, method);
    const path = requireAsyncPath(a[0], method);
    const data = n.sbBytes(method, a[1]);
    let callback;
    if (a.length >= 4) {
      validateNoOptions(a[2], method);
      callback = requireCallback(a[3], method);
    } else callback = requireCallback(a[2], method);
    enqueueCallback(name, { path, data }, ERROR_ONLY, callback);
  };
  def(fs, 'writeFile', 4, (a) => writeForm('writeFile', a));
  def(fs, 'appendFile', 4, (a) => writeForm('appendFile', a));
  def(fs, 'mkdir', 3, (a) => {
    validateCount(a, 3, 'fs.mkdir');
    const path = requireAsyncPath(a[0], 'fs.mkdir');
    const oc = optionsAndCallback(a, 1, 'fs.mkdir');
    enqueueCallback('mkdir', { path, recursive: validateRecursiveOptions(oc.options, 'fs.mkdir') }, MKDIR,
      oc.callback);
  });
  const noOptionsForm = (name, length) => def(fs, name, length, (a) => {
    const method = 'fs.' + name;
    validateCount(a, 3, method);
    const path = requireAsyncPath(a[0], method);
    let callback;
    if (a.length >= 3) {
      validateNoOptions(a[1], method);
      callback = requireCallback(a[2], method);
    } else callback = requireCallback(a[1], method);
    enqueueCallback(name, { path }, VALUE, callback);
  });
  noOptionsForm('readdir', 3);
  noOptionsForm('stat', 1);
  def(fs, 'exists', 2, (a) => {
    validateCount(a, 2, 'fs.exists');
    const path = requireAsyncPath(a[0], 'fs.exists');
    enqueueCallback('exists', { path }, EXISTS, requireCallback(a[1], 'fs.exists'));
  });
  def(fs, 'rm', 3, (a) => {
    validateCount(a, 3, 'fs.rm');
    const path = requireAsyncPath(a[0], 'fs.rm');
    const oc = optionsAndCallback(a, 1, 'fs.rm');
    enqueueCallback('rm', { path, recursive: validateRecursiveOptions(oc.options, 'fs.rm') }, ERROR_ONLY,
      oc.callback);
  });
  def(fs, 'rename', 3, (a) => {
    validateCount(a, 3, 'fs.rename');
    const path = requireAsyncPath(a[0], 'fs.rename');
    const dest = requireAsyncPath(a[1], 'fs.rename');
    enqueueCallback('rename', { path, dest }, ERROR_ONLY, requireCallback(a[2], 'fs.rename'));
  });
  def(fs, 'copyFile', 4, (a) => {
    validateCount(a, 4, 'fs.copyFile');
    const path = requireAsyncPath(a[0], 'fs.copyFile');
    const dest = requireAsyncPath(a[1], 'fs.copyFile');
    let callback;
    if (a.length >= 4) {
      validateCopyMode(a[2], 'fs.copyFile');
      callback = requireCallback(a[3], 'fs.copyFile');
    } else callback = requireCallback(a[2], 'fs.copyFile');
    enqueueCallback('copyFile', { path, dest }, ERROR_ONLY, callback);
  });
  def(fs, 'glob', 3, (a) => {
    validateCount(a, 3, 'fs.glob');
    let options, callback;
    if (a.length >= 3) {
      options = a[1];
      callback = requireCallback(a[2], 'fs.glob');
    } else callback = requireCallback(a[1], 'fs.glob');
    enqueueCallback('glob', parseGlob(a[0], options, 'fs.glob'), VALUE, callback);
  });

  // ---- fs.promises ----
  def(promises, 'readFile', 2, (a) => promised(() => {
    validateCount(a, 2, 'fs.promises.readFile');
    const path = requireAsyncPath(a[0], 'fs.promises.readFile');
    return ['readFile', { path, text: parseReadOptions(a[1], 'fs.promises.readFile') }];
  }));
  const promiseWrite = (name) => def(promises, name, 3, (a) => promised(() => {
    const method = 'fs.promises.' + name;
    validateCount(a, 3, method);
    const path = requireAsyncPath(a[0], method);
    const data = n.sbBytes(method, a[1]);
    validateNoOptions(a[2], method);
    return [name, { path, data }];
  }));
  promiseWrite('writeFile');
  promiseWrite('appendFile');
  def(promises, 'mkdir', 2, (a) => promised(() => {
    validateCount(a, 2, 'fs.promises.mkdir');
    const path = requireAsyncPath(a[0], 'fs.promises.mkdir');
    return ['mkdir', { path, recursive: validateRecursiveOptions(a[1], 'fs.promises.mkdir') }];
  }));
  const promiseNoOptions = (name, length) => def(promises, name, length, (a) => promised(() => {
    const method = 'fs.promises.' + name;
    validateCount(a, 2, method);
    const path = requireAsyncPath(a[0], method);
    validateNoOptions(a[1], method);
    return [name, { path }];
  }));
  promiseNoOptions('readdir', 2);
  promiseNoOptions('stat', 1);
  def(promises, 'rm', 2, (a) => promised(() => {
    validateCount(a, 2, 'fs.promises.rm');
    const path = requireAsyncPath(a[0], 'fs.promises.rm');
    return ['rm', { path, recursive: validateRecursiveOptions(a[1], 'fs.promises.rm') }];
  }));
  def(promises, 'rename', 2, (a) => promised(() => {
    validateCount(a, 2, 'fs.promises.rename');
    const path = requireAsyncPath(a[0], 'fs.promises.rename');
    return ['rename', { path, dest: requireAsyncPath(a[1], 'fs.promises.rename') }];
  }));
  def(promises, 'copyFile', 3, (a) => promised(() => {
    validateCount(a, 3, 'fs.promises.copyFile');
    const path = requireAsyncPath(a[0], 'fs.promises.copyFile');
    const dest = requireAsyncPath(a[1], 'fs.promises.copyFile');
    validateCopyMode(a[2], 'fs.promises.copyFile');
    return ['copyFile', { path, dest }];
  }));
  // an async iterator over the paths, found when the first next() runs
  const iterResult = (value, done) => new PromiseC((resolve) => resolve({ value, done }));
  def(promises, 'glob', 2, (a) => {
    validateCount(a, 2, 'fs.promises.glob');
    const g = parseGlob(a[0], a[1], 'fs.promises.glob');
    let started = false, paths, index = 0;
    const it = {};
    def(it, 'next', 0, () => {
      try {
        if (!started) {
          started = true;
          paths = ops.glob(g);
          index = 0;
        }
        if (paths === undefined || index >= paths.length) return iterResult(undefined, true);
        index++;
        return iterResult(paths[index - 1], false);
      } catch (e) {
        return new PromiseC((resolve, reject) => reject(e));
      }
    });
    def(it, 'return', 0, (r) => {
      started = true;
      paths = undefined;
      index = 0;
      return iterResult(missing(r[0]) ? undefined : r[0], true);
    });
    defineProperty(it, asyncIterator, { __proto__: null, value: named('[Symbol.asyncIterator]', 0, (r, self) => self),
      writable: true, configurable: true });
    return it;
  });
  defineProperty(fs, 'promises', { __proto__: null, value: promises, writable: true, enumerable: true, configurable: true });

  // ---- goccia: $ and runScript ----
  const quote = (v) => "'" + ArrayPrototypeJoin(StringPrototypeSplit(StringC(v), "'"), "'\\''") + "'";
  const commandLine = (a) => {
    if (a.length === 0) return '';
    const t = a[0];
    if (typeof t === 'string') return t;
    if (!isObject(t)) throw new TypeErrorC('$ expects a tagged template or command string');
    const length = t.length;
    const count = length > 0 ? MathMin(MathFloor(length), MAX_SAFE_INTEGER) : 0;
    let line = '';
    for (let i = 0; i < count; i++) {
      line += StringC(t[i]);
      if (i + 1 < a.length) line += quote(a[i + 1]);
    }
    return line;
  };
  const shellCommand = (line) => {
    const cmd = {};
    let executed = false, result, quiet = false, noThrow = false;
    const ensure = () => {
      if (executed) return;
      result = n.sbShell(line);
      executed = true;
      if (result[0] !== 0 && !noThrow)
        throw new TypeErrorC('Shell command failed with exit code ' + result[0] + ': ' + trim(result[2]));
    };
    const settle = (f) => {
      try {
        const v = f();
        return new PromiseC((resolve) => resolve(v));
      } catch (e) {
        return new PromiseC((resolve, reject) => reject(e));
      }
    };
    def(cmd, 'run', 0, () => settle(() => {
      ensure();
      return { exitCode: result[0], stdout: quiet ? '' : result[1], stderr: quiet ? '' : result[2],
        ok: result[0] === 0 };
    }));
    def(cmd, 'text', 0, () => settle(() => { ensure(); return quiet ? '' : result[1]; }));
    def(cmd, 'json', 0, () => settle(() => { ensure(); return parseJSON(result[1]); }));
    def(cmd, 'quiet', 0, () => { quiet = true; return cmd; });
    def(cmd, 'nothrow', 0, () => { noThrow = true; return cmd; });
    return cmd;
  };

  const RUN_OPTIONS = ['sandbox', 'copy', 'diff'];
  const rejectRemoved = (o, name, replacement) => {
    if (o[name] !== undefined) throw new TypeErrorC('runScript option "' + name + '" was removed; ' + replacement);
  };
  const objectString = (o, name, required) => {
    const v = o[name];
    if (missing(v)) {
      if (required) throw new TypeErrorC('runScript requires option "' + name + '"');
      return '';
    }
    if (typeof v !== 'string') throw new TypeErrorC('runScript option "' + name + '" must be a string');
    return v;
  };
  const objectBoolean = (o, name) => {
    const v = o[name];
    if (missing(v)) return false;
    if (typeof v !== 'boolean') throw new TypeErrorC('runScript option "' + name + '" must be a boolean');
    return v;
  };
  // each copy as [kind, a, b] (Goccia.Sandbox sbRunScript)
  const addCopy = (copies, v) => {
    if (typeof v === 'string') {
      const eq = StringPrototypeIndexOf(v, '=');
      push(copies, '0');
      push(copies, eq >= 0 ? StringPrototypeSlice(v, 0, eq) : v);
      push(copies, eq >= 0 ? StringPrototypeSlice(v, eq + 1) : '');
      return;
    }
    if (!isObject(v)) throw new TypeErrorC('runScript copy entries must be strings or objects');
    const sources = (missing(v.from) ? 0 : 1) + (missing(v.text) ? 0 : 1) + (missing(v.base64) ? 0 : 1);
    if (sources !== 1)
      throw new TypeErrorC('runScript copy entries require exactly one of "from", "text", or "base64"');
    if (!missing(v.from)) {
      push(copies, '0');
      push(copies, objectString(v, 'from', true));
      push(copies, objectString(v, 'to', false));
      return;
    }
    const path = objectString(v, 'path', true);
    if (!missing(v.base64)) {
      push(copies, '2');
      push(copies, path);
      push(copies, objectString(v, 'base64', true));
    } else {
      push(copies, '1');
      push(copies, path);
      push(copies, objectString(v, 'text', true));
    }
  };
  const runScript = named('runScript', 2, (a) => {
    const path = requireStringArg(a[0], 'runScript');
    const o = a[1];
    let isolated = false, includeDiff = false, diffFormat = '';
    const copies = [];
    if (!missing(o)) {
      if (!isObject(o)) throw new TypeErrorC('runScript options must be an object');
      rejectRemoved(o, 'seed', 'use "copy" (a path, {from, to}, {path, text}, {path, base64}, or an array of them)');
      rejectRemoved(o, 'seeds', 'use "copy"');
      rejectRemoved(o, 'diffFormat', 'use diff: "json" or diff: "unified"');
      rejectRemoved(o, 'diffMetadata', 'JSON diffs always include timestamp metadata; use diff: true');
      const keys = ownKeys(o);
      for (let k = 0; k < keys.length; k++) {
        const key = keys[k];
        if (typeof key !== 'string') continue;
        let known = false;
        for (let i = 0; i < RUN_OPTIONS.length; i++) if (RUN_OPTIONS[i] === key) known = true;
        if (!known)
          throw new TypeErrorC('runScript does not accept option "' + key +
            '" (options: sandbox, copy, diff; a child inherits its parent\'s capabilities)');
      }
      if (objectBoolean(o, 'sandbox')) isolated = true;
      const diff = o.diff;
      if (!missing(diff)) {
        if (typeof diff === 'boolean') {
          if (diff) includeDiff = true;
        } else if (typeof diff === 'string') {
          if (diff !== 'json' && diff !== 'unified')
            throw new TypeErrorC('runScript option "diff" must be true, "json", or "unified"');
          diffFormat = diff;
          includeDiff = true;
        } else throw new TypeErrorC('runScript option "diff" must be true, "json", or "unified"');
      }
      const copy = o.copy;
      if (!missing(copy)) {
        if (isArray(copy)) {
          const len = copy.length;
          for (let i = 0; i < len; i++) addCopy(copies, copy[i]);
        } else addCopy(copies, copy);
      }
    }
    const r = n.sbRunScript(path, isolated, includeDiff, diffFormat, copies);
    return { ok: r[0], exitCode: r[1], stdout: r[2], stderr: r[3], result: r[4], error: r[5], diff: r[6],
      failureKind: r[7] };
  });
  const $ = named('$', 0, (a) => shellCommand(commandLine(a)));
  const goccia = {};
  defineProperty(goccia, '$', { __proto__: null, value: $, writable: true, enumerable: true, configurable: true });
  defineProperty(goccia, 'runScript', { __proto__: null, value: runScript, writable: true, enumerable: true, configurable: true });

  // ---- goccia:search ----
  const GREP_OPTIONS = ['cwd', 'glob', 'exclude', 'regex', 'ignoreCase', 'maxResults', 'maxBytes', 'context'];
  const FUZZY_OPTIONS = ['cwd', 'glob', 'exclude', 'limit'];
  const MAX_GREP_CONTEXT = 536870911;
  const grep = named('grep', 2, (a) => {
    validateCount(a, 2, 'grep');
    const pattern = requireSearchText(a[0], 'grep', 'pattern');
    const o = searchOptions(a[1], GREP_OPTIONS, 'grep');
    const sel = searchSelection(o, 'grep');
    const regex = searchBoolean(o, 'regex', 'grep');
    const ignoreCase = searchBoolean(o, 'ignoreCase', 'grep');
    const maxResults = MathMin(searchInteger(o, 'maxResults', 'grep', 1000, 1), MAX_INT);
    const maxBytes = searchInteger(o, 'maxBytes', 'grep', -1, 0);
    const context = MathMin(searchInteger(o, 'context', 'grep', 0, 0), MAX_GREP_CONTEXT);
    const r = n.sbGrep(pattern, regex, ignoreCase, maxResults, maxBytes, context, sel.cwd, sel.hasGlob, sel.globs,
      sel.excludes);
    const matches = [];
    for (let i = 1; i < r.length; i++) {
      const m = r[i];
      push(matches, context > 0
        ? { path: m[0], line: m[1], column: m[2], text: m[3], before: m[4], after: m[5] }
        : { path: m[0], line: m[1], column: m[2], text: m[3] });
    }
    return { matches, truncated: r[0] };
  });
  const fuzzyFind = named('fuzzyFind', 2, (a) => {
    validateCount(a, 2, 'fuzzyFind');
    const query = requireSearchText(a[0], 'fuzzyFind', 'query');
    const o = searchOptions(a[1], FUZZY_OPTIONS, 'fuzzyFind');
    const sel = searchSelection(o, 'fuzzyFind');
    const limit = MathMin(searchInteger(o, 'limit', 'fuzzyFind', 100, 1), MAX_INT);
    const r = n.sbFuzzy(query, limit, sel.cwd, sel.hasGlob, sel.globs, sel.excludes);
    const matches = [];
    for (let i = 1; i < r.length; i++) push(matches, { path: r[i][0], score: r[i][1], positions: r[i][2] });
    return { matches, truncated: r[0] };
  });

  // ---- the modules ----
  const exportList = shared.exportList;
  const FS_EXPORTS = ['readFile', 'writeFile', 'appendFile', 'mkdir', 'readdir', 'stat', 'exists', 'rm', 'rename',
    'copyFile', 'glob', 'readFileSync', 'writeFileSync', 'appendFileSync', 'mkdirSync', 'readdirSync', 'statSync',
    'existsSync', 'rmSync', 'renameSync', 'copyFileSync', 'globSync', 'promises'];
  shared.hostModules['fs'] = () => {
    const out = ['default', fs];
    for (let i = 0; i < FS_EXPORTS.length; i++) { push(out, FS_EXPORTS[i]); push(out, fs[FS_EXPORTS[i]]); }
    return exportList(out);
  };
  shared.hostModules['goccia'] = () => exportList(['default', goccia, '$', $, 'runScript', runScript]);
  shared.hostModules['goccia:search'] = () => exportList(['grep', grep, 'fuzzyFind', fuzzyFind]);
})

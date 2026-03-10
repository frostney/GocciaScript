import { readFile } from "node:fs/promises";

const wasmPath = process.argv[2];
if (!wasmPath) {
  process.stderr.write("Usage: node souffle-host.mjs <file.wasm>\n");
  process.exit(1);
}

const DEBUG = process.env.SOUFFLE_DEBUG === "1";

function parseConstants(buffer) {
  const view = new DataView(buffer);
  let offset = 0;
  const count = view.getUint32(offset, true);
  offset += 4;
  const constants = new Array(count);
  for (let i = 0; i < count; i++) {
    const kind = view.getUint8(offset);
    offset += 1;
    switch (kind) {
      case 0:
        constants[i] = undefined;
        break;
      case 1:
        constants[i] = true;
        break;
      case 2:
        constants[i] = false;
        break;
      case 3:
        constants[i] = Number(view.getBigInt64(offset, true));
        offset += 8;
        break;
      case 4:
        constants[i] = view.getFloat64(offset, true);
        offset += 8;
        break;
      case 5: {
        const len = view.getUint32(offset, true);
        offset += 4;
        const bytes = new Uint8Array(buffer, offset, len);
        constants[i] = new TextDecoder().decode(bytes);
        offset += len;
        break;
      }
    }
  }
  return constants;
}

class SouffleClosure {
  constructor(wasmFn, upvalueCount) {
    this.wasmFn = wasmFn;
    this.upvalues = new Array(upvalueCount).fill(undefined);
  }
}

class SouffleRecord {
  constructor() {
    this._props = new Map();
  }
  get(key) { return this._props.get(key); }
  set(key, value) { this._props.set(key, value); }
  delete(key) { return this._props.delete(key); }
}

class SouffleBlueprint {
  constructor(name) {
    this.name = name;
    this.methods = new Map();
    this.parent = null;
  }
  instantiate() { return new SouffleInstance(this); }
  lookupMethod(name) {
    let bp = this;
    while (bp) {
      if (bp.methods.has(name)) return bp.methods.get(name);
      bp = bp.parent;
    }
    return undefined;
  }
}

class SouffleInstance {
  constructor(blueprint) {
    this._blueprint = blueprint;
    this._slots = new Map();
  }
}

class SouffleError {
  constructor(message) {
    this.message = message;
    this.name = "Error";
  }
  toString() { return `${this.name}: ${this.message}`; }
}

const wasmBytes = await readFile(wasmPath);
const wasmModule = new WebAssembly.Module(wasmBytes);

const sections = WebAssembly.Module.customSections(wasmModule, "souffle:constants");
if (sections.length === 0) {
  process.stderr.write("Error: no souffle:constants custom section found\n");
  process.exit(1);
}
const constants = parseConstants(sections[0]);

const globals = {
  console: {
    log: (...args) => { process.stdout.write(args.map(coerceToString).join(" ") + "\n"); },
    error: (...args) => { process.stderr.write(args.map(coerceToString).join(" ") + "\n"); },
  },
  undefined: undefined,
  null: null,
  true: true,
  false: false,
  NaN: NaN,
  Infinity: Infinity,
  Error: SouffleError,
};

const exnTag = new WebAssembly.Tag({ parameters: ['externref'] });

let wasmExports = null;
let currentClosure = undefined;

function coerceToString(v) {
  if (v === undefined) return "undefined";
  if (v === null) return "null";
  if (typeof v === "boolean") return v ? "true" : "false";
  if (typeof v === "number") {
    if (Object.is(v, -0)) return "0";
    return String(v);
  }
  if (typeof v === "string") return v;
  if (v instanceof SouffleClosure) return "[Function]";
  if (v instanceof SouffleError) return v.toString();
  if (Array.isArray(v)) return v.map(coerceToString).join(",");
  if (typeof v === "object") return "[object Object]";
  return String(v);
}

function callFn(callee, receiver, args) {
  if (callee instanceof SouffleClosure) {
    const prevClosure = currentClosure;
    currentClosure = callee;
    try {
      return callee.wasmFn(receiver ?? undefined, ...(args || []));
    } finally {
      currentClosure = prevClosure;
    }
  }
  if (typeof callee === "function") {
    return callee.apply(receiver, args || []);
  }
  throw new Error(`Not callable: ${coerceToString(callee)}`);
}

function getProperty(obj, name) {
  if (obj instanceof SouffleInstance) {
    const val = obj._slots.get(name);
    if (val !== undefined) return val;
    const method = obj._blueprint.lookupMethod(name);
    if (method !== undefined) return method;
    return undefined;
  }
  if (obj instanceof SouffleRecord) return obj.get(name);
  if (obj instanceof SouffleError && name === "message") return obj.message;
  if (obj instanceof SouffleError && name === "name") return obj.name;
  if (typeof obj === "string" && name === "length") return obj.length;
  if (Array.isArray(obj) && name === "length") return obj.length;
  if (obj !== null && obj !== undefined && typeof obj === "object") return obj[name];
  return undefined;
}

const souffle = {
  rt_nil: (flags) => (flags === 1 ? null : undefined),
  rt_true: () => true,
  rt_false: () => false,
  rt_box_int: (n) => Number(n),
  rt_load_const: (idx) => constants[idx],
  rt_to_bool: (v) => {
    if (v === false || v === 0 || v === "" || v === null || v === undefined) return 0;
    if (typeof v === "number" && isNaN(v)) return 0;
    return 1;
  },

  rt_add: (a, b) => {
    if (typeof a === "string" || typeof b === "string") return coerceToString(a) + coerceToString(b);
    return a + b;
  },
  rt_sub: (a, b) => a - b,
  rt_mul: (a, b) => a * b,
  rt_div: (a, b) => a / b,
  rt_mod: (a, b) => a % b,
  rt_pow: (a, b) => a ** b,
  rt_neg: (a) => -a,
  rt_band: (a, b) => a & b,
  rt_bor: (a, b) => a | b,
  rt_bxor: (a, b) => a ^ b,
  rt_shl: (a, b) => a << b,
  rt_shr: (a, b) => a >> b,
  rt_ushr: (a, b) => a >>> b,
  rt_bnot: (a) => ~a,

  rt_eq: (a, b) => a === b,
  rt_neq: (a, b) => a !== b,
  rt_lt: (a, b) => a < b,
  rt_gt: (a, b) => a > b,
  rt_lte: (a, b) => a <= b,
  rt_gte: (a, b) => a >= b,
  rt_not: (a) => !souffle.rt_to_bool(a) ? true : false,
  rt_typeof: (a) => typeof a === "object" && a !== null ? "object" : typeof a,
  rt_is_instance: (obj, ctor) => {
    if (obj instanceof SouffleInstance && ctor instanceof SouffleBlueprint) {
      let bp = obj._blueprint;
      while (bp) {
        if (bp === ctor) return true;
        bp = bp.parent;
      }
    }
    return false;
  },
  rt_has_property: (a, b) => {
    if (a instanceof SouffleRecord) return a._props.has(coerceToString(b));
    if (a instanceof SouffleInstance) return a._slots.has(coerceToString(b));
    return false;
  },
  rt_to_boolean_val: (a) => souffle.rt_to_bool(a) ? true : false,

  rt_get_prop: (obj, idx) => {
    const name = constants[idx];
    return getProperty(obj, name);
  },
  rt_set_prop: (obj, idx, val) => {
    const name = constants[idx];
    if (obj instanceof SouffleInstance) { obj._slots.set(name, val); return; }
    if (obj instanceof SouffleRecord) { obj.set(name, val); return; }
    if (obj !== null && obj !== undefined && typeof obj === "object") { obj[name] = val; }
  },
  rt_get_index: (obj, key) => {
    if (Array.isArray(obj)) return obj[key];
    if (obj instanceof SouffleRecord) return obj.get(coerceToString(key));
    if (typeof obj === "string") return obj[key];
    return undefined;
  },
  rt_set_index: (obj, key, val) => {
    if (Array.isArray(obj)) { obj[key] = val; return; }
    if (obj instanceof SouffleRecord) { obj.set(coerceToString(key), val); return; }
  },
  rt_del_prop: (obj, idx) => {
    const name = constants[idx];
    if (obj instanceof SouffleRecord) { obj.delete(name); return true; }
    return false;
  },
  rt_del_index: (obj, key) => {
    if (Array.isArray(obj)) { delete obj[key]; return true; }
    return false;
  },

  rt_call: (callee, args, argc) => callFn(callee, undefined, Array.isArray(args) ? args : []),
  rt_call_method: (recv, callee, args, argc) => callFn(callee, recv, Array.isArray(args) ? args : []),
  rt_construct: (ctor, args, argc) => {
    const argArr = Array.isArray(args) ? args : [];
    if (ctor === SouffleError || ctor === globals.Error) {
      return new SouffleError(argArr[0] !== undefined ? coerceToString(argArr[0]) : "");
    }
    if (ctor instanceof SouffleBlueprint) {
      const inst = ctor.instantiate();
      const ctorMethod = ctor.lookupMethod("constructor");
      if (ctorMethod) callFn(ctorMethod, inst, argArr);
      return inst;
    }
    throw new Error(`Not a constructor: ${coerceToString(ctor)}`);
  },

  rt_get_iter: (obj, kind) => {
    if (Array.isArray(obj)) return { _arr: obj, _idx: 0 };
    return { _arr: [], _idx: 0 };
  },
  rt_iter_next: (iter) => {
    if (iter._idx < iter._arr.length) return { value: iter._arr[iter._idx++], done: false };
    return { value: undefined, done: true };
  },

  rt_import: (idx) => { throw new Error("Module imports not supported in WASM host"); },
  rt_export: (val, idx) => {},
  rt_await: (val) => val,

  rt_get_global: (idx) => {
    const name = constants[idx];
    return (name in globals) ? globals[name] : undefined;
  },
  rt_set_global: (val, idx) => { globals[constants[idx]] = val; },
  rt_has_global: (idx) => (constants[idx] in globals) ? true : undefined,

  rt_to_string: (v) => coerceToString(v),
  rt_ext_op: (subOp, a, b, c) => a,
  rt_concat: (a, b) => coerceToString(a) + coerceToString(b),
  rt_to_primitive: (v) => v,

  rt_new_array: (cap) => [],
  rt_array_push: (arr, val) => { arr.push(val); },
  rt_array_pop: (arr) => arr.pop(),
  rt_array_get: (arr, idx) => arr[idx],
  rt_array_set: (arr, idx, val) => { arr[idx] = val; },
  rt_new_record: (cap) => new SouffleRecord(),
  rt_record_get: (rec, idx) => {
    const name = constants[idx];
    if (rec instanceof SouffleRecord) return rec.get(name);
    if (rec instanceof SouffleBlueprint) return rec.methods.get(name);
    if (rec instanceof SouffleInstance) {
      const val = rec._slots.get(name);
      if (val !== undefined) return val;
      return rec._blueprint.lookupMethod(name);
    }
    if (typeof rec === "string" && name === "length") return rec.length;
    if (Array.isArray(rec) && name === "length") return rec.length;
    if (rec !== null && rec !== undefined && typeof rec === "object") return rec[name];
    return undefined;
  },
  rt_record_set: (rec, idx, val) => {
    const name = constants[idx];
    if (rec instanceof SouffleRecord) rec.set(name, val);
    else if (rec instanceof SouffleBlueprint) rec.methods.set(name, val);
    else if (rec instanceof SouffleInstance) rec._slots.set(name, val);
    else if (rec !== null && rec !== undefined && typeof rec === "object") rec[name] = val;
  },
  rt_record_delete: (rec, idx) => {
    const name = constants[idx];
    if (rec instanceof SouffleRecord) rec.delete(name);
  },
  rt_get_length: (obj) => {
    if (Array.isArray(obj)) return obj.length;
    if (typeof obj === "string") return obj.length;
    return 0;
  },
  rt_new_blueprint: (idx) => new SouffleBlueprint(constants[idx]),
  rt_inherit: (child, parent) => {
    if (child instanceof SouffleBlueprint && parent instanceof SouffleBlueprint) {
      child.parent = parent;
    }
    return child;
  },
  rt_instantiate: (bp) => {
    if (bp instanceof SouffleBlueprint) return bp.instantiate();
    return undefined;
  },
  rt_get_slot: (obj, idx) => {
    const name = constants[idx];
    if (obj instanceof SouffleInstance) return obj._slots.get(name);
    if (obj instanceof SouffleBlueprint) return obj.methods.get(name);
    return undefined;
  },
  rt_set_slot: (obj, idx, val) => {
    const name = constants[idx];
    if (obj instanceof SouffleInstance) obj._slots.set(name, val);
    else if (obj instanceof SouffleBlueprint) obj.methods.set(name, val);
  },
  rt_unpack: (obj, idx) => {
    if (Array.isArray(obj)) return obj[idx];
    return undefined;
  },
  rt_arg_count: () => 0,
  rt_pack_args: (start) => [],
  rt_check_type: (val, typeKind) => {},

  rt_closure: (funcIdx) => {
    const fnName = `__fn_${funcIdx}`;
    const fn = wasmExports[fnName];
    if (!fn) throw new Error(`WASM function ${fnName} not found`);
    return new SouffleClosure(fn, 0);
  },
  rt_get_upvalue: (_ignored, idx) => {
    if (currentClosure) return currentClosure.upvalues[idx];
    return undefined;
  },
  rt_set_upvalue: (closure, idx, val) => {
    if (closure instanceof SouffleClosure) {
      while (closure.upvalues.length <= idx) closure.upvalues.push(undefined);
      closure.upvalues[idx] = val;
    }
  },
  rt_close_upvalue: (closure, regIdx) => {},

  __exn_tag: exnTag,
};

const instance = await WebAssembly.instantiate(wasmModule, { souffle });
wasmExports = instance.exports;

try {
  wasmExports._start(null);
} catch (e) {
  if (e instanceof WebAssembly.Exception && e.is(exnTag)) {
    const val = e.getArg(exnTag, 0);
    process.stderr.write(`Uncaught: ${coerceToString(val)}\n`);
    process.exit(1);
  }
  if (e instanceof SouffleError) {
    process.stderr.write(`Uncaught ${e.toString()}\n`);
    process.exit(1);
  }
  if (e instanceof WebAssembly.RuntimeError) {
    process.stderr.write(`WASM RuntimeError: ${e.message}\n`);
    process.exit(1);
  }
  throw e;
}

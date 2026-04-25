// test262 prototype isolation -- GocciaScript-specific harness piece.
//
// Goccia worker threads share their built-in prototypes (Array.prototype,
// Object.prototype, etc.) across every test file the worker processes
// sequentially.  A test262 fixture that mutates one of these prototypes
// (e.g. tests that probe how `Array.prototype.foo = ...` is observed by
// `for-in`) leaves the mutation visible to whatever file the worker picks
// up next, producing spurious failures unrelated to the second test.
//
// Real engines avoid this by spinning up a fresh global per test; that's
// not how our worker pool is shaped.  Instead we snapshot the own
// descriptor set of every standard prototype before each test runs and
// restore it afterwards, so each file observes the prototypes in their
// pristine state regardless of what the previous file did.
//
// The hooks register on the root suite — the test runner walks parents
// when collecting beforeEach/afterEach, so this fires for every test no
// matter how many describe layers wrap it.

const __test262Iso_protos = (() => {
  const out = [];
  const add = (p) => { if (p && typeof p === "object") out.push(p); };
  if (typeof Object !== "undefined") add(Object.prototype);
  if (typeof Array !== "undefined") add(Array.prototype);
  if (typeof String !== "undefined") add(String.prototype);
  if (typeof Number !== "undefined") add(Number.prototype);
  if (typeof Boolean !== "undefined") add(Boolean.prototype);
  if (typeof BigInt !== "undefined") add(BigInt.prototype);
  if (typeof Function !== "undefined") add(Function.prototype);
  if (typeof Error !== "undefined") add(Error.prototype);
  if (typeof RegExp !== "undefined") add(RegExp.prototype);
  if (typeof Date !== "undefined") add(Date.prototype);
  if (typeof Map !== "undefined") add(Map.prototype);
  if (typeof Set !== "undefined") add(Set.prototype);
  if (typeof Promise !== "undefined") add(Promise.prototype);
  return out;
})();

const __test262Iso_ownKeys = (obj) => {
  // Reflect.ownKeys returns string + symbol keys in spec order; that's
  // what we want so descriptor restoration preserves any well-known
  // symbol entries (Array.prototype[Symbol.iterator], etc.).
  if (typeof Reflect !== "undefined" && typeof Reflect.ownKeys === "function") {
    return Reflect.ownKeys(obj);
  }
  const names = Object.getOwnPropertyNames(obj);
  const syms = Object.getOwnPropertySymbols(obj);
  const all = [];
  for (const k of names) all.push(k);
  for (const s of syms) all.push(s);
  return all;
};

const __test262Iso_snapshotProto = (proto) => {
  const keys = __test262Iso_ownKeys(proto);
  const descs = new Map();
  for (const k of keys) {
    descs.set(k, Object.getOwnPropertyDescriptor(proto, k));
  }
  return { proto, keys, descs };
};

const __test262Iso_restoreProto = (snap) => {
  const proto = snap.proto;
  const originalKeys = new Set(snap.keys);

  // Phase 1: drop any keys the test added that weren't in the snapshot.
  // (Note: this harness avoids `continue` because Goccia's for-of loop
  // doesn't currently bind it as a control-flow keyword.  The inverted
  // `if` works the same and sidesteps the issue.)
  const currentKeys = __test262Iso_ownKeys(proto);
  for (const k of currentKeys) {
    if (!originalKeys.has(k)) {
      try { delete proto[k]; } catch (_) { /* non-configurable; nothing we can do */ }
    }
  }

  // Phase 2: restore descriptors for keys present in the snapshot.  This
  // also re-adds keys the test deleted, and reverts in-place value or
  // attribute mutations.  Non-configurable + non-writable identical-value
  // descriptors will throw on redefinition; swallow those — the property
  // is already in the right state by definition.
  for (const k of snap.keys) {
    const desc = snap.descs.get(k);
    if (desc) {
      try {
        Object.defineProperty(proto, k, desc);
      } catch (_) { /* see comment above */ }
    }
  }
};

let __test262Iso_snapshots = [];

beforeEach(() => {
  __test262Iso_snapshots = [];
  for (const p of __test262Iso_protos) {
    __test262Iso_snapshots.push(__test262Iso_snapshotProto(p));
  }
});

afterEach(() => {
  for (const snap of __test262Iso_snapshots) {
    __test262Iso_restoreProto(snap);
  }
  __test262Iso_snapshots = [];
});

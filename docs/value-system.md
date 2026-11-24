# Value System

*For contributors working on the runtime — type coercion, property access, or adding new value types.*

## Executive Summary

- **One 64-bit `TValue`** — Every JavaScript value is a NaN-boxed `QWord` (`G2.Core`): doubles as themselves, int32s, five special constants and heap pointers under their own tags. Numbers never allocate.
- **Heap cells, not classes** — Strings, symbols, BigInts, accessors, value arrays, upvalues and objects are cells with an 8-byte header (`TCellHeader`). There is no class hierarchy: an object is one record, `TObj`, and its `OKind` byte says which exotic or built-in behaviour applies.
- **Shapes** — An object's property keys, attributes and prototype live in its shape (`TShape`), shared through a transition tree; values live in the object's slots. Inline caches key on the shape.
- **Spec operations as routines** — Property access (`GetV`, `SetV`, `GetFromObject`), conversions (`ToPrimitiveHint`, `ToNumberV`, `ToStringV`, `ToPropKey`) and the descriptor-level internal methods (`G2.ObjModel`) are plain functions that return `False` with the exception in `Pending`.
- **Per-realm intrinsics** — Built-in prototypes are threadvars of the running realm (`ObjectProto`, `ArrayProto`, ...), swapped by `SwitchRealm`; one engine per thread owns them.

The value system is the foundation of GocciaScript's runtime. [G2 design](g2/design.md) gives the short version; this page is the contributor's reference for how values, objects and conversions work in `source/g2`.

## Value Representation

`TValue` is a `QWord` (`G2.Core`). Values below `DOUBLE_LIMIT` (`$FFF9 0000 0000 0000`) are IEEE 754 doubles stored as their bits. Above it the top 16 bits are a tag:

| Tag (top 16 bits) | Constant | Payload | Holds |
|-------------------|----------|---------|-------|
| below `$FFF9` | — | the double's bits | Numbers that are not small integers, NaN (canonical), `-0` |
| `$FFF9` | `TAG_INT` | low 32 bits | Numbers that are int32 values |
| `$FFFA` | `TAG_SPECIAL` | 0 to 4 | `VUndefined`, `VNull`, `VFalse`, `VTrue`, `VHole` |
| `$FFFB` | `TAG_PTR` | 48-bit address | a heap cell |

`VHole` is the array hole: it never reaches script code. Reads of a hole see `undefined` or continue on the prototype chain.

The helpers in `G2.Core` test and build values without allocating: `IsDouble`, `IsInt`, `IsNumber`, `IsPtr`, `IsString`, `IsObject`, `IsNullish`, `IsCallable`, `MkInt`, `MkDouble`, `MkNumber`, `MkBool`, `MkPtr`, `NumOf`, `IntOf`, `ObjOf`, `StrOf`. `G2.BigIntV` adds `IsBigInt`.

### Number Representation

A Number (ES2026 §6.1.6.1) is either an int32 tag or a double. `MkNumber` picks the int32 form for any integral value in int32 range except `-0`, which keeps its sign bit as a double, so `Object.is(-0, 0)` compares different bits. Arithmetic results go through `MkArith`, and `MkDouble` canonicalizes every NaN to `CANON_NAN` (`$7FF8 0000 0000 0000`): the negative NaN space above `$FFF9` belongs to the tags, so no other NaN bit pattern may appear as a value. `NumOf` reads either form as a `Double`.

Rules that follow:

- **Compare numbers by value, not bits.** `1` may be an int32 or a double `1.0` depending on how it was made. `StrictEquals` and `SameValueV` compare through `NumOf`; only the dispatch loop's fast path compares raw bits first.
- **Negative zero is a double.** Test for it on the double's sign bit, not with `= 0`, which is true for both zeros.
- **NaN is one value.** Since every NaN is canonical, `V = CANON_NAN` is a valid NaN test for a value; for a computed `Double` use `Math.IsNaN`.

The old engine's number designs are recorded in [ADR 0001](adr/0001-number-dual-representation.md) and [ADR 0016](adr/0016-ieee-754-number-representation.md).

### Undefined, Null, Booleans

`VUndefined`, `VNull`, `VFalse` and `VTrue` are constants: identity is equality. `MkBool` maps a Pascal `Boolean`.

### Strings and Atoms

A string is a `ckString` cell (`TStr`): length, a cached hash and UTF-16 code units, so `length` and indexing follow ECMAScript's code-unit semantics. Concatenations of 256 or more characters build views on a growable `ckStrBuf` buffer (`ConcatStrings`), so a loop of `s += x` appends in place; a substring of 32 or more characters shares its parent's characters (`SubString`, `cfSlice`). `NewString`, `StrFromUnicode` and `StrToUnicode` convert to and from Pascal strings.

Property keys are atoms: interned `TStr` records outside the collected heap, compared by pointer. `AtomOf` interns Pascal text, `Atomize` a string cell, `IntAtom` an integer's decimal text. Keys no source text can name are key atoms (`NewKeyAtom`) told apart by `Hdr.Extra`: `kfSymbol` for a symbol's key, `kfPrivate` for a class's private name, `kfInternal` for an engine slot.

### Symbols

A symbol is a `ckSymbol` cell allocated as a permanent cell (`cfPermanent`): it never moves, so collections hash it by address. It holds its description and its key atom; `SymKey` returns that key, under which objects store symbol-keyed properties in the same shape as string keys. The well-known symbols are made once and shared by every realm. `Symbol.for` keeps its registry in a plain object (`G2.Builtins3`), so the collector sees the registered symbols.

Coercion follows ECMA-262: `ToBool` is `true`; `ToNumberV` throws `TypeError` ("Cannot convert a Symbol value to a number"); `ToStringV` throws `TypeError` ("Cannot convert a Symbol value to a string"), so template literals, `+` with a string and other implicit conversions throw. `String(symbol)` and `symbol.toString()` return `"Symbol(description)"`; `typeof` is `"symbol"`. `Object(symbol)` makes an `okSymbolObj` wrapper whose `Aux1` is the symbol.

A registered symbol (`Symbol.for`) cannot be held weakly (ES2026 §9.13 CanBeHeldWeakly); other symbols can be WeakMap keys and WeakRef targets.

### BigInt

A BigInt is a `ckBigInt` cell (`TBigCell` in `G2.BigIntV`): a sign and 32-bit limbs. Arithmetic converts to Goccia's `TBigInteger` and back (`BigToValue`, `ValueToBig`). Map and Set keys compare BigInts by value. `Object(1n)` is an `okPlain` object with the BigInt in `Aux1` and `BigInt.prototype` as its prototype.

## Heap Cells

Every heap value starts with a `TCellHeader`: size, kind, flags and a kind-specific `Extra` word.

| Kind | Record | Used for |
|------|--------|----------|
| `ckString` | `TStr` | strings, atoms |
| `ckStrBuf` | `TStrBuf` | the growable buffer behind concatenation views |
| `ckValueArray` | `TValueArray` | array elements, overflow slots, internal lists |
| `ckUpval` | `TUpval` | a captured variable (open on the stack or closed) |
| `ckAccessor` | `TAccessor` | the getter and setter pair of an accessor property |
| `ckObject` | `TObj` | every object |
| `ckSymbol` | — | symbols (permanent) |
| `ckBigInt` | `TBigCell` | BigInts (`G2.BigIntV`) |
| `ckBytes` | `TBytesCell` | an ArrayBuffer's bytes (`G2.Typed`) |

How cells are allocated, moved and freed is in [Garbage Collector](garbage-collector.md) and [G2 design § Heap and GC](g2/design.md#heap-and-gc). The rules that matter to code that handles values are in [GC Integration](#gc-integration).

## Objects

An object is a `TObj`:

| Field | Meaning |
|-------|---------|
| `Shape` | keys, attributes and prototype (`TShape`) |
| `Elements` | dense indexed storage (`TValueArray`), or nil |
| `Overflow` | property slots past the inline ones |
| `Native` | a pointer the collector does not trace: a function template, a native routine, a SharedArrayBuffer block |
| `Aux1`, `Aux2` | two traced values for the kind's internal slots |
| `InlineCap` | the number of inline slots |
| `OKind` | the object kind |
| `OFlags` | flags; bit 8 means not extensible for every kind, the others depend on the kind |
| `Pad` | 32 untraced bits (a function's realm and native data, a typed array's byte offset, a promise's state) |
| `Slots` | the inline property slots |

### Object Kinds

`OKind` selects the exotic and built-in behaviour. `G2.Core` declares the core kinds; units that own a built-in declare theirs.

| Kind | Declared in | Internal slots |
|------|-------------|----------------|
| `okPlain` | `G2.Core` | ordinary object (also a BigInt wrapper: `Aux1` the BigInt) |
| `okArray` | `G2.Core` | `Elements`; with `ofBigLen`, `Aux2` is the length |
| `okFunction` | `G2.Core` | bytecode closure: `Native` the `PFuncTemplate`, `Aux1` the home object, `Aux2` the upvalues |
| `okNative` | `G2.Core` | native function: `Native` the `TNativeFn`, `Pad` its realm and data |
| `okBound` | `G2.Core` | bound function: `Aux1` the target, `Aux2` `[this, args...]` |
| `okError` | `G2.Core` | an Error instance |
| `okBoolean`, `okNumber`, `okStringObj`, `okSymbolObj` | `G2.Core` | primitive wrappers: `Aux1` the primitive |
| `okArguments`, `okArgumentsOrd` | `G2.Core` | an arguments object, and one turned ordinary (`DemoteArguments`) |
| `okMap`, `okSet` | `G2.Core` | `Aux1` the entries, `Aux2` the hash index |
| `okIterator` | `G2.Core` | array and string iterators |
| `okGenerator` | `G2.Core` | a generator object |
| `okProxy` | `G2.Core` | `Aux1` the target, `Aux2` the handler |
| `okModuleNS` | `G2.Core` | module namespace: `Aux1` the sorted export cells |
| `okTypedArray`, `okArrayBuffer`, `okDataView` | `G2.Typed` | see [Binary Data](#binary-data) |
| `okRegExp` | `G2.RegExpV` | a RegExp |
| `okDate` | `G2.Date` | `Aux1` the time value |
| `okPromise` | `G2.Promise` | `Aux1` the result, `Aux2` the reactions, `Pad` the state |
| `okAsyncGen` | `G2.VM` | an async generator object |
| `okAsyncFromSync` | `G2.Promise` | an async-from-sync iterator |
| `okRawJSON` | `G2.JSON` | a `JSON.rawJSON` object |

Built-ins written as JavaScript preludes (WeakMap, WeakRef, Temporal, Intl, ...) have no kind of their own: their instances are ordinary objects whose internal slots are private fields.

### Shapes and Property Storage

A shape (`TShape`) maps each key to a slot index and attribute byte and names the object's prototype. Adding a property moves the object along a transition edge to a child shape (`ShapeAdd`, `AddProperty`); objects built the same way share shapes, which is what the inline caches key on. A shape with more than 8 transitions hashes them. An object that reaches 64 properties gets a private dictionary shape (`IsDict`), changed in place. Deleting a property (`DeleteProperty`) or changing its attributes (`ChangeFlags`) rebuilds the object's shape from its prototype's root shape (`RootShapeFor`).

Attribute bits (`G2.Core`):

| Flag | Meaning |
|------|---------|
| `pfWritable` | `[[Writable]]` |
| `pfEnumerable` | `[[Enumerable]]` |
| `pfConfigurable` | `[[Configurable]]` |
| `pfAccessor` | the slot holds a `ckAccessor` cell with the getter and setter |
| `pfDefault` | writable, enumerable, configurable: an assignment's new property |
| `pfHidden` | writable, configurable, not enumerable: built-in methods |

`GetSlot`, `SetSlot`, `GetOwnSlot` and `ShapeLookup` read the storage directly. `DefineOwn` defines or redefines a data property with given flags; it skips validation and is meant for building intrinsics.

### Arrays and Elements

An array (`okArray`) keeps its indexed values in `Elements`, up to `ArrayDenseMax` elements; holes are `VHole`. An index property that is not a default-attribute data property (an accessor, a read-only element) lives in the shape under its string key, with a hole in the elements, and the object gets the element guard (`ofElemGuard`) so element writes take the checked path. A length past the dense elements sets `ofBigLen` and keeps the length in `Aux2` (`ArrayLength`, `ArraySetLengthSparse`). A non-writable length sets `ofLenReadOnly`. This models the array exotic object of ES2026 §10.4.2.

### Property Order

`OwnPropertyKeysV` (`G2.ObjModel`) returns array-index keys in ascending order, then string keys in creation order, then symbols in creation order (ES2026 §10.1.11.1 OrdinaryOwnPropertyKeys). Shapes keep creation order; elements supply the indices.

### Prototype Chain and Property Lookup

The prototype is part of the shape: `ProtoOf` reads `Shape^.Proto`, and `SetProto` moves the object to a shape under the new prototype's root shape. `GetFromObject` implements `[[Get]]` (ES2026 §10.1.8.1 OrdinaryGet) as a loop over the chain, so a long chain uses no native stack. At each object it:

1. hands a proxy to its `get` trap, a module namespace to its export lookup and a mapped arguments object to its parameter map;
2. answers a typed array's integer index from its elements (§10.4.5.4);
3. looks the key up in the shape and calls an accessor's getter with the original receiver;
4. answers the virtual properties (`length` of arrays, arguments objects and String objects, String indices) and dense elements;
5. continues at `Shape^.Proto`.

`GetV` adds the primitive receivers: a string answers `length` and indices itself, and every primitive continues at its wrapper prototype without allocating a wrapper. `GetElemV`, `SetV`, `SetElemV` and the strict forms (`SetVS`, `SetVT`) are the computed-key and `[[Set]]` counterparts; `HasPropertyV` is `[[HasProperty]]`. Compiled code reaches the same semantics through inline caches that record (shape, slot) for own properties and (receiver shape, holder, holder shape) for prototype hits ([G2 design § Objects](g2/design.md#objects)).

### Property Descriptors

`G2.ObjModel` implements the descriptor-level internal methods for ordinary objects and the exotic objects G2 models (arrays, String objects, typed arrays, proxies through their traps):

| Routine | Spec |
|---------|------|
| `GetOwnPropertyV` | `[[GetOwnProperty]]` |
| `DefineOwnPropertyV` | `[[DefineOwnProperty]]` with ValidateAndApplyPropertyDescriptor (ES2026 §10.1.6.3) |
| `DefinePropertyOrThrow` | DefinePropertyOrThrow |
| `ToPropertyDescriptor` | ES2026 §6.2.6.5 |
| `FromPropertyDescriptor` | ES2026 §6.2.6.4 |
| `SetIntegrityLevel`, `TestIntegrityLevel` | ES2026 §7.3.15, §7.3.16 |
| `PreventExtensionsV` | `[[PreventExtensions]]` |
| `OwnPropertyKeysV` | `[[OwnPropertyKeys]]` |

A descriptor is a `TPropDesc` record with a `Has*` flag per field, so "absent" and "false" stay distinct, and redefining an existing property keeps the attributes the new descriptor leaves out. `Object.defineProperty` and `Reflect.defineProperty` share this path. The descriptor's values are rooted by the caller.

`Object.freeze` and `Object.seal` call `SetIntegrityLevel`; `Object.isFrozen` and `Object.isSealed` call `TestIntegrityLevel`, which checks the object's actual state, so an object made non-extensible with only non-writable, non-configurable properties is frozen without `freeze`.

### Failed Writes and Deletes

`DeleteIfConfigurable` returns `False` for a non-configurable property. In strict code a `delete` that returns `false` throws `TypeError` (ES2026 §13.5.1.2), and a failed `[[Set]]` (a read-only property, a setter-less accessor, a non-extensible object) throws `TypeError` ("Cannot assign to read only property ..."). All code is strict unless script source enables `--compat-non-strict-mode`; then non-strict functions keep the `false` result and silent write. Module code is always strict.

## Type Conversion

The conversions that may run JavaScript are in `G2.VM`; those that cannot are in `G2.Core`.

| Routine | Spec | Notes |
|---------|------|-------|
| `ToPrimitiveHint` | ES2026 §7.1.1 ToPrimitive | `TPrimHint` is `phDefault`, `phNumber` or `phString`; reads `Symbol.toPrimitive` first |
| `ToPrimitiveV` | ES2026 §7.1.1 | the string or number hint as a `Boolean` |
| `OrdinaryToPrimitive` | ES2026 §7.1.1.1 | `toString` then `valueOf` for the string hint, the reverse otherwise |
| `ToBool` | ES2026 §7.1.2 ToBoolean | never calls JavaScript |
| `ToNumberV` | ES2026 §7.1.4 ToNumber | throws for BigInt and Symbol |
| `PrimitiveToNumber` | ES2026 §7.1.4 | primitives only |
| `ToStringV` | ES2026 §7.1.17 ToString | throws for Symbol |
| `NumberToStr` | Number::toString | through the shared `NumericText` unit |
| `ToObjectV` | ES2026 §7.1.18 ToObject | makes a wrapper; throws for `undefined` and `null` |
| `ToPropKey` | ES2026 §7.1.19 ToPropertyKey | returns the atom, or the symbol's key atom |
| `ToBigIntV` | ToBigInt | |

`ToPrimitiveHint` probes `@@toPrimitive` with GetMethod: a callable one is called with the hint string and must return a primitive, a present non-callable one throws `TypeError`, and an absent one falls through to `OrdinaryToPrimitive`. Binary `+` and `==` pass `phDefault`.

Results of the common conversions:

| Value | ToBoolean | ToNumber | ToString |
|-------|-----------|----------|----------|
| `undefined` | `false` | `NaN` | `"undefined"` |
| `null` | `false` | `0` | `"null"` |
| `true` / `false` | — | `1` / `0` | `"true"` / `"false"` |
| `0`, `-0`, `NaN` | `false` | — | `"0"`, `"0"`, `"NaN"` |
| `""` | `false` | `0` | — |
| Symbol | `true` | `TypeError` | `TypeError` |
| BigInt | `false` for `0n` | `TypeError` | decimal digits |
| Object | `true` | ToPrimitive(number) | ToPrimitive(string) |

### Equality and `typeof`

- `StrictEquals` — ES2026 §7.2.14 IsStrictlyEqual: numbers by value (so `NaN !== NaN`, `0 === -0`), strings by content, BigInts by value, everything else by identity.
- `LooseEquals` — ES2026 §7.2.13 IsLooselyEqual, with conversions that may call JavaScript.
- `SameValueV` — ES2026 §7.2.9 SameValue (`Object.is`).
- SameValueZero (ES2026 §7.2.10) is what Map, Set and `includes` use.
- `TypeOfStr` — ES2026 §13.5.3 `typeof`: `"function"` for anything callable (bytecode, native and bound functions, callable proxies).

An object flagged `cfEmulatesUndefined` is the `[[IsHTMLDDA]]` object of ES2026 §B.3.6 (the conformance host's `$262.IsHTMLDDA`): `typeof` gives `"undefined"`, ToBoolean gives `false`, and it is loosely equal to `undefined` and `null`.

## Functions

| Kind | Made by | Call |
|------|---------|------|
| `okFunction` | the compiler's closures (functions, arrows, methods, generators, async functions, classes) | a bytecode frame; the `PFuncTemplate` flags (`IsArrow`, `IsClassCtor`, `IsDerived`, `IsMethod`, `IsGenerator`, `IsAsync`, `IsStrict`) select the behaviour |
| `okNative` | `NewNativeFunction`, `NewNativeFunctionData` | a Pascal `TNativeFn` |
| `okBound` | `Function.prototype.bind` | the target with the bound `this` and arguments prepended |

`length` and `name` are ordinary configurable shape properties (`FnShapeFor`). A function's `prototype` object is made on first use (`FunctionPrototypeOf`). A native function given a `prototype` property while the intrinsics are built is a built-in constructor.

### Native Functions

```pascal
TNativeFn = function(Args: PValue; Argc: Integer; var Result: TValue): Boolean;
```

`Args[0]` is `this` and `Args[1..Argc]` are the arguments; the slots live on the value stack, so they are rooted. The function writes `Result` and returns `True`, or returns `False` with the exception in `Pending` (see [Error Values](#error-values)). Under `new`, `CurNewTarget` holds new.target; otherwise it is `undefined`. `NewNativeFunctionData` stores a small integer in the function's `Pad`, which the native reads from `NativeData`, so one routine can serve several methods.

### `this` Binding

| Function form | `this` |
|---------------|--------|
| Ordinary function, method, class method | the call's receiver |
| Arrow function | the enclosing function's `this`, read through the upvalue `%this` |
| Async and generator forms | as the non-async, non-generator form |

Strict functions receive `undefined` for a call without a receiver. A non-strict function (script source with `--compat-non-strict-mode`) gets the global object for an `undefined` or `null` receiver and a wrapper for a primitive one (ES2026 §10.2.1.2 OrdinaryCallBindThis).

### Classes and Private Names

A class constructor is an `okFunction` whose template has `IsClassCtor` (and `IsDerived` for `extends`). Methods are closures whose `Aux1` holds their home object (`opSetHome`), which `super` property lookups start from. The constructor's field initializer adds the private methods, then the fields in source order (public and private together); a base class runs it at construction, a derived class after `super()` returns. A public field is defined, not assigned (ES2026 §7.3.32 DefineField), so it never calls a setter of the same name.

A private name is a key atom with `kfPrivate`, made fresh each time the class is evaluated. Private fields, methods and accessors are stored in the instance's shape under that key, so two classes' `#x` never collide and no string key reaches them. `PrivateGet` and `PrivateSet` (ES2026 §7.3.30, §7.3.31) look the key up and throw `TypeError` when the object lacks it; `#x in o` is a shape lookup.

### Enums

An `enum` declaration (TC39 proposal-enum, as GocciaScript has it) compiles to an object whose members are set in order, then finalized by the `enumFinalize` helper of the builtins5 prelude (`INTRINSIC_ENUM_FINALIZE`): a non-extensible object with a null prototype, each member enumerable, non-writable and non-configurable, members restricted to Number, String and Symbol values (`TypeError` otherwise), a `Symbol.iterator` that yields `[name, value]` entries and `Symbol.toStringTag` set to the enum's name.

## Collections

### Map and Set

`okMap` and `okSet` objects (`G2.Builtins`) keep their entries in insertion order in a value array in `Aux1`, as key and value pairs; a deleted entry's key becomes `VHole`, so iteration that is under way skips it. `Aux2` is an open-addressing hash index over the entries. Keys compare by SameValueZero. Numbers and strings hash by value, BigInts share one bucket, symbols hash by their permanent address, and other objects by their address: since a minor collection can move a young object, an index that holds address-hashed keys is rebuilt after one, and until then a lookup scans the entries without allocating.

### Weak Collections

`WeakMap`, `WeakSet`, `WeakRef` and `FinalizationRegistry` are classes in the builtins5 prelude (`source/g2/prelude/builtins5.js`) over natives from `G2.Weak`. WeakMap and WeakSet keep their entries in an ephemeron table outside the heap: an entry's value is reachable only while its key is. A WeakRef or FinalizationRegistry cell keeps its target in an untraced box; after each collection `G2.Weak` clears the boxes whose targets died and queues the registries' cleanup, which the event loop runs one job at a time. WeakRef targets returned by the constructor or `deref()` stay alive until the current job ends. Keys and targets must satisfy ES2026 §9.13 CanBeHeldWeakly: objects and unregistered symbols.

### Promises

A promise is an `okPromise` object (`G2.Promise`): `Aux1` the result, `Aux2` its reactions (none, one, or an array), `Pad` the state, and an `OFlags` bit for `[[PromiseIsHandled]]`. Jobs are queued in one ring buffer, so queueing a job allocates nothing. An `await` reaction holds the suspended async function and resumes it directly.

## Binary Data

`G2.Typed` holds the storage; `G2.TypedArrays` implements the constructors and prototypes (ES2026 §25.1, §23.2, §25.3).

- **ArrayBuffer** (`okArrayBuffer`) — `Aux1` is the bytes cell (`ckBytes`), whose length is the capacity (`maxByteLength` for a resizable buffer); `Aux2` is the current byte length. `OFlags` carries `bfResizable`, `bfDetached` and `bfImmutable`. Detaching (`DetachBuffer`) drops the bytes, and every view then sees length 0.
- **SharedArrayBuffer** — an `okArrayBuffer` with `bfShared`, whose `Native` is a reference-counted block outside every heap (`G2.Shared`). The block never moves; a growable buffer reserves its maximum length up front. The block can be handed to another engine's thread, which wraps it in an object of its own.
- **Typed arrays** (`okTypedArray`) — `Aux1` the buffer, `Aux2` the fixed length or -1 for a length-tracking view, `Pad` the byte offset, `OFlags` the element kind (`TAKind`). The twelve kinds include `Float16Array`, `BigInt64Array` and `BigUint64Array`. `TALength` follows TypedArrayLength, so a view out of bounds or over a detached buffer has length 0. Integer-index reads and writes never reach the prototype chain (ES2026 §10.4.5).
- **DataView** (`okDataView`) — a view with endian-aware reads and writes over either buffer type.

## Realms

A realm's intrinsics and global object live in threadvars while it runs: `ObjectProto`, `FunctionProto`, `ArrayProto`, `ErrorProto` and the rest in `G2.Core` and `G2.VM`, plus each built-in unit's own. `SwitchRealm` saves the running realm's values and loads another's; a function carries its realm in `Pad`, and a call to another realm's function switches to it for the call. `CreateRealm` reruns the built-ins' Init procedures for a new realm (ShadowRealm, `$262.createRealm`). Each threadvar that holds realm state is registered with `RealmVals` (or `RealmPtrs`, `RealmInts`) so the switch covers it. See [Adding a New Built-in Type](adding-built-in-types.md) for the registration a new built-in needs.

## GC Integration

The collector is precise and moves young cells ([Garbage Collector](garbage-collector.md)). Code that handles `TValue`s follows these rules:

- **Any allocation can move young cells.** A `TValue` in a Pascal local is stale after anything that may allocate or run JavaScript. Keep it on the temporary root stack (`PushRoot`, `RootAt`, `PopRoots`) and read it back from there, or re-read it from a rooted place (`Args`, a rooted threadvar).
- **Threadvars that hold values are roots.** Register them once with `AddRoot` (`AddRootRange` for an array).
- **Traced fields.** The collector traces an object's slots, `Elements`, `Overflow`, `Aux1`, `Aux2` and its shape's prototype. `Native` and `Pad` are not traced: never keep a heap value there.
- **Write barrier.** Storing a pointer into an object that may be old needs `WriteBarrier` on its header. `SetSlot`, `AddProperty` and `DefineOwn` do it; a direct store into `Aux1` or `Aux2` must do it itself.
- **Outside the heap.** Atoms, shapes, function templates and symbols are not collected cells. Memory a cell owns outside the heap is released from a hook such as `GCFinalizeHook` (`G2.Shared` releases SharedArrayBuffer blocks that way).

`scripts/g2/gcstress-tests.py` runs the test suite with a small nursery and heap verification to find missed roots and barriers.

## Error Values

See [Errors](errors.md) for the error types and their display, and [JSON Output](json-output.md) for the JSON envelope.

An Error instance is an `okError` object with a `message` property and, under GocciaScript's hosts, a `stack` property (`AttachStack`). Native code makes one with `MakeError(Proto, Msg)` and throws through `ThrowError(Kind, Msg)` or `ThrowTypeError(Msg)`, which set `Pending` and return `False`:

```pascal
if not IsObject(Args[0]) then
  Exit(ThrowTypeError('this is not a Foo object'));
```

There is no Pascal exception for a JavaScript throw. A native that gets `False` from a routine it called returns `False` itself, leaving `Pending` as it is; the VM unwinds through its own handler records, and the dispatch loop has no Pascal `try`. Pascal exceptions are for engine set-up failures, such as a prelude that does not compile, and for the shared front end: the parser and lexer raise `TGocciaSyntaxError` and `TGocciaLexerError` (subclasses of `TGocciaError` in `Goccia.Error`), which `G2.Eval` turns into a `SyntaxError`, or a `ReferenceError` for `TGocciaReferenceError`, with the source position.

// test262 wellKnownIntrinsicObjects -- GocciaScript-compatible subset

const WellKnownIntrinsicSources = [
  ["%AggregateError%", "AggregateError"],
  ["%Array%", "Array"],
  ["%ArrayBuffer%", "ArrayBuffer"],
  ["%ArrayIteratorPrototype%", "Object.getPrototypeOf([][Symbol.iterator]())"],
  ["%AsyncFunction%", "(async function() {}).constructor"],
  ["%AsyncGeneratorFunction%", "(async function* () {}).constructor"],
  ["%AsyncGeneratorPrototype%", "Object.getPrototypeOf(async function* () {}).prototype"],
  ["%AsyncIteratorPrototype%", "Object.getPrototypeOf(Object.getPrototypeOf(async function* () {}).prototype)"],
  ["%BigInt%", "BigInt"],
  ["%BigInt64Array%", "BigInt64Array"],
  ["%BigUint64Array%", "BigUint64Array"],
  ["%Boolean%", "Boolean"],
  ["%Date%", "Date"],
  ["%decodeURI%", "decodeURI"],
  ["%decodeURIComponent%", "decodeURIComponent"],
  ["%encodeURI%", "encodeURI"],
  ["%encodeURIComponent%", "encodeURIComponent"],
  ["%Error%", "Error"],
  ["%EvalError%", "EvalError"],
  ["%Float32Array%", "Float32Array"],
  ["%Float64Array%", "Float64Array"],
  ["%Function%", "Function"],
  ["%GeneratorFunction%", "(function* () {}).constructor"],
  ["%GeneratorPrototype%", "Object.getPrototypeOf(function* () {}).prototype"],
  ["%Int8Array%", "Int8Array"],
  ["%Int16Array%", "Int16Array"],
  ["%Int32Array%", "Int32Array"],
  ["%isFinite%", "isFinite"],
  ["%isNaN%", "isNaN"],
  ["%Iterator%", "Iterator"],
  ["%IteratorHelperPrototype%", "Object.getPrototypeOf(Iterator.from([]).drop(0))"],
  ["%JSON%", "JSON"],
  ["%Map%", "Map"],
  ["%MapIteratorPrototype%", "Object.getPrototypeOf(new Map()[Symbol.iterator]())"],
  ["%Math%", "Math"],
  ["%Number%", "Number"],
  ["%Object%", "Object"],
  ["%parseFloat%", "parseFloat"],
  ["%parseInt%", "parseInt"],
  ["%Promise%", "Promise"],
  ["%Proxy%", "Proxy"],
  ["%RangeError%", "RangeError"],
  ["%ReferenceError%", "ReferenceError"],
  ["%Reflect%", "Reflect"],
  ["%RegExp%", "RegExp"],
  ["%RegExpStringIteratorPrototype%", "Object.getPrototypeOf(RegExp.prototype[Symbol.matchAll](\"\"))"],
  ["%Set%", "Set"],
  ["%SetIteratorPrototype%", "Object.getPrototypeOf(new Set()[Symbol.iterator]())"],
  ["%SharedArrayBuffer%", "SharedArrayBuffer"],
  ["%String%", "String"],
  ["%StringIteratorPrototype%", "Object.getPrototypeOf(new String()[Symbol.iterator]())"],
  ["%Symbol%", "Symbol"],
  ["%SyntaxError%", "SyntaxError"],
  ["%ThrowTypeError%", "(function() { \"use strict\"; return Object.getOwnPropertyDescriptor(arguments, \"callee\").get; })()"],
  ["%TypedArray%", "Object.getPrototypeOf(Uint8Array)"],
  ["%TypeError%", "TypeError"],
  ["%Uint8Array%", "Uint8Array"],
  ["%Uint8ClampedArray%", "Uint8ClampedArray"],
  ["%Uint16Array%", "Uint16Array"],
  ["%Uint32Array%", "Uint32Array"],
  ["%URIError%", "URIError"],
  ["%WeakMap%", "WeakMap"],
  ["%WeakSet%", "WeakSet"],
  ["%WrapForValidIteratorPrototype%", "Object.getPrototypeOf(Iterator.from({ [Symbol.iterator]() { return {}; } }))"],
];

function resolveWellKnownIntrinsic(source) {
  try {
    return Function("return " + source)();
  } catch (exception) {
    return undefined;
  }
}

const WellKnownIntrinsicObjects = WellKnownIntrinsicSources.map(([name, source]) => ({
  name,
  source,
  value: resolveWellKnownIntrinsic(source),
}));

function getWellKnownIntrinsicObject(key) {
  for (const intrinsic of WellKnownIntrinsicObjects) {
    if (intrinsic.name === key) {
      if (intrinsic.value !== undefined) {
        return intrinsic.value;
      }
      throw new Test262Error("this implementation could not obtain " + key);
    }
  }

  throw new Test262Error("unknown well-known intrinsic " + key);
}

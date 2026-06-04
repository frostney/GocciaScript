// test262 $262 host hooks — GocciaScript adaptation.
//
// The test262 convention expects engines to provide a $262 global with
// host-specific capabilities.  Stock harness files (e.g. detachArrayBuffer.js)
// call methods on it.  This file is always loaded before other harness
// includes so that stock files work unchanged.
//
// Tests that need to mock $262 (e.g. harness self-tests) redefine it with
// var, which overrides this definition.
//
// Only methods Goccia can implement are provided.  Missing methods cause
// the stock harness to throw Test262Error, which the runner classifies as
// an honest FAIL.

if (typeof Goccia === "undefined" || Goccia.test262Host !== true) {
  throw new Error("$262 host hooks require GocciaScriptLoaderBare --test262-host");
}

var $262 = {
  detachArrayBuffer(buffer) {
    buffer.transfer();
  },

  gc() {
    Goccia.gc();
  },

  createRealm() {
    return {
      global: createTest262RealmGlobal(),
    };
  },
};

function createTest262RealmGlobal() {
  var realmGlobal = {};
  realmGlobal.Error = createRealmErrorConstructor("Error", Error, Error.prototype);
  realmGlobal.EvalError = createRealmErrorConstructor("EvalError", EvalError, realmGlobal.Error.prototype);
  realmGlobal.RangeError = createRealmErrorConstructor("RangeError", RangeError, realmGlobal.Error.prototype);
  realmGlobal.ReferenceError = createRealmErrorConstructor("ReferenceError", ReferenceError, realmGlobal.Error.prototype);
  realmGlobal.SyntaxError = createRealmErrorConstructor("SyntaxError", SyntaxError, realmGlobal.Error.prototype);
  realmGlobal.TypeError = createRealmErrorConstructor("TypeError", TypeError, realmGlobal.Error.prototype);
  realmGlobal.URIError = createRealmErrorConstructor("URIError", URIError, realmGlobal.Error.prototype);
  return realmGlobal;
}

function createRealmErrorConstructor(name, baseConstructor, parentPrototype) {
  var constructor = function(message) {
    var error = new baseConstructor(message);
    error.name = name;
    Object.setPrototypeOf(error, constructor.prototype);
    return error;
  };
  Object.defineProperty(constructor, "name", {
    value: name,
    configurable: true,
  });
  constructor.prototype = Object.create(parentPrototype);
  Object.defineProperty(constructor.prototype, "constructor", {
    value: constructor,
    writable: true,
    configurable: true,
  });
  return constructor;
}

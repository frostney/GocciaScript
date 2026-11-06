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
//
// $262 is the only global binding this file creates.  Everything else lives
// inside the function below, because a test may declare any other name at
// its top level: the annexB emulates-undefined tests start with
// `var IsHTMLDDA = $262.IsHTMLDDA;`, and a global `const IsHTMLDDA` here
// would make that a SyntaxError (ES2026 §16.1.7 GlobalDeclarationInstantiation
// step 4.a: a var name may not already be a global lexical binding).

if (typeof Goccia === "undefined" || Goccia.test262Host !== true) {
  throw new Error("$262 host hooks require GocciaTest262Runner");
}

var $262 = (function () {
  const AbstractModuleSource = () => {
    throw new TypeError("%AbstractModuleSource% is not a constructor");
  };
  const IsHTMLDDA = Goccia.test262.isHTMLDDA;

  function installRealm262(realm) {
    const realm262 = {
      global: realm.global,
      AbstractModuleSource,
      IsHTMLDDA,

      detachArrayBuffer(buffer) {
        if (buffer.detached) return;
        buffer.transfer();
      },

      evalScript(sourceText) {
        return realm.evalScript(sourceText);
      },

      createRealm() {
        return installRealm262(realm.createRealm());
      },
    };
    realm.global.$262 = realm262;
    return realm262;
  }

  const host262 = {
    global: globalThis,
    AbstractModuleSource,
    IsHTMLDDA,

    detachArrayBuffer(buffer) {
      if (buffer.detached) return;
      buffer.transfer();
    },

    evalScript(sourceText) {
      return Goccia.test262.evalScript(sourceText);
    },

    gc() {
      Goccia.gc();
    },

    createRealm() {
      return installRealm262(Goccia.test262.createRealm());
    },

    agent: {
      broadcast(value) {
        return Goccia.test262.agent.broadcast(value);
      },

      getReport() {
        return Goccia.test262.agent.getReport();
      },

      leaving() {
        return Goccia.test262.agent.leaving();
      },

      monotonicNow() {
        return Goccia.test262.agent.monotonicNow();
      },

      receiveBroadcast(callback) {
        return Goccia.test262.agent.receiveBroadcast(callback);
      },

      report(value) {
        return Goccia.test262.agent.report(value);
      },

      sleep(ms) {
        return Goccia.test262.agent.sleep(ms);
      },

      start(sourceText) {
        return Goccia.test262.agent.start(sourceText);
      },
    },
  };

  Object.defineProperty(AbstractModuleSource, "prototype", {
    value: Goccia.test262.abstractModuleSourcePrototype,
    writable: false,
    enumerable: false,
    configurable: false,
  });
  Object.defineProperty(AbstractModuleSource.prototype, "constructor", {
    value: AbstractModuleSource,
    writable: true,
    enumerable: false,
    configurable: true,
  });
  Object.defineProperty(AbstractModuleSource.prototype, Symbol.toStringTag, {
    get() {
      // Strict, as a built-in getter is: this file runs as a non-strict
      // Script, where a primitive this would be boxed into an object.
      "use strict";
      return this &&
        typeof this === "object" &&
        this !== AbstractModuleSource.prototype
        ? "ModuleSource"
        : undefined;
    },
    enumerable: false,
    configurable: true,
  });

  return host262;
})();

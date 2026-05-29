import { add, multiply, PI } from "./helpers/math-utils.js";
import { loadDep } from "./helpers/dynamic-import-caller.js";

describe("dynamic import()", () => {
  test("returns a promise that resolves to the module namespace", async () => {
    const mod = await import("./helpers/math-utils.js");
    expect(mod.add(2, 3)).toBe(5);
    expect(mod.multiply(3, 4)).toBe(12);
    expect(mod.PI).toBe(3.14159);
  });

  test("works with .then()", async () => {
    let result = 0;
    await import("./helpers/math-utils.js").then((mod) => {
      result = mod.add(10, 20);
    });
    expect(result).toBe(30);
  });

  test("supports expression specifier", async () => {
    const path = "./helpers/math-utils.js";
    const mod = await import(path);
    expect(mod.add(1, 2)).toBe(3);
  });

  test("supports template literal specifier", async () => {
    const dir = "./helpers";
    const mod = await import(`${dir}/math-utils.js`);
    expect(mod.multiply(5, 6)).toBe(30);
  });

  test("accepts trailing commas and import options", async () => {
    let optionsEvaluated = false;
    const mod = await import("./helpers/math-utils.js", { with: { type: "javascript" }, seen: (optionsEvaluated = true) },);
    expect(optionsEvaluated).toBe(true);
    expect(mod.add(3, 4)).toBe(7);
  });

  test("accepts source and defer import call forms", () => {
    const sourcePromise = import.source("./helpers/math-utils.js");
    const deferPromise = import.defer("./helpers/math-utils.js");
    expect(typeof sourcePromise.then).toBe("function");
    expect(typeof deferPromise.then).toBe("function");
    expect(sourcePromise.constructor).toBe(Promise);
    expect(deferPromise.constructor).toBe(Promise);
  });

  test("source import returns module source without evaluating the module", async () => {
    globalThis.__gocciaSourceDynamicImportEvaluated = false;
    const source = await import.source("./helpers/source-dynamic-import-side-effect.js");
    expect(Object.prototype.toString.call(source)).toBe("[object ModuleSource]");
    expect(globalThis.__gocciaSourceDynamicImportEvaluated).toBe(false);
  });

  test("deferred import evaluates the module when the namespace is observed", async () => {
    globalThis.__gocciaDeferredDynamicImportEvaluated = false;
    const mod = await import.defer("./helpers/deferred-dynamic-import-side-effect.js");
    expect(globalThis.__gocciaDeferredDynamicImportEvaluated).toBe(false);
    expect(mod.value).toBe(17);
    expect(globalThis.__gocciaDeferredDynamicImportEvaluated).toBe(true);
  });

  test("deferred import rejects when the module cannot be resolved", async () => {
    let caught = false;
    try {
      await import.defer("./helpers/nonexistent.js");
    } catch (e) {
      caught = true;
    }
    expect(caught).toBe(true);
  });

  test("rejects for non-existent module", async () => {
    let caught = false;
    try {
      await import("./helpers/nonexistent.js");
    } catch (e) {
      caught = true;
    }
    expect(caught).toBe(true);
  });

  test("works inside conditional", async () => {
    const shouldLoad = true;
    let result;
    if (shouldLoad) {
      const mod = await import("./helpers/math-utils.js");
      result = mod.PI;
    }
    expect(result).toBe(3.14159);
  });

  test("shares module cache with static imports", async () => {
    const mod = await import("./helpers/math-utils.js");
    expect(mod.add).toBe(add);
    expect(mod.multiply).toBe(multiply);
    expect(mod.PI).toBe(PI);
  });

  test("dynamically imports JSON modules", async () => {
    const config = await import("./helpers/config.json");
    expect(config.name).toBe("goccia-test");
  });

  test("works inside an arrow function", async () => {
    const loadAndAdd = async (a, b) => {
      const mod = await import("./helpers/math-utils.js");
      return mod.add(a, b);
    };
    const result = await loadAndAdd(7, 8);
    expect(result).toBe(15);
  });

  test("resolves relative to the defining module, not the caller", async () => {
    // loadDep is exported from helpers/dynamic-import-caller.js and calls
    // import("./dynamic-import-dep.js") — that path must resolve relative to
    // the helpers/ directory, not this test file's directory.
    const dep = await loadDep();
    expect(dep.SECRET).toBe(42);
  });
});

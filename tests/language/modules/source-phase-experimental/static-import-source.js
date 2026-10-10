import source mathModuleSource from "../helpers/math-utils.js";
import source sideEffectModuleSource from "../helpers/source-dynamic-import-side-effect.js";
import { sharedSource } from "../../../../fixtures/modules/import-access-source-barrel.js";

describe("experimental static source-phase imports", () => {
  test("binds ModuleSource objects without evaluating modules", () => {
    expect(typeof mathModuleSource).toBe("object");
    expect(Object.prototype.toString.call(mathModuleSource)).toBe("[object ModuleSource]");
    expect(Object.prototype.toString.call(sideEffectModuleSource)).toBe("[object ModuleSource]");
    expect(globalThis.__gocciaSourceDynamicImportEvaluated).toBeUndefined();
  });

  test("ModuleSource prototypes inherit from Object.prototype", () => {
    const moduleSourcePrototype = Object.getPrototypeOf(mathModuleSource);
    const abstractModuleSourcePrototype = Object.getPrototypeOf(moduleSourcePrototype);

    expect(Object.getPrototypeOf(abstractModuleSourcePrototype)).toBe(Object.prototype);
  });

  test("the ModuleSource tag is an accessor on the abstract prototype", () => {
    const abstractModuleSourcePrototype = Object.getPrototypeOf(
      Object.getPrototypeOf(mathModuleSource),
    );
    const descriptor = Object.getOwnPropertyDescriptor(
      abstractModuleSourcePrototype,
      Symbol.toStringTag,
    );

    expect(typeof descriptor.get).toBe("function");
    expect(descriptor.set).toBeUndefined();
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(true);
    expect(descriptor.get.call(mathModuleSource)).toBe("ModuleSource");
    expect(descriptor.get.call({})).toBeUndefined();
    expect(descriptor.get.call(1)).toBeUndefined();
    expect(Object.prototype.toString.call(Object.create(Object.getPrototypeOf(mathModuleSource)))).toBe(
      "[object Object]",
    );
  });

  test("duplicate star exports of one ModuleSource remain unambiguous", () => {
    expect(sharedSource).toBe(mathModuleSource);
  });
});

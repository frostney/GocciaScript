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

  test("duplicate star exports of one ModuleSource remain unambiguous", () => {
    expect(sharedSource).toBe(mathModuleSource);
  });
});

/*---
description: >
  A vi.mock factory the compatibility shim cannot analyse statically produces a
  module that fails on first import with a named diagnostic, rather than a mock
  with silently guessed exports. Automocking reports itself the same way.
features: [modules, runtime-modules]
---*/

import { describe, expect, test, vi } from "vitest";

// Every target below is imported dynamically inside its test, never statically:
// the diagnostic is raised when the generated module is first evaluated, which
// for a static import would happen while the entry module is still linking and
// would fail the whole file instead of the one test.

// No factory: automocking is not implemented.
vi.mock("./helpers/mock-error-automock.js");

// A spread hides the export names from static analysis.
vi.mock("./helpers/mock-error-spread-factory.js", () => ({ ...{ value: 1 } }));

// A computed key does the same.
vi.mock("./helpers/mock-error-computed-key-factory.js", () => ({ ["value"]: 1 }));

// A factory that demonstrably does not return an object.
vi.mock("./helpers/mock-error-non-object-factory.js", () => 42);

// An interpolated template literal is just as demonstrably a string as a plain
// one, so it gets the same Vitest-shaped TypeError rather than a generic
// "could not be analysed" report.
vi.mock("./helpers/mock-error-template-factory.js", () => `value-${1 + 1}`);

// A reserved word is identifier-like but is not a BindingIdentifier, so the
// generated module cannot declare it as an export.
vi.mock("./helpers/mock-error-reserved-key-factory.js", () => ({ class: 1 }));

// Strict-mode reserved words are rejected for the same reason: every generated
// mock module is module source, and module source is always strict.
vi.mock("./helpers/mock-error-strict-reserved-key-factory.js", () => ({
  static: 1,
}));

// An async factory only has its object after a microtask, which is too late.
vi.mock("./helpers/mock-error-async-factory.js", async () => ({ value: 1 }));

const importError = async (specifier) => {
  try {
    await import(specifier);
  } catch (error) {
    return error;
  }
  return undefined;
};

describe("vi.mock factories the shim cannot generate a module for", () => {
  test("a missing factory reports automocking as unimplemented", async () => {
    const error = await importError("./helpers/mock-error-automock.js");

    expect(error instanceof Error).toBe(true);
    expect(error.message).toContain("without a factory is not implemented");
    expect(error.message).toContain("docs/testing-api.md");
  });

  test("a spread in the factory object reports the export names as unknown", async () => {
    const error = await importError("./helpers/mock-error-spread-factory.js");

    expect(error instanceof Error).toBe(true);
    expect(error.message).toContain(
      "uses a spread, a computed key or an accessor",
    );
  });

  test("a computed key in the factory object reports the same", async () => {
    const error = await importError(
      "./helpers/mock-error-computed-key-factory.js",
    );

    expect(error.message).toContain(
      "uses a spread, a computed key or an accessor",
    );
  });

  test("a factory returning a non-object reports a TypeError", async () => {
    const error = await importError("./helpers/mock-error-non-object-factory.js");

    expect(error instanceof TypeError).toBe(true);
    expect(error.message).toContain("is not returning an object");
    expect(error.message).toContain('return an object with a "default" key');
  });

  test("a factory returning an interpolated template reports a TypeError", async () => {
    const error = await importError("./helpers/mock-error-template-factory.js");

    expect(error instanceof TypeError).toBe(true);
    expect(error.message).toContain("is not returning an object");
  });

  test("a reserved word as an export name is rejected", async () => {
    const error = await importError(
      "./helpers/mock-error-reserved-key-factory.js",
    );

    expect(error instanceof Error).toBe(true);
    expect(error.message).toContain('"class" is a reserved word');
  });

  test("a strict-mode reserved word as an export name is rejected too", async () => {
    const error = await importError(
      "./helpers/mock-error-strict-reserved-key-factory.js",
    );

    expect(error instanceof Error).toBe(true);
    expect(error.message).toContain('"static" is a reserved word');
  });

  test("an async factory is rejected", async () => {
    const error = await importError("./helpers/mock-error-async-factory.js");

    expect(error instanceof Error).toBe(true);
    expect(error.message).toContain("an async factory is not supported");
  });

  test("mocking an unresolvable specifier still fails as module-not-found", async () => {
    // Vitest lets a mock name a module that does not exist until something
    // imports it, so the hoist pre-pass must drop the directive rather than
    // fail the file.
    const error = await importError("./helpers/mock-does-not-exist.js");

    expect(error instanceof Error).toBe(true);
    expect(error.message).toContain("mock-does-not-exist.js");
  });
});

vi.mock("./helpers/mock-does-not-exist.js", () => ({ label: "unreachable" }));

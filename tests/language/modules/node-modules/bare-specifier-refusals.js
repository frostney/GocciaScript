// The failure half of node_modules resolution. Every case here must surface as
// a catchable module-resolution error, never as a SyntaxError from trying to
// parse something GocciaScript does not load.
const messageOf = async (specifier) => {
  try {
    await import(specifier);
    return "";
  } catch (error) {
    return error.message;
  }
};

describe("bare specifier resolution refusals", () => {
  test("a CommonJS-only package is named, not parsed", async () => {
    expect(await messageOf("pkg-commonjs")).toBe(
      'Package "pkg-commonjs" resolved to a CommonJS file (index.js); GocciaScript loads only ES modules',
    );
  });

  test("a null exports target blocks a subpath the wildcard would match", async () => {
    // "./sub/*" -> "./src/*.ts" would reach the real ./src/private.ts; the
    // exact "./sub/private": null key beats the pattern and blocks it.
    expect(await messageOf("pkg-exports/sub/private")).toBe(
      'Module not found: "pkg-exports/sub/private"',
    );
  });

  test("a subpath no exports key matches is not found", async () => {
    expect(await messageOf("pkg-exports/not-listed")).toBe(
      'Module not found: "pkg-exports/not-listed"',
    );
  });

  test("a wildcard match whose file is missing is not found", async () => {
    expect(await messageOf("pkg-exports/sub/absent")).toBe(
      'Module not found: "pkg-exports/sub/absent"',
    );
  });

  test("an uninstalled package is not found", async () => {
    expect(await messageOf("pkg-does-not-exist")).toBe(
      'Module not found: "pkg-does-not-exist"',
    );
  });

  test("a resolution failure message carries no host path", async () => {
    const message = await messageOf("pkg-does-not-exist");
    expect(message.includes("/node_modules/")).toBe(false);
  });
});

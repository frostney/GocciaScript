// A resolved package must stay inside its own directory. Every specifier here
// aims at ./helpers/outside-target.js, a file that really exists beside
// node_modules — so a passing test means the resolver refused, not that it
// missed a file. The positive control at the end proves the wildcard the
// escapes ride on actually works, which is what keeps the refusals meaningful.
//
// These are the *lexical* escapes. The physical one — a package shipping a
// symlinked child that resolves outside itself — is covered in
// source/units/Goccia.Modules.NodeResolution.Test.pas instead, because the
// fixture has to be a real symlink: this suite's fixtures are committed files,
// and a committed symlink materializes as an ordinary text file wherever git
// checks out without symlink support (Windows without developer mode), which
// would turn the escape into a file that legitimately lives inside the package
// and silently invert the assertion. The Pascal tests create the link at run
// time and are registered as skipped where that is not possible.
import { wildcardLabel } from "pkg-escape-wildcard";
import { insideLabel } from "pkg-escape-wildcard/sub/inside";

const messageOf = async (specifier) => {
  try {
    await import(specifier);
    return "";
  } catch (error) {
    return error.message;
  }
};

describe("package boundary containment", () => {
  test("a legacy subpath cannot walk out of the package", async () => {
    // No exports map, so the subpath is taken literally against the package
    // directory — the route that reached outside before containment existed.
    expect(await messageOf("pkg-main/../../helpers/outside-target.js")).toBe(
      'Module not found: "pkg-main/../../helpers/outside-target.js"',
    );
  });

  test("a wildcard star value cannot walk out of the package", async () => {
    // "./sub/*" -> "./src/*.js" with a traversing star value: the caller
    // controls this half of the substitution.
    expect(
      await messageOf("pkg-escape-wildcard/sub/../../../helpers/outside-target"),
    ).toBe(
      'Module not found: "pkg-escape-wildcard/sub/../../../helpers/outside-target"',
    );
  });

  test("an exports value pointing outside the package is refused", async () => {
    // The package itself is malformed rather than the specifier: its whole
    // "exports" string resolves above its own directory.
    expect(await messageOf("pkg-escape-exports")).toBe(
      'Module not found: "pkg-escape-exports"',
    );
  });

  test("a node_modules segment in a subpath is refused", async () => {
    expect(await messageOf("pkg-main/node_modules/other")).toBe(
      'Module not found: "pkg-main/node_modules/other"',
    );
  });

  test("a refusal message still carries no host path", async () => {
    const message = await messageOf("pkg-escape-exports");
    expect(message.includes("/node_modules/")).toBe(false);
    expect(message.includes("outside-target")).toBe(false);
  });

  test("the wildcard the escapes ride on resolves normally", () => {
    expect(wildcardLabel).toBe("inside");
    expect(insideLabel).toBe("inside-src");
  });
});

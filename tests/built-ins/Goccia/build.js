/*---
description: Goccia.build exposes compile-time platform information (os and arch)
features: [Goccia]
---*/

const hasGoccia = typeof Goccia !== "undefined";

const KNOWN_OS_VALUES = [
  "darwin",
  "linux",
  "windows",
  "freebsd",
  "netbsd",
  "openbsd",
  "android",
  "aix",
  "solaris",
];

const KNOWN_ARCH_VALUES = [
  "x86_64",
  "aarch64",
  "x86",
  "arm",
  "powerpc64",
  "powerpc",
];

describe.runIf(hasGoccia)("Goccia.build", () => {
  test("build is an object", () => {
    expect(typeof Goccia.build).toBe("object");
    expect(Goccia.build !== null).toBe(true);
  });

  test("build.os is a non-empty string", () => {
    expect(typeof Goccia.build.os).toBe("string");
    expect(Goccia.build.os.length > 0).toBe(true);
  });

  test("build.os is a known value", () => {
    const os = Goccia.build.os;
    expect(KNOWN_OS_VALUES.includes(os) || os === "unknown").toBe(true);
  });

  test("build.arch is a non-empty string", () => {
    expect(typeof Goccia.build.arch).toBe("string");
    expect(Goccia.build.arch.length > 0).toBe(true);
  });

  test("build.arch is a known value", () => {
    const arch = Goccia.build.arch;
    expect(KNOWN_ARCH_VALUES.includes(arch) || arch === "unknown").toBe(true);
  });

  test("build properties are stable across accesses", () => {
    expect(Goccia.build.os).toBe(Goccia.build.os);
    expect(Goccia.build.arch).toBe(Goccia.build.arch);
  });

  test("build.os is read-only", () => {
    const original = Goccia.build.os;
    expect(() => { Goccia.build.os = "other"; }).toThrow(TypeError);
    expect(Goccia.build.os).toBe(original);
  });

  test("build.arch is read-only", () => {
    const original = Goccia.build.arch;
    expect(() => { Goccia.build.arch = "other"; }).toThrow(TypeError);
    expect(Goccia.build.arch).toBe(original);
  });

  test("build.os is enumerable", () => {
    const keys = Object.keys(Goccia.build);
    expect(keys.includes("os")).toBe(true);
  });

  test("build.arch is enumerable", () => {
    const keys = Object.keys(Goccia.build);
    expect(keys.includes("arch")).toBe(true);
  });
});

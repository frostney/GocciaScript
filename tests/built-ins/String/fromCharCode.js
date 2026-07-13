describe("String.fromCharCode", () => {
  test("single ASCII character", () => {
    expect(String.fromCharCode(65)).toBe("A");
    expect(String.fromCharCode(97)).toBe("a");
    expect(String.fromCharCode(48)).toBe("0");
  });

  test("multiple characters", () => {
    expect(String.fromCharCode(72, 101, 108, 108, 111)).toBe("Hello");
  });

  test("no arguments returns empty string", () => {
    expect(String.fromCharCode()).toBe("");
  });

  test("null character", () => {
    expect(String.fromCharCode(0).length).toBe(1);
  });

  test("wraps values to 16-bit unsigned", () => {
    expect(String.fromCharCode(65536 + 65)).toBe("A");
    expect(String.fromCharCode(-1)).toBe(String.fromCharCode(65535));
  });
});

describe("String.fromCharCode ToUint16 non-finite code units", () => {
  test("Infinity maps to code unit 0", () => {
    const s = String.fromCharCode(Infinity);
    expect(s.length).toBe(1);
    expect(s.charCodeAt(0)).toBe(0);
  });

  test("-Infinity maps to code unit 0", () => {
    expect(String.fromCharCode(-Infinity).charCodeAt(0)).toBe(0);
  });

  test("NaN maps to code unit 0", () => {
    expect(String.fromCharCode(NaN).charCodeAt(0)).toBe(0);
  });

  test("huge positive double reduces modulo 2^16", () => {
    expect(String.fromCharCode(1e30).charCodeAt(0)).toBe(0);
  });

  test("huge negative double reduces modulo 2^16", () => {
    expect(String.fromCharCode(-1e30).charCodeAt(0)).toBe(0);
  });
});

describe("String.fromCharCode surrogate-pair identity", () => {
  test("paired code units equal the corresponding supplementary character", () => {
    const pair = String.fromCharCode(0xD83D, 0xDE00);
    expect(pair).toBe("😀");
    expect(new Map([["😀", 1]]).get(pair)).toBe(1);
    expect({ ["😀"]: 2 }[pair]).toBe(2);
  });
});

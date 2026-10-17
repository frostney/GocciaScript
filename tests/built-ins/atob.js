const NOT_CORRECTLY_ENCODED =
  "Failed to execute 'atob': The string to be decoded is not correctly encoded.";

const thrownBy = (fn) => {
  try {
    fn();
  } catch (error) {
    return error;
  }
  return undefined;
};

const expectInvalidCharacterError = (input) => {
  const error = thrownBy(() => atob(input));
  expect(error instanceof DOMException).toBe(true);
  expect(error.name).toBe("InvalidCharacterError");
  expect(error.code).toBe(5);
  expect(error.message).toBe(NOT_CORRECTLY_ENCODED);
};

// Base64 of the bytes 0, 1, ..., 255, 0, 1, ... up to ALength, and the
// string atob must decode it to. 768 bytes are three whole ramps and a whole
// number of base64 groups, so longer inputs repeat that block.
const RAMP_BYTES = new Uint8Array(768).map((_, i) => i % 256);
const RAMP_BASE64 = RAMP_BYTES.toBase64();
const RAMP_STRING = String.fromCharCode(...RAMP_BYTES.subarray(0, 256));

const rampBase64 = (aLength) =>
  RAMP_BASE64.repeat(Math.floor(aLength / 768)) +
  RAMP_BYTES.subarray(0, aLength % 768).toBase64();

const rampString = (aLength) =>
  RAMP_STRING.repeat(Math.ceil(aLength / 256)).slice(0, aLength);

describe("atob", () => {
  test("typeof atob is function", () => {
    expect(typeof atob).toBe("function");
  });

  test("decodes empty string", () => {
    expect(atob("")).toBe("");
  });

  test("decodes basic ASCII base64 strings", () => {
    expect(atob("SGVsbG8=")).toBe("Hello");
    expect(atob("SGVsbG8sIFdvcmxkIQ==")).toBe("Hello, World!");
    expect(atob("YWJj")).toBe("abc");
    expect(atob("YWI=")).toBe("ab");
    expect(atob("YQ==")).toBe("a");
  });

  test("decodes without padding (forgiving base64)", () => {
    // Missing both == padding
    expect(atob("YQ")).toBe("a");
    // Missing single = padding
    expect(atob("YWI")).toBe("ab");
    // No padding needed (length divisible by 4 content)
    expect(atob("YWJj")).toBe("abc");
  });

  test("strips ASCII whitespace before decoding", () => {
    // Spaces
    expect(atob("SGVs bG8=")).toBe("Hello");
    // Tabs
    expect(atob("SGVs\tbG8=")).toBe("Hello");
    // Newlines
    expect(atob("SGVs\nbG8=")).toBe("Hello");
    // Carriage returns
    expect(atob("SGVs\rbG8=")).toBe("Hello");
    // Form feeds
    expect(atob("SGVs\x0CbG8=")).toBe("Hello");
    // Mixed whitespace
    expect(atob(" S G V s b G 8 = ")).toBe("Hello");
  });

  test("decodes to Latin-1 characters (bytes > 0x7F)", () => {
    // Base64 for byte 0xC0 (À)
    expect(atob("wA==")).toBe(String.fromCharCode(0xC0));
    // Base64 for byte 0xFF (ÿ)
    expect(atob("/w==")).toBe(String.fromCharCode(0xFF));
    // Base64 for byte 0x80
    expect(atob("gA==")).toBe(String.fromCharCode(0x80));
  });

  test("throws InvalidCharacterError for invalid base64 characters", () => {
    expect(() => atob("!!!!")).toThrow();
    expect(() => atob("====")).toThrow();
    expect(() => atob("abc!")).toThrow();
  });

  test("throws InvalidCharacterError when length mod 4 = 1 after stripping", () => {
    // Single valid base64 char after cleanup → mod 4 = 1
    expect(() => atob("A")).toThrow();
    expect(() => atob("AAAAA")).toThrow();
  });

  test("throws TypeError when called with no arguments", () => {
    expect(() => atob()).toThrow();
    const error = thrownBy(() => atob());
    expect(error instanceof TypeError).toBe(true);
    expect(error.message).toBe(
      "Failed to execute 'atob': 1 argument required, but only 0 present."
    );
  });

  test("reports invalid input as a DOMException InvalidCharacterError", () => {
    // Character outside the base64 alphabet
    expectInvalidCharacterError("abc!");
    expectInvalidCharacterError("YW-j");
    expectInvalidCharacterError("YW_j");
    expectInvalidCharacterError("YWJ" + String.fromCharCode(0xE9));
    expectInvalidCharacterError("YWJ\u20AC");
    expectInvalidCharacterError("YWJ\uD83D");
    // Whitespace other than ASCII whitespace is not stripped
    expectInvalidCharacterError("YW\u00A0Jj");
    expectInvalidCharacterError("YW\vJj");
    // Padding in the middle, too much padding, or padding on a short input
    expectInvalidCharacterError("YQ=a");
    expectInvalidCharacterError("YQ===");
    expectInvalidCharacterError("YQ=");
    expectInvalidCharacterError("Y===");
    expectInvalidCharacterError("====");
    // Length mod 4 = 1 after removing whitespace and padding
    expectInvalidCharacterError("A");
    expectInvalidCharacterError("AAAAA");
    expectInvalidCharacterError("AAAAA===");
  });

  test("decodes every byte value across 8 KiB slice boundaries", () => {
    [1, 255, 256, 257, 8191, 8192, 8193, 3 * 8192 + 5].forEach((length) => {
      expect(atob(rampBase64(length))).toBe(rampString(length));
    });
  });

  test("decodes a 100,000-character input", () => {
    // Building the result one byte at a time made inputs of this size slow
    // and memory-hungry.
    const decoded = atob("QUJD".repeat(25000));
    expect(decoded.length).toBe(75000);
    expect(decoded).toBe("ABC".repeat(25000));
    expect(atob(rampBase64(75000))).toBe(rampString(75000));
  });

  test("decodes large whitespace-wrapped input", () => {
    // MIME-style lines of 76 characters separated by CRLF
    const line = "QUJD".repeat(19);
    const wrapped = Array.from({ length: 1000 }, () => line).join("\r\n");
    expect(atob(wrapped)).toBe("ABC".repeat(19 * 1000));
  });

  test("rejects an invalid character at either end of a large input", () => {
    expectInvalidCharacterError("QUJD".repeat(25000) + "QUJ!");
    expectInvalidCharacterError("!" + "QUJD".repeat(25000));
  });

  test("handles only-whitespace input", () => {
    expect(atob("   ")).toBe("");
    expect(atob("\t\n")).toBe("");
  });

  test("decodes standard padding variants", () => {
    // RFC 4648 test vectors
    expect(atob("Zg==")).toBe("f");
    expect(atob("Zm8=")).toBe("fo");
    expect(atob("Zm9v")).toBe("foo");
    expect(atob("Zm9vYg==")).toBe("foob");
    expect(atob("Zm9vYmE=")).toBe("fooba");
    expect(atob("Zm9vYmFy")).toBe("foobar");
  });

  test("round-trips with btoa", () => {
    const inputs = ["", "Hello", "Hello, World!", "abc", "a", "ab", "foobar"];
    inputs.forEach((input) => {
      expect(atob(btoa(input))).toBe(input);
    });
  });

  test("round-trips binary data", () => {
    const bytes = [];
    let i = 0;
    const step = 1;
    // Test a range of byte values
    [0, 1, 127, 128, 200, 255].forEach((b) => {
      const ch = String.fromCharCode(b);
      expect(atob(btoa(ch))).toBe(ch);
    });
  });
});

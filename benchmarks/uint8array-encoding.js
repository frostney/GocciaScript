/*---
description: Uint8Array Base64 and Hex encoding/decoding benchmarks
---*/

const SHORT_BYTES = new Uint8Array([72, 101, 108, 108, 111]);
const MEDIUM_BYTES = new Uint8Array(450);
const LARGE_BYTES = new Uint8Array(4096);

// Fill with varied data
for (const i of Array(450).keys()) MEDIUM_BYTES[i] = i % 256;
for (const i of Array(4096).keys()) LARGE_BYTES[i] = (i * 7 + 13) % 256;

const SHORT_B64 = SHORT_BYTES.toBase64();
const MEDIUM_B64 = MEDIUM_BYTES.toBase64();
const LARGE_B64 = LARGE_BYTES.toBase64();

const SHORT_HEX = SHORT_BYTES.toHex();
const MEDIUM_HEX = MEDIUM_BYTES.toHex();
const LARGE_HEX = LARGE_BYTES.toHex();

suite("Uint8Array.prototype.toBase64", () => {
  bench("short (5 bytes)", {
    run: () => {
      SHORT_BYTES.toBase64();
    },
  });

  bench("medium (450 bytes)", {
    run: () => {
      MEDIUM_BYTES.toBase64();
    },
  });

  bench("large (4096 bytes)", {
    run: () => {
      LARGE_BYTES.toBase64();
    },
  });

  bench("base64url alphabet", {
    run: () => {
      MEDIUM_BYTES.toBase64({ alphabet: "base64url" });
    },
  });

  bench("omitPadding", {
    run: () => {
      SHORT_BYTES.toBase64({ omitPadding: true });
    },
  });
});

suite("Uint8Array.fromBase64", () => {
  bench("short (8 chars)", {
    run: () => {
      Uint8Array.fromBase64(SHORT_B64);
    },
  });

  bench("medium (600 chars)", {
    run: () => {
      Uint8Array.fromBase64(MEDIUM_B64);
    },
  });

  bench("large (5464 chars)", {
    run: () => {
      Uint8Array.fromBase64(LARGE_B64);
    },
  });
});

suite("Uint8Array.prototype.toHex", () => {
  bench("short (5 bytes)", {
    run: () => {
      SHORT_BYTES.toHex();
    },
  });

  bench("medium (450 bytes)", {
    run: () => {
      MEDIUM_BYTES.toHex();
    },
  });

  bench("large (4096 bytes)", {
    run: () => {
      LARGE_BYTES.toHex();
    },
  });
});

suite("Uint8Array.fromHex", () => {
  bench("short (10 chars)", {
    run: () => {
      Uint8Array.fromHex(SHORT_HEX);
    },
  });

  bench("medium (900 chars)", {
    run: () => {
      Uint8Array.fromHex(MEDIUM_HEX);
    },
  });

  bench("large (8192 chars)", {
    run: () => {
      Uint8Array.fromHex(LARGE_HEX);
    },
  });
});

suite("Uint8Array setFrom* (in-place)", () => {
  bench("setFromBase64 (450 bytes)", {
    setup: () => new Uint8Array(450),
    run: (target) => {
      target.setFromBase64(MEDIUM_B64);
    },
  });

  bench("setFromHex (450 bytes)", {
    setup: () => new Uint8Array(450),
    run: (target) => {
      target.setFromHex(MEDIUM_HEX);
    },
  });
});

suite("round-trip", () => {
  bench("toBase64 → fromBase64 (450 bytes)", {
    run: () => {
      Uint8Array.fromBase64(MEDIUM_BYTES.toBase64());
    },
  });

  bench("toHex → fromHex (450 bytes)", {
    run: () => {
      Uint8Array.fromHex(MEDIUM_BYTES.toHex());
    },
  });
});

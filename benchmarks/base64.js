/*---
description: Base64 encoding and decoding benchmarks (btoa/atob)
---*/

const SHORT_ASCII = "Hello, World!";
const MEDIUM_ASCII = "The quick brown fox jumps over the lazy dog. ".repeat(10);
const LATIN1 = String.fromCharCode(0x00, 0x41, 0x80, 0xC0, 0xFF, 0xA9, 0xBE, 0x7F);

const SHORT_B64 = btoa(SHORT_ASCII);
const MEDIUM_B64 = btoa(MEDIUM_ASCII);
const LATIN1_B64 = btoa(LATIN1);

suite("btoa", () => {
  bench("short ASCII (13 chars)", {
    run: () => {
      btoa(SHORT_ASCII);
    },
  });

  bench("medium ASCII (450 chars)", {
    run: () => {
      btoa(MEDIUM_ASCII);
    },
  });

  bench("Latin-1 characters", {
    run: () => {
      btoa(LATIN1);
    },
  });
});

suite("atob", () => {
  bench("short base64 (20 chars)", {
    run: () => {
      atob(SHORT_B64);
    },
  });

  bench("medium base64 (600 chars)", {
    run: () => {
      atob(MEDIUM_B64);
    },
  });

  bench("Latin-1 output", {
    run: () => {
      atob(LATIN1_B64);
    },
  });

  bench("forgiving (no padding)", {
    run: () => {
      atob("SGVsbG8");
    },
  });

  bench("with whitespace", {
    run: () => {
      atob("SGVs bG8s\nIFdv\tcmxk IQ==");
    },
  });
});

suite("round-trip", () => {
  bench("atob(btoa(short))", {
    run: () => {
      atob(btoa(SHORT_ASCII));
    },
  });

  bench("atob(btoa(medium))", {
    run: () => {
      atob(btoa(MEDIUM_ASCII));
    },
  });
});

/*---
description: Enum members can reference prior members and the enum itself
features: [enum-declaration]
---*/

test("member references prior member by name", () => {
  enum Numbers {
    One = 1,
    Two = 2,
    Three = One + Two
  }

  expect(Numbers.Three).toBe(3);
});

test("member references enum by name", () => {
  enum Flags {
    Read = 1,
    Write = 2,
    ReadWrite = Flags.Read | Flags.Write
  }

  expect(Flags.ReadWrite).toBe(3);
});

test("member references both prior members and enum", () => {
  enum Bits {
    A = 1,
    B = 2,
    C = A | Bits.B
  }

  expect(Bits.C).toBe(3);
});

test("member references multiple prior members", () => {
  enum Perms {
    None = 0,
    Read = 4,
    Write = 2,
    Execute = 1,
    All = Read | Write | Execute
  }

  expect(Perms.All).toBe(7);
});

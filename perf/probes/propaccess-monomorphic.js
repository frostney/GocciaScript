__gocciaRegisterProbe({
  name: "propaccess-monomorphic",
  run: (innerIterations) => {
    const o = { a: 1, b: 2, c: 3, d: 4 };
    let s = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      o.a = o.a + 1;
      s = s + o.a + o.b + o.c + o.d;
    }
    return s | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});

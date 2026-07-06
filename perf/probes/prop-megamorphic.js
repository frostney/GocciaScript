__gocciaRegisterProbe({
  name: "prop-megamorphic",
  run: (innerIterations) => {
    const values = [];
    for (let i = 0; i < 32; i = i + 1) {
      const o = { x: i, y: i * 2 };
      o["pad" + i] = i;
      values.push(o);
    }
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      const o = values[i & 31];
      total = total + o.x + o.y;
    }
    return total | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});

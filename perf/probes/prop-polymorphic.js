__gocciaRegisterProbe({
  name: "prop-polymorphic",
  run: (innerIterations) => {
    const values = [
      { x: 1, y: 2 },
      { y: 3, x: 4 },
      { x: 5, z: 1, y: 6 },
      { z: 2, x: 7, y: 8 },
    ];
    let total = 0;
    for (let i = 0; i < innerIterations; i = i + 1) {
      const o = values[i & 3];
      total = total + o.x + o.y;
    }
    return total | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});

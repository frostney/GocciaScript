__gocciaRegisterProbe({
  name: "nbody-minimal",
  run: (innerIterations) => {
    const bodies = [];
    for (let i = 0; i < 5; i = i + 1) {
      bodies.push({
        x: i + 1,
        y: i * 2 + 1,
        z: i * 0.5 + 1,
        vx: 0.1,
        vy: 0.2,
        vz: 0.05,
        m: 1 + i * 0.1,
      });
    }
    let e = 0;
    for (let step = 0; step < innerIterations; step = step + 1) {
      for (let i = 0; i < bodies.length; i = i + 1) {
        const a = bodies[i];
        for (let j = i + 1; j < bodies.length; j = j + 1) {
          const b = bodies[j];
          const dx = a.x - b.x;
          const dy = a.y - b.y;
          const dz = a.z - b.z;
          const d2 = dx * dx + dy * dy + dz * dz + 0.01;
          const inv = 1 / Math.sqrt(d2);
          e = e + (a.m * b.m) * inv;
          a.vx = a.vx - dx * inv * 0.0001;
          b.vx = b.vx + dx * inv * 0.0001;
        }
        a.x = a.x + a.vx * 0.001;
        a.y = a.y + a.vy * 0.001;
        a.z = a.z + a.vz * 0.001;
      }
    }
    return e | 0;
  },
  verify: (checksum) => checksum === (checksum | 0),
});

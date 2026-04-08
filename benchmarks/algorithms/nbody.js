/*---
description: N-body simulation benchmarks — adapted from the Computer Language Benchmarks Game
---*/

const PI = Math.PI;
const SOLAR_MASS = 4 * PI * PI;
const DAYS_PER_YEAR = 365.24;

const createBody = (x, y, z, vx, vy, vz, mass) => ({
  x, y, z, vx, vy, vz, mass,
});

const createSolarSystem = () => [
  // Sun
  createBody(0, 0, 0, 0, 0, 0, SOLAR_MASS),
  // Jupiter
  createBody(
    4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
    1.66007664274403694e-03 * DAYS_PER_YEAR, 7.69901118419740425e-03 * DAYS_PER_YEAR, -6.90460016972063023e-05 * DAYS_PER_YEAR,
    9.54791938424326609e-04 * SOLAR_MASS
  ),
  // Saturn
  createBody(
    8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
    -2.76742510726862411e-03 * DAYS_PER_YEAR, 4.99852801234917238e-03 * DAYS_PER_YEAR, 2.30417297573763929e-05 * DAYS_PER_YEAR,
    2.85885980666130812e-04 * SOLAR_MASS
  ),
  // Uranus
  createBody(
    1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
    2.96460137564761618e-03 * DAYS_PER_YEAR, 2.37847173959480950e-03 * DAYS_PER_YEAR, -2.96589568540237556e-05 * DAYS_PER_YEAR,
    4.36624404335156298e-05 * SOLAR_MASS
  ),
  // Neptune
  createBody(
    1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
    2.68067772490389322e-03 * DAYS_PER_YEAR, 1.62824170038242295e-03 * DAYS_PER_YEAR, -9.51592254519715870e-05 * DAYS_PER_YEAR,
    5.15138902046611451e-05 * SOLAR_MASS
  ),
];

const offsetMomentum = (bodies) => {
  let px = 0;
  let py = 0;
  let pz = 0;
  for (const b of bodies) {
    px = px + b.vx * b.mass;
    py = py + b.vy * b.mass;
    pz = pz + b.vz * b.mass;
  }
  const sun = bodies[0];
  sun.vx = -px / SOLAR_MASS;
  sun.vy = -py / SOLAR_MASS;
  sun.vz = -pz / SOLAR_MASS;
};

const energy = (bodies) => {
  let e = 0;
  for (const [i, bi] of bodies.entries()) {
    e = e + 0.5 * bi.mass * (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz);
    for (const bj of bodies.slice(i + 1)) {
      const dx = bi.x - bj.x;
      const dy = bi.y - bj.y;
      const dz = bi.z - bj.z;
      const dist = Math.sqrt(dx * dx + dy * dy + dz * dz);
      e = e - (bi.mass * bj.mass) / dist;
    }
  }
  return e;
};

const advance = (bodies, dt) => {
  for (const [i, bi] of bodies.entries()) {
    for (const bj of bodies.slice(i + 1)) {
      const dx = bi.x - bj.x;
      const dy = bi.y - bj.y;
      const dz = bi.z - bj.z;
      const distSq = dx * dx + dy * dy + dz * dz;
      const dist = Math.sqrt(distSq);
      const mag = dt / (distSq * dist);
      bi.vx = bi.vx - dx * bj.mass * mag;
      bi.vy = bi.vy - dy * bj.mass * mag;
      bi.vz = bi.vz - dz * bj.mass * mag;
      bj.vx = bj.vx + dx * bi.mass * mag;
      bj.vy = bj.vy + dy * bi.mass * mag;
      bj.vz = bj.vz + dz * bi.mass * mag;
    }
  }
  for (const b of bodies) {
    b.x = b.x + dt * b.vx;
    b.y = b.y + dt * b.vy;
    b.z = b.z + dt * b.vz;
  }
};

const simulate = (steps, dt) => {
  const bodies = createSolarSystem();
  offsetMomentum(bodies);
  const range = {
    [Symbol.iterator]() {
      let i = 0;
      return {
        next() {
          if (i < steps) { i = i + 1; return { value: i, done: false }; }
          return { value: undefined, done: true };
        }
      };
    }
  };
  for (const _ of range) {
    advance(bodies, dt);
  }
  return energy(bodies);
};

suite("n-body simulation", () => {
  bench("250 steps", {
    run: () => {
      simulate(250, 0.01);
    },
  });

  bench("500 steps", {
    run: () => {
      simulate(500, 0.01);
    },
  });

  bench("1000 steps", {
    run: () => {
      simulate(1000, 0.01);
    },
  });

  bench("energy computation", {
    setup: () => {
      const bodies = createSolarSystem();
      offsetMomentum(bodies);
      return bodies;
    },
    run: (bodies) => {
      energy(bodies);
    },
  });
});

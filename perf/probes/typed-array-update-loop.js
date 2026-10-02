// Structure-of-arrays sprite update: a for...of loop over indices that reads
// and writes Float64Array and Float32Array elements, compares against
// function-level const bounds, and reads top-level const offsets per element.
const STRIDE = 16;
const X_OFFSET = 3;
const Y_OFFSET = 7;
const SPRITES = 10000;

const createState = () => {
  const state = {
    x: new Float64Array(SPRITES),
    y: new Float64Array(SPRITES),
    velocityX: new Float64Array(SPRITES),
    velocityY: new Float64Array(SPRITES),
    transforms: new Float32Array(SPRITES * STRIDE),
    indices: [],
  };
  let seed = 1234;
  const next = () => {
    seed = (seed * 1103515245 + 12345) % 2147483648;
    return (seed % 501) - 250;
  };
  for (let index = 0; index < SPRITES; index = index + 1) {
    state.x[index] = 640;
    state.y[index] = 360;
    state.velocityX[index] = next() / 60;
    state.velocityY[index] = next() / 60;
    state.indices.push(index);
  }
  return state;
};

const update = (state, deltaScale, width, height, screenWidth, screenHeight) => {
  const minimumX = -width / 2;
  const maximumX = screenWidth - width / 2;
  const minimumY = 40 - height / 2;
  const maximumY = screenHeight - height / 2;
  const xValues = state.x;
  const yValues = state.y;
  const velocityXValues = state.velocityX;
  const velocityYValues = state.velocityY;
  const transforms = state.transforms;

  for (const index of state.indices) {
    const x = xValues[index] + velocityXValues[index] * deltaScale;
    const y = yValues[index] + velocityYValues[index] * deltaScale;

    xValues[index] = x;
    yValues[index] = y;
    if (x > maximumX || x < minimumX) velocityXValues[index] *= -1;
    if (y > maximumY || y < minimumY) velocityYValues[index] *= -1;

    const transformOffset = index * STRIDE;
    transforms[transformOffset + X_OFFSET] = x;
    transforms[transformOffset + Y_OFFSET] = y;
  }
};

__gocciaRegisterProbe({
  name: "typed-array-update-loop",
  run: (innerIterations) => {
    const state = createState();
    for (let frame = 0; frame < innerIterations; frame = frame + 1) {
      update(state, 1, 32, 32, 1280, 720);
    }
    let checksum = 0;
    for (let index = 0; index < SPRITES; index = index + 1) {
      checksum = (checksum + Math.round(state.x[index] * 16) +
        Math.round(state.transforms[index * STRIDE + Y_OFFSET] * 16) +
        Math.round(state.velocityY[index] * 60)) | 0;
    }
    return checksum;
  },
  // The default run's checksum was computed by Node.js; other sizes only
  // check that the result is an integer.
  verify: (checksum, innerIterations) =>
    innerIterations === 40 ? checksum === 159794152 : checksum === (checksum | 0),
});

/*---
description: A stack trace renders at most 100 frames and counts the rest in a final line
features: [Error, stack, stack-depth-limit]
---*/

// The engine's stack trace frame limit (STACK_TRACE_FRAME_LIMIT).
const LIMIT = 100;

const framesOf = (error) => error.stack.split("\n").slice(1);

// Calls `create` below `depth` + 1 frames of makeAt. No call here or in the
// tests is in tail position, so a proper tail call never drops a frame.
const makeAt = (depth, create) => {
  const error = depth === 0 ? create() : makeAt(depth - 1, create);
  return error;
};

// The frames of the test function that calls this and of what runs it. The
// Error constructor's own frame is not part of a trace.
const callerFrames = () => framesOf(new Error("caller")).length - 1;

// The makeAt depth at which a one-frame `create`, with makeAt called from the
// test function, yields a trace of exactly `frames` frames.
const depthFor = (frames, base) => frames - base - 2;

test("a trace of exactly the limit renders every frame and no marker", () => {
  const base = callerFrames();
  const frames = framesOf(makeAt(depthFor(LIMIT, base), () => new Error("at limit")));
  expect(frames.length).toBe(LIMIT);
  expect(frames.every((line) => line.startsWith("    at "))).toBe(true);
});

test("one frame past the limit ends with '... 1 more frame'", () => {
  const base = callerFrames();
  const frames = framesOf(makeAt(depthFor(LIMIT + 1, base), () => new Error("past limit")));
  expect(frames.length).toBe(LIMIT + 1);
  expect(frames[LIMIT]).toBe("    ... 1 more frame");
  expect(frames.slice(0, LIMIT).every((line) => line.startsWith("    at "))).toBe(true);
});

test("a deeper trace keeps the innermost frames and counts the rest", () => {
  const base = callerFrames();
  const error = makeAt(depthFor(LIMIT + 250, base), () => new Error("deep"));
  const lines = error.stack.split("\n");
  expect(lines[0]).toBe("Error: deep");
  expect(lines.length).toBe(1 + LIMIT + 1);
  expect(lines[LIMIT + 1]).toBe("    ... 250 more frames");
  // The innermost frames are the ones kept: the error's creator, then makeAt.
  expect(lines[1].includes("makeAt")).toBe(false);
  expect(lines.slice(2, LIMIT + 1).every((line) => line.startsWith("    at makeAt ("))).toBe(true);
});

test("errors the engine throws are truncated the same way", () => {
  const base = callerFrames();
  const read = (target) => target.property;
  const create = () => {
    const value = read(null);
    return value;
  };
  let caught;
  try {
    // read is one frame more than a one-frame create.
    makeAt(depthFor(LIMIT + 40, base) - 1, create);
  } catch (e) {
    caught = e;
  }
  expect(caught instanceof TypeError).toBe(true);
  const frames = framesOf(caught);
  expect(frames.length).toBe(LIMIT + 1);
  expect(frames[LIMIT]).toBe("    ... 40 more frames");
});

test("AggregateError traces are truncated the same way", () => {
  const base = callerFrames();
  const frames = framesOf(makeAt(depthFor(LIMIT + 5, base), () => new AggregateError([], "many")));
  expect(frames.length).toBe(LIMIT + 1);
  expect(frames[LIMIT]).toBe("    ... 5 more frames");
});

test("a stack-overflow RangeError under the default --max-stack is truncated", () => {
  const base = callerFrames();
  let depth = 0;
  const recurse = () => {
    depth++;
    recurse();
  };
  let caught;
  try {
    recurse();
  } catch (e) {
    caught = e;
  }
  expect(caught instanceof RangeError).toBe(true);
  // The test runner's default --max-stack of 2,200 is far past the limit.
  expect(depth > LIMIT).toBe(true);
  const lines = caught.stack.split("\n");
  expect(lines[0]).toBe("RangeError: Maximum call stack size exceeded");
  expect(lines.length).toBe(1 + LIMIT + 1);
  expect(lines.slice(1, LIMIT + 1).every((line) => line.startsWith("    at recurse ("))).toBe(true);
  // Every recurse frame and the test function's frames, less the ones
  // rendered. The interpreter also lists the refused call, whose body never
  // ran, so its count is one higher.
  const total = depth + base;
  expect([
    `    ... ${total - LIMIT} more frames`,
    `    ... ${total + 1 - LIMIT} more frames`,
  ].includes(lines[LIMIT + 1])).toBe(true);
});

/*---
description: >
  A request the memory limit refused is collected for again once the program
  has dropped what it held: the retry succeeds without a call to Goccia.gc(),
  and so does a smaller request in between.
features: [Goccia.gc]
---*/

const CHUNK = 8 * 1024 * 1024;

const fill = () => {
  const held = [];
  let count = 0;
  try {
    for (const _ of Array.from({ length: Math.ceil(Goccia.gc.maxBytes / CHUNK) + 1 })) {
      const buffer = new ArrayBuffer(CHUNK);
      held.push(buffer);
      count++;
    }
  } catch (error) {
    return { count, name: error.name };
  }
  return { count, name: "none" };
};

const retry = (size) => {
  try {
    new ArrayBuffer(size);
    return "ok";
  } catch (error) {
    return error.name;
  }
};

// The collector's record of the refusal is made inside fill(); everything fill
// held is unreachable when it returns. Each outcome is read here at the top
// level of the script, where the same sequence a program runs is observed.
Goccia.gc();
const filled = fill();
const firstRetry = retry(CHUNK);
const smallRequest = retry(100);
const secondRetry = retry(CHUNK);

describe("a request the memory limit refused", () => {
  test("is refused while the program holds what filled the heap", () => {
    expect(filled.name).toBe("RangeError");
    expect(filled.count).toBeGreaterThan(1);
  });

  test("fits on the first retry once the program has dropped it", () => {
    expect(firstRetry).toBe("ok");
  });

  test("fits on the retries after it, and so does a smaller request", () => {
    expect(smallRequest).toBe("ok");
    expect(secondRetry).toBe("ok");
  });
});

describe("a request the memory limit keeps refusing", () => {
  test("is refused on every attempt while the heap stays full", () => {
    const held = [];
    let refused = 0;
    try {
      for (const _ of Array.from({ length: Math.ceil(Goccia.gc.maxBytes / CHUNK) + 1 })) {
        held.push(new ArrayBuffer(CHUNK));
      }
    } catch (error) {
      refused++;
    }
    for (const _ of Array.from({ length: 50 })) {
      if (retry(CHUNK) !== "ok") {
        refused++;
      }
    }
    expect(refused).toBe(51);
    expect(held.length).toBeGreaterThan(1);
  });
});

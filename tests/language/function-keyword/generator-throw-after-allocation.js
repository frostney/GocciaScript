/*---
description: a generator whose body allocates and then throws is done afterwards, however many times it runs (compiled or not)
features: [generators]
---*/

// ES2026 27.5.3.3 GeneratorResume: a generator whose body completes abruptly
// is completed, so its next() returns { done: true }. Called often enough the
// body is compiled; the body allocates enough to run collections before it
// throws, which once left the generator marked as still running ("Generator
// is already running") on the compiled path.
function* allocatesThenThrows(n) {
  let total = 0;
  for (const i of Array.from({ length: n }, (_, k) => k)) {
    const o = { i, a: [i, i, i] };
    total += o.a.length;
  }
  if (n > 0) throw new Error("boom");
  yield total;
}

const consume = (n) => {
  const g = allocatesThenThrows(n);
  let caught = "";
  try {
    for (const _ of g) caught += "value";
  } catch (error) {
    caught += error.message;
  }
  let after;
  try {
    after = JSON.stringify(g.next());
  } catch (error) {
    after = "threw " + error.message;
  }
  return caught + " " + after;
};

describe("a generator that throws after allocating", () => {
  test("is done after the throw, every time", () => {
    for (const _ of Array.from({ length: 30 })) consume(0);
    const seen = new Set();
    for (const _ of Array.from({ length: 30 })) seen.add(consume(20000));
    expect([...seen]).toEqual(['boom {"done":true}']);
  });
});

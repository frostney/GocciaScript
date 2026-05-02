/*---
description: |
  Per ES2024 §7.4.2 GetIteratorDirect, an iteratorRecord's
  [[NextMethod]] is captured ONCE at iterator acquisition; §7.4.5
  IteratorStep then calls that captured reference.  Mid-iteration
  mutation of `iterator.next` must therefore have no effect on
  subsequent next() calls — the captured method keeps being called.

  Guards against a regression where IterableToArray /
  TryIterableToArray (Goccia.VM.pas) and TGocciaGenericIteratorValue
  (Goccia.Values.Iterator.Generic.pas) re-resolved
  `iterator.GetProperty('next')` on every loop turn, picking up the
  mutated reference and diverging from the spec.
features: [iterators]
---*/

test("array spread captures next once (mid-iteration mutation ignored)", () => {
  let firstCalls = 0;
  let secondCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      const self = {
        next: () => {
          firstCalls++;
          if (firstCalls === 1) {
            self.next = () => {
              secondCalls++;
              return { done: true };
            };
            return { done: false, value: "first" };
          }
          return { done: true };
        }
      };
      return self;
    }
  };

  const result = [...iter];
  expect(result).toEqual(["first"]);
  // Captured next called twice (once for "first", once that returns done:true).
  expect(firstCalls).toBe(2);
  // Replacement never invoked because next was captured before the
  // mutation.
  expect(secondCalls).toBe(0);
});

test("array destructuring captures next once", () => {
  let firstCalls = 0;
  let secondCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      const self = {
        next: () => {
          firstCalls++;
          if (firstCalls === 1) {
            self.next = () => { secondCalls++; return { done: true }; };
            return { done: false, value: 42 };
          }
          return { done: true };
        }
      };
      return self;
    }
  };

  const [a, b] = iter;
  expect(a).toBe(42);
  expect(b).toBeUndefined();
  expect(firstCalls).toBe(2);
  expect(secondCalls).toBe(0);
});

test("for-of captures next once", () => {
  let firstCalls = 0;
  let secondCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      const self = {
        next: () => {
          firstCalls++;
          if (firstCalls === 1) {
            self.next = () => { secondCalls++; return { done: true }; };
            return { done: false, value: 7 };
          }
          return { done: true };
        }
      };
      return self;
    }
  };

  const seen = [];
  for (const x of iter) seen.push(x);
  expect(seen).toEqual([7]);
  expect(firstCalls).toBe(2);
  expect(secondCalls).toBe(0);
});

test("non-callable next throws TypeError, not silent termination", () => {
  // ES2024 §7.4.5 IteratorStep: a missing/non-callable [[NextMethod]]
  // is a TypeError, not silent break.  Validated at iterator
  // acquisition (GetIteratorDirect) so this throws synchronously.
  const iter = {
    [Symbol.iterator]() {
      return { next: 42 }; // non-callable
    }
  };
  expect(() => [...iter]).toThrow(TypeError);
  expect(() => { const [x] = iter; }).toThrow(TypeError);
});

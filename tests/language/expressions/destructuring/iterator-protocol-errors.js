/*---
description: |
  Iterator protocol errors must surface with iterator-specific
  messaging, not the generic "object is not iterable" — the outer
  source IS iterable (its [@@iterator] returned an object), the
  iterator returned just doesn't satisfy the protocol.

  Also covers the abrupt-completion IteratorClose path: when user
  next() throws, iter.return() must run best-effort before the
  original error propagates (ES2024 §7.4.10 step 5).

  Guards against:
    - GetIteratorValue (Goccia.VM.pas) previously falling through to
      SErrorNotIterable when the inner next was non-callable.
    - TryIterableToArray (Goccia.VM.pas) previously not invoking
      iter.return() when DirectNext or NextMethod.Call threw.
features: [iterators]
---*/

test("iterator with non-callable next throws TypeError", () => {
  const iter = {
    [Symbol.iterator]() {
      return { next: 42 };
    }
  };
  expect(() => [...iter]).toThrow(TypeError);
});

test("iterator with undefined next throws TypeError", () => {
  const iter = {
    [Symbol.iterator]() {
      return { next: undefined };
    }
  };
  expect(() => [...iter]).toThrow(TypeError);
});

test("[@@iterator]() returning null throws TypeError", () => {
  const iter = {
    [Symbol.iterator]() {
      return null;
    }
  };
  expect(() => [...iter]).toThrow(TypeError);
});

test("destructuring with user next() that throws closes the iterator (abrupt completion)", () => {
  let returnCalled = 0;
  const iter = {
    [Symbol.iterator]() {
      let calls = 0;
      return {
        next: () => {
          calls = calls + 1;
          if (calls === 1) return { done: false, value: "first" };
          throw new Error("user-next-threw");
        },
        return: () => {
          returnCalled = returnCalled + 1;
          return { done: true };
        }
      };
    }
  };

  let caught = null;
  try {
    const [a, b, c] = iter;
  } catch (e) {
    caught = e;
  }
  expect(caught instanceof Error).toBe(true);
  expect(caught.message).toBe("user-next-threw");
  expect(returnCalled).toBe(1);
});

test("destructuring with primitive IteratorResult throws TypeError + closes iterator", () => {
  let returnCalled = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next: () => 42, // primitive — protocol violation
        return: () => {
          returnCalled++;
          return { done: true };
        }
      };
    }
  };
  let caught = null;
  try {
    const [a] = iter;
  } catch (e) {
    caught = e;
  }
  expect(caught instanceof TypeError).toBe(true);
  expect(returnCalled).toBe(1);
});

test("missing IteratorResult.value normalizes to undefined in rest pattern", () => {
  // §7.4.4 IteratorValue: a missing `value` property yields undefined.
  const iter = {
    [Symbol.iterator]() {
      let i = 0;
      return {
        next: () => {
          const cur = i;
          i = i + 1;
          if (cur < 2) return { done: false }; // no `value`
          return { done: true };
        }
      };
    }
  };
  const [...rest] = iter;
  expect(rest.length).toBe(2);
  expect(rest[0]).toBeUndefined();
  expect(rest[1]).toBeUndefined();
});

test("missing IteratorResult.value normalizes to undefined for fixed-position binding", () => {
  let i = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next: () => {
          const cur = i;
          i = i + 1;
          if (cur < 2) return { done: false }; // no `value`
          return { done: true };
        }
      };
    }
  };
  const [a, b] = iter;
  expect(a).toBeUndefined();
  expect(b).toBeUndefined();
});

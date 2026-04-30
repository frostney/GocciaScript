/*---
description: Array destructuring stops iterating after consuming the bound
  number of elements and calls iter.return() (IteratorClose).  Without this,
  destructuring against an iterator that returns done:false indefinitely
  would allocate unboundedly.  ES2024 §8.5.3 IteratorBindingInitialization.
features: [destructuring, iterators]
---*/

test("[a, b] consumes exactly two elements then closes the iterator", () => {
  let nextCalls = 0;
  let returnCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next() { nextCalls++; return { value: nextCalls, done: false }; },
        return() { returnCalls++; return { value: undefined, done: true }; }
      };
    }
  };

  const [a, b] = iter;

  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(nextCalls).toBe(2);
  expect(returnCalls).toBe(1);
});

test("[x] consumes one element then closes the iterator (single binding)", () => {
  let nextCalls = 0;
  let returnCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next() { nextCalls++; return { value: 'v', done: false }; },
        return() { returnCalls++; return { value: undefined, done: true }; }
      };
    }
  };

  const [x] = iter;

  expect(x).toBe('v');
  expect(nextCalls).toBe(1);
  expect(returnCalls).toBe(1);
});

test("[a, b, c] with infinite iterator does not OOM and closes after three", () => {
  // Without the iterator-close fix, this would allocate unboundedly because
  // iterator.next() always returns done:false.
  let nextCalls = 0;
  let returnCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next() { nextCalls++; return { value: nextCalls * 10, done: false }; },
        return() { returnCalls++; return { value: undefined, done: true }; }
      };
    }
  };

  const [a, b, c] = iter;

  expect(a).toBe(10);
  expect(b).toBe(20);
  expect(c).toBe(30);
  expect(nextCalls).toBe(3);
  expect(returnCalls).toBe(1);
});

test("[a, b, ...rest] drains the iterator (rest pattern present)", () => {
  // With a rest pattern, the spec requires consuming the entire iterator.
  // No early IteratorClose call is needed because the iterator naturally
  // reaches done:true.
  let nextCalls = 0;
  let returnCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      let i = 0;
      return {
        next() {
          nextCalls++;
          i++;
          if (i > 5) return { value: undefined, done: true };
          return { value: i, done: false };
        },
        return() { returnCalls++; return { value: undefined, done: true }; }
      };
    }
  };

  const [a, b, ...rest] = iter;

  expect(a).toBe(1);
  expect(b).toBe(2);
  expect(rest.length).toBe(3);
  expect(rest[0]).toBe(3);
  expect(rest[1]).toBe(4);
  expect(rest[2]).toBe(5);
  expect(nextCalls).toBe(6);  // 5 values + 1 done
  expect(returnCalls).toBe(0);
});

test("[a, , c] holes still advance the iterator and the trailing slot is reached", () => {
  let nextCalls = 0;
  let returnCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next() { nextCalls++; return { value: nextCalls, done: false }; },
        return() { returnCalls++; return { value: undefined, done: true }; }
      };
    }
  };

  const [a, , c] = iter;

  expect(a).toBe(1);
  expect(c).toBe(3);
  expect(nextCalls).toBe(3);
  expect(returnCalls).toBe(1);
});

test("destructuring an array literal does not call return() (Array iterator finishes naturally)", () => {
  // Sanity check: when the source is an actual array, our internal
  // TGocciaArrayIteratorValue is used and naturally terminates at the end
  // of the array.  We don't add a custom return() to verify, but we do
  // verify destructuring still works after the fix.
  const [a, b] = [10, 20, 30];
  expect(a).toBe(10);
  expect(b).toBe(20);
});

test("nested destructuring with bound inner pattern", () => {
  let outerNext = 0;
  let outerReturn = 0;
  let innerNext = 0;
  let innerReturn = 0;
  const inner = {
    [Symbol.iterator]() {
      return {
        next() { innerNext++; return { value: innerNext * 100, done: false }; },
        return() { innerReturn++; return { value: undefined, done: true }; }
      };
    }
  };
  const outer = {
    [Symbol.iterator]() {
      let yielded = false;
      return {
        next() {
          outerNext++;
          if (yielded) return { value: undefined, done: true };
          yielded = true;
          return { value: inner, done: false };
        },
        return() { outerReturn++; return { value: undefined, done: true }; }
      };
    }
  };

  const [[x, y]] = outer;

  expect(x).toBe(100);
  expect(y).toBe(200);
  expect(innerNext).toBe(2);
  expect(innerReturn).toBe(1);
  expect(outerNext).toBe(1);
  expect(outerReturn).toBe(1);
});

test("function parameter destructuring closes the iterator", () => {
  let nextCalls = 0;
  let returnCalls = 0;
  const iter = {
    [Symbol.iterator]() {
      return {
        next() { nextCalls++; return { value: nextCalls, done: false }; },
        return() { returnCalls++; return { value: undefined, done: true }; }
      };
    }
  };

  const f = ([a, b]) => [a, b];
  const result = f(iter);

  expect(result[0]).toBe(1);
  expect(result[1]).toBe(2);
  expect(nextCalls).toBe(2);
  expect(returnCalls).toBe(1);
});

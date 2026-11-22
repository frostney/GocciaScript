// for await over a sync iterable, and yield* from an async generator into one, step through the iterator's
// async-from-sync wrapper (27.1.6). Its next, return and throw make a promise capability first: a collection that
// allocation runs moves the young wrapper and the value passed in, so both are read again after it (copies taken
// before were stale: next called the wrong object's method, "not an iterator of this kind").
let bad = 0;
function check(what, got, want) { if (got !== want) { bad++; console.log('FAIL', what, got, want); } }

const arr = [1, 2, 3, 4, 5];
async function sumAll() {
  let sum = 0;
  for await (const x of arr) sum += x;
  return sum;
}

// return(): a sync iterator whose return method allocates and records what it was handed
let returned = 0;
function closing() {
  let i = 0;
  return {
    [Symbol.iterator]() { return this; },
    next() { i++; return { value: { i }, done: i > 3 }; },
    return(v) { returned++; return { value: [v, { pad: new Array(8).fill(i) }], done: true }; },
  };
}
async function breakEarly() {
  for await (const x of closing()) if (x.i === 2) break;
}

// throw(): yield* hands the thrown value to the sync iterator's throw method
function catching() {
  return {
    [Symbol.iterator]() { return this; },
    next() { return { value: { n: 1 }, done: false }; },
    throw(e) { return { value: e.tag, done: true }; },
  };
}
async function* delegate() { return yield* catching(); }
async function throwInto(k) {
  const it = delegate();
  await it.next();
  const r = await it.throw({ tag: 'thrown' + k, pad: new Array(16).fill(k) });
  return r.value;
}

(async () => {
  for (let k = 0; k < 400; k++) {
    try {
      check('sum ' + k, await sumAll(), 15);
      await breakEarly();
      check('throw ' + k, await throwInto(k), 'thrown' + k);
    } catch (e) {
      bad++;
      console.log('FAIL at', k, String(e));
      break;
    }
  }
  check('returns', returned, bad === 0 ? 400 : returned);
  console.log(bad === 0 ? 'ok' : 'bad ' + bad);
})();

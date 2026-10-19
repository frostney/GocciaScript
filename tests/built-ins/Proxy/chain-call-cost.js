/*---
description: A call through a chain of Proxies costs time linear in the chain's depth, not quadratic
features: [Proxy]
---*/

// ES2026 §10.5.15 ProxyCreate steps 5-6 fix whether a Proxy has [[Call]] and
// [[Construct]] when it is created, so a call through n Proxies is n forwards
// (§10.5.12) and nothing re-walks the chain at each one (#1799). The check
// compares two depths on the same machine instead of an absolute time: a call
// 8 times deeper costs about 8 times as much when the cost is linear, and
// measured over 40 times as much (200 and 1,600 deep) when every link walked the rest of the chain.
const nest = (depth, target) => {
  let proxy = target;
  for (const _ of Array.from({ length: depth })) {
    proxy = new Proxy(proxy, {});
  }
  return proxy;
};

// The fastest of several rounds, per call, so a collection or a busy machine
// during one round does not decide the result.
const perCall = (fn) => {
  const calls = 10;
  let best = Infinity;
  for (const _ of Array.from({ length: 7 })) {
    const start = performance.now();
    for (const __ of Array.from({ length: calls })) fn();
    best = Math.min(best, (performance.now() - start) / calls);
  }
  return best;
};

describe.runIf(typeof Goccia !== "undefined")("a call through a chain of Proxies", () => {
  test("costs at most about twice the linear growth when the chain is 8 times deeper", () => {
    const shallow = nest(200, () => 1);
    const deep = nest(1600, () => 1);
    expect(shallow()).toBe(1);
    expect(deep()).toBe(1);
    const ratio = perCall(deep) / perCall(shallow);
    expect(ratio).toBeLessThan(16);
  });
});

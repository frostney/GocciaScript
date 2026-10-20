/*---
description: Calls through Proxy chains at two depths
---*/

import { bench, group } from "goccia:microbench";

const nest = (depth, target) => {
  let proxy = target;
  for (const _ of Array.from({ length: depth })) {
    proxy = new Proxy(proxy, {});
  }
  return proxy;
};

// Timing is advisory (ADR 0076). Native capability-query counts guard against
// re-walking the target chain at every forwarding step without a clock ratio.
group("Proxy chain calls", () => {
  const shallow = nest(200, () => 1);
  const deep = nest(1600, () => 1);

  bench("call through 200 Proxies", () => shallow());
  bench("call through 1600 Proxies", () => deep());
});

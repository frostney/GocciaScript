const order =
  globalThis.__gocciaDeferredTLAOrder ||
  (globalThis.__gocciaDeferredTLAOrder = []);

order.push("tla start");
await Promise.resolve(0);
order.push("tla end");

export const value = 23;

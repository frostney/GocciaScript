import { resolveFirst, resolveThird, second } from "./deferred-async-cycle-promises.js";
import "./deferred-async-cycle-observer.js";

await Promise.resolve();
resolveFirst();

await second;
resolveThird();

export const foo = 1;

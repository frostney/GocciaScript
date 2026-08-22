// Second file of the ShadowRealm runtime-module warm-up regression (see
// async-hooks-import.js). A different runtime module (goccia:csv) confirms the
// fault was general to host-module warm-up under --unsafe-shadowrealm, not
// specific to node:async_hooks. Two files under one unsafe-shadowrealm config
// are what forced the crashing collection between engines on a single worker.

import { parse } from "goccia:csv";

describe("ShadowRealm + goccia:csv warm-up", () => {
  test("imports the runtime module without faulting", () => {
    expect(typeof parse).toBe("function");
    const rows = parse("name,age\nAlice,30", { headers: false });
    expect(rows.length).toBe(2);
    expect(rows[0][0]).toBe("name");
    expect(rows[1][1]).toBe("30");
  });

  test("ShadowRealm still constructs a child realm", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("'x' + 'y'")).toBe("xy");
  });
});

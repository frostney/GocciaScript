export {
  default as rawConfig,
} from "../modules/helpers/config.json" with { type: "text" };

describe("source-type=module entry re-export attributes", () => {
  test("preserves attributed re-exports on the entry module namespace", async () => {
    const entryNamespace = await import("./entry-re-export-attributes.js");
    expect(typeof entryNamespace.rawConfig).toBe("string");
    expect(entryNamespace.rawConfig.startsWith("{\n  \"name\": \"goccia-test\"")).toBe(true);
  });
});

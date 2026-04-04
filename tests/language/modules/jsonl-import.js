import {
  "0" as firstRecord,
  "1" as count,
  "2" as labels,
} from "./helpers/event-stream.jsonl";
import {
  "0" as blankLineFirstRecord,
  "1" as blankLineCount,
  "2" as blankLineLabels,
} from "./helpers/event-stream-with-blank-lines.jsonl";
import {
  count as reExportedCount,
  firstRecord as reExportedFirstRecord,
  labels as reExportedLabels,
} from "./helpers/jsonl-re-exporter.js";

describe("JSONL module imports", () => {
  test("imports each record by string index", () => {
    expect(firstRecord.id).toBe(1);
    expect(firstRecord.kind).toBe("alpha");
    expect(count).toBe(42);
    expect(labels.length).toBe(2);
    expect(labels[0]).toBe("x");
    expect(labels[1]).toBe("y");
  });

  test("re-exports indexed JSONL records through JavaScript modules", () => {
    expect(reExportedFirstRecord.kind).toBe("alpha");
    expect(reExportedCount).toBe(42);
    expect(reExportedLabels[1]).toBe("y");
  });

  test("ignores blank lines when importing JSONL modules", () => {
    expect(blankLineFirstRecord.id).toBe(1);
    expect(blankLineCount).toBe(42);
    expect(blankLineLabels.length).toBe(2);
    expect(blankLineLabels[0]).toBe("x");
    expect(blankLineLabels[1]).toBe("y");
  });
});

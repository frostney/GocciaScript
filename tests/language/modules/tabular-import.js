import { "0" as csvRow } from "./helpers/unicode-table.csv";
import { "0" as tsvRow } from "./helpers/unicode-table.tsv";

describe("CSV and TSV module imports", () => {
  test("preserves UTF-8 text in file-backed CSV modules", () => {
    expect(csvRow.name).toBe("Jos\u00e9");
    expect(csvRow.city).toBe("Z\u00fcrich");
    expect(csvRow.note).toBe("Caf\u00e9 d\u00e9j\u00e0 vu");
  });

  test("preserves UTF-8 text in file-backed TSV modules", () => {
    expect(tsvRow.name).toBe("Jos\u00e9");
    expect(tsvRow.city).toBe("Z\u00fcrich");
    expect(tsvRow.note).toBe("Caf\u00e9 d\u00e9j\u00e0 vu");
  });
});

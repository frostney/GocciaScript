/*---
description: TSV parse and stringify benchmarks
---*/

suite("TSV.parse", () => {
  bench("parse simple 3-column TSV", {
    run: () => {
      const result = TSV.parse("name\tage\tcity\nAlice\t30\tNYC\nBob\t25\tLA\nCharlie\t35\tChicago");
    },
  });

  bench("parse 10-row TSV", {
    setup: () => {
      const header = "id\tname\temail\tscore\tactive";
      const rows = Array.from({ length: 10 }, (_, i) =>
        i + "\tuser" + i + "\tuser" + i + "@test.com\t" + (i * 10) + "\ttrue"
      );
      return header + "\n" + rows.join("\n");
    },
    run: (tsv) => {
      const result = TSV.parse(tsv);
    },
  });

  bench("parse 100-row TSV", {
    setup: () => {
      const header = "id\tname\tvalue";
      const rows = Array.from({ length: 100 }, (_, i) =>
        i + "\titem" + i + "\t" + (i * 3.14)
      );
      return header + "\n" + rows.join("\n");
    },
    run: (tsv) => {
      const result = TSV.parse(tsv);
    },
  });

  bench("parse TSV with backslash-escaped fields", {
    setup: () => {
      const header = "path\tline";
      const rows = Array.from({ length: 20 }, (_, i) =>
        "C:\\\\Users\\\\test" + i + "\tline" + i + "\\nand more"
      );
      return header + "\n" + rows.join("\n");
    },
    run: (tsv) => {
      const result = TSV.parse(tsv);
    },
  });

  bench("parse without headers (array of arrays)", {
    setup: () => {
      return Array.from({ length: 50 }, (_, i) =>
        i + "\t" + (i * 2) + "\t" + (i * 3)
      ).join("\n");
    },
    run: (tsv) => {
      const result = TSV.parse(tsv, { headers: false });
    },
  });
});

suite("TSV.stringify", () => {
  bench("stringify array of objects", {
    setup: () => Array.from({ length: 10 }, (_, i) => ({
      id: String(i),
      name: "user" + i,
      active: "true",
    })),
    run: (data) => {
      const s = TSV.stringify(data);
    },
  });

  bench("stringify array of arrays", {
    setup: () => Array.from({ length: 50 }, (_, i) => [
      String(i), "item" + i, String(i * 2),
    ]),
    run: (data) => {
      const s = TSV.stringify(data, { headers: false });
    },
  });

  bench("stringify with values needing escaping", {
    setup: () => Array.from({ length: 10 }, (_, i) => ({
      path: "C:\\Users\\test",
      note: "line1\nline2",
      tab: "col1\tcol2",
    })),
    run: (data) => {
      const s = TSV.stringify(data);
    },
  });
});

suite("TSV roundtrip", () => {
  bench("parse then stringify", {
    setup: () => {
      const header = "id\tname\tscore";
      const rows = Array.from({ length: 20 }, (_, i) =>
        i + "\tuser" + i + "\t" + (i * 10)
      );
      return header + "\n" + rows.join("\n");
    },
    run: (tsv) => {
      const parsed = TSV.parse(tsv);
      const back = TSV.stringify(parsed);
    },
  });
});

/*---
description: TSV parse and stringify benchmarks
---*/

import { bench, group } from "goccia:microbench";
import { parse, stringify } from "goccia:tsv";

group("TSV.parse", () => {
  bench("parse simple 3-column TSV", () => {
    const result = parse("name\tage\tcity\nAlice\t30\tNYC\nBob\t25\tLA\nCharlie\t35\tChicago");
  });

  bench("parse 10-row TSV", ({ *setup() {
    const tsv = (() => {
      const header = "id\tname\temail\tscore\tactive";
      const rows = Array.from({ length: 10 }, (_, i) =>
        i + "\tuser" + i + "\tuser" + i + "@test.com\t" + (i * 10) + "\ttrue"
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(tsv);
    };
  } }).setup);

  bench("parse 100-row TSV", ({ *setup() {
    const tsv = (() => {
      const header = "id\tname\tvalue";
      const rows = Array.from({ length: 100 }, (_, i) =>
        i + "\titem" + i + "\t" + (i * 3.14)
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(tsv);
    };
  } }).setup);

  bench("parse TSV with backslash-escaped fields", ({ *setup() {
    const tsv = (() => {
      const header = "path\tline";
      const rows = Array.from({ length: 20 }, (_, i) =>
        "C:\\\\Users\\\\test" + i + "\tline" + i + "\\nand more"
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(tsv);
    };
  } }).setup);

  bench("parse without headers (array of arrays)", ({ *setup() {
    const tsv = (() => {
      return Array.from({ length: 50 }, (_, i) =>
        i + "\t" + (i * 2) + "\t" + (i * 3)
      ).join("\n");
    })();
    yield () => {
      const result = parse(tsv, { headers: false });
    };
  } }).setup);
});

group("TSV.stringify", () => {
  bench("stringify array of objects", ({ *setup() {
    const data = (() => Array.from({ length: 10 }, (_, i) => ({
      id: String(i),
      name: "user" + i,
      active: "true",
    })))();
    yield () => {
      const s = stringify(data);
    };
  } }).setup);

  bench("stringify array of arrays", ({ *setup() {
    const data = (() => Array.from({ length: 50 }, (_, i) => [
      String(i), "item" + i, String(i * 2),
    ]))();
    yield () => {
      const s = stringify(data, { headers: false });
    };
  } }).setup);

  bench("stringify with values needing escaping", ({ *setup() {
    const data = (() => Array.from({ length: 10 }, (_, i) => ({
      path: "C:\\Users\\test",
      note: "line1\nline2",
      tab: "col1\tcol2",
    })))();
    yield () => {
      const s = stringify(data);
    };
  } }).setup);
});

group("TSV roundtrip", () => {
  bench("parse then stringify", ({ *setup() {
    const tsv = (() => {
      const header = "id\tname\tscore";
      const rows = Array.from({ length: 20 }, (_, i) =>
        i + "\tuser" + i + "\t" + (i * 10)
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const parsed = parse(tsv);
      const back = stringify(parsed);
    };
  } }).setup);
});

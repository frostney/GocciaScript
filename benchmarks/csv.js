/*---
description: CSV parse and stringify benchmarks
---*/

import { bench, group } from "goccia:microbench";
import { parse, stringify } from "goccia:csv";

group("CSV.parse", () => {
  bench("parse simple 3-column CSV", () => {
    const result = parse("name,age,city\nAlice,30,NYC\nBob,25,LA\nCharlie,35,Chicago");
  });

  bench("parse 10-row CSV", ({ *setup() {
    const csv = (() => {
      const header = "id,name,email,score,active";
      const rows = Array.from({ length: 10 }, (_, i) =>
        i + ",user" + i + ",user" + i + "@test.com," + (i * 10) + ",true"
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(csv);
    };
  } }).setup);

  bench("parse 100-row CSV", ({ *setup() {
    const csv = (() => {
      const header = "id,name,value";
      const rows = Array.from({ length: 100 }, (_, i) =>
        i + ",item" + i + "," + (i * 3.14)
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(csv);
    };
  } }).setup);

  bench("parse CSV with quoted fields", () => {
    const result = parse('name,address,note\n"Smith, John","123 Main St, Apt 4","He said ""hello"""');
  });

  bench("parse without headers (array of arrays)", ({ *setup() {
    const csv = (() => {
      return Array.from({ length: 50 }, (_, i) =>
        i + "," + (i * 2) + "," + (i * 3)
      ).join("\n");
    })();
    yield () => {
      const result = parse(csv, { headers: false });
    };
  } }).setup);

  bench("parse with semicolon delimiter", ({ *setup() {
    const csv = (() => {
      const header = "nom;prenom;age";
      const rows = Array.from({ length: 20 }, (_, i) =>
        "Nom" + i + ";Prenom" + i + ";" + (20 + i)
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(csv, { delimiter: ";" });
    };
  } }).setup);
});

group("CSV.stringify", () => {
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
      text: 'value with "quotes" and, commas',
      note: "line1\nline2",
    })))();
    yield () => {
      const s = stringify(data);
    };
  } }).setup);
});

group("CSV.parse with reviver", () => {
  bench("reviver converts numbers", ({ *setup() {
    const csv = (() => {
      const header = "a,b,c";
      const rows = Array.from({ length: 20 }, (_, i) =>
        (i * 1) + "," + (i * 2) + "," + (i * 3)
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const result = parse(csv, {}, (key, value, ctx) => {
        const n = Number(value);
        return Number.isNaN(n) ? value : n;
      });
    };
  } }).setup);

  bench("reviver filters empty to null", () => {
    const result = parse("a,b,c\n1,,3\n,2,\n,,", {}, (key, value, ctx) => {
      if (!ctx.quoted && value === "") return null;
      return value;
    });
  });
});

group("CSV roundtrip", () => {
  bench("parse then stringify", ({ *setup() {
    const csv = (() => {
      const header = "id,name,score";
      const rows = Array.from({ length: 20 }, (_, i) =>
        i + ",user" + i + "," + (i * 10)
      );
      return header + "\n" + rows.join("\n");
    })();
    yield () => {
      const parsed = parse(csv);
      const back = stringify(parsed);
    };
  } }).setup);

  bench("stringify then parse", ({ *setup() {
    const data = (() => Array.from({ length: 20 }, (_, i) => ({
      id: String(i),
      name: "item" + i,
      active: i % 2 === 0 ? "true" : "false",
    })))();
    yield () => {
      const csv = stringify(data);
      const parsed = parse(csv);
    };
  } }).setup);
});

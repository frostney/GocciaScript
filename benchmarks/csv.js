/*---
description: CSV parse and stringify benchmarks
---*/

suite("CSV.parse", () => {
  bench("parse simple 3-column CSV", {
    run: () => {
      const result = CSV.parse("name,age,city\nAlice,30,NYC\nBob,25,LA\nCharlie,35,Chicago");
    },
  });

  bench("parse 10-row CSV", {
    setup: () => {
      const header = "id,name,email,score,active";
      const rows = Array.from({ length: 10 }, (_, i) =>
        i + ",user" + i + ",user" + i + "@test.com," + (i * 10) + ",true"
      );
      return header + "\n" + rows.join("\n");
    },
    run: (csv) => {
      const result = CSV.parse(csv);
    },
  });

  bench("parse 100-row CSV", {
    setup: () => {
      const header = "id,name,value";
      const rows = Array.from({ length: 100 }, (_, i) =>
        i + ",item" + i + "," + (i * 3.14)
      );
      return header + "\n" + rows.join("\n");
    },
    run: (csv) => {
      const result = CSV.parse(csv);
    },
  });

  bench("parse CSV with quoted fields", {
    run: () => {
      const result = CSV.parse('name,address,note\n"Smith, John","123 Main St, Apt 4","He said ""hello"""');
    },
  });

  bench("parse without headers (array of arrays)", {
    setup: () => {
      return Array.from({ length: 50 }, (_, i) =>
        i + "," + (i * 2) + "," + (i * 3)
      ).join("\n");
    },
    run: (csv) => {
      const result = CSV.parse(csv, { headers: false });
    },
  });

  bench("parse with semicolon delimiter", {
    setup: () => {
      const header = "nom;prenom;age";
      const rows = Array.from({ length: 20 }, (_, i) =>
        "Nom" + i + ";Prenom" + i + ";" + (20 + i)
      );
      return header + "\n" + rows.join("\n");
    },
    run: (csv) => {
      const result = CSV.parse(csv, { delimiter: ";" });
    },
  });
});

suite("CSV.stringify", () => {
  bench("stringify array of objects", {
    setup: () => Array.from({ length: 10 }, (_, i) => ({
      id: String(i),
      name: "user" + i,
      active: "true",
    })),
    run: (data) => {
      const s = CSV.stringify(data);
    },
  });

  bench("stringify array of arrays", {
    setup: () => Array.from({ length: 50 }, (_, i) => [
      String(i), "item" + i, String(i * 2),
    ]),
    run: (data) => {
      const s = CSV.stringify(data, { headers: false });
    },
  });

  bench("stringify with values needing escaping", {
    setup: () => Array.from({ length: 10 }, (_, i) => ({
      text: 'value with "quotes" and, commas',
      note: "line1\nline2",
    })),
    run: (data) => {
      const s = CSV.stringify(data);
    },
  });
});

suite("CSV.parse with reviver", () => {
  bench("reviver converts numbers", {
    setup: () => {
      const header = "a,b,c";
      const rows = Array.from({ length: 20 }, (_, i) =>
        (i * 1) + "," + (i * 2) + "," + (i * 3)
      );
      return header + "\n" + rows.join("\n");
    },
    run: (csv) => {
      const result = CSV.parse(csv, {}, (key, value, ctx) => {
        const n = Number(value);
        return Number.isNaN(n) ? value : n;
      });
    },
  });

  bench("reviver filters empty to null", {
    run: () => {
      const result = CSV.parse("a,b,c\n1,,3\n,2,\n,,", {}, (key, value, ctx) => {
        if (!ctx.quoted && value === "") return null;
        return value;
      });
    },
  });
});

suite("CSV roundtrip", () => {
  bench("parse then stringify", {
    setup: () => {
      const header = "id,name,score";
      const rows = Array.from({ length: 20 }, (_, i) =>
        i + ",user" + i + "," + (i * 10)
      );
      return header + "\n" + rows.join("\n");
    },
    run: (csv) => {
      const parsed = CSV.parse(csv);
      const back = CSV.stringify(parsed);
    },
  });

  bench("stringify then parse", {
    setup: () => Array.from({ length: 20 }, (_, i) => ({
      id: String(i),
      name: "item" + i,
      active: i % 2 === 0 ? "true" : "false",
    })),
    run: (data) => {
      const csv = CSV.stringify(data);
      const parsed = CSV.parse(csv);
    },
  });
});

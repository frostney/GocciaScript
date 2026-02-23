/*---
description: JSON parse and stringify benchmarks
---*/

suite("JSON.parse", () => {
  bench("parse simple object", {
    run: () => {
      const obj = JSON.parse('{"name":"test","value":42,"active":true}');
    },
  });

  bench("parse nested object", {
    run: () => {
      const obj = JSON.parse('{"user":{"name":"Alice","address":{"city":"NYC","zip":"10001"}},"score":95}');
    },
  });

  bench("parse array of objects", {
    run: () => {
      const arr = JSON.parse('[{"id":1,"name":"a"},{"id":2,"name":"b"},{"id":3,"name":"c"},{"id":4,"name":"d"},{"id":5,"name":"e"}]');
    },
  });

  bench("parse large flat object", {
    run: () => {
      const obj = JSON.parse('{"a":1,"b":2,"c":3,"d":4,"e":5,"f":6,"g":7,"h":8,"i":9,"j":10,"k":11,"l":12}');
    },
  });

  bench("parse mixed types", {
    run: () => {
      const obj = JSON.parse('{"str":"hello","num":3.14,"bool":true,"nil":null,"arr":[1,2,3],"obj":{"x":1}}');
    },
  });
});

suite("JSON.stringify", () => {
  bench("stringify simple object", {
    run: () => {
      const s = JSON.stringify({ name: "test", value: 42, active: true });
    },
  });

  bench("stringify nested object", {
    run: () => {
      const s = JSON.stringify({
        user: { name: "Alice", address: { city: "NYC", zip: "10001" } },
        score: 95
      });
    },
  });

  bench("stringify array of objects", {
    setup: () => Array.from({ length: 10 }, (_, i) => ({ id: i, name: "item" + i })),
    run: (items) => {
      const s = JSON.stringify(items);
    },
  });

  bench("stringify mixed types", {
    run: () => {
      const s = JSON.stringify({
        str: "hello",
        num: 3.14,
        bool: true,
        nil: null,
        arr: [1, 2, 3],
        obj: { x: 1 }
      });
    },
  });
});

suite("JSON.parse with reviver", () => {
  bench("reviver doubles numbers", {
    run: () => {
      const result = JSON.parse('{"a":1,"b":2,"c":3,"d":4,"e":5}', (key, value) => {
        if (typeof value === "number") {
          return value * 2;
        }
        return value;
      });
    },
  });

  bench("reviver filters properties", {
    run: () => {
      const result = JSON.parse('{"keep":1,"drop":2,"keep2":3,"drop2":4}', (key, value) => {
        if (key.startsWith("drop")) {
          return undefined;
        }
        return value;
      });
    },
  });

  bench("reviver on nested object", {
    run: () => {
      const result = JSON.parse('{"user":{"name":"Alice","age":30},"score":95}', (key, value) => {
        if (typeof value === "string") {
          return value.toUpperCase();
        }
        return value;
      });
    },
  });

  bench("reviver on array", {
    run: () => {
      const result = JSON.parse('[1,2,3,4,5,6,7,8,9,10]', (key, value) => {
        if (typeof value === "number") {
          return value + 10;
        }
        return value;
      });
    },
  });
});

suite("JSON.stringify with replacer", () => {
  bench("replacer function doubles numbers", {
    run: () => {
      const s = JSON.stringify({ a: 1, b: 2, c: 3, d: 4, e: 5 }, (key, value) => {
        if (typeof value === "number") {
          return value * 2;
        }
        return value;
      });
    },
  });

  bench("replacer function excludes properties", {
    run: () => {
      const s = JSON.stringify({ a: 1, secret: "hidden", b: 2, password: "nope" }, (key, value) => {
        if (key === "secret" || key === "password") {
          return undefined;
        }
        return value;
      });
    },
  });

  bench("array replacer (allowlist)", {
    run: () => {
      const s = JSON.stringify({ a: 1, b: 2, c: 3, d: 4, e: 5 }, ["a", "c", "e"]);
    },
  });

  bench("stringify with 2-space indent", {
    run: () => {
      const s = JSON.stringify({ name: "test", items: [1, 2, 3], nested: { x: 1 } }, null, 2);
    },
  });

  bench("stringify with tab indent", {
    run: () => {
      const s = JSON.stringify({ name: "test", items: [1, 2, 3], nested: { x: 1 } }, null, "\t");
    },
  });
});

suite("JSON roundtrip", () => {
  bench("parse then stringify", {
    run: () => {
      const json = '{"users":[{"id":1,"name":"Alice"},{"id":2,"name":"Bob"}],"total":2}';
      const obj = JSON.parse(json);
      const back = JSON.stringify(obj);
    },
  });

  bench("stringify then parse", {
    setup: () => ({
      items: Array.from({ length: 5 }, (_, i) => ({ id: i, active: i % 2 === 0 })),
      count: 5
    }),
    run: (data) => {
      const json = JSON.stringify(data);
      const obj = JSON.parse(json);
    },
  });
});

export type OutputLine = {
  kind: "meta" | "log" | "err" | "out" | "result";
  text: string;
};

export type Example = {
  id: string;
  label: string;
  desc: string;
  code: string;
  runner?: "execute" | "test";
  ext?: "js" | "ts" | "jsx" | "tsx";
};

export const EXAMPLES: Example[] = [
  {
    id: "blank",
    label: "Blank — start from scratch",
    desc: "An empty editor. Tab completes the suggested word.",
    code: `// Write your GocciaScript here.

`,
  },
  {
    id: "types",
    ext: "ts",
    label: "Types-as-comments — TypeScript-style annotations",
    desc: "TC39 types-as-comments: TypeScript-style annotations are parsed and stripped at runtime — no separate compilation step.",
    code: `// Type annotations are allowed, but treated as comments at runtime.
// Runs directly — no transpilation step.

type Currency = "USD" | "EUR" | "GBP";

interface LineItem {
  sku: string;
  qty: number;
  unit: number;
}

const subtotal = (items: LineItem[]): number =>
  items.reduce((t, it) => t + it.qty * it.unit, 0);

const format = (amount: number, currency: Currency): string => {
  const symbols: Record<Currency, string> = {
    USD: "$", EUR: "€", GBP: "£",
  };
  return \`\${symbols[currency]}\${amount.toFixed(2)}\`;
};

const cart: LineItem[] = [
  { sku: "ESP-01", qty: 2, unit: 2.5 },
  { sku: "LAT-07", qty: 1, unit: 4.0 },
];

console.log("Items:", cart.length);
console.log("Total:", format(subtotal(cart), "EUR"));`,
  },
  {
    id: "enums",
    ext: "ts",
    label: "Enums — TC39 Stage-1 proposal",
    desc: "Enum declarations compile to frozen objects with reverse-mapping. Enums are exhaustive in a switch.",
    code: `// Enum declarations — TC39 Stage-1 proposal
enum OrderStatus {
  Pending = 0,
  Brewing = 1,
  Ready = 2,
  Delivered = 3,
}

enum PaymentMethod {
  Cash = "cash",
  Card = "card",
  Crypto = "crypto",
}

const describe = (status) => {
  switch (status) {
    case OrderStatus.Pending:   return "waiting on barista";
    case OrderStatus.Brewing:   return "in the portafilter";
    case OrderStatus.Ready:     return "ready for pickup";
    case OrderStatus.Delivered: return "enjoy!";
  }
};

console.log(OrderStatus.Brewing, "→", describe(OrderStatus.Brewing));
console.log(OrderStatus[2], "→", describe(2));
console.log("Paid with:", PaymentMethod.Card);
console.log(Object.isFrozen(OrderStatus));`,
  },
  {
    id: "generator-style",
    label: "Generators — method shorthand + yield",
    desc: "Generator methods, yield, yield*, and iterator helpers.",
    code: `// Generator method shorthand is default GocciaScript syntax.
const sequences = {
  *numbers(start, count, step = 1) {
    yield* Array.from({ length: count }, (_, i) => start + i * step);
  },

  *playlist() {
    yield "espresso";
    yield* ["latte", "cortado"];
  },
};

const evens = Iterator.from(sequences.numbers(2, 10, 2))
  .map((n) => n * n)
  .filter((n) => n % 3 !== 0)
  .take(5)
  .toArray();

console.log("First filtered squares:", evens);
const queue = [];
for (const drink of sequences.playlist()) {
  queue.push(drink);
}
console.log("Queue:", queue);

let sum = 0;
for (const n of sequences.numbers(1, 5)) {
  sum += n;
}
console.log("Range sum:", sum);`,
  },
  {
    id: "pattern-matching",
    label: "Pattern matching — match / is",
    desc: "TC39 proposal syntax for match clauses and is-pattern filters.",
    code: `// TC39 pattern matching proposal syntax.
const describe = (event) => match (event) {
  { type: "order", items: [const drink, const qty] }:
    \`order: \${qty} x \${drink}\`;
  { type: "error", reason: const reason }:
    \`error: \${reason}\`;
  { type: const type }:
    \`event: \${type}\`;
  default:
    "unknown event";
};

console.log(describe({ type: "order", items: ["espresso", 2] }));
console.log(describe({ type: "error", reason: "beans unavailable" }));
console.log(describe({ type: "tick" }));

const orders = [
  { total: 7,  customer: "Ada" },
  { total: 14, customer: "Lin" },
  { status: "draft" },
];

for (const order is { total: const total } if (total > 10) of orders) {
  console.log("Large order:", order.customer, total);
}`,
  },
  {
    id: "resources",
    label: "Resource management — using / await using",
    desc: "ES2026 explicit resource management: 'using' and 'await using' bindings auto-dispose at block exit, in reverse declaration order. DisposableStack collects an unknown number of resources.",
    code: `// ES2026 explicit resource management — automatic cleanup at block exit.
class Grinder {
  constructor(label) {
    this.label = label;
    console.log(\`→ \${label}: warming up\`);
  }
  grind(beans) {
    return \`\${beans} grounds\`;
  }
  [Symbol.dispose]() {
    console.log(\`← \${this.label}: cleaned\`);
  }
}

class SteamWand {
  constructor() {
    console.log("→ wand: pressurized");
  }
  steam(milk) {
    return \`foamed \${milk}\`;
  }
  async [Symbol.asyncDispose]() {
    await Promise.resolve();
    console.log("← wand: purged");
  }
}

// Resources declared with 'using' / 'await using' auto-dispose
// when the surrounding { } block exits — in reverse declaration order.
{
  using grinder = new Grinder("grinder");
  await using wand = new SteamWand();

  const shot = grinder.grind("Arabica");
  const foam = wand.steam("oat");
  console.log(\`  ☕ \${shot} + \${foam}\`);
  // Block exit:
  //   1. await wand[Symbol.asyncDispose]()
  //   2. grinder[Symbol.dispose]()
}

// DisposableStack collects resources opened in a loop.
{
  using stack = new DisposableStack();
  for (const drink of ["espresso", "cortado"]) {
    stack.use(new Grinder(\`mill: \${drink}\`));
  }
  // 'stack' is itself bound with 'using', so it disposes
  // every enrolled resource (LIFO) when the block ends.
}`,
  },
  {
    id: "test-runner",
    label: "Test runner — describe / test / expect",
    desc: "GocciaTestRunner globals for suites, assertions, and runner output.",
    runner: "test",
    code: `// Runs with GocciaTestRunner, so describe/test/expect are available.
const menu = [
  { name: "espresso", price: 2.5 },
  { name: "latte", price: 4 },
  { name: "cortado", price: 3.5 },
];

const totalFor = (names) =>
  names
    .map((name) => menu.find((item) => item.name === name)?.price ?? 0)
    .reduce((sum, price) => sum + price, 0);

describe("coffee order helpers", () => {
  test("calculates an order total", () => {
    expect(totalFor(["espresso", "latte"])).toBe(6.5);
    expect(totalFor(["unknown"])).toBe(0);
  });

  test("filters orders with pattern matching", () => {
    const orders = [
      { customer: "Ada", total: 7 },
      { customer: "Lin", total: 14 },
      { status: "draft" },
    ];
    const large = [];

    for (const order is { total: const total } if (total > 10) of orders) {
      large.push(\`\${order.customer}:\${total}\`);
    }

    expect(large).toEqual(["Lin:14"]);
  });

  test("calculates a single item order", () => {
    expect(totalFor(["espresso"])).toBe(2.5);
  });
});`,
  },
  {
    id: "coffee",
    label: "CoffeeShop — private fields + class",
    desc: "Classes with hash-prefixed private fields, getters, and array methods — from the README.",
    code: `// CoffeeShop — class with private fields
class CoffeeShop {
  #name = "Goccia Coffee";
  #beans = ["Arabica", "Robusta", "Ethiopian"];
  #prices = { espresso: 2.5, latte: 4.0, cappuccino: 3.75 };

  getMenu() {
    return this.#beans.map((bean) => \`\${bean} blend\`);
  }

  calculateTotal(order) {
    return order.reduce((total, item) => total + (this.#prices[item] ?? 0), 0);
  }

  get name() {
    return this.#name;
  }
}

const shop = new CoffeeShop();
const order = ["espresso", "latte"];
const total = shop.calculateTotal(order);

console.log(\`Welcome to \${shop.name}!\`);
console.log(\`Your order total: $\${total.toFixed(2)}\`);
console.log("Menu:", shop.getMenu());`,
  },
  {
    id: "coffee-typed",
    ext: "ts",
    label: "CoffeeShop · typed — types, enums, private fields",
    desc: "The home-page example. Types-as-comments, enums, interfaces, private fields, getters, and mapped object types — all in one program.",
    code: `// Types-as-comments, enums, private fields — all stock GocciaScript.
type Currency = "USD" | "EUR" | "GBP";

enum DrinkSize {
  Small = "small",
  Medium = "medium",
  Large = "large",
}

interface MenuItem {
  name: string;
  basePrice: number;
}

class CoffeeShop {
  #name: string = "Goccia Coffee";
  #menu: MenuItem[] = [
    { name: "Espresso",   basePrice: 2.5 },
    { name: "Latte",      basePrice: 4.0 },
    { name: "Cappuccino", basePrice: 3.75 },
  ];

  total(items: string[], size: DrinkSize, currency: Currency): string {
    const multiplier =
      size === DrinkSize.Large ? 1.3 :
      size === DrinkSize.Medium ? 1.1 : 1.0;
    const subtotal = items.reduce((sum, item) => {
      const found = this.#menu.find((m) => m.name === item);
      return sum + (found ? found.basePrice * multiplier : 0);
    }, 0);
    const symbols: Record<Currency, string> = { USD: "$", EUR: "€", GBP: "£" };
    return \`\${symbols[currency]}\${subtotal.toFixed(2)}\`;
  }

  get name(): string { return this.#name; }
}

const shop = new CoffeeShop();
const total = shop.total(["Espresso", "Latte"], DrinkSize.Medium, "EUR");
console.log(\`Welcome to \${shop.name}!\`);
console.log(\`Your total: \${total}\`);`,
  },
  {
    id: "fetch",
    label: "Async / await — fetch + Promise",
    desc: "Top-level await, async arrow functions, and a sandboxed fetch (GET/HEAD only, explicit allow-listed hosts).",
    code: `const fetchJoke = async () => {
  const res = await fetch("https://icanhazdadjoke.com/", {
    headers: { Accept: "application/json" },
  });
  const data = await res.json();
  return data.joke;
};

// Top-level await (ES2022+)
const joke = await fetchJoke();
console.log("Today's dad joke:");
console.log(joke);

// Promise.all fans out
const results = await Promise.all([
  Promise.resolve("espresso"),
  Promise.resolve("latte"),
  Promise.resolve("cortado"),
]);
console.log("Brew queue:", results);`,
  },
  {
    id: "temporal",
    label: "Temporal — date math without Date",
    desc: "Temporal API for unambiguous date/time arithmetic and calendar handling.",
    code: `// Today's instant in Rome
const now = Temporal.Now.zonedDateTimeISO("Europe/Rome");
console.log("Now in Rome:", now.toString());

// 90-day project schedule
const kickoff = Temporal.PlainDate.from("2026-05-01");
const milestones = [30, 60, 90].map((days) =>
  kickoff.add({ days }).toString(),
);

console.log("Kickoff:", kickoff.toString());
console.log("Milestones:", milestones);

// Duration between two dates
const delivery = Temporal.PlainDate.from("2026-07-30");
const span = kickoff.until(delivery, { largestUnit: "months" });
console.log("Time to ship:", span.toString());`,
  },
  {
    id: "data",
    label: "Structured data — JSON / TOML / YAML import",
    desc: "Import .json, .toml, .yaml, .csv files directly as ES modules — or parse at runtime.",
    code: `// Runtime parsing
const tomlSource = \`
[server]
host = "gocciascript.dev"
port = 8443

[features]
sandbox = true
temporal = true
\`;

const config = TOML.parse(tomlSource);
console.log("Host:", config.server.host);
console.log("Features:", Object.keys(config.features));

// CSV → array of rows
const csv = "name,price\\nespresso,2.5\\nlatte,4.0";
const rows = CSV.parse(csv, { headers: true });
console.log("Menu rows:", rows);`,
  },
  {
    id: "binary",
    label: "TypedArrays — binary data",
    desc: "ArrayBuffer + TypedArrays for low-level binary work; all views share the same backing memory.",
    code: `// 16 bytes of backing storage
const buf = new ArrayBuffer(16);

// Two views over the same buffer
const u32 = new Uint32Array(buf);
const u8  = new Uint8Array(buf);

u32[0] = 0xdeadbeef;
u32[1] = 0xc0ffee42;

console.log("u32:", Array.from(u32).map((n) => "0x" + n.toString(16)));
console.log("u8 :", Array.from(u8));

// TextEncoder round-trip
const enc = new TextEncoder();
const bytes = enc.encode("Goccia ☕");
console.log("bytes:", bytes);
console.log("decoded:", new TextDecoder().decode(bytes));`,
  },
  {
    id: "decorators",
    label: "Decorators — class members",
    desc: "Stage-3 decorators using object notation wrappers.",
    code: `const log = (value, context) => {
  return ({
    [context.name](...args) {
      console.log(\`→ \${context.name}(\${args.join(", ")})\`);
      const result = value.apply(this, args);
      console.log(\`← \${result}\`);
      return result;
    },
  })[context.name];
};

const memoize = (value, context) => {
  const cache = new Map();
  return ({
    [context.name](arg) {
      if (cache.has(arg)) return cache.get(arg);
      const result = value.call(this, arg);
      cache.set(arg, result);
      return result;
    },
  })[context.name];
};

class Math2 {
  @log
  @memoize
  fib(n) {
    if (n < 2) return n;
    return this.fib(n - 1) + this.fib(n - 2);
  }
}

const m = new Math2();
console.log("fib(6) =", m.fib(6));`,
  },
  {
    id: "fibfizz",
    label: "FizzBuzz + fibonacci",
    desc: "Classic algorithms — no traditional for loops, just Array methods and for...of.",
    code: `// FizzBuzz via map
const fizzbuzz = (n) =>
  Array.from({ length: n }, (_, i) => {
    const k = i + 1;
    if (k % 15 === 0) return "FizzBuzz";
    if (k % 3 === 0) return "Fizz";
    if (k % 5 === 0) return "Buzz";
    return String(k);
  });

console.log(fizzbuzz(15).join(" "));

// Fibonacci as a hand-rolled iterator (function declarations are excluded,
// so no function*; instead implement the iterator protocol directly).
class FibIterator {
  constructor() {
    this.a = 0;
    this.b = 1;
  }
  next() {
    const value = this.a;
    const next = this.a + this.b;
    this.a = this.b;
    this.b = next;
    return { value, done: false };
  }
  [Symbol.iterator]() {
    return this;
  }
}

const seq = [];
let count = 0;
for (const n of new FibIterator()) {
  if (count++ >= 10) break;
  seq.push(n);
}
console.log("First 10 fib:", seq);`,
  },
];

const DEFAULT_TOKENS = [
  "const",
  "let",
  "if",
  "else",
  "return",
  "class",
  "extends",
  "new",
  "this",
  "super",
  "import",
  "export",
  "from",
  "as",
  "async",
  "await",
  "for",
  "of",
  "while",
  "switch",
  "case",
  "default",
  "break",
  "continue",
  "try",
  "catch",
  "finally",
  "throw",
  "typeof",
  "instanceof",
  "yield",
  "yield*",
  "match",
  "when",
  "and",
  "or",
  "is",
  "enum",
  "type",
  "interface",
  "console",
  "Math",
  "JSON",
  "JSON5",
  "TOML",
  "YAML",
  "JSONL",
  "CSV",
  "TSV",
  "Object",
  "Array",
  "Number",
  "String",
  "RegExp",
  "Symbol",
  "Set",
  "Map",
  "WeakMap",
  "WeakSet",
  "Promise",
  "Temporal",
  "Iterator",
  "Proxy",
  "Reflect",
  "ArrayBuffer",
  "SharedArrayBuffer",
  "Uint8Array",
  "Uint8ClampedArray",
  "Int8Array",
  "Int16Array",
  "Uint16Array",
  "Int32Array",
  "Uint32Array",
  "Float32Array",
  "Float64Array",
  "fetch",
  "Headers",
  "Response",
  "URL",
  "URLSearchParams",
  "TextEncoder",
  "TextDecoder",
  "Error",
  "TypeError",
  "ReferenceError",
  "RangeError",
  "SyntaxError",
  "DOMException",
  "console.log",
  "console.error",
  "console.warn",
  "console.info",
  "console.debug",
  "console.table",
  "Math.PI",
  "Math.E",
  "Math.abs",
  "Math.floor",
  "Math.ceil",
  "Math.round",
  "Math.sqrt",
  "Math.pow",
  "Math.max",
  "Math.min",
  "Math.random",
  "Temporal.Now",
  "Temporal.PlainDate",
  "Temporal.PlainDateTime",
  "Temporal.PlainTime",
  "Temporal.ZonedDateTime",
  "Temporal.Duration",
  "Temporal.Instant",
  "JSON.parse",
  "JSON.stringify",
  "JSON5.parse",
  "TOML.parse",
  "YAML.parse",
  "CSV.parse",
  "TSV.parse",
  "JSONL.parse",
];

const TEST_TOKENS = [
  "describe",
  "describe.only",
  "describe.skip",
  "test",
  "test.only",
  "test.skip",
  "it",
  "it.only",
  "it.skip",
  "expect",
  "toBe",
  "toEqual",
  "toThrow",
];

const COMPAT_TOKENS = {
  var: ["var"],
  function: ["function", "function*"],
} as const;

export function buildAutocompleteTokens({
  runner,
  compatVar,
  compatFunction,
}: {
  runner: "execute" | "test";
  compatVar: boolean;
  compatFunction: boolean;
}) {
  return [
    ...DEFAULT_TOKENS,
    ...(runner === "test" ? TEST_TOKENS : []),
    ...(compatVar ? COMPAT_TOKENS.var : []),
    ...(compatFunction ? COMPAT_TOKENS.function : []),
  ];
}

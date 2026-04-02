describe("YAML.parse", () => {
  test("parses block mappings and sequences", () => {
    const value = YAML.parse(`
name: app
flags:
  - alpha
  - beta
nested:
  enabled: true
  retries: 3
`);

    expect(value.name).toBe("app");
    expect(value.flags.length).toBe(2);
    expect(value.flags[0]).toBe("alpha");
    expect(value.flags[1]).toBe("beta");
    expect(value.nested.enabled).toBe(true);
    expect(value.nested.retries).toBe(3);
  });

  test("parses flow collections, comments, and scalar values", () => {
    const value = YAML.parse(`
items: [1, 2, { ok: true }, foo: bar]
city: London # trailing comment
zip: "SW1A 1AA"
pi: 3.14159
`);

    expect(value.items.length).toBe(4);
    expect(value.items[0]).toBe(1);
    expect(value.items[2].ok).toBe(true);
    expect(value.items[3].foo).toBe("bar");
    expect(value.city).toBe("London");
    expect(value.zip).toBe("SW1A 1AA");
    expect(value.pi).toBe(3.14159);
  });

  test("allows trailing commas in flow collections", () => {
    const value = YAML.parse(`
items: [one, two,]
mapping: { sky: blue, sea: green, }
`);

    expect(value.items.length).toBe(2);
    expect(value.items[1]).toBe("two");
    expect(value.mapping.sky).toBe("blue");
    expect(value.mapping.sea).toBe("green");
  });

  test("parses multiline flow collections and empty implicit keys", () => {
    const value = YAML.parse(`
key: value
: empty key
list: [
  one,
  two
]
flow:
  {
    key: value,
    : empty key
  }
items:
  - [ : empty key ]
`);

    expect(value.key).toBe("value");
    expect(value[""]).toBe("empty key");
    expect(value.list.length).toBe(2);
    expect(value.list[1]).toBe("two");
    expect(value.flow.key).toBe("value");
    expect(value.flow[""]).toBe("empty key");
    expect(value.items[0][0][""]).toBe("empty key");
  });

  test("parses inline sequence values after mapping keys", () => {
    const value = YAML.parse(`
key: - a
     - b
explicit:
  ? a
  : -\tb
    -  -\tc
       - d
`);

    expect(value.key.length).toBe(2);
    expect(value.key[0]).toBe("a");
    expect(value.key[1]).toBe("b");
    expect(value.explicit.a.length).toBe(2);
    expect(value.explicit.a[0]).toBe("b");
    expect(value.explicit.a[1].length).toBe(2);
    expect(value.explicit.a[1][0]).toBe("c");
    expect(value.explicit.a[1][1]).toBe("d");
  });

  test("allows tabs in content after indentation", () => {
    const value = YAML.parse(`
mapping:
 - x
  \tx
literal: |
  literal
  \ttext
folded: >
  \t
  detected
`);

    expect(value.mapping.length).toBe(1);
    expect(value.mapping[0]).toBe("x x");
    expect(value.literal).toBe("literal\n\ttext\n");
    expect(value.folded).toBe("\ndetected\n");
  });

  test("parses tags and anchors inside flow collection entries", () => {
    const value = YAML.parse(`
items: [foo: !!int "5", bar: !!str 7, baz: &x { n: 1 }, qux: *x]
nested: { a: !!seq [1, 2], b: !!map { c: 3 } }
`);

    expect(value.items[0].foo).toBe(5);
    expect(value.items[1].bar).toBe("7");
    expect(value.items[2].baz.n).toBe(1);
    expect(value.items[3].qux.n).toBe(1);
    expect(value.items[2].baz === value.items[3].qux).toBe(true);
    expect(value.nested.a.length).toBe(2);
    expect(value.nested.a[1]).toBe(2);
    expect(value.nested.b.c).toBe(3);
  });

  test("preserves custom tag metadata inside flow collections", () => {
    const value = YAML.parse(`
%TAG !e! tag:example.com,2026:
items: [!e!x 1, !foo { a: 2 }, entry: &x !e!y test, again: *x]
`);

    expect(value.items[0].tagName).toBe("tag:example.com,2026:x");
    expect(value.items[0].value).toBe(1);
    expect(value.items[1].tagName).toBe("!foo");
    expect(value.items[1].value.a).toBe(2);
    expect(value.items[2].entry.tagName).toBe("tag:example.com,2026:y");
    expect(value.items[2].entry.value).toBe("test");
    expect(value.items[2].entry === value.items[3].again).toBe(true);
  });

  test("parses YAML 1.2 numeric scalar forms", () => {
    const value = YAML.parse(`
integer: 1_000
float: 12_345.67_89
leadingDot: .5
trailingDot: 5.
exponent: 1.2e1_0
hex: 0xCA_FE
octal: 0o7_5_5
binary: 0b1010_0110
positiveInfinity: +.inf
negativeInfinity: -.inf
nanValue: .nan
legacyBool: on
legacyString: yes
`);

    expect(value.integer).toBe(1000);
    expect(value.float).toBe(12345.6789);
    expect(value.leadingDot).toBe(0.5);
    expect(value.trailingDot).toBe(5);
    expect(value.exponent).toBe(12000000000);
    expect(value.hex).toBe(0xCAFE);
    expect(value.octal).toBe(493);
    expect(value.binary).toBe(166);
    expect(value.positiveInfinity).toBe(Infinity);
    expect(value.negativeInfinity).toBe(-Infinity);
    expect(Number.isNaN(value.nanValue)).toBe(true);
    expect(value.legacyBool).toBe("on");
    expect(value.legacyString).toBe("yes");
  });

  test("leaves invalid implicit numeric scalars as strings", () => {
    const value = YAML.parse(`
doubleUnderscore: 1__2
underscoreBeforeFraction: 1_.2
underscoreAfterDot: 1._2
underscoreInExponentSignificand: 1e_2
underscoreAfterPrefix: 0x_10
underscoreAtStart: _12
underscoreAtEnd: 12_
`);

    expect(value.doubleUnderscore).toBe("1__2");
    expect(value.underscoreBeforeFraction).toBe("1_.2");
    expect(value.underscoreAfterDot).toBe("1._2");
    expect(value.underscoreInExponentSignificand).toBe("1e_2");
    expect(value.underscoreAfterPrefix).toBe("0x_10");
    expect(value.underscoreAtStart).toBe("_12");
    expect(value.underscoreAtEnd).toBe("12_");
  });

  test("returns an array for a single explicit document", () => {
    const value = YAML.parse(`
---
name: app
...
`);

    expect(value.length).toBe(1);
    expect(value[0].name).toBe("app");
  });

  test("returns an array for multi-document streams", () => {
    const values = YAML.parse(`
---
name: first
count: 1
---
name: second
items:
  - x
  - y
`);

    expect(values.length).toBe(2);
    expect(values[0].name).toBe("first");
    expect(values[0].count).toBe(1);
    expect(values[1].name).toBe("second");
    expect(values[1].items.length).toBe(2);
    expect(values[1].items[1]).toBe("y");
  });

  test("supports merge keys with anchors and aliases", () => {
    const value = YAML.parse(`
defaults: &defaults
  host: localhost
  port: 5432
  enabled: true

database:
  <<: *defaults
  port: 5433
`);

    expect(value.database.host).toBe("localhost");
    expect(value.database.port).toBe(5433);
    expect(value.database.enabled).toBe(true);
  });

  test("preserves alias identity and self-references", () => {
    const value = YAML.parse(`
root: &root
  name: root
  self: *root
  child:
    ref: *root
loop: &loop
  - *loop
pair: [ &item { label: same }, *item ]
`);

    expect(value.root.self === value.root).toBe(true);
    expect(value.root.child.ref === value.root).toBe(true);
    expect(value.loop[0] === value.loop).toBe(true);
    expect(value.pair[0] === value.pair[1]).toBe(true);
  });

  test("merge key sequences preserve earlier source precedence", () => {
    const value = YAML.parse(`
base: &base
  host: localhost
  port: 5432
  enabled: false

override: &override
  port: 6432
  enabled: true

database:
  <<: [*base, *override]
`);

    expect(value.database.host).toBe("localhost");
    expect(value.database.port).toBe(5432);
    expect(value.database.enabled).toBe(false);
  });

  test("parses literal block scalars", () => {
    const value = YAML.parse(`
message: |
  hello
  # not a comment
  world

  done
`);

    expect(value.message).toBe("hello\n# not a comment\nworld\n\ndone\n");
  });

  test("parses folded block scalars with chomping", () => {
    const value = YAML.parse(`
summary: >-
  hello
  world

  next paragraph
indented: |2-
  one
  two
`);

    expect(value.summary).toBe("hello world\nnext paragraph");
    expect(value.indented).toBe("one\ntwo");
  });

  test("parses multiline plain and quoted scalars with folding", () => {
    const value = YAML.parse(`
plain: first
  second

  third
double: "hello
  world"
single: 'left
  right'
items:
  - alpha
    beta
  - "one
    two"
`);

    expect(value.plain).toBe("first second\nthird");
    expect(value.double).toBe("hello world");
    expect(value.single).toBe("left right");
    expect(value.items[0]).toBe("alpha beta");
    expect(value.items[1]).toBe("one two");
  });

  test("parses YAML double-quoted escape sequences", () => {
    const value = YAML.parse([
      'ascii: "\\x41"',
      'omega: "\\u03A9"',
      'emoji: "\\U0001F600"',
      'controls: "\\0\\a\\b\\t\\n\\v\\f\\r\\e"',
      'spaces: "left\\ right"',
      'separators: "\\N\\_\\L\\P"',
      'slashes: "\\"\\\\/"',
      'continued: "first' + "\\",
      '  second"',
      'trailing: "slash\\\\"',
    ].join("\n"));

    expect(value.ascii).toBe("A");
    expect(value.omega).toBe("Ω");
    expect(value.emoji).toBe("😀");
    expect(value.controls.length).toBe(9);
    expect(value.controls.codePointAt(0)).toBe(0);
    expect(value.controls.codePointAt(1)).toBe(7);
    expect(value.controls.codePointAt(2)).toBe(8);
    expect(value.controls.codePointAt(3)).toBe(9);
    expect(value.controls.codePointAt(4)).toBe(10);
    expect(value.controls.codePointAt(5)).toBe(11);
    expect(value.controls.codePointAt(6)).toBe(12);
    expect(value.controls.codePointAt(7)).toBe(13);
    expect(value.controls.codePointAt(8)).toBe(27);
    expect(value.spaces).toBe("left right");
    expect(value.separators).toBe("\u0085\u00a0\u2028\u2029");
    expect(value.slashes).toBe("\"\\/");
    expect(value.continued).toBe("firstsecond");
    expect(value.trailing).toBe("slash\\");
  });

  test("parses multiline quoted scalars with trailing comments and escaped tabs", () => {
    const value = YAML.parse(`
a: "double
  quotes" # trailing comment
b:
 &node # attached to the next node
 - x: y
inline: "2 inline\\\ttab"
continued: "2 leading
    \\\ttab"
`);

    expect(value.a).toBe("double quotes");
    expect(value.b.length).toBe(1);
    expect(value.b[0].x).toBe("y");
    expect(value.inline).toBe("2 inline\ttab");
    expect(value.continued).toBe("2 leading \ttab");
  });

  test("preserves implicit scalar typing when plain scalars do not continue", () => {
    const value = YAML.parse(`
enabled: true

disabled: false
`);

    expect(value.enabled).toBe(true);
    expect(value.disabled).toBe(false);
  });

  test("parses standalone scalar nodes that continue at the same indentation", () => {
    const value = YAML.parse(`
plain:
  This unquoted scalar
  spans many lines.

quoted: "So does this
  quoted scalar.\\n"
`);

    expect(value.plain).toBe("This unquoted scalar spans many lines.");
    expect(value.quoted).toBe("So does this quoted scalar.\n");
  });

  test("parses flow mappings with omitted values and complex single-pair items", () => {
    const value = YAML.parse(`
mapping: { unquoted : "separate", http://foo.com, omitted value:, "key":value, "key2"::value }
items:
  - { single line, a: b}
  - { "single line", a: b}
complex: [ [ a, [ [[b,c]]: d, e]]: 23 ]
`);

    expect(value.mapping.unquoted).toBe("separate");
    expect(value.mapping["http://foo.com"]).toBe(null);
    expect(value.mapping["omitted value"]).toBe(null);
    expect(value.mapping.key).toBe("value");
    expect(value.mapping.key2).toBe(":value");
    expect(value.items[0]["single line"]).toBe(null);
    expect(value.items[0].a).toBe("b");
    expect(value.items[1]["single line"]).toBe(null);
    expect(value.items[1].a).toBe("b");
    expect(value.complex[0]["[\"a\", [{\"[[\\\"b\\\", \\\"c\\\"]]\": \"d\"}, \"e\"]]"]).toBe(23);
  });

  test("does not treat document markers inside block scalars as a stream", () => {
    const value = YAML.parse(`
message: |
  ---
  ...
  done
`);

    expect(value.message).toBe("---\n...\ndone\n");
  });

  test("parses YAML directives and standard tags", () => {
    const value = YAML.parse(`
%YAML 1.2
%TAG !e! tag:example.com,2026:
name: !!str true
count: !!int "42"
ratio: !!float "3.5"
enabled: !!bool "true"
missing: !!null ""
settings: !!map { mode: safe }
items: !!seq [1, 2]
custom: !e!widget
  title: demo
typed: [!<tag:yaml.org,2002:str> 123, !!int "5"]
created: !!timestamp 2026-04-02
payload: !!binary SGVsbG8=
`);

    expect(value.name).toBe("true");
    expect(value.count).toBe(42);
    expect(value.ratio).toBe(3.5);
    expect(value.enabled).toBe(true);
    expect(value.missing).toBe(null);
    expect(value.settings.mode).toBe("safe");
    expect(value.items.length).toBe(2);
    expect(value.items[1]).toBe(2);
    expect(value.custom.title).toBe("demo");
    expect(value.custom.tagName).toBe("tag:example.com,2026:widget");
    expect(value.custom.value.title).toBe("demo");
    expect(value.typed[0]).toBe("123");
    expect(value.typed[1]).toBe(5);
    expect(value.created.tagName).toBe("tag:yaml.org,2002:timestamp");
    expect(value.created.value).toBe("2026-04-02");
    expect(value.payload.tagName).toBe("tag:yaml.org,2002:binary");
    expect(value.payload.value).toBe("Hello");
  });

  test("supports the primary %TAG handle", () => {
    const value = YAML.parse(`
%TAG ! tag:example.com,2026:
node: !widget demo
`);

    expect(value.node.tagName).toBe("tag:example.com,2026:widget");
    expect(value.node.value).toBe("demo");
  });

  test("parses explicit keys and canonicalizes complex keys", () => {
    const value = YAML.parse(`
? name
: app
? [red, blue]
: palette
?
  env: prod
  region: eu-west-1
: deployment
flow: { [1, 2]: pair }
`);

    expect(value.name).toBe("app");
    expect(value["[\"red\", \"blue\"]"]).toBe("palette");
    expect(value["{\"env\": \"prod\", \"region\": \"eu-west-1\"}"]).toBe("deployment");
    expect(value.flow["[1, 2]"]).toBe("pair");
  });

  test("parses explicit keys with omitted values and zero-indented sequences", () => {
    const value = YAML.parse(`
?
- a
- b
:
- c
- d
items:
  - &a
  - a
  -
    &a : a
    b: &b
  -
    &c : &a
  -
    ? &d
  -
    ? &e
    : &a
`);

    expect(value["[\"a\", \"b\"]"].length).toBe(2);
    expect(value["[\"a\", \"b\"]"][0]).toBe("c");
    expect(value["[\"a\", \"b\"]"][1]).toBe("d");
    expect(value.items.length).toBe(6);
    expect(value.items[0]).toBe(null);
    expect(value.items[1]).toBe("a");
    expect(value.items[2][""]).toBe("a");
    expect(value.items[2].b).toBe(null);
    expect(value.items[3][""]).toBe(null);
    expect(value.items[4][""]).toBe(null);
    expect(value.items[5][""]).toBe(null);
  });

  test("throws on invalid tagged scalars", () => {
    expect(() => YAML.parse("count: !!int nope")).toThrow(SyntaxError);
    expect(() => YAML.parse("enabled: !!bool maybe")).toThrow(SyntaxError);
    expect(() => YAML.parse("created: !!timestamp nope")).toThrow(SyntaxError);
    expect(() => YAML.parse("payload: !!binary not-base64!")).toThrow(SyntaxError);
    expect(() => YAML.parse("count: !!int 1__2")).toThrow(SyntaxError);
    expect(() => YAML.parse("ratio: !!float 1_.2")).toThrow(SyntaxError);
  });

  test("throws on invalid double-quoted escapes", () => {
    expect(() => YAML.parse('value: "\\q"')).toThrow(SyntaxError);
    expect(() => YAML.parse('value: "\\x4"')).toThrow(SyntaxError);
    expect(() => YAML.parse('value: "\\U00110000"')).toThrow(SyntaxError);
    expect(() => YAML.parse('quoted: "a\nb\nc"\n')).toThrow(SyntaxError);
    expect(() => YAML.parse('key: "value"# invalid comment\n')).toThrow(SyntaxError);
  });

  test("throws on invalid YAML directives", () => {
    expect(() => YAML.parse(`
%YAML
name: app
`)).toThrow(SyntaxError);
    expect(() => YAML.parse(`
%YAML 2.0
name: app
`)).toThrow(SyntaxError);
    expect(() => YAML.parse(`
%TAG !broken tag:example.com,2026:
name: app
`)).toThrow(SyntaxError);
    expect(() => YAML.parse(`
%TAG !e! tag:example.com,a:
%TAG !e! tag:example.com,b:
name: app
`)).toThrow(SyntaxError);
  });

  test("throws on unknown aliases", () => {
    expect(() => YAML.parse(`
database:
  <<: *missing
`)).toThrow(SyntaxError);
  });

  test("throws on invalid flow mappings", () => {
    expect(() => YAML.parse("[1,,2]")).toThrow(SyntaxError);
    expect(() => YAML.parse("{ a: 1,, b: 2 }")).toThrow(SyntaxError);
    expect(() => YAML.parse("{, a: 1 }")).toThrow(SyntaxError);
    expect(() => YAML.parse("[\n--- ,\n...\n]")).toThrow(SyntaxError);
    expect(() => YAML.parse("-\t-")).toThrow(SyntaxError);
    expect(() => YAML.parse("- \t-")).toThrow(SyntaxError);
    expect(() => YAML.parse("?\t-")).toThrow(SyntaxError);
    expect(() => YAML.parse("? -\n:\t-")).toThrow(SyntaxError);
    expect(() => YAML.parse("?\tkey:")).toThrow(SyntaxError);
  });
});

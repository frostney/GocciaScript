describe("TOML.parse", () => {
  test("parses scalars, strings, arrays, tables, and arrays of tables", () => {
    const value = TOML.parse(`
title = "TOML Example"
enabled = true
count = 42
ratio = 3.1415
createdAt = 1979-05-27T07:32:00Z
alarmAt = 07:32
tags = ["alpha", "beta", { nested = true }]

[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]

[[products]]
name = "Hammer"
sku = 738594937

[[products]]
name = "Nail"
sku = 284758393
color = "gray"
`);

    expect(value.title).toBe("TOML Example");
    expect(value.enabled).toBe(true);
    expect(value.count).toBe(42);
    expect(value.ratio).toBe(3.1415);
    expect(value.createdAt).toBe("1979-05-27T07:32:00Z");
    expect(value.alarmAt).toBe("07:32");
    expect(value.tags.length).toBe(3);
    expect(value.tags[2].nested).toBe(true);
    expect(value.database.server).toBe("192.168.1.1");
    expect(value.database.ports[2]).toBe(8002);
    expect(value.products.length).toBe(2);
    expect(value.products[0].name).toBe("Hammer");
    expect(value.products[1].color).toBe("gray");
  });

  test("parses dotted keys, quoted keys, and inline tables", () => {
    const value = TOML.parse(`
fruit.name = "apple"
fruit.color = "red"
"google.com" = true
site."example.org".enabled = false
point = { x = 1, y = 2, meta = { label = "origin" } }
`);

    expect(value.fruit.name).toBe("apple");
    expect(value.fruit.color).toBe("red");
    expect(value["google.com"]).toBe(true);
    expect(value.site["example.org"].enabled).toBe(false);
    expect(value.point.x).toBe(1);
    expect(value.point.meta.label).toBe("origin");
  });

  test("parses multiline basic and literal strings", () => {
    const value = TOML.parse([
      'basic = """',
      'Roses are red',
      'Violets are blue',
      '"""',
      'joined = """',
      'The quick brown \\',
      '',
      '  fox jumps over \\',
      '    the lazy dog.',
      '"""',
      "literal = '''",
      'C:\\Users\\nodejs\\templates',
      'Line two',
      "'''",
      '',
    ].join("\n"));

    expect(value.basic).toBe("Roses are red\nViolets are blue\n");
    expect(value.joined).toBe("The quick brown fox jumps over the lazy dog.\n");
    expect(value.literal).toBe("C:\\Users\\nodejs\\templates\nLine two\n");
  });

  test("normalizes CRLF multiline strings to LF", () => {
    const value = TOML.parse(
      'basic = """\r\nRoses are red\r\nViolets are blue\r\n"""\r\n' +
      "literal = '''\r\nC:\\Users\\nodejs\\templates\r\nLine two\r\n'''\r\n"
    );

    expect(value.basic).toBe("Roses are red\nViolets are blue\n");
    expect(value.literal).toBe("C:\\Users\\nodejs\\templates\nLine two\n");
  });

  test("parses integer bases and special floats", () => {
    const value = TOML.parse(`
decimal = 1_000
hex = 0xDEAD_BEEF
octal = 0o755
binary = 0b11010110
positiveInfinity = inf
negativeInfinity = -inf
nanValue = nan
`);

    expect(value.decimal).toBe(1000);
    expect(value.hex).toBe(0xDEADBEEF);
    expect(value.octal).toBe(493);
    expect(value.binary).toBe(214);
    expect(value.positiveInfinity).toBe(Infinity);
    expect(value.negativeInfinity).toBe(-Infinity);
    expect(Number.isNaN(value.nanValue)).toBe(true);
  });

  test("throws on duplicate keys and incompatible redefinitions", () => {
    expect(() => TOML.parse(`
name = "first"
name = "second"
`)).toThrow(SyntaxError);

    expect(() => TOML.parse(`
fruit.apple = 1
fruit.apple.smooth = true
`)).toThrow(SyntaxError);

    expect(() => TOML.parse(`
point = { x = 1 }
point.y = 2
`)).toThrow(SyntaxError);
  });

  test("throws on invalid literals", () => {
    expect(() => TOML.parse('flag = TRUE')).toThrow(SyntaxError);
    expect(() => TOML.parse('value = .7')).toThrow(SyntaxError);
    expect(() => TOML.parse('value = 7.')).toThrow(SyntaxError);
    expect(() => TOML.parse('value = 01')).toThrow(SyntaxError);
    expect(() => TOML.parse('value = 0x_FF')).toThrow(SyntaxError);
    expect(() => TOML.parse('value = INF')).toThrow(SyntaxError);
    expect(() => TOML.parse('value = NAN')).toThrow(SyntaxError);
    expect(() => TOML.parse('text = "\\q"')).toThrow(SyntaxError);
  });
});

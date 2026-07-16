describe("expect.schemaMatching", () => {
  const numberSchema = {
    "~standard": {
      version: 1,
      vendor: "test",
      validate(value) {
        if (typeof value === "number") {
          return { value };
        }
        return { issues: [{ message: "Expected a number" }] };
      },
    },
  };

  test("matches synchronous Standard Schema validation", () => {
    expect(42).toEqual(expect.schemaMatching(numberSchema));
    expect("42").not.toEqual(expect.schemaMatching(numberSchema));
    expect("42").toEqual(expect.not.schemaMatching(numberSchema));
  });

  test("preserves the Standard Schema this value", () => {
    const standard = {
      version: 1,
      vendor: "test",
      expected: 7,
      validate(value) {
        return value === this.expected
          ? { value }
          : { issues: [{ message: "Unexpected value" }] };
      },
    };

    expect(7).toEqual(expect.schemaMatching({ "~standard": standard }));
  });

  test("rejects invalid and asynchronous schemas", () => {
    expect(() => expect.schemaMatching({})).toThrow(
      "SchemaMatching expected to receive a Standard Schema",
    );

    const asyncSchema = {
      "~standard": {
        version: 1,
        vendor: "test",
        async validate(value) {
          return { value };
        },
      },
    };

    expect(() =>
      expect(1).toEqual(expect.schemaMatching(asyncSchema)),
    ).toThrow(
      "Async schema validation is not supported in asymmetric matchers",
    );
  });
});

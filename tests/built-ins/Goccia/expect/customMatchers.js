describe("custom asymmetric matchers", () => {
  test("toBeOneOf supports arrays, sets, deep equality, and negation", () => {
    expect({ id: 2 }).toEqual(expect.toBeOneOf([{ id: 1 }, { id: 2 }]));
    expect("b").toEqual(expect.toBeOneOf(new Set(["a", "b"])));
    expect(3).toEqual(expect.not.toBeOneOf([1, 2]));
    expect("anything").toEqual(expect.toBeOneOf([]));
    expect("anything").toEqual(expect.toBeOneOf(new Set()));
    expect(() => expect(1).toEqual(expect.toBeOneOf({}))).toThrow(
      "You must provide an array or set to toBeOneOf",
    );
  });

  test("toSatisfy invokes the predicate and supports negation", () => {
    expect(18).toEqual(expect.toSatisfy((value) => value >= 18));
    expect(17).toEqual(expect.not.toSatisfy((value) => value >= 18));
    expect(() => expect(1).toEqual(expect.toSatisfy(42))).toThrow(
      "toSatisfy expects a predicate function",
    );
  });

  test("benchmark comparisons use latency means and delta thresholds", () => {
    const baseline = { latency: { mean: 10 }, throughput: { mean: 100 } };
    const faster = { latency: { mean: 5 }, throughput: { mean: 200 } };
    const slower = { latency: { mean: 15 }, throughput: { mean: 50 } };
    const slightlyFaster = { latency: { mean: 9 }, throughput: { mean: 110 } };

    expect(faster).toEqual(expect.toBeFasterThan(baseline));
    expect(slower).toEqual(expect.toBeSlowerThan(baseline));
    expect(slower).toEqual(expect.not.toBeFasterThan(baseline));
    expect(faster).toEqual(expect.not.toBeSlowerThan(baseline));
    expect(slightlyFaster).toEqual(
      expect.not.toBeFasterThan(baseline, { delta: 0.2 }),
    );
    expect(() =>
      expect({}).toEqual(expect.toBeFasterThan(baseline)),
    ).toThrow("expects the actual value to be a benchmark result");
  });
});

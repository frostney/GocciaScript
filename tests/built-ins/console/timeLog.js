test("console.timeLog exists and returns undefined", () => {
  expect(typeof console.timeLog).toBe("function");
  console.time("log-timer");
  expect(console.timeLog("log-timer")).toBeUndefined();
  console.timeEnd("log-timer");
});

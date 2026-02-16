/*---
description: Private fields are accessible inside array method callbacks via lexical this
features: [private-fields, arrow-functions, array-methods]
---*/

describe("Private fields accessed in array method callbacks", () => {
  test("private field with computed access in reduce callback", () => {
    class PriceCalculator {
      #prices = { apple: 1.5, banana: 2.0, cherry: 3.0 };

      total(items) {
        return items.reduce((sum, item) => sum + this.#prices[item], 0);
      }
    }

    const calc = new PriceCalculator();
    expect(calc.total(["apple", "banana"])).toBe(3.5);
    expect(calc.total(["cherry"])).toBe(3);
  });

  test("private field accessed in filter callback", () => {
    class Threshold {
      #min = 5;

      filter(nums) {
        return nums.filter((n) => n >= this.#min);
      }
    }

    const t = new Threshold();
    const result = t.filter([1, 5, 3, 10, 2, 8]);
    expect(result.length).toBe(3);
    expect(result[0]).toBe(5);
    expect(result[1]).toBe(10);
    expect(result[2]).toBe(8);
  });

  test("private field accessed through custom class method callback", () => {
    class Processor {
      #multiplier = 3;

      process(items, callback) {
        return items.map((item) => callback(item * this.#multiplier));
      }
    }

    const p = new Processor();
    const result = p.process([1, 2, 3], (x) => x + 1);
    expect(result).toEqual([4, 7, 10]);
  });
});

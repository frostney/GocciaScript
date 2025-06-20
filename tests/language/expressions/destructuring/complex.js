/*---
description: Complex destructuring
features: [destructuring]
---*/

test("nested destructuring with complex patterns", () => {
  const data = {
    user: {
      name: "Alice",
      details: {
        age: 30,
        location: "NYC",
      },
    },
    scores: [95, 87, 92],
  };

  let name, age, location, firstScore, secondScore, remaining;

  // Nested object destructuring
  ({
    user: {
      name,
      details: { age, location },
    },
  } = data);
  expect(name).toBe("Alice");
  expect(age).toBe(30);
  expect(location).toBe("NYC");

  // Mixed object/array destructuring
  ({
    scores: [firstScore, secondScore, ...remaining],
  } = data);
  expect(firstScore).toBe(95);
  expect(secondScore).toBe(87);
  expect(remaining).toEqual([92]);

  // Error case: destructuring nested null
  expect(() => {
    ({
      user: {
        profile: { bio },
      },
    } = { user: { profile: null } });
  }).toThrow(TypeError);
});

test("mixed types array destructuring", () => {
  let mixed = [42, "text", true, { prop: "value" }, [1, 2, 3]];
  let [num, str, bool, obj, arr] = mixed;
  expect(num).toBe(42);
  expect(str).toBe("text");
  expect(bool).toBe(true);
  expect(obj.prop).toBe("value");
  expect(arr[0]).toBe(1);
});

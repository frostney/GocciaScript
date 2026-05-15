describe("with statement", () => {
  test("reads and writes object properties before outer bindings", () => {
    const obj = { x: 2 };
    let result = 0;

    with (obj) {
      result = x;
      x = 5;
    }

    expect(result).toBe(2);
    expect(obj.x).toBe(5);
  });

  test("lexical declarations inside the body shadow the object environment", () => {
    const obj = { x: 1 };
    let result = 0;

    with (obj) {
      let x = 4;
      result = x;
    }

    expect(result).toBe(4);
    expect(obj.x).toBe(1);
  });

  test("falls back to the outer scope when the object lacks a binding", () => {
    const obj = {};
    let value = 1;

    with (obj) {
      value = 3;
    }

    expect(value).toBe(3);
  });

  test("honors Symbol.unscopables", () => {
    const obj = { x: 1 };
    obj[Symbol.unscopables] = { x: true };
    let x = 9;
    let result = 0;

    with (obj) {
      result = x;
    }

    expect(result).toBe(9);
  });

  test("closures created inside with retain the object environment", () => {
    const obj = { x: 4 };
    let read;

    with (obj) {
      read = () => x;
    }

    obj.x = 6;
    expect(read()).toBe(6);
  });

  test("calls object-environment methods with the object as this", () => {
    const obj = {
      x: 3,
      get() {
        return this.x;
      },
      inc() {
        this.x += 1;
      }
    };
    let seen = 0;

    with (obj) {
      seen = get();
      inc();
    }

    expect(seen).toBe(3);
    expect(obj.x).toBe(4);
  });

  test("compound assignments and updates target the object binding", () => {
    const obj = { x: 1 };

    with (obj) {
      x += 2;
      x++;
    }

    expect(obj.x).toBe(4);
  });

  test("throws when the object expression cannot be converted to an object", () => {
    expect(() => {
      with (null) {
      }
    }).toThrow(TypeError);
  });
});

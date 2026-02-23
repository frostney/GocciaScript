/*---
description: Symbol.metadata is assigned on decorated classes with prototype chain inheritance
features: [decorators, decorator-metadata]
---*/

describe("decorator metadata", () => {
  test("Symbol.metadata exists on decorated class", () => {
    const track = (value, context) => {
      context.metadata.tracked = true;
    };

    @track
    class C {}

    const meta = C[Symbol.metadata];
    expect(meta).not.toBe(undefined);
    expect(meta.tracked).toBe(true);
  });

  test("metadata is shared across decorators on same class", () => {
    const first = (value, context) => {
      context.metadata.first = true;
    };

    const second = (value, context) => {
      context.metadata.second = true;
    };

    class C {
      @first
      foo() {}

      @second
      bar() {}
    }

    const meta = C[Symbol.metadata];
    expect(meta.first).toBe(true);
    expect(meta.second).toBe(true);
  });

  test("metadata object is provided to all element decorators", () => {
    const collected = [];

    const track = (value, context) => {
      collected.push(context.metadata);
    };

    class C {
      @track
      a() {}

      @track
      b() {}
    }

    expect(collected.length).toBe(2);
    expect(collected[0] === collected[1]).toBe(true);
  });

  test("subclass metadata inherits from superclass metadata", () => {
    const tag = (key, val) => (value, context) => {
      context.metadata[key] = val;
    };

    @tag('base', true)
    class Base {}

    @tag('child', true)
    class Child extends Base {}

    const baseMeta = Base[Symbol.metadata];
    const childMeta = Child[Symbol.metadata];

    expect(baseMeta.base).toBe(true);
    expect(childMeta.child).toBe(true);
    expect(childMeta.base).toBe(true);
    expect(baseMeta.child).toBe(undefined);
  });
});

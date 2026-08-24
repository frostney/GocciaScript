/*---
description: Hoisted function declarations in imported modules construct module classes
features: [modules, compat-function, class]
---*/

import {
  Counted,
  Factory,
  Point,
  Tagged,
  arrowConstruct,
  derivedTickCount,
  fnDeclArrowConstruct,
  fnDeclBoundConstruct,
  fnDeclClosureRead,
  fnDeclConstruct,
  fnDeclDerivedConstruct,
  fnDeclLocalImplicitSubclassConstruct,
  fnDeclLocalSubclassConstruct,
  fnDeclLocalSubclassOfDerivedConstruct,
  fnDeclNestedConstruct,
  fnDeclSpreadConstruct,
} from "./helpers/module-classes.js";

describe("imported module functions construct module classes", () => {
  test("a hoisted function declaration runs the constructor body", () => {
    const point = fnDeclConstruct();
    expect(point instanceof Point).toBe(true);
    expect(Object.keys(point)).toEqual(["label", "x", "y"]);
    expect(point.x).toBe(1);
    expect(point.y).toBe(2);
    expect(point.label).toBe("point");
    expect(point.sum).toBe(3);
  });

  test("arrow and method forms construct the same way", () => {
    expect(Object.keys(arrowConstruct())).toEqual(["label", "x", "y"]);
    expect(new Factory().methodConstruct().sum).toBe(3);
  });

  test("nested and arrow call sites inside a declaration construct", () => {
    expect(fnDeclNestedConstruct().sum).toBe(15);
    expect(fnDeclArrowConstruct().sum).toBe(19);
  });

  test("spread and bound construction run the constructor body", () => {
    expect(fnDeclSpreadConstruct().sum).toBe(11);
    expect(fnDeclBoundConstruct().sum).toBe(23);
  });

  test("a derived module class runs its base constructor", () => {
    const tagged = fnDeclDerivedConstruct();
    expect(tagged instanceof Tagged).toBe(true);
    expect(tagged instanceof Point).toBe(true);
    expect(tagged.x).toBe(4);
    expect(tagged.y).toBe(5);
    expect(tagged.tag).toBe("id-tagged");
  });

  test("a subclass declared inside the function runs the imported base", () => {
    const explicitSuper = fnDeclLocalSubclassConstruct();
    expect(explicitSuper.x).toBe(13);
    expect(explicitSuper.y).toBe(14);
    expect(explicitSuper.label).toBe("point");
    expect(explicitSuper.local).toBe("id-local");

    const implicitSuper = fnDeclLocalImplicitSubclassConstruct();
    expect(implicitSuper.x).toBe(15);
    expect(implicitSuper.y).toBe(16);
    expect(implicitSuper.label).toBe("point");
  });

  // The base here is itself derived, so its own super() is what initializes its
  // fields. Constructing twice through the one helper is deliberate: a repeated
  // initializer run would advance `seq` by two, and a super()-called flag left
  // set by the first construction only becomes visible on the second.
  test("a subclass of a derived module class initializes each field once", () => {
    const ticksBefore = derivedTickCount();
    const first = fnDeclLocalSubclassOfDerivedConstruct();
    const second = fnDeclLocalSubclassOfDerivedConstruct();

    for (const instance of [first, second]) {
      expect(instance instanceof Counted).toBe(true);
      expect(instance instanceof Point).toBe(true);
      // Sorted because the two modes disagree on own-property *order* for a
      // derived class's field initializers, which is a separate question from
      // whether each field is initialized exactly once.
      expect(Object.keys(instance).sort()).toEqual([
        "label",
        "local",
        "owner",
        "seq",
        "stamp",
        "x",
        "y",
      ]);
      expect(instance.label).toBe("point");
      expect(instance.x).toBe(20);
      expect(instance.y).toBe(21);
      expect(instance.sum).toBe(41);
      expect(instance.owner).toBe("counted");
      expect(instance.stamp).toBe("local");
      expect(instance.local).toBe(true);
      expect(instance.brand()).toBe("id-counted");
      expect(instance.localBrand()).toBe("id-local");
    }

    expect(second.seq - first.seq).toBe(1);
    expect(derivedTickCount() - ticksBefore).toBe(2);
  });

  test("a hoisted function declaration still reads module closure bindings", () => {
    expect(fnDeclClosureRead()).toBe("id-closure");
  });

  test("constructing an imported class from the entry file is unchanged", () => {
    expect(Object.keys(new Point(1, 2))).toEqual(["label", "x", "y"]);
    expect(new Tagged(4).tag).toBe("id-tagged");
  });
});

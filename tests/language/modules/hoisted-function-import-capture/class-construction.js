/*---
description: Hoisted function declarations in imported modules construct module classes
features: [modules, compat-function, class]
---*/

import {
  Factory,
  Point,
  Tagged,
  arrowConstruct,
  fnDeclArrowConstruct,
  fnDeclBoundConstruct,
  fnDeclClosureRead,
  fnDeclConstruct,
  fnDeclDerivedConstruct,
  fnDeclLocalImplicitSubclassConstruct,
  fnDeclLocalSubclassConstruct,
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

  test("a hoisted function declaration still reads module closure bindings", () => {
    expect(fnDeclClosureRead()).toBe("id-closure");
  });

  test("constructing an imported class from the entry file is unchanged", () => {
    expect(Object.keys(new Point(1, 2))).toEqual(["label", "x", "y"]);
    expect(new Tagged(4).tag).toBe("id-tagged");
  });
});

// Differential suite L — construction and closure reads from the three callable
// forms, in an imported module and in the entry file.
//
// A module's top-level function declarations are created while the module is
// linked, not compiled with the rest of the module body, so in bytecode mode a
// declaration body and an arrow body next to it reach the same class through
// different machinery. The entry file has no such split, which is why every
// probe is run on both sides of the import.
import {
  Factory,
  Point,
  Tagged,
  arrowClosureRead,
  arrowConstruct,
  arrowFieldRead,
  fnDeclArrowConstruct,
  fnDeclBoundConstruct,
  fnDeclClosureRead,
  fnDeclConstruct,
  fnDeclDerivedConstruct,
  fnDeclFieldRead,
  fnDeclLocalImplicitSubclassConstruct,
  fnDeclLocalSubclassConstruct,
  fnDeclNestedConstruct,
  fnDeclSpreadConstruct,
} from "./mods/fndecl.js";

const ENTRY_PREFIX = "entry-";

class EntryPoint {
  label = "entry-point";

  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  get sum() {
    return this.x + this.y;
  }
}

function entryFnDeclConstruct() {
  return new EntryPoint(1, 2);
}

function entryFnDeclImportedConstruct() {
  return new Point(1, 2);
}

function entryFnDeclClosureRead() {
  return ENTRY_PREFIX + "closure";
}

const entryArrowConstruct = () => new EntryPoint(1, 2);
const entryArrowImportedConstruct = () => new Point(1, 2);
const entryArrowClosureRead = () => ENTRY_PREFIX + "closure";

class EntryFactory {
  methodConstruct() {
    return new EntryPoint(1, 2);
  }

  methodImportedConstruct() {
    return new Point(1, 2);
  }

  methodClosureRead() {
    return ENTRY_PREFIX + "closure";
  }
}

describe("construction from module function declarations", () => {
  test("imported module: every callable form runs the constructor body", () => {
    for (const instance of [
      fnDeclConstruct(),
      arrowConstruct(),
      new Factory().methodConstruct(),
    ]) {
      expect(instance instanceof Point).toBe(true);
      expect(Object.keys(instance)).toEqual(["label", "x", "y"]);
      expect(instance.x).toBe(1);
      expect(instance.y).toBe(2);
      expect(instance.sum).toBe(3);
    }
  });

  test("imported module: every callable form reads instance fields", () => {
    expect(fnDeclFieldRead()).toBe("point:id-tagged");
    expect(arrowFieldRead()).toBe("point:id-tagged");
    expect(new Factory().methodFieldRead()).toBe("point:id-tagged");
  });

  test("imported module: every callable form reads module bindings", () => {
    expect(fnDeclClosureRead()).toBe("id-0");
    expect(arrowClosureRead()).toBe("id-0");
    expect(new Factory().methodClosureRead()).toBe("id-0");
  });

  test("entry file: every callable form runs the constructor body", () => {
    for (const instance of [
      entryFnDeclConstruct(),
      entryArrowConstruct(),
      new EntryFactory().methodConstruct(),
    ]) {
      expect(instance instanceof EntryPoint).toBe(true);
      expect(Object.keys(instance)).toEqual(["label", "x", "y"]);
      expect(instance.label).toBe("entry-point");
      expect(instance.sum).toBe(3);
    }
  });

  test("entry file: every callable form constructs an imported class", () => {
    for (const instance of [
      entryFnDeclImportedConstruct(),
      entryArrowImportedConstruct(),
      new EntryFactory().methodImportedConstruct(),
      new Point(1, 2),
    ]) {
      expect(instance instanceof Point).toBe(true);
      expect(Object.keys(instance)).toEqual(["label", "x", "y"]);
      expect(instance.sum).toBe(3);
    }
  });

  test("entry file: every callable form reads entry bindings", () => {
    expect(entryFnDeclClosureRead()).toBe("entry-closure");
    expect(entryArrowClosureRead()).toBe("entry-closure");
    expect(new EntryFactory().methodClosureRead()).toBe("entry-closure");
  });

  test("a module function declaration constructs a derived module class", () => {
    const tagged = fnDeclDerivedConstruct();
    expect(tagged instanceof Tagged).toBe(true);
    expect(tagged instanceof Point).toBe(true);
    expect(Object.keys(tagged)).toEqual(["label", "x", "y", "tag"]);
    expect(tagged.x).toBe(4);
    expect(tagged.y).toBe(5);
    expect(tagged.tag).toBe("id-tagged");
  });

  test("spread, bound, nested and arrow call sites all construct", () => {
    expect(fnDeclSpreadConstruct().sum).toBe(11);
    expect(fnDeclBoundConstruct().sum).toBe(23);
    expect(fnDeclNestedConstruct().sum).toBe(15);
    expect(fnDeclArrowConstruct().sum).toBe(19);
  });

  test("a subclass declared inside a module function runs the imported base", () => {
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
});

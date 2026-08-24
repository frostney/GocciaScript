// Differential suite L — construction and closure reads from the three callable
// forms, in an imported module and in the entry file.
//
// A module's top-level function declarations are created while the module is
// linked, not compiled with the rest of the module body, so in bytecode mode a
// declaration body and an arrow body next to it reach the same class through
// different machinery. The entry file has no such split, which is why every
// probe is run on both sides of the import.
import {
  Counted,
  Factory,
  ImplicitLeaf,
  Labelled,
  OverriddenLeaf,
  OverriddenMiddle,
  Point,
  StampedLabel,
  Tagged,
  arrowClosureRead,
  arrowConstruct,
  arrowFieldRead,
  derivedTickCount,
  fnDeclArrowConstruct,
  fnDeclBoundConstruct,
  fnDeclClosureRead,
  fnDeclConstruct,
  fnDeclDerivedConstruct,
  fnDeclFieldRead,
  fnDeclImplicitLeafConstruct,
  fnDeclLocalImplicitSubclassConstruct,
  fnDeclLocalSubclassConstruct,
  fnDeclLocalSubclassOfDerivedConstruct,
  fnDeclNestedConstruct,
  fnDeclOverriddenLeafConstruct,
  fnDeclSpreadConstruct,
  implicitTickCount,
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

  // The base of this local class is itself derived, so the base's own super()
  // is what initializes its fields. Constructing twice through the one helper
  // is deliberate: initializing a second time would advance `seq` by two, and a
  // super()-called flag left set by the first construction only becomes visible
  // on the second.
  test("a subclass of a derived module class initializes each field once", () => {
    const ticksBefore = derivedTickCount();
    const first = fnDeclLocalSubclassOfDerivedConstruct();
    const second = fnDeclLocalSubclassOfDerivedConstruct();

    for (const instance of [first, second]) {
      expect(instance instanceof Counted).toBe(true);
      expect(instance instanceof Point).toBe(true);
      // Exact insertion order: each class's fields are initialized when its own
      // super() returns (ES2026 §13.3.7.1 step 11), so Point's `label` comes
      // first, then Point's constructor writes, then Counted's `seq`, then
      // Counted's body, then Local's `stamp`, then Local's body.
      expect(Object.keys(instance)).toEqual([
        "label",
        "x",
        "y",
        "seq",
        "owner",
        "stamp",
        "local",
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

  // The entry file has no PREFIX binding. A field initializer resolved against
  // the scope that ran `new` would throw a ReferenceError here instead.
  test("entry file: a module class's field initializers read module bindings", () => {
    const labelled = new Labelled(7);
    expect(Object.keys(labelled)).toEqual(["label", "n"]);
    expect(labelled.label).toBe("id-labelled");
    expect(labelled.n).toBe(7);
  });

  test("entry file: a derived module class keeps module scope and field order", () => {
    const stamped = new StampedLabel(9);
    expect(stamped instanceof Labelled).toBe(true);
    expect(Object.keys(stamped)).toEqual(["label", "n", "stamp", "tail"]);
    expect(stamped.label).toBe("id-labelled");
    expect(stamped.stamp).toBe("id-stamp");
    expect(stamped.tail).toBe("id-tail");
    expect(stamped.secret()).toBe("id-secret");
  });

  test("entry file: constructing a derived module class initializes it once", () => {
    const ticksBefore = derivedTickCount();
    const counted = new Counted(30);

    expect(Object.keys(counted)).toEqual(["label", "x", "y", "seq", "owner"]);
    expect(counted.brand()).toBe("id-counted");
    expect(derivedTickCount() - ticksBefore).toBe(1);
  });

  // Classes with no constructor of their own take the implicit-default path
  // (§15.7.14 step 15a), which is separate machinery from an explicit super().
  // Every probe here is run from both sides of the import for that reason.
  test("an override returned by a base constructor carries no fields from its own layer", () => {
    for (const leaf of [fnDeclOverriddenLeafConstruct(), new OverriddenLeaf()]) {
      // §10.2.2 step 12: Overriding's `a` went on the receiver it was called
      // with, which the returned object replaced.
      expect(Object.keys(leaf)).toEqual(["tag", "b", "c"]);
      expect(leaf.tag).toBe("id-override");
      expect(leaf.b).toBe("id-b");
      expect(leaf.c).toBe("id-c");
    }
  });

  test("an implicit layer below an explicit derived one initializes each layer once", () => {
    for (const construct of [
      fnDeclImplicitLeafConstruct,
      () => new ImplicitLeaf(),
    ]) {
      const ticksBefore = implicitTickCount();
      const leaf = construct();

      expect(Object.keys(leaf)).toEqual(["x", "b", "c", "C", "d"]);
      expect(implicitTickCount() - ticksBefore).toBe(3);
      expect(leaf.c - leaf.b).toBe(1);
      expect(leaf.d - leaf.c).toBe(1);
    }
  });

  test("entry file: an all-implicit chain over a returning base still overrides", () => {
    expect(Object.keys(new OverriddenMiddle())).toEqual(["tag", "b"]);
  });
});

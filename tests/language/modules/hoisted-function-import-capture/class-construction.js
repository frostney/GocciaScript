/*---
description: Hoisted function declarations in imported modules construct module classes
features: [modules, compat-function, class]
---*/

import {
  Counted,
  Factory,
  Labelled,
  Point,
  StampedLabel,
  Tagged,
  arrowConstruct,
  derivedTickCount,
  fnDeclArrowConstruct,
  fnDeclBoundConstruct,
  fnDeclClosureRead,
  fnDeclConstruct,
  makeEvaluatorBuiltBase,
  makeFieldedSubclassOf,
  makeSubclassOf,
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
      // Exact insertion order: each class's fields land when its own super()
      // returns (§13.3.7.1 step 11), so Point's `label`, then Point's
      // constructor writes, then Counted's `seq`, then Counted's body, then
      // Local's `stamp`, then Local's body.
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

  test("a hoisted function declaration still reads module closure bindings", () => {
    expect(fnDeclClosureRead()).toBe("id-closure");
  });

  test("constructing an imported class from the entry file is unchanged", () => {
    expect(Object.keys(new Point(1, 2))).toEqual(["label", "x", "y"]);
    expect(new Tagged(4).tag).toBe("id-tagged");
  });

  // The entry file has no PREFIX binding of its own. A field initializer that
  // resolved against the scope running `new` would throw a ReferenceError here.
  test("entry-file construction runs field initializers in the module scope", () => {
    const labelled = new Labelled(7);
    expect(Object.keys(labelled)).toEqual(["label", "n"]);
    expect(labelled.label).toBe("id-labelled");
    expect(labelled.n).toBe(7);
  });

  test("entry-file construction of a derived class keeps module scope and order", () => {
    const stamped = new StampedLabel(9);
    expect(stamped instanceof Labelled).toBe(true);
    expect(Object.keys(stamped)).toEqual(["label", "n", "stamp", "tail"]);
    expect(stamped.label).toBe("id-labelled");
    expect(stamped.stamp).toBe("id-stamp");
    expect(stamped.tail).toBe("id-tail");
    expect(stamped.secret()).toBe("id-secret");
  });

  // §10.2.2 step 5: an implicit constructor forwards newTarget up the chain,
  // and the compiled superclass's implicit branch reads it back off the VM to
  // pick the receiver's prototype. Constructing from the entry file is what
  // makes the answer observable — the exotic Array receiver is allocated
  // several links above the class `new` names.
  test("an implicit compiled constructor forwards newTarget", () => {
    class CompiledMid extends Array {}
    const Sub = makeSubclassOf(CompiledMid);

    const direct = new Sub();
    expect(Object.getPrototypeOf(direct)).toBe(Sub.prototype);
    expect(Array.isArray(direct)).toBe(true);

    const Other = class Other extends CompiledMid {};
    const redirected = Reflect.construct(Sub, [], Other);
    expect(Object.getPrototypeOf(redirected)).toBe(Other.prototype);
    expect(Array.isArray(redirected)).toBe(true);
  });

  // TGocciaMethodValue.CallWithThisValue runs only the constructor body, so a
  // superclass whose instance elements are AST expressions loses them unless
  // the VM hands them back to the evaluator. Interpreted mode never had the
  // gap, which made this a mode divergence rather than a missing fast path.
  test("a compiled subclass of an evaluator-built base runs the base's fields", () => {
    const Base = makeEvaluatorBuiltBase();

    class ExplicitSuper extends Base {
      own = "explicit";

      constructor() {
        super(3);
        this.tail = "explicit-tail";
      }
    }

    class ImplicitSuper extends Base {}

    const explicit = new ExplicitSuper();
    expect(Object.keys(explicit)).toEqual(["label", "n", "own", "tail"]);
    expect(explicit.label).toBe("id-evaluator-base");
    expect(explicit.n).toBe(3);
    expect(explicit.brand()).toBe("id-evaluator-brand");

    const implicit = new ImplicitSuper(5);
    expect(implicit.label).toBe("id-evaluator-base");
    expect(implicit.n).toBe(5);
    expect(implicit.brand()).toBe("id-evaluator-brand");
  });

  // §15.7.14 step 15a: the evaluator-built class in the middle has no
  // constructor of its own, so the compiled subclass's implicit constructor
  // used to walk straight past it to the base that does — dropping the middle
  // class's fields and its private brand.
  test("a compiled subclass runs an evaluator-built middle class's fields", () => {
    const Base = makeEvaluatorBuiltBase();
    const Middle = makeFieldedSubclassOf(Base);

    class Compiled extends Middle {
      own = "compiled";
    }

    const instance = new Compiled(4);
    expect(Object.keys(instance)).toEqual(["label", "n", "middle", "own"]);
    expect(instance.middle).toBe("id-evaluator-middle");
    expect(instance.middleBrand()).toBe("id-evaluator-middle-brand");
    expect(instance.brand()).toBe("id-evaluator-brand");
    expect(instance.n).toBe(4);

    const constructed = Reflect.construct(Compiled, [6]);
    expect(Object.keys(constructed)).toEqual(["label", "n", "middle", "own"]);
    expect(constructed.middle).toBe("id-evaluator-middle");
  });

  test("entry-file construction of a derived module class initializes once", () => {
    const ticksBefore = derivedTickCount();
    const counted = new Counted(30);

    expect(Object.keys(counted)).toEqual(["label", "x", "y", "seq", "owner"]);
    expect(counted.brand()).toBe("id-counted");
    expect(derivedTickCount() - ticksBefore).toBe(1);
  });
});

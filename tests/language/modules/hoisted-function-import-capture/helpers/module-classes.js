const PREFIX = "id-";

export class Point {
  label = "point";

  constructor(x, y) {
    this.x = x;
    this.y = y;
  }

  get sum() {
    return this.x + this.y;
  }
}

export class Tagged extends Point {
  constructor(x) {
    super(x, x + 1);
    this.tag = PREFIX + "tagged";
  }
}

// ES2026 §15.7.10 ClassFieldDefinitionEvaluation step 2b captures the class's
// own definition environment as each field initializer's [[Environment]].
// These two read PREFIX from a *field* initializer rather than a constructor
// body, so constructing them from the entry file — which has no PREFIX — is
// what proves the initializer did not resolve against the caller's scope.
export class Labelled {
  label = PREFIX + "labelled";

  constructor(n) {
    this.n = n;
  }
}

export class StampedLabel extends Labelled {
  stamp = PREFIX + "stamp";

  #secret = PREFIX + "secret";

  constructor(n) {
    super(n);
    this.tail = PREFIX + "tail";
  }

  secret() {
    return this.#secret;
  }
}

let derivedTicks = 0;

export function derivedTickCount() {
  return derivedTicks;
}

// A derived module class that calls super() and owns both a side-effecting
// instance field and a private field, so a repeated initializer run is
// observable as a doubled tick or a twice-stamped private brand.
export class Counted extends Point {
  seq = ++derivedTicks;

  #brand = PREFIX + "counted";

  constructor(x) {
    super(x, x + 1);
    this.owner = "counted";
  }

  brand() {
    return this.#brand;
  }
}

export function fnDeclConstruct() {
  return new Point(1, 2);
}

export function fnDeclDerivedConstruct() {
  return new Tagged(4);
}

export function fnDeclSpreadConstruct() {
  return new Point(...[5, 6]);
}

export function fnDeclBoundConstruct() {
  const Bound = Point.bind(null, 11);
  return new Bound(12);
}

export function fnDeclNestedConstruct() {
  function inner() {
    return new Point(7, 8);
  }

  return inner();
}

export function fnDeclArrowConstruct() {
  const make = () => new Point(9, 10);
  return make();
}

export function fnDeclLocalSubclassConstruct() {
  class Local extends Point {
    constructor() {
      super(13, 14);
      this.local = PREFIX + "local";
    }
  }

  return new Local();
}

export function fnDeclLocalImplicitSubclassConstruct() {
  class Local extends Point {}

  return new Local(15, 16);
}

export function fnDeclLocalSubclassOfDerivedConstruct() {
  class Local extends Counted {
    stamp = "local";

    #localBrand = PREFIX + "local";

    constructor() {
      super(20);
      this.local = true;
    }

    localBrand() {
      return this.#localBrand;
    }
  }

  return new Local();
}

export function fnDeclClosureRead() {
  return PREFIX + "closure";
}

export const arrowConstruct = () => new Point(1, 2);

export class Factory {
  methodConstruct() {
    return new Point(1, 2);
  }
}

// A class *created inside* a hoisted function declaration: in bytecode mode
// the declaration runs in the tree-walk evaluator, so this class records its
// instance elements as AST expressions rather than as compiled initializers.
// Extending it from the entry file is what puts an evaluator-built superclass
// under a compiled one.
export function makeEvaluatorBuiltBase() {
  return class EvaluatorBuiltBase {
    label = PREFIX + "evaluator-base";

    #brand = PREFIX + "evaluator-brand";

    constructor(n) {
      this.n = n;
    }

    brand() {
      return this.#brand;
    }
  };
}

// Another evaluator-built class, this time with no constructor of its own, so
// construction reaches the compiled superclass's *implicit* branch.
export function makeSubclassOf(Base) {
  return class EvaluatorBuiltSub extends Base {};
}

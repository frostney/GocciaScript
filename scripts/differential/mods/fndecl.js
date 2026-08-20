// Support module for differential suite L. Every callable form below — hoisted
// function declaration, arrow, class method — performs the same three probes:
// it constructs a module class, reads a field off the instance, and reads a
// module-level binding through its closure. A module's top-level function
// declarations are created while linking rather than compiled with the rest of
// the module, so the declaration forms and the arrow/method forms take
// genuinely different paths to the same class.
const PREFIX = "id-";
const ORIGIN = { x: 0, y: 0 };

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

// ES2026 §15.7.10 ClassFieldDefinitionEvaluation step 2b: a field initializer
// closes over the class's own definition environment. These two read PREFIX
// from a *field* initializer rather than a constructor body, so constructing
// them from the entry file — which has no PREFIX — is what shows the
// initializer did not resolve against the scope that ran `new`.
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

// A *derived* module class — it calls super(), and it owns both an instance
// field with an observable side effect and a private field. Extending this one
// rather than the base Point is what exercises the ordering rule that a derived
// class initializes its fields when its own super() returns: running them a
// second time would advance the tick twice and stamp the private brand twice.
let derivedTicks = 0;

export function derivedTickCount() {
  return derivedTicks;
}

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

// Implicit-constructor layers. A class with no constructor of its own takes the
// implicit-default-constructor path (§15.7.14 step 15a), which is separate
// machinery from an explicit super() — and in a module it is reached through
// the linking-time function-declaration path as well.
export class Overriding {
  a = PREFIX + "a";

  constructor() {
    return { tag: PREFIX + "override" };
  }
}

export class OverriddenMiddle extends Overriding {
  b = PREFIX + "b";
}

export class OverriddenLeaf extends OverriddenMiddle {
  c = PREFIX + "c";
}

let implicitTicks = 0;

export function implicitTickCount() {
  return implicitTicks;
}

export class ImplicitBase {
  constructor() {
    this.x = 1;
  }
}

export class ImplicitMiddle extends ImplicitBase {
  b = ++implicitTicks;
}

export class ExplicitOverMiddle extends ImplicitMiddle {
  c = ++implicitTicks;

  constructor() {
    super();
    this.C = 1;
  }
}

export class ImplicitLeaf extends ExplicitOverMiddle {
  d = ++implicitTicks;
}

export function fnDeclImplicitLeafConstruct() {
  return new ImplicitLeaf();
}

export function fnDeclOverriddenLeafConstruct() {
  return new OverriddenLeaf();
}

export function fnDeclConstruct() {
  return new Point(1, 2);
}

export function fnDeclFieldRead() {
  return new Point(1, 2).label + ":" + new Tagged(4).tag;
}

export function fnDeclClosureRead() {
  return PREFIX + ORIGIN.x;
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

// Called more than once on purpose: the constructor being entered owns the
// super()-called flag, so a leak only shows on the second call.
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

export const arrowConstruct = () => new Point(1, 2);
export const arrowFieldRead = () => new Point(1, 2).label + ":" + new Tagged(4).tag;
export const arrowClosureRead = () => PREFIX + ORIGIN.x;

export class Factory {
  methodConstruct() {
    return new Point(1, 2);
  }

  methodFieldRead() {
    return new Point(1, 2).label + ":" + new Tagged(4).tag;
  }

  methodClosureRead() {
    return PREFIX + ORIGIN.x;
  }
}

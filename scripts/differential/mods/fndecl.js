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

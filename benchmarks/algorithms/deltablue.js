/*---
description: DeltaBlue constraint solver benchmarks — adapted from Octane 2.0 DeltaBlue benchmark
---*/

// Infinite iterator for replacing while loops
const forever = {
  [Symbol.iterator]() {
    return { next() { return { value: undefined, done: false }; } };
  }
};

// Strength levels (higher number = weaker)
const REQUIRED = 0;
const STRONG_PREFERRED = 1;
const PREFERRED = 2;
const STRONG_DEFAULT = 3;
const NORMAL = 4;
const WEAK_DEFAULT = 5;
const WEAKEST = 6;

const stronger = (a, b) => a < b;
const weaker = (a, b) => a > b;

// Direction constants
const NONE = 0;
const FORWARD = 1;
const BACKWARD = 2;

class Variable {
  constructor(name, initialValue) {
    this.name = name;
    this.value = initialValue;
    this.constraints = [];
    this.determinedBy = null;
    this.mark = 0;
    this.walkStrength = WEAKEST;
    this.stay = true;
  }

  addConstraint(c) { this.constraints.push(c); }

  removeConstraint(c) {
    this.constraints = this.constraints.filter((x) => x !== c);
    if (this.determinedBy === c) { this.determinedBy = null; }
  }
}

class Constraint {
  constructor(strength) {
    this.strength = strength;
  }

  addConstraint(planner) {
    this.addToGraph();
    planner.incrementalAdd(this);
  }

  destroyConstraint(planner) {
    if (this.isSatisfied()) { planner.incrementalRemove(this); }
    this.removeFromGraph();
  }

  inputsKnown(mark) { return true; }
}

class UnaryConstraint extends Constraint {
  constructor(v, strength) {
    super(strength);
    this.output = v;
    this.satisfied = false;
    this.direction = NONE;
  }

  addToGraph() {
    this.output.addConstraint(this);
    this.direction = NONE;
    this.satisfied = false;
  }

  removeFromGraph() {
    this.output.removeConstraint(this);
    this.satisfied = false;
    this.direction = NONE;
  }

  isSatisfied() { return this.satisfied; }

  chooseMethod(mark) {
    this.satisfied = this.output.mark !== mark && stronger(this.strength, this.output.walkStrength);
    this.direction = this.satisfied ? FORWARD : NONE;
  }

  markInputs(mark) {}

  inputsKnown(mark) { return true; }

  output_() { return this.output; }

  recalculate() {
    this.output.walkStrength = this.strength;
    this.output.stay = true;
    this.execute();
  }

  markUnsatisfied() {
    this.satisfied = false;
    this.direction = NONE;
  }
}

class StayConstraint extends UnaryConstraint {
  execute() {} // Value stays the same
}

class EditConstraint extends UnaryConstraint {
  execute() {} // Value is set externally
}

class BinaryConstraint extends Constraint {
  constructor(v1, v2, strength) {
    super(strength);
    this.v1 = v1;
    this.v2 = v2;
    this.direction = NONE;
  }

  addToGraph() {
    this.v1.addConstraint(this);
    this.v2.addConstraint(this);
    this.direction = NONE;
  }

  removeFromGraph() {
    this.v1.removeConstraint(this);
    this.v2.removeConstraint(this);
    this.direction = NONE;
  }

  isSatisfied() { return this.direction !== NONE; }

  chooseMethod(mark) {
    if (this.v1.mark === mark) {
      this.direction = this.v2.mark !== mark && stronger(this.strength, this.v2.walkStrength)
        ? FORWARD : NONE;
    } else if (this.v2.mark === mark) {
      this.direction = this.v1.mark !== mark && stronger(this.strength, this.v1.walkStrength)
        ? BACKWARD : NONE;
    } else if (weaker(this.v1.walkStrength, this.v2.walkStrength)) {
      this.direction = stronger(this.strength, this.v1.walkStrength) ? BACKWARD : NONE;
    } else {
      this.direction = stronger(this.strength, this.v2.walkStrength) ? FORWARD : NONE;
    }
  }

  markInputs(mark) {
    if (this.direction === FORWARD) {
      this.v1.mark = mark;
    } else {
      this.v2.mark = mark;
    }
  }

  inputsKnown(mark) {
    const input = this.direction === FORWARD ? this.v1 : this.v2;
    return input.mark === mark || input.stay || input.determinedBy === null;
  }

  output_() {
    return this.direction === FORWARD ? this.v2 : this.v1;
  }

  markUnsatisfied() { this.direction = NONE; }

  recalculate() {
    const input = this.direction === FORWARD ? this.v1 : this.v2;
    const output = this.output_();
    output.walkStrength = weaker(this.strength, input.walkStrength) ? this.strength : input.walkStrength;
    output.stay = input.stay;
    if (output.stay) { this.execute(); }
  }
}

class EqualityConstraint extends BinaryConstraint {
  execute() {
    if (this.direction === FORWARD) {
      this.v2.value = this.v1.value;
    } else {
      this.v1.value = this.v2.value;
    }
  }
}

class ScaleConstraint extends BinaryConstraint {
  constructor(src, scale, offset, dest, strength) {
    super(src, dest, strength);
    this.scale = scale;
    this.offset = offset;
  }

  execute() {
    if (this.direction === FORWARD) {
      this.v2.value = this.v1.value * this.scale.value + this.offset.value;
    } else {
      this.v1.value = (this.v2.value - this.offset.value) / this.scale.value;
    }
  }
}

class Planner {
  constructor() {
    this.currentMark = 0;
  }

  newMark() {
    this.currentMark = this.currentMark + 1;
    return this.currentMark;
  }

  incrementalAdd(c) {
    const mark = this.newMark();
    let overridden = this.#satisfy(c, mark);
    for (const _ of forever) {
      if (overridden === null) { break; }
      overridden.recalculate();
      overridden = this.#satisfy(overridden, mark);
    }
  }

  #satisfy(c, mark) {
    c.chooseMethod(mark);
    if (!c.isSatisfied()) { return null; }
    c.markInputs(mark);
    const out = c.output_();
    const overridden = out.determinedBy;
    if (overridden !== null) { overridden.markUnsatisfied(); }
    out.determinedBy = c;
    c.recalculate();
    return overridden;
  }

  incrementalRemove(c) {
    const out = c.output_();
    c.markUnsatisfied();
    c.removeFromGraph();
    const unsatisfied = this.#removePropagateFrom(out);
    for (const u of unsatisfied) {
      this.incrementalAdd(u);
    }
  }

  #removePropagateFrom(out) {
    out.determinedBy = null;
    out.walkStrength = WEAKEST;
    out.stay = true;
    const unsatisfied = [];
    const todo = [out];
    for (const _ of forever) {
      if (todo.length === 0) { break; }
      const v = todo[0];
      todo.splice(0, 1);
      for (const c of v.constraints) {
        if (!c.isSatisfied()) { unsatisfied.push(c); }
      }
      for (const c of v.constraints) {
        if (c.isSatisfied() && c.output_() !== v) {
          c.recalculate();
          todo.push(c.output_());
        }
      }
    }
    return unsatisfied;
  }

  extractPlanFromConstraints(constraints) {
    const sources = constraints.filter((c) => c.isSatisfied() && c.inputsKnown(this.currentMark));
    const plan = [];
    const mark = this.newMark();
    const queue = [...sources];
    for (const _ of forever) {
      if (queue.length === 0) { break; }
      const c = queue[0];
      queue.splice(0, 1);
      if (c.output_().mark !== mark && c.inputsKnown(mark)) {
        plan.push(c);
        c.output_().mark = mark;
        const newSources = c.output_().constraints.filter(
          (next) => next !== c && next.isSatisfied() && next.inputsKnown(mark)
        );
        for (const s of newSources) {
          queue.push(s);
        }
      }
    }
    return plan;
  }
}

// Benchmark: chain of equality constraints
const chainTest = (n) => {
  const planner = new Planner();
  const vars = Array.from({ length: n + 1 }, (_, i) => new Variable("v" + i, 0));

  // Build chain of equality constraints
  for (const [i, v] of vars.slice(0, -1).entries()) {
    const c = new EqualityConstraint(v, vars[i + 1], REQUIRED);
    c.addConstraint(planner);
  }

  // Add stay constraints
  for (const v of vars) {
    const s = new StayConstraint(v, WEAK_DEFAULT);
    s.addConstraint(planner);
  }

  // Edit the first variable
  const edit = new EditConstraint(vars[0], STRONG_PREFERRED);
  edit.addConstraint(planner);
  vars[0].value = 100;

  // Propagate
  const plan = planner.extractPlanFromConstraints(
    vars.flatMap((v) => v.constraints)
  );
  for (const c of plan) { c.execute(); }

  edit.destroyConstraint(planner);
  return vars[n].value;
};

// Benchmark: projection with scale constraints
const projectionTest = (n) => {
  const planner = new Planner();
  const scale = new Variable("scale", 2);
  const offset = new Variable("offset", 10);
  const src = Array.from({ length: n }, (_, i) => new Variable("src" + i, i));
  const dst = Array.from({ length: n }, (_, i) => new Variable("dst" + i, 0));

  for (const [i, s] of src.entries()) {
    const c = new ScaleConstraint(s, scale, offset, dst[i], REQUIRED);
    c.addConstraint(planner);
  }

  for (const v of [...src, ...dst]) {
    const s = new StayConstraint(v, WEAK_DEFAULT);
    s.addConstraint(planner);
  }

  // Edit source values
  for (const [i, s] of src.entries()) {
    const edit = new EditConstraint(s, STRONG_PREFERRED);
    edit.addConstraint(planner);
    s.value = i * 3;
    const plan = planner.extractPlanFromConstraints(
      [...src, ...dst].flatMap((v) => v.constraints)
    );
    for (const c of plan) { c.execute(); }
    edit.destroyConstraint(planner);
  }

  return dst.map((d) => d.value);
};

suite("deltablue constraint solver", () => {
  bench("chain test (50 constraints)", {
    run: () => {
      chainTest(50);
    },
  });

  bench("chain test (100 constraints)", {
    run: () => {
      chainTest(100);
    },
  });

  bench("projection test (20 variables)", {
    run: () => {
      projectionTest(20);
    },
  });

  bench("projection test (50 variables)", {
    run: () => {
      projectionTest(50);
    },
  });
});

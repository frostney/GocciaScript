/*---
description: A closure that writes an enclosing let binding is seen by the next pass of a loop, wherever in the loop body the closure is created
features: [let, arrow-functions, closures, for-of, classes, destructuring]
---*/

// The compiler examines a loop before compiling it, because a closure created
// late in the body runs before the reads at the top on the next pass. Each
// variant below puts the closure in a different syntactic position.

const pass = (value) => value;

describe("a closure created in each kind of position inside a loop", () => {
  // Every variant reads `value`, then writes it through the closure created on
  // the pass before, then creates the closure for the next pass somewhere else
  // in the loop body.
  test("is seen by the read on the next pass", () => {
    const sink = (value) => value;
    const variants = {
      statement: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          write = ((next) => (value = next));
        }
        return seen;
      },
      ifCondition: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          if ((write = ((next) => (value = next)))) { sink(1); }
        }
        return seen;
      },
      ifThen: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          if (step) { write = ((next) => (value = next)); }
        }
        return seen;
      },
      ifElse: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          if (!step) { sink(1); } else { write = ((next) => (value = next)); }
        }
        return seen;
      },
      block: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          { { write = ((next) => (value = next)); } }
        }
        return seen;
      },
      tryBlock: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          try { write = ((next) => (value = next)); } catch (error) { sink(error); }
        }
        return seen;
      },
      catchBlock: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          try { throw step; } catch (error) { write = ((next) => (value = next)); }
        }
        return seen;
      },
      finallyBlock: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          try { sink(1); } finally { write = ((next) => (value = next)); }
        }
        return seen;
      },
      thrown: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          try { throw (write = ((next) => (value = next))); } catch (error) { sink(error); }
        }
        return seen;
      },
      switchDiscriminant: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          switch ((write = ((next) => (value = next))) ? 1 : 0) { default: sink(1); }
        }
        return seen;
      },
      switchCaseTest: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          switch (1) { case (write = ((next) => (value = next))) ? 1 : 0: sink(1); }
        }
        return seen;
      },
      switchClause: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          switch (1) { case 1: write = ((next) => (value = next)); }
        }
        return seen;
      },
      nestedLoopIterable: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          for (const inner of ((write = ((next) => (value = next))), [1])) { sink(inner); }
        }
        return seen;
      },
      nestedLoopBody: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          for (const inner of [1]) { write = ((next) => (value = next)); }
        }
        return seen;
      },
      declaration: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          const made = (write = ((next) => (value = next))); sink(made);
        }
        return seen;
      },
      destructuringDeclaration: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          const [made = (write = ((next) => (value = next)))] = []; sink(made);
        }
        return seen;
      },
      destructuringInitializer: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          const [made] = [(write = ((next) => (value = next)))]; sink(made);
        }
        return seen;
      },
      callArgument: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink((write = ((next) => (value = next))));
        }
        return seen;
      },
      callCallee: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ((write = ((next) => (value = next))), sink)(1);
        }
        return seen;
      },
      constructArgument: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(new Array((write = ((next) => (value = next)))));
        }
        return seen;
      },
      arrayLiteral: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink([(write = ((next) => (value = next)))]);
        }
        return seen;
      },
      arraySpread: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink([...[(write = ((next) => (value = next)))]]);
        }
        return seen;
      },
      objectLiteral: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink({ made: (write = ((next) => (value = next))) });
        }
        return seen;
      },
      objectComputedKey: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink({ [((write = ((next) => (value = next))), "k")]: 1 });
        }
        return seen;
      },
      memberKey: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(seen[((write = ((next) => (value = next))), 0)]);
        }
        return seen;
      },
      storeValue: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          holder.made = (write = ((next) => (value = next)));
        }
        return seen;
      },
      elementStoreKey: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          holder[((write = ((next) => (value = next))), "made")] = 1;
        }
        return seen;
      },
      compoundStoreValue: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          holder.text += typeof (write = ((next) => (value = next)));
        }
        return seen;
      },
      conditional: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(step ? (write = ((next) => (value = next))) : 0);
        }
        return seen;
      },
      logical: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(step && (write = ((next) => (value = next))));
        }
        return seen;
      },
      nullish: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(null ?? (write = ((next) => (value = next))));
        }
        return seen;
      },
      sequence: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink((0, (write = ((next) => (value = next)))));
        }
        return seen;
      },
      template: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(`${typeof (write = ((next) => (value = next)))}`);
        }
        return seen;
      },
      unary: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(!(write = ((next) => (value = next))));
        }
        return seen;
      },
      binary: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(1 + typeof (write = ((next) => (value = next))));
        }
        return seen;
      },
      destructuringAssignment: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          [write] = [((next) => (value = next))];
        }
        return seen;
      },
      destructuringAssignmentDefault: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          [write = ((next) => (value = next))] = [];
        }
        return seen;
      },
      binaryLeft: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(typeof (write = ((next) => (value = next))) + 1);
        }
        return seen;
      },
      memberObject: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(((write = ((next) => (value = next))), seen)[0]);
        }
        return seen;
      },
      conditionalTest: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink((write = ((next) => (value = next))) ? 1 : 0);
        }
        return seen;
      },
      conditionalElse: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(!step ? 0 : (write = ((next) => (value = next))));
        }
        return seen;
      },
      compoundAssignment: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          let text = ""; text += typeof (write = ((next) => (value = next))); sink(text);
        }
        return seen;
      },
      memberIncrement: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          holder[((write = ((next) => (value = next))), "count")]++;
        }
        return seen;
      },
      storeObject: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ((write = ((next) => (value = next))), holder).made = 1;
        }
        return seen;
      },
      elementStoreValue: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          holder["made"] = (write = ((next) => (value = next)));
        }
        return seen;
      },
      elementStoreObject: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ((write = ((next) => (value = next))), holder)["made"] = 1;
        }
        return seen;
      },
      compoundElementStore: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          holder["text"] += typeof (write = ((next) => (value = next)));
        }
        return seen;
      },
      objectComputedValue: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink({ ["k"]: (write = ((next) => (value = next))) });
        }
        return seen;
      },
      taggedTemplate: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(sink`${(write = ((next) => (value = next)))}`);
        }
        return seen;
      },
      optionalCall: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(holder.missing?.((write = 0)), (write = ((next) => (value = next))));
        }
        return seen;
      },
      loopBindingDefault: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          for (const [inner = (write = ((next) => (value = next)))] of [[]]) { sink(inner); }
        }
        return seen;
      },
      catchBindingDefault: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          try { throw {}; } catch ({ inner = (write = ((next) => (value = next))) }) { sink(inner); }
        }
        return seen;
      },
      loopAssignmentTargetDefault: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          for ([holder.made = (write = ((next) => (value = next)))] of [[]]) { sink(holder.made); }
        }
        return seen;
      },
      restElementTarget: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          [...holder[((write = ((next) => (value = next))), "rest")]] = [1];
        }
        return seen;
      },
      objectPatternComputedKey: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ({ [((write = ((next) => (value = next))), "k")]: holder.made } = { k: 1 });
        }
        return seen;
      },
      objectPatternTarget: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ({ k: holder[((write = ((next) => (value = next))), "made")] } = { k: 1 });
        }
        return seen;
      },
      defaultedMemberTarget: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          [holder[((write = ((next) => (value = next))), "made")] = 1] = [];
        }
        return seen;
      },
      returnOverriddenByFinally: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          try { return (write = ((next) => (value = next))); } finally { continue; }
        }
        return seen;
      },
      arrowDirectly: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          write = (next) => { value = next; };
        }
        return seen;
      },
      methodDirectly: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          write = { set(next) { value = next; } }.set;
        }
        return seen;
      },
      accessorDirectly: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          write = (next) => { ({ set to(amount) { value = amount; } }).to = next; };
        }
        return seen;
      },
      classDirectly: (steps) => {
        let value = 1;
        let write = null;
        const seen = [];
        const holder = { text: "" };
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          write = new (class { set(next) { value = next; } })().set;
        }
        return seen;
      },
    };
    const results = {};
    for (const name of Object.keys(variants)) {
      results[name] = variants[name](pass([1, 2, 3])).join();
    }
    const expected = {};
    for (const name of Object.keys(variants)) {
      expected[name] = "2,2,21";
    }

    expect(results).toEqual(expected);
  });

  test("is seen when it sits in a private member position", () => {
    class Holder {
      #made = 0;
      store(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ((write = (next) => (value = next)), this).#made = step;
        }
        return seen.join();
      }
      storeValue(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          this.#made = write = (next) => (value = next);
        }
        return seen.join();
      }
      compound(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          ((write = (next) => (value = next)), this).#made += step;
        }
        return seen.join();
      }
      compoundValue(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          this.#made += typeof (write = (next) => (value = next));
        }
        return seen.join();
      }
      read(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          sink(((write = (next) => (value = next)), this).#made);
        }
        return seen.join();
      }
      destructure(steps) {
        let value = 1;
        let write = null;
        const seen = [];
        for (const step of steps) {
          seen.push(value + 1);
          if (write) write(step * 10);
          [((write = (next) => (value = next)), this).#made] = [step];
        }
        return seen.join();
      }
    }
    const sink = (value) => value;
    const steps = pass([1, 2, 3]);

    expect([
      new Holder().store(steps),
      new Holder().storeValue(steps),
      new Holder().compound(steps),
      new Holder().compoundValue(steps),
      new Holder().read(steps),
      new Holder().destructure(steps),
    ]).toEqual(["2,2,21", "2,2,21", "2,2,21", "2,2,21", "2,2,21", "2,2,21"]);
  });
});

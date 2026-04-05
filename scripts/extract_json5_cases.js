#!/usr/bin/env node

const Module = require("module");
const path = require("path");

if (process.argv.length !== 3) {
  console.error("Usage: extract_json5_cases.js <json5-repo-dir>");
  process.exit(2);
}

const suiteDir = path.resolve(process.argv[2]);
const testDir = path.join(suiteDir, "test");
const realJSON5 = require(path.join(suiteDir, "lib"));

const cases = [];
const parseQueue = [];
let ignoredAssertionCount = 0;
let lastThrownSource = null;

function encodeNumber(value) {
  if (Number.isNaN(value)) {
    return { type: "number", value: "NaN" };
  }
  if (value === Infinity) {
    return { type: "number", value: "Infinity" };
  }
  if (value === -Infinity) {
    return { type: "number", value: "-Infinity" };
  }
  if (Object.is(value, -0)) {
    return { type: "number", value: "-0" };
  }
  return { type: "number", value: String(value) };
}

function encodeValue(value) {
  if (value === null) {
    return { type: "null" };
  }
  if (typeof value === "boolean") {
    return { type: "boolean", value };
  }
  if (typeof value === "string") {
    return { type: "string", value };
  }
  if (typeof value === "number") {
    return encodeNumber(value);
  }
  if (Array.isArray(value)) {
    return {
      type: "array",
      items: value.map((item) => encodeValue(item)),
    };
  }
  if (typeof value === "object") {
    return {
      type: "object",
      entries: Object.getOwnPropertyNames(value).map((key) => ({
        key,
        value: encodeValue(value[key]),
      })),
    };
  }
  throw new Error(`Unsupported expected value type: ${typeof value}`);
}

function recordValidCase(description) {
  if (ignoredAssertionCount > 0) {
    ignoredAssertionCount -= 1;
    return;
  }

  const record = parseQueue.shift();
  if (!record) {
    throw new Error(
      `No captured parse result for ${description ?? "unnamed valid case"}`,
    );
  }
  cases.push({
    id: description || `valid-${cases.length + 1}`,
    valid: true,
    source: record.source,
    expected: encodeValue(record.value),
  });
}

function recordInvalidCase(description) {
  if (lastThrownSource === null) {
    throw new Error(
      `No captured failing source for ${description ?? "unnamed invalid case"}`,
    );
  }
  cases.push({
    id: description || `invalid-${cases.length + 1}`,
    valid: false,
    source: lastThrownSource,
  });
  lastThrownSource = null;
}

const wrappedJSON5 = {
  parse(text, reviver) {
    const source = String(text);
    if (arguments.length > 1) {
      ignoredAssertionCount += 1;
      return realJSON5.parse(text, reviver);
    }

    try {
      const value = realJSON5.parse(text);
      parseQueue.push({ source, value });
      return value;
    } catch (error) {
      lastThrownSource = source;
      throw error;
    }
  },
};

function createTapHarness() {
  const harness = {
    test(_name, callback) {
      callback(harness);
    },
    end() {},
    strictSame(_actual, _expected, description) {
      recordValidCase(description);
    },
    equal(_actual, _expected, description) {
      recordValidCase(description);
    },
    ok(_condition, description) {
      recordValidCase(description);
    },
    throws(callback, _matcher, description) {
      try {
        callback();
      } catch (_error) {
        recordInvalidCase(description);
        return;
      }
      throw new Error(
        `Expected parser failure for ${description ?? "unnamed invalid case"}`,
      );
    },
  };
  return { test: (_name, callback) => callback(harness) };
}

function createSinonHarness() {
  return {
    mock() {
      return {
        expects() {
          return this;
        },
        twice() {
          return this;
        },
        calledWithMatch() {
          return this;
        },
        verify() {},
        restore() {},
      };
    },
  };
}

const assertHarness = {
  deepStrictEqual() {
    recordValidCase("assert.deepStrictEqual");
  },
  strictEqual() {
    recordValidCase("assert.strictEqual");
  },
  equal() {
    recordValidCase("assert.equal");
  },
};

const realLoad = Module._load;
Module._load = function patchedLoad(request, parent, isMain) {
  if (request === "tap") {
    return createTapHarness();
  }
  if (request === "sinon") {
    return createSinonHarness();
  }
  if (request === "assert") {
    return assertHarness;
  }
  if (request === "../lib") {
    return wrappedJSON5;
  }
  return realLoad.call(this, request, parent, isMain);
};

require(path.join(testDir, "parse.js"));
require(path.join(testDir, "errors.js"));

process.stdout.write(`${JSON.stringify({ cases }, null, 2)}\n`);

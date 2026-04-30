// test262 propertyHelper.js -- GocciaScript-compatible reimplementation
// Provides verifyProperty and related helpers for checking property descriptors.

const verifyProperty = (obj, name, desc, options) => {
  const actual = Object.getOwnPropertyDescriptor(obj, name);

  if (actual === undefined) {
    throw new Test262Error(
      "Expected property '" + String(name) + "' to exist on object"
    );
  }

  if ("value" in desc) {
    if (!Object.is(actual.value, desc.value)) {
      throw new Test262Error(
        "Expected " + String(name) + ".value to be " +
        String(desc.value) + " but got " + String(actual.value)
      );
    }
  }

  if ("writable" in desc) {
    if (actual.writable !== desc.writable) {
      throw new Test262Error(
        "Expected " + String(name) + ".writable to be " +
        String(desc.writable) + " but got " + String(actual.writable)
      );
    }
  }

  if ("enumerable" in desc) {
    if (actual.enumerable !== desc.enumerable) {
      throw new Test262Error(
        "Expected " + String(name) + ".enumerable to be " +
        String(desc.enumerable) + " but got " + String(actual.enumerable)
      );
    }
  }

  if ("configurable" in desc) {
    if (actual.configurable !== desc.configurable) {
      throw new Test262Error(
        "Expected " + String(name) + ".configurable to be " +
        String(desc.configurable) + " but got " + String(actual.configurable)
      );
    }
  }

  if ("get" in desc) {
    if (actual.get !== desc.get) {
      throw new Test262Error(
        "Expected " + String(name) + ".get to match"
      );
    }
  }

  if ("set" in desc) {
    if (actual.set !== desc.set) {
      throw new Test262Error(
        "Expected " + String(name) + ".set to match"
      );
    }
  }
};

const verifyNotEnumerable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  if (desc === undefined) {
    throw new Test262Error(
      "Expected property '" + String(name) + "' to exist on object"
    );
  }
  if (desc.enumerable) {
    throw new Test262Error(
      "Expected " + String(name) + " to not be enumerable"
    );
  }
};

const verifyEnumerable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  if (!desc || !desc.enumerable) {
    throw new Test262Error(
      "Expected " + String(name) + " to be enumerable"
    );
  }
};

const verifyNotWritable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  if (desc === undefined) {
    throw new Test262Error(
      "Expected property '" + String(name) + "' to exist on object"
    );
  }
  if (desc.writable) {
    throw new Test262Error(
      "Expected " + String(name) + " to not be writable"
    );
  }
};

const verifyWritable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  if (!desc || !desc.writable) {
    throw new Test262Error(
      "Expected " + String(name) + " to be writable"
    );
  }
};

const verifyNotConfigurable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  if (desc === undefined) {
    throw new Test262Error(
      "Expected property '" + String(name) + "' to exist on object"
    );
  }
  if (desc.configurable) {
    throw new Test262Error(
      "Expected " + String(name) + " to not be configurable"
    );
  }
};

const verifyConfigurable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  if (!desc || !desc.configurable) {
    throw new Test262Error(
      "Expected " + String(name) + " to be configurable"
    );
  }
};

const isConfigurable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  return desc !== undefined && desc.configurable === true;
};

const isEnumerable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  return desc !== undefined && desc.enumerable === true;
};

const isWritable = (obj, name) => {
  const desc = Object.getOwnPropertyDescriptor(obj, name);
  return desc !== undefined && desc.writable === true;
};

// Verify a built-in callable property: matches the descriptor on `obj[name]`
// and additionally checks `value.name` and `value.length` per the spec
// defaults for built-in functions.
const verifyCallableProperty = (obj, name, functionName, functionLength, desc) => {
  const value = obj[name];
  if (typeof value !== "function") {
    throw new Test262Error(
      "Expected obj['" + String(name) + "'] to be a function"
    );
  }

  if (desc === undefined) {
    desc = {
      writable: true,
      enumerable: false,
      configurable: true,
      value: value,
    };
  } else if (!Object.prototype.hasOwnProperty.call(desc, "value") &&
             !Object.prototype.hasOwnProperty.call(desc, "get")) {
    desc.value = value;
  }

  verifyProperty(obj, name, desc);

  if (functionName === undefined) {
    if (typeof name === "symbol") {
      functionName = "[" + name.description + "]";
    } else {
      functionName = name;
    }
  }

  verifyProperty(value, "name", {
    value: functionName,
    writable: false,
    enumerable: false,
    configurable: desc.configurable,
  });

  verifyProperty(value, "length", {
    value: functionLength,
    writable: false,
    enumerable: false,
    configurable: desc.configurable,
  });
};

// Primordial variants — ECMAScript distinguishes primordial (built-in)
// objects from non-primordial ones for verification, but the checks are
// identical for our purposes. Match upstream propertyHelper.js.
const verifyPrimordialProperty = verifyProperty;
const verifyPrimordialCallableProperty = verifyCallableProperty;

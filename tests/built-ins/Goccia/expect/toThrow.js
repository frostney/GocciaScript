class DriftError extends Error {}

describe("toThrow", () => {
  test("passes for any thrown value when called without an argument", () => {
    expect(() => {
      throw new Error("boom");
    }).toThrow();
    expect(() => {
      throw "a thrown string";
    }).toThrow();
    expect(() => {
      throw 42;
    }).toThrow();
  });

  test("treats an explicit undefined argument as the no-argument form", () => {
    expect(() => {
      throw new Error("boom");
    }).toThrow(undefined);
    expect(() => 1).not.toThrow(undefined);
  });

  test("negates for functions that return normally", () => {
    expect(() => 42).not.toThrow();
    expect(() => 42).not.toThrow(Error);
    expect(() => 42).not.toThrow("anything");
    expect(() => 42).not.toThrow(/anything/);
  });

  test("matches a string argument as a substring of the message", () => {
    expect(() => {
      throw new Error("full message here");
    }).toThrow("message");
    expect(() => {
      throw new Error("full message here");
    }).not.toThrow("absent");
  });

  test("treats an empty string as an empty-message assertion", () => {
    // Vitest compiles toThrow("") to /^$/ rather than a substring test.
    expect(() => {
      throw new Error("");
    }).toThrow("");
    expect(() => {
      throw new Error("anything");
    }).not.toThrow("");
  });

  test("matches a regular expression against the message", () => {
    expect(() => {
      throw new Error("expected an items array in the response");
    }).toThrow(/items array/);
    expect(() => {
      throw new DriftError("drift detected");
    }).toThrow(/drift/);
    expect(() => {
      throw new Error("boom");
    }).not.toThrow(/zap/);
  });

  test("honours regular expression flags", () => {
    expect(() => {
      throw new Error("Case Sensitive");
    }).toThrow(/case sensitive/i);
    expect(() => {
      throw new Error("Case Sensitive");
    }).not.toThrow(/case sensitive/);
    expect(() => {
      throw new Error("aXbXc");
    }).toThrow(/X/g);
    expect(() => {
      throw new Error("aXbXc");
    }).toThrow(/X/g);
  });

  test("matches an error class, including subclasses", () => {
    expect(() => {
      throw new DriftError("x");
    }).toThrow(Error);
    expect(() => {
      throw new DriftError("x");
    }).toThrow(DriftError);
    expect(() => {
      throw new TypeError("t");
    }).toThrow(TypeError);
    expect(() => {
      throw new TypeError("t");
    }).toThrow(Error);
    expect(() => {
      throw new Error("t");
    }).not.toThrow(TypeError);
    expect(() => {
      throw new Error("t");
    }).not.toThrow(DriftError);
  });

  test("matches non-error classes by instance", () => {
    class Plain {}
    expect(() => {
      throw new Plain();
    }).toThrow(Plain);
    expect(() => {
      throw new Plain();
    }).not.toThrow(Error);
  });

  test("matches an error instance by error-aware deep equality", () => {
    // Vitest is the oracle: an expected error instance is compared as a
    // value, so name, own properties and an expected-side cause all count.
    expect(() => {
      throw new Error("exact text");
    }).toThrow(new Error("exact text"));
    expect(() => {
      throw new Error("exact text and more");
    }).not.toThrow(new Error("exact text"));
    expect(() => {
      throw new TypeError("same");
    }).not.toThrow(new Error("same"));

    expect(() => {
      const thrown = new Error("m");
      thrown.code = "X";
      throw thrown;
    }).not.toThrow(new Error("m"));

    expect(() => {
      throw new Error("m", { cause: "c1" });
    }).not.toThrow(new Error("m", { cause: "c2" }));
    expect(() => {
      throw new Error("m");
    }).not.toThrow(new Error("m", { cause: "c" }));
    // A cause the expectation does not mention is ignored.
    expect(() => {
      throw new Error("m", { cause: "c" });
    }).toThrow(new Error("m"));

    expect(() => {
      throw { message: "boom" };
    }).not.toThrow(new Error("boom"));
  });

  test("reads the message subject the way Vitest does", () => {
    // A thrown string is its own subject.
    expect(() => {
      throw "boom string";
    }).toThrow("boom");
    expect(() => {
      throw "boom string";
    }).toThrow(/boom/);
    expect(() => {
      throw "boom string";
    }).not.toThrow(Error);

    // A message-carrying object contributes its message.
    expect(() => {
      throw { message: "objmsg" };
    }).toThrow("objmsg");
    expect(() => {
      throw { message: "" };
    }).toThrow("");

    // Anything else offers no subject, so the string and RegExp forms never
    // match it — not even the empty string.
    expect(() => {
      throw 42;
    }).not.toThrow("42");
    expect(() => {
      throw 42;
    }).not.toThrow(/42/);
    expect(() => {
      throw 42;
    }).not.toThrow("");
    expect(() => {
      throw true;
    }).not.toThrow("true");
    expect(() => {
      throw { code: 1 };
    }).not.toThrow("1");

    // The no-argument form still sees the throw.
    expect(() => {
      throw 42;
    }).toThrow();
  });

  test("lets a thrown nullish value match any message", () => {
    // Replicates Vitest exactly: with nothing to read a message from, the
    // string and RegExp forms impose no constraint. The class form still does.
    expect(() => {
      throw null;
    }).toThrow("anything at all");
    expect(() => {
      throw null;
    }).toThrow(/anything/);
    expect(() => {
      throw undefined;
    }).toThrow("anything at all");
    expect(() => {
      throw null;
    }).not.toThrow(Error);
  });

  test("reads the message property of thrown plain objects", () => {
    expect(() => {
      throw { message: "objmsg" };
    }).toThrow("objmsg");
    expect(() => {
      throw { message: "objmsg" };
    }).toThrow(/objmsg/);
  });

  test("matches errors raised by the engine itself", () => {
    expect(() => null.foo).toThrow(TypeError);
    expect(() => null.foo).toThrow(Error);
    expect(() => null.foo).toThrow(/null/);
    expect(() => null.foo).not.toThrow(RangeError);
  });

  test("matches asymmetric matchers", () => {
    expect(() => {
      throw new TypeError("t");
    }).toThrow(expect.any(TypeError));
    expect(() => {
      throw new TypeError("t");
    }).not.toThrow(expect.any(RangeError));
  });

  test("rejects unusable matcher arguments", () => {
    expect(() =>
      expect(() => {
        throw new Error("42");
      }).toThrow(42),
    ).toThrow("toThrow expects a string, RegExp, error class, or Error instance");
  });

  test("requires a function when the actual value is not a rejection", () => {
    expect(() => expect("not a function").toThrow()).toThrow(
      "toThrow expects actual value to be a function",
    );
  });

  describe("rejects", () => {
    test("matches the rejection reason with every argument form", async () => {
      await expect(
        (async () => {
          throw new Error("async boom");
        })(),
      ).rejects.toThrow(/boom/);
      await expect(Promise.reject(new Error("async boom"))).rejects.toThrow(
        "async boom",
      );
      await expect(Promise.reject(new DriftError("d"))).rejects.toThrow(Error);
      await expect(Promise.reject(new DriftError("d"))).rejects.toThrow(
        DriftError,
      );
      await expect(Promise.reject(new Error("aaa"))).rejects.toThrow(
        new Error("aaa"),
      );
      await expect(Promise.reject(new Error("aaa"))).rejects.toThrow();
    });

    test("matches non-error rejection reasons", async () => {
      await expect(Promise.reject("plain string reason")).rejects.toThrow(
        "plain string",
      );
      await expect(Promise.reject("plain string reason")).rejects.toThrow(
        /plain/,
      );
    });

    test("supports negation on the rejection reason", async () => {
      await expect(Promise.reject(new Error("aaa"))).rejects.not.toThrow(/bbb/);
      await expect(Promise.reject(new Error("aaa"))).rejects.not.toThrow(
        TypeError,
      );
      await expect(Promise.reject(new Error("aaa"))).not.rejects.toThrow(/bbb/);
    });

    test("matches rejections produced by mocks", async () => {
      const failing = mock();
      failing.mockRejectedValue(new Error("nope"));

      await expect(failing()).rejects.toThrow(/nope/);
      await expect(failing()).rejects.toThrow("nope");
      await expect(failing()).rejects.toThrow(Error);
    });
  });
});

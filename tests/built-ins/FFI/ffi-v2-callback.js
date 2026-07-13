describe("FFI.callback", () => {
  const lib = FFI.open("./fixtures/ffi/libfixture" + FFI.suffix);
  const I32Callback = FFI.callback({ args: ["i32"], returns: "i32" });
  const CompareI32 = FFI.callback({
    args: ["pointer", "pointer"],
    returns: "i32",
  });
  const Point = FFI.struct({ x: "f64", y: "f64" });
  const MixedRecord = FFI.struct({ value: "f64", tag: "i32" });
  const LargeVector = FFI.struct({
    first: "f64",
    second: "f64",
    third: "f64",
  });
  const DoubleUnion = FFI.union({ asDouble: "f64", alternate: "f64" });
  const Float3 = FFI.struct({ first: "f32", second: "f32", third: "f32" });
  const Float1 = FFI.struct({ value: "f32" });
  const PointCallback = FFI.callback({ args: [Point], returns: Point });
  const MixedRecordCallback = FFI.callback({
    args: [MixedRecord],
    returns: MixedRecord,
  });
  const LargeVectorCallback = FFI.callback({
    args: [LargeVector],
    returns: LargeVector,
  });
  const DoubleUnionCallback = FFI.callback({
    args: [DoubleUnion],
    returns: DoubleUnion,
  });
  const ExhaustedHFACallback = FFI.callback({
    args: ["f64", "f64", "f64", "f64", "f64", "f64", "f64", Point],
    returns: "f64",
  });
  const CompactSpilledHFACallback = FFI.callback({
    args: [
      DoubleUnion,
      DoubleUnion,
      DoubleUnion,
      DoubleUnion,
      DoubleUnion,
      DoubleUnion,
      Float3,
      Float1,
    ],
    returns: "f64",
  });

  afterAll(() => lib.close());

  test("passes a call-scoped comparator to native qsort", () => {
    const sortI32 = lib.bind("ffi_v2_sort_i32", {
      args: ["pointer", "i32", CompareI32],
      returns: "void",
    });
    const readI32 = lib.bind("read_i32", {
      args: ["pointer"],
      returns: "i32",
    });
    const values = new Int32Array([9, -1, 4, 2]);

    sortI32(values, values.length, (left, right) => readI32(left) - readI32(right));

    expect(values[0]).toBe(-1);
    expect(values[1]).toBe(2);
    expect(values[2]).toBe(4);
    expect(values[3]).toBe(9);
  });

  test("supports same-thread JavaScript to C to JavaScript reentry", () => {
    const invoke = lib.bind("ffi_v2_call_i32_callback", {
      args: [I32Callback, "i32"],
      returns: "i32",
    });
    const add = lib.bind("add_i32", {
      args: ["i32", "i32"],
      returns: "i32",
    });

    expect(invoke((value) => add(value, 5), 37)).toBe(42);
  });

  test("keeps a persistent callback callable across native calls", () => {
    const store = lib.bind("ffi_v2_store_i32_callback", {
      args: [I32Callback],
      returns: "void",
    });
    const invokeStored = lib.bind("ffi_v2_invoke_stored_i32_callback", {
      args: ["i32"],
      returns: "i32",
    });
    const clearStored = lib.bind("ffi_v2_clear_stored_i32_callback", {
      args: [],
      returns: "void",
    });
    const isNull = lib.bind("is_null", {
      args: ["pointer"],
      returns: "i32",
    });
    const callback = I32Callback.create((value) => value * 3);

    expect(callback.address).not.toBe(0);
    expect(isNull(callback)).toBe(0);
    store(callback);
    Goccia.gc();
    expect(invokeStored(14)).toBe(42);
    expect(invokeStored(7)).toBe(21);

    clearStored();
    callback.close();
    expect(() => callback.address).toThrow(TypeError);
    expect(() => isNull(callback)).toThrow(TypeError);
  });

  test("rejects a closed persistent callback before entering native code", () => {
    const invoke = lib.bind("ffi_v2_call_i32_callback", {
      args: [I32Callback, "i32"],
      returns: "i32",
    });
    const callback = I32Callback.create((value) => value);

    callback.close();

    expect(() => invoke(callback, 42)).toThrow(TypeError);
  });

  test("stores persistent callback pointers in aggregate fields", () => {
    const Holder = FFI.struct({ callback: I32Callback });
    const callback = I32Callback.create((value) => value + 1);
    const holder = Holder.create({ callback });

    expect(holder.callback.address).toBe(callback.address);

    callback.close();
  });

  test("defers a callback exception until native code returns", () => {
    const invokeTwice = lib.bind("ffi_v2_call_i32_callback_twice", {
      args: [I32Callback, "i32"],
      returns: "i32",
    });
    const getProgress = lib.bind("ffi_v2_get_callback_progress", {
      args: [],
      returns: "i32",
    });
    let callbackCalls = 0;
    let caught;

    try {
      invokeTwice(() => {
        callbackCalls++;
        throw new Error("callback failed");
      }, 10);
    } catch (error) {
      caught = error;
    }

    expect(caught).toBeInstanceOf(Error);
    expect(caught.message).toBe("callback failed");
    expect(getProgress()).toBe(2);
    expect(callbackCalls).toBe(1);
  });

  test("passes and returns small and large aggregates through callbacks", () => {
    const callPoint = lib.bind("ffi_v2_call_point_callback", {
      args: [PointCallback, Point],
      returns: Point,
    });
    const callLargeVector = lib.bind("ffi_v2_call_large_vector_callback", {
      args: [LargeVectorCallback, LargeVector],
      returns: LargeVector,
    });
    const callMixedRecord = lib.bind("ffi_v2_call_mixed_record_callback", {
      args: [MixedRecordCallback, MixedRecord],
      returns: MixedRecord,
    });

    const point = callPoint(
      (value) => Point.create({ x: value.x + 10, y: value.y + 20 }),
      Point.create({ x: 1, y: 2 }),
    );
    const vector = callLargeVector(
      (value) => LargeVector.create({
        first: value.first + 10,
        second: value.second + 20,
        third: value.third + 30,
      }),
      LargeVector.create({ first: 1, second: 2, third: 3 }),
    );
    const mixed = callMixedRecord(
      (value) => MixedRecord.create({
        value: value.value + 40,
        tag: value.tag + 50,
      }),
      MixedRecord.create({ value: 1, tag: 2 }),
    );

    expect(point.x).toBe(11);
    expect(point.y).toBe(22);
    expect(vector.first).toBe(11);
    expect(vector.second).toBe(22);
    expect(vector.third).toBe(33);
    expect(mixed.value).toBe(41);
    expect(mixed.tag).toBe(52);
  });

  test("uses ARM homogeneous aggregate rules in reverse callbacks", () => {
    const callDoubleUnion = lib.bind("ffi_v2_call_double_union_callback", {
      args: [DoubleUnionCallback, DoubleUnion],
      returns: DoubleUnion,
    });
    const callExhaustedHFA = lib.bind("ffi_v2_call_exhausted_hfa_callback", {
      args: [ExhaustedHFACallback],
      returns: "f64",
    });
    const union = callDoubleUnion(
      (value) => DoubleUnion.create({ asDouble: value.asDouble + 2 }),
      DoubleUnion.create({ asDouble: 40 }),
    );

    expect(union.asDouble).toBe(42);
    expect(
      callExhaustedHFA(
        (first, second, third, fourth, fifth, sixth, seventh, point) =>
          first +
          second +
          third +
          fourth +
          fifth +
          sixth +
          seventh +
          point.x +
          point.y,
      ),
    ).toBe(45);
  });

  test("reads consecutive compact spilled HFAs in reverse callbacks", () => {
    const callCompactSpilledHFA = lib.bind(
      "ffi_v2_call_compact_spilled_hfa_callback",
      {
        args: [CompactSpilledHFACallback],
        returns: "f64",
      },
    );

    expect(
      callCompactSpilledHFA(
        (first, second, third, fourth, fifth, sixth, value, tail) =>
          first.asDouble +
          second.asDouble +
          third.asDouble +
          fourth.asDouble +
          fifth.asDouble +
          sixth.asDouble +
          value.first +
          value.second +
          value.third +
          tail.value,
      ),
    ).toBe(55);
  });

  test("rejects callbacks invoked from a foreign runtime thread", () => {
    const invoke = lib.bind("ffi_v2_call_i32_callback_on_foreign_thread", {
      args: [I32Callback, "i32"],
      returns: "i32",
    });

    expect(() => invoke((value) => value + 1, 41)).toThrow(TypeError);
  });

  test("surfaces foreign-thread violations from callback fields", () => {
    const Holder = FFI.struct({ callback: I32Callback, value: "i32" });
    const invoke = lib.bind("ffi_v2_call_callback_holder_on_foreign_thread", {
      args: [Holder],
      returns: "i32",
    });
    const callback = I32Callback.create((value) => value + 1);

    try {
      const holder = Holder.create({ callback, value: 41 });
      expect(() => invoke(holder)).toThrow(TypeError);
    } finally {
      callback.close();
    }
  });

  test("zeroes hidden aggregate results on callback failure", () => {
    const capture = lib.bind("ffi_v2_capture_large_callback_result", {
      args: [LargeVectorCallback, LargeVector],
      returns: "void",
    });
    const getCapturedSum = lib.bind("ffi_v2_get_last_large_callback_sum", {
      args: [],
      returns: "f64",
    });
    const input = LargeVector.create({ first: 1, second: 2, third: 3 });

    expect(() =>
      capture(() => {
        throw new Error("aggregate callback failed");
      }, input),
    ).toThrow(Error);
    expect(getCapturedSum()).toBe(0);
  });

  test("keeps indirect by-value callback arguments separate from hidden returns", () => {
    const capture = lib.bind("ffi_v2_capture_large_callback_result", {
      args: [LargeVectorCallback, LargeVector],
      returns: "void",
    });
    const getCapturedSum = lib.bind("ffi_v2_get_last_large_callback_sum", {
      args: [],
      returns: "f64",
    });
    const getInputSum = lib.bind("ffi_v2_get_last_large_callback_input_sum", {
      args: [],
      returns: "f64",
    });
    const input = LargeVector.create({ first: 1, second: 2, third: 3 });

    capture(
      (value) =>
        LargeVector.create({
          first: value.first + 10,
          second: value.second + 20,
          third: value.third + 30,
        }),
      input,
    );

    expect(getInputSum()).toBe(6);
    expect(getCapturedSum()).toBe(66);
  });

  test("zeroes hidden aggregate results on foreign-thread invocation", () => {
    const capture = lib.bind(
      "ffi_v2_capture_large_callback_result_on_foreign_thread",
      {
        args: [LargeVectorCallback, LargeVector],
        returns: "void",
      },
    );
    const getCapturedSum = lib.bind("ffi_v2_get_last_large_callback_sum", {
      args: [],
      returns: "f64",
    });
    const input = LargeVector.create({ first: 1, second: 2, third: 3 });

    expect(() => capture((value) => value, input)).toThrow(TypeError);
    expect(getCapturedSum()).toBe(0);
  });

  test("reports callback slot exhaustion and reuses released slots", () => {
    const callbacks = [];
    let replacement;

    try {
      for (const unused of new Array(64)) {
        callbacks.push(I32Callback.create((value) => value));
      }
      expect(() => I32Callback.create((value) => value)).toThrow(RangeError);

      callbacks[0].close();
      replacement = I32Callback.create((value) => value + 1);
      expect(replacement.closed).toBe(false);
    } finally {
      for (const callback of callbacks) callback.close();
      if (replacement) replacement.close();
    }
  });
});

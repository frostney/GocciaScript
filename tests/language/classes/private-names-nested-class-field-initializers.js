/*---
description: >
  Field initializers of a class nested in another class's code resolve private
  names through the enclosing class bodies
features: [classes, private-fields, private-methods, class-static-fields-private]
---*/

// ES2026 §15.7.14 ClassDefinitionEvaluation gives a class body a
// PrivateEnvironment holding only its own PrivateBoundIdentifiers, whose outer
// environment is the running one. A field initializer is a function created in
// that body, so §9.2.1.2 ResolvePrivateIdentifier resolves a name the nested
// class does not declare in an enclosing class body; §7.3.30 PrivateGet then
// throws a TypeError for a receiver without that Private Name.
class Outer {
  #secret = 42;
  #method() {
    return "m" + this.#secret;
  }
  get #accessor() {
    return "g" + this.#secret;
  }
  set #accessor(value) {
    this.#secret = value;
  }
  static #staticField = "s";
  static #staticMethod() {
    return "sm";
  }

  static instanceField(o) {
    class Inner {
      value = o.#secret;
    }
    return new Inner().value;
  }
  static instanceMethod(o) {
    class Inner {
      read() {
        return o.#secret;
      }
    }
    return new Inner().read();
  }
  static instanceFieldCallingArrow(o) {
    const read = () => o.#secret;
    class Inner {
      value = read();
    }
    return new Inner().value;
  }
  static staticField(o) {
    class Inner {
      static value = o.#secret;
    }
    return Inner.value;
  }
  static instanceFieldMembers(o) {
    class Inner {
      method = o.#method();
      getter = o.#accessor;
      staticField = Outer.#staticField;
      staticMethod = Outer.#staticMethod();
    }
    return new Inner();
  }
  static staticFieldMembers(o) {
    class Inner {
      static method = o.#method();
      static getter = o.#accessor;
      static staticField = Outer.#staticField;
      static staticMethod = Outer.#staticMethod();
    }
    return Inner;
  }
  static instanceFieldWrite(o, value) {
    class Inner {
      result = ((o.#accessor = value), o.#secret);
    }
    return new Inner().result;
  }
  static brandChecks(o) {
    class Inner {
      outer = #secret in o;
      self = #secret in this;
    }
    return new Inner();
  }
  static withOwnPrivateNames(o) {
    class Inner {
      #own = 1;
      static own(inner) {
        return inner.#own;
      }
      static hasOwn(value) {
        return #own in value;
      }
      sum = o.#secret + this.#own;
    }
    const inner = new Inner();
    return [inner.sum, Inner.own(inner), Inner.hasOwn(inner), Inner.hasOwn(o)];
  }
  static shadowed(o) {
    class Inner {
      #secret = 7;
      own = this.#secret;
      value = o.#secret;
    }
    return new Inner();
  }
  static subclassFields(o) {
    class Base {
      base = o.#secret;
    }
    class Derived extends Base {
      derived = o.#secret + 1;
    }
    const derived = new Derived();
    return [derived.base, derived.derived];
  }
  static twoLevelsInMethod(o) {
    class Middle {
      make() {
        class Innermost {
          value = o.#secret;
          staticField = Outer.#staticField;
        }
        return new Innermost();
      }
    }
    return new Middle().make();
  }
  static twoLevelsInField(o) {
    class Middle {
      value = (() => {
        class Innermost {
          value = o.#secret;
        }
        return new Innermost().value;
      })();
    }
    return new Middle().value;
  }
}

test("an instance field initializer reads the outer class's private field", () => {
  const o = new Outer();
  expect(Outer.instanceField(o)).toBe(42);
  expect(Outer.instanceMethod(o)).toBe(42);
  expect(Outer.instanceFieldCallingArrow(o)).toBe(42);
  expect(Outer.staticField(o)).toBe(42);
});

test("instance and static field initializers reach the outer class's methods and accessors", () => {
  const o = new Outer();
  const inner = Outer.instanceFieldMembers(o);
  expect(inner.method).toBe("m42");
  expect(inner.getter).toBe("g42");
  expect(inner.staticField).toBe("s");
  expect(inner.staticMethod).toBe("sm");

  const Inner = Outer.staticFieldMembers(o);
  expect(Inner.method).toBe("m42");
  expect(Inner.getter).toBe("g42");
  expect(Inner.staticField).toBe("s");
  expect(Inner.staticMethod).toBe("sm");
});

test("an instance field initializer writes through the outer class's setter", () => {
  const o = new Outer();
  expect(Outer.instanceFieldWrite(o, 5)).toBe(5);
  expect(Outer.instanceField(o)).toBe(5);
});

test("the nested class does not take the outer class's private name", () => {
  const inner = Outer.brandChecks(new Outer());
  expect(inner.outer).toBe(true);
  expect(inner.self).toBe(false);
});

test("the nested class keeps its own private names", () => {
  expect(Outer.withOwnPrivateNames(new Outer())).toEqual([43, 1, true, false]);
});

test("a nested class's own private name shadows the outer one", () => {
  expect(() => Outer.shadowed(new Outer())).toThrow(TypeError);
});

test("a nested subclass's field initializers read the outer class's private field", () => {
  expect(Outer.subclassFields(new Outer())).toEqual([42, 43]);
});

test("a field initializer two class bodies deep reads the outermost class's private names", () => {
  const o = new Outer();
  const innermost = Outer.twoLevelsInMethod(o);
  expect(innermost.value).toBe(42);
  expect(innermost.staticField).toBe("s");
  expect(Outer.twoLevelsInField(o)).toBe(42);
});

test("a receiver without the outer class's private field throws a TypeError", () => {
  expect(() => Outer.instanceField({})).toThrow(TypeError);
  expect(() => Outer.staticField({})).toThrow(TypeError);
  expect(() => Outer.twoLevelsInField({})).toThrow(TypeError);
  expect(Outer.brandChecks({}).outer).toBe(false);
});

// The 270 public fields push the nested class's own private field key past
// constant 255, so its field initializer defines it through a register.
class ManyFieldsOuter {
  #secret = 42;
  static make(o) {
    class Inner {
      f0; f1; f2; f3; f4; f5; f6; f7; f8; f9; f10; f11; f12; f13; f14;
      f15; f16; f17; f18; f19; f20; f21; f22; f23; f24; f25; f26; f27; f28; f29;
      f30; f31; f32; f33; f34; f35; f36; f37; f38; f39; f40; f41; f42; f43; f44;
      f45; f46; f47; f48; f49; f50; f51; f52; f53; f54; f55; f56; f57; f58; f59;
      f60; f61; f62; f63; f64; f65; f66; f67; f68; f69; f70; f71; f72; f73; f74;
      f75; f76; f77; f78; f79; f80; f81; f82; f83; f84; f85; f86; f87; f88; f89;
      f90; f91; f92; f93; f94; f95; f96; f97; f98; f99; f100; f101; f102; f103; f104;
      f105; f106; f107; f108; f109; f110; f111; f112; f113; f114; f115; f116; f117; f118; f119;
      f120; f121; f122; f123; f124; f125; f126; f127; f128; f129; f130; f131; f132; f133; f134;
      f135; f136; f137; f138; f139; f140; f141; f142; f143; f144; f145; f146; f147; f148; f149;
      f150; f151; f152; f153; f154; f155; f156; f157; f158; f159; f160; f161; f162; f163; f164;
      f165; f166; f167; f168; f169; f170; f171; f172; f173; f174; f175; f176; f177; f178; f179;
      f180; f181; f182; f183; f184; f185; f186; f187; f188; f189; f190; f191; f192; f193; f194;
      f195; f196; f197; f198; f199; f200; f201; f202; f203; f204; f205; f206; f207; f208; f209;
      f210; f211; f212; f213; f214; f215; f216; f217; f218; f219; f220; f221; f222; f223; f224;
      f225; f226; f227; f228; f229; f230; f231; f232; f233; f234; f235; f236; f237; f238; f239;
      f240; f241; f242; f243; f244; f245; f246; f247; f248; f249; f250; f251; f252; f253; f254;
      f255; f256; f257; f258; f259; f260; f261; f262; f263; f264; f265; f266; f267; f268; f269;
      #own = 9;
      value = o.#secret;
      static own(inner) {
        return inner.#own;
      }
      static hasOwn(value) {
        return #own in value;
      }
    }
    return Inner;
  }
}

test("a nested class with more field constants than an 8-bit operand keeps its own private field", () => {
  const o = new ManyFieldsOuter();
  const Inner = ManyFieldsOuter.make(o);
  const inner = new Inner();
  expect(inner.value).toBe(42);
  expect(inner.f269).toBe(undefined);
  expect(Inner.own(inner)).toBe(9);
  expect(Inner.hasOwn(inner)).toBe(true);
  expect(Inner.hasOwn(o)).toBe(false);
  expect(() => Inner.own(o)).toThrow(TypeError);
});

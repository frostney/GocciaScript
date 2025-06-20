test("instanceof operator for primitive types", () => {
  expect(1 instanceof Number).toBe(false);
  expect("hello" instanceof String).toBe(false);
  expect(true instanceof Boolean).toBe(false);
});

test("instanceof operator for null and undefined", () => {
  expect(null instanceof Object).toBe(false);
  expect(undefined instanceof Object).toBe(false);
});

test("instanceof for value constructors", () => {
  expect(new Number(1) instanceof Number).toBe(true);
  expect(new String("hello") instanceof String).toBe(true);
  expect(new Boolean(true) instanceof Boolean).toBe(true);
});

test("instanceof operator for objects with prototype", () => {
  const obj = Object.create({ name: "John" });
  expect(obj instanceof Object).toBe(true);
});

test("instanceof operator for objects with prototype chain", () => {
  const obj = Object.create({ name: "John" });
  const obj2 = Object.create(obj);
  expect(obj2 instanceof Object).toBe(true);
});

test("instanceof operator for objects", () => {
  const obj = { name: "John" };
  expect(obj instanceof Object).toBe(true);
});

test("instanceof operator for arrays", () => {
  const arr = [1, 2, 3];
  expect(arr instanceof Array).toBe(true);
});

test("instanceof operator for functions", () => {
  const fn = () => {};
  expect(fn instanceof Function).toBe(true);
});

test("instanceof operator for classes", () => {
  class Animal {
    constructor(name) {
      this.name = name;
    }
  }
  class Dog extends Animal {
    constructor(name, breed) {
      super(name);
      this.breed = breed;
    }

    bark() {
      return `${this.name} the ${this.breed} says woof!`;
    }
  }

  const animal = new Animal("Buddy");
  const dog = new Dog("Buddy", "Golden Retriever");

  expect(Animal instanceof Function).toBe(true);
  expect(Dog instanceof Function).toBe(true);

  expect(Animal.prototype instanceof Object).toBe(true);
  expect(Dog.prototype instanceof Animal).toBe(true);
  expect(Dog.prototype instanceof Object).toBe(true);

  expect(dog.prototype instanceof Animal).toBe(false);
  expect(dog.prototype instanceof Dog).toBe(false);
  expect(dog.prototype instanceof Object).toBe(false);

  expect(animal instanceof Object).toBe(true);
  expect(animal instanceof Animal).toBe(true);
  expect(animal instanceof Function).toBe(false);

  expect(dog instanceof Animal).toBe(true);
  expect(dog instanceof Dog).toBe(true);
  expect(dog instanceof Function).toBe(false);
  expect(dog instanceof Object).toBe(true);
});

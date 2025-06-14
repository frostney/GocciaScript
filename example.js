const a = 1;
const b = 2;
const c = a + b;
console.log("Sum of a (", a, ") and b (", b, ") is:", c);

// Variables and arrow functions
const greeting = "Hello";
let count = 0;

const greet = (name) => {
  count = count + 1;
  return greeting + ", " + name + "!";
};

console.log(greet("World"));
console.log("Called", count, "times");

// Arrays and methods
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map((n) => n * 2);
console.log("Doubled:", doubled);

// Objects
const person = {
  name: "Alice",
  age: 30,
  greet: () => "Hi, I'm " + person.name,
};

console.log(person.greet());

// Classes
class Animal {
  constructor(name) {
    this.name = name;
  }

  speak() {
    return this.name + " makes a sound";
  }
}

class Dog extends Animal {
  speak() {
    return this.name + " barks";
  }
}

const animal = new Animal("Animal");
console.log(animal.speak());

const dog = new Dog("Rex");
console.log(dog.speak());

const isDog = dog instanceof Dog;
console.log("dog instanceof Dog =", isDog);

const isAnimal = dog instanceof Animal;
console.log("dog instanceof Animal =", isAnimal);

const printObject = (obj) =>
  Object.keys(obj)
    .map((key) => key + ": " + obj[key])
    .join(", ");

// Math operations
console.log("Math.sqrt(16) =", Math.sqrt(16));
console.log("Math.max(5, 3, 9, 1) =", Math.max(5, 3, 9, 1));

// Global constants and functions
console.log("NaN =", NaN);
console.log("Infinity =", Infinity);
console.log("isNaN(NaN) =", isNaN(NaN));
console.log("isFinite(Infinity) =", isFinite(Infinity));
console.log("isFinite(10) =", isFinite(10));
console.log("parseFloat('3.14') =", parseFloat("3.14"));
console.log("parseInt('10') =", parseInt("10"));

// Global objects
console.log("Array.isArray([1, 2, 3]) =", Array.isArray([1, 2, 3]));
console.log("Array.isArray(1) =", Array.isArray(1));

console.log("Object.keys({a: 1, b: 2}) =", Object.keys({ a: 1, b: 2 }));
console.log("Object.values({a: 1, b: 2}) =", Object.values({ a: 1, b: 2 }));
console.log("Object.entries({a: 1, b: 2}) =", Object.entries({ a: 1, b: 2 }));
console.log(
  "Object.assign({a: 1}, {b: 2}) =",
  printObject(Object.assign({ a: 1 }, { b: 2 }))
);
console.log("Object.create({a: 1}) =", printObject(Object.create({ a: 1 })));

// Typeof checks
console.log("typeof 1 =", typeof 1);
console.log("typeof '1' =", typeof "1");
console.log("typeof true =", typeof true);
console.log("typeof false =", typeof false);
console.log("typeof null =", typeof null);
console.log("typeof undefined =", typeof undefined);
console.log("typeof {} =", typeof {});
console.log("typeof [] =", typeof []);
console.log("typeof () => {} =", typeof (() => {}));

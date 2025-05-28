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

// Math operations
console.log("Math.sqrt(16) =", Math.sqrt(16));
console.log("Math.max(5, 3, 9, 1) =", Math.max(5, 3, 9, 1));

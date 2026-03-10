// Expected: woof
// Expected: Buddy
// Expected: true
class Animal {
  constructor(name) {
    this.name = name;
  }
  speak() {
    return "...";
  }
}
class Dog extends Animal {
  speak() {
    return "woof";
  }
}
const dog = new Dog("Buddy");
console.log(dog.speak());
console.log(dog.name);
console.log(dog instanceof Animal);

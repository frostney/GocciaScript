/*---
description: Private fields and methods work correctly
features: [private-fields, private-methods]
---*/

test("private fields", () => {
  class BankAccount {
    #balance = 0;

    constructor(initialBalance) {
      this.#balance = initialBalance;
    }

    deposit(amount) {
      this.#balance += amount;
    }

    withdraw(amount) {
      if (amount <= this.#balance) {
        this.#balance -= amount;
        return amount;
      }
      return 0;
    }

    getBalance() {
      return this.#balance;
    }
  }

  const account = new BankAccount(100);
  expect(account.getBalance()).toBe(100);

  account.deposit(50);
  expect(account.getBalance()).toBe(150);

  const withdrawn = account.withdraw(30);
  expect(withdrawn).toBe(30);
  expect(account.getBalance()).toBe(120);

  // Private field is not accessible
  expect(account.balance).toBeUndefined();
  expect(account["#balance"]).toBeUndefined();
});

test("private methods", () => {
  class Calculator {
    #result = 0;

    #validate(value) {
      return typeof value === "number" && !Number.isNaN(value);
    }

    add(value) {
      if (this.#validate(value)) {
        this.#result += value;
      }
      return this;
    }

    multiply(value) {
      if (this.#validate(value)) {
        this.#result *= value;
      }
      return this;
    }

    getResult() {
      return this.#result;
    }
  }

  const calc = new Calculator();
  expect(calc.add(5).multiply(3).getResult()).toBe(15);

  // Invalid operations are ignored
  calc.add("invalid").multiply(2);
  expect(calc.getResult()).toBe(30); // Only valid multiply(2) was applied

  // Private method is not accessible
  expect(calc.validate).toBeUndefined();
});

test("comprehensive private fields with inheritance", () => {
  class Counter {
    #count = 0;
    #maxValue = 100;

    constructor(initialValue) {
      if (initialValue !== undefined) {
        this.#count = initialValue;
      }
    }

    increment() {
      if (this.#count < this.#maxValue) {
        this.#count = this.#count + 1;
      }
      return this;
    }

    getCount() {
      return this.#count;
    }

    #validateCount(value) {
      return value >= 0 && value <= this.#maxValue;
    }

    setCount(value) {
      if (this.#validateCount(value)) {
        this.#count = value;
      }
      return this;
    }
  }

  // Test basic functionality
  const counter = new Counter();
  expect(counter.getCount()).toBe(0);

  counter.increment().increment().increment();
  expect(counter.getCount()).toBe(3);

  counter.setCount(10);
  expect(counter.getCount()).toBe(10);

  // Test private field initialization with constructor
  const counter2 = new Counter(5);
  expect(counter2.getCount()).toBe(5);

  // Test inheritance - private fields are not inherited
  class AdvancedCounter extends Counter {
    #multiplier = 2;

    doubleIncrement() {
      // This should access the parent's public methods
      super.increment();
      super.increment();
      return this;
    }

    getMultiplier() {
      return this.#multiplier;
    }
  }

  const advCounter = new AdvancedCounter(1);
  expect(advCounter.getCount()).toBe(1);

  advCounter.doubleIncrement();
  expect(advCounter.getCount()).toBe(3);

  expect(advCounter.getMultiplier()).toBe(2);
});

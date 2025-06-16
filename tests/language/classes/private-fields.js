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
      return typeof value === "number" && !isNaN(value);
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

// Heap snapshot prototype: the target script. It pauses on hostCall() with a
// closure over shared variables, an async function between two awaits, a
// .then chain, a Map, a class instance with a private field and a typed array
// live, and finishes once the host answers.

const makeCounter = () => {
  let count = 0;
  const inc = () => {
    count += 1;
    return count;
  };
  const get = () => count;
  return { inc, get };
};

class Account {
  #balance;
  constructor(owner, balance) {
    this.owner = owner;
    this.#balance = balance;
  }
  deposit(amount) {
    this.#balance += amount;
    return this.#balance;
  }
  get balance() {
    return this.#balance;
  }
}

const counter = makeCounter();
counter.inc();
counter.inc();

const account = new Account("ada", 100);
const ledger = new Map([["opening", 100]]);
const bytes = new Uint8Array([1, 2, 3, 4]);
const log = [];

const convert = async (currency) => {
  const base = await Promise.resolve(10);
  log.push(`base ${base}`);
  const rate = await hostCall("rate", currency);
  log.push(`rate ${rate}`);
  const total = await Promise.resolve(base * rate);
  ledger.set(currency, account.deposit(total));
  bytes[0] = counter.inc();
  return total;
};

const done = convert("eur")
  .then((total) => {
    log.push(`then1 ${total}`);
    return total + 1;
  })
  .then((next) => {
    log.push(`then2 ${next}`);
    return next;
  });

done.then((final) => {
  print("final", final);
  print("log", log.join(";"));
  print("ledger", JSON.stringify([...ledger]));
  print("bytes", Array.from(bytes).join(","));
  print("account", account.owner, account.balance, counter.get());
});

print("before pause", counter.get(), ledger.size, bytes.length);

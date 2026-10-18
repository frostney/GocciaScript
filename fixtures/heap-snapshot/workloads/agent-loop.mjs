// Heap snapshot workload: the durable case, an agent-style loop paused on a
// host answer.
//
// A planner keeps a conversation history, a tool registry of closures, a
// task queue of class instances and a generator that yields plan steps. It
// runs steps until one needs a tool the host provides, awaits the host's
// answer (the idle pause point), and continues the plan with the answer.
// The pause happens several times; a snapshot can be taken at any of them.

class Task {
  #attempts = 0;
  constructor(id, kind, input) {
    this.id = id;
    this.kind = kind;
    this.input = input;
    this.result = undefined;
  }
  attempt() {
    this.#attempts += 1;
    return this.#attempts;
  }
  get attempts() {
    return this.#attempts;
  }
}

const history = [];
const memory = new Map();
const tools = {
  sum: (xs) => xs.reduce((a, b) => a + b, 0),
  upper: (s) => s.toUpperCase(),
  remember: (k, v) => { memory.set(k, v); return memory.size; },
};

const planner = {
  *steps() {
    let step = 0;
  for (const kind of ["sum", "lookup", "upper", "lookup", "remember", "lookup", "sum"]) {
    step += 1;
    yield new Task(`t${step}`, kind, kind === "sum" ? [step, step * 2, step * 3] : `item-${step}`);
  }
  },
};

const runTask = async (task) => {
  task.attempt();
  if (task.kind === "lookup") {
    const answer = await hostCall("lookup", task.input);
    history.push({ role: "tool", task: task.id, answer });
    return `${task.input}=>${answer}`;
  }
  if (task.kind === "remember") return tools.remember(task.id, task.input);
  return tools[task.kind](task.input);
};

const run = async () => {
  const results = [];
  for (const task of planner.steps()) {
    history.push({ role: "plan", task: task.id, kind: task.kind });
    task.result = await runTask(task);
    results.push(task);
  }
  return results;
};

const transcript = Array.from({ length: 300 }, (_, i) => ({
  role: i % 2 === 0 ? "user" : "assistant",
  content: `message ${i}: ${"lorem ipsum dolor sit amet ".repeat(1 + (i % 4))}`,
}));
history.push(...transcript);

run().then((results) => {
  print("results", results.map((t) => `${t.id}:${t.kind}:${t.result}:${t.attempts}`).join(" "));
  print("history", history.length, "memory", JSON.stringify([...memory]));
});

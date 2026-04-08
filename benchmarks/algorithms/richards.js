/*---
description: Richards OS task scheduler benchmarks — adapted from Octane 2.0 Richards benchmark
---*/

// Infinite iterator for replacing while loops
const forever = {
  [Symbol.iterator]() {
    return { next() { return { value: undefined, done: false }; } };
  }
};

// Task states (bitfield-style, matching Octane)
const STATE_RUNNING = 0;
const STATE_RUNNABLE = 1;
const STATE_SUSPENDED = 2;
const STATE_HELD = 4;
const STATE_SUSPENDED_RUNNABLE = 3; // SUSPENDED | RUNNABLE

// Packet kinds
const KIND_DEVICE = 0;
const KIND_WORK = 1;

// Task IDs
const ID_IDLE = 0;
const ID_WORKER = 1;
const ID_HANDLER_A = 2;
const ID_HANDLER_B = 3;
const ID_DEVICE_A = 4;
const ID_DEVICE_B = 5;

const DATA_SIZE = 4;
const EXPECTED_QUEUE_COUNT = 2322;
const EXPECTED_HOLD_COUNT = 928;

const createPacket = (link, id, kind) => ({
  link, id, kind, datum: 0, data: [0, 0, 0, 0],
});

const addPacketTo = (packet, queue) => {
  packet.link = null;
  if (queue === null) { return packet; }
  let next = queue;
  for (const _ of forever) {
    if (next.link === null) { break; }
    next = next.link;
  }
  next.link = packet;
  return queue;
};

const createTcb = (link, id, priority, queue, state, fn) => ({
  link, id, priority, queue, state, fn,
});

const isHeldOrSuspended = (tcb) =>
  (tcb.state & STATE_HELD) !== 0 || tcb.state === STATE_SUSPENDED;

const tcbRun = (tcb) => {
  let packet = null;
  if (tcb.state === STATE_SUSPENDED_RUNNABLE) {
    packet = tcb.queue;
    tcb.queue = packet.link;
    tcb.state = tcb.queue === null ? STATE_RUNNING : STATE_RUNNABLE;
  }
  return tcb.fn(packet);
};

const checkPriorityAdd = (tcb, task, packet) => {
  if (tcb.queue === null) {
    tcb.queue = packet;
    tcb.state = tcb.state | STATE_RUNNABLE;
    if (tcb.priority > task.priority) { return tcb; }
  } else {
    tcb.queue = addPacketTo(packet, tcb.queue);
  }
  return task;
};

const runRichards = () => {
  const blocks = [null, null, null, null, null, null];
  let list = null;
  let currentTcb = null;
  let currentId = 0;
  let queueCount = 0;
  let holdCount = 0;

  const addTask = (id, priority, queue, state, fn) => {
    const tcb = createTcb(list, id, priority, queue, state, fn);
    list = tcb;
    blocks[id] = tcb;
  };

  const holdCurrent = () => {
    holdCount = holdCount + 1;
    currentTcb.state = currentTcb.state | STATE_HELD;
    return currentTcb.link;
  };

  const suspendCurrent = () => {
    currentTcb.state = currentTcb.state | STATE_SUSPENDED;
    return currentTcb;
  };

  const release = (id) => {
    const t = blocks[id];
    if (t === null) { return currentTcb; }
    t.state = t.state & ~STATE_HELD;
    if (t.priority > currentTcb.priority) { return t; }
    return currentTcb;
  };

  const queuePacket = (packet) => {
    const t = blocks[packet.id];
    if (t === null) { return t; }
    queueCount = queueCount + 1;
    packet.link = null;
    packet.id = currentId;
    return checkPriorityAdd(t, currentTcb, packet);
  };

  // Idle task: v1 is the alternation bit pattern (starts at 1), count is the countdown
  let idleV1 = 1;
  let idleCount = 1000;
  addTask(ID_IDLE, 0, null, STATE_RUNNING, (packet) => {
    idleCount = idleCount - 1;
    if (idleCount === 0) { return holdCurrent(); }
    if ((idleV1 & 1) === 0) {
      idleV1 = idleV1 >> 1;
      return release(ID_DEVICE_A);
    }
    idleV1 = (idleV1 >> 1) ^ 0xD008;
    return release(ID_DEVICE_B);
  });

  // Worker task
  let workerV1 = ID_HANDLER_A;
  let workerV2 = 0;
  const workerQueue = createPacket(createPacket(null, ID_WORKER, KIND_WORK), ID_WORKER, KIND_WORK);
  addTask(ID_WORKER, 1000, workerQueue, STATE_SUSPENDED_RUNNABLE, (packet) => {
    if (packet === null) { return suspendCurrent(); }
    workerV1 = workerV1 === ID_HANDLER_A ? ID_HANDLER_B : ID_HANDLER_A;
    packet.id = workerV1;
    packet.datum = 0;
    for (const [i] of packet.data.entries()) {
      workerV2 = workerV2 + 1;
      if (workerV2 > 26) { workerV2 = 1; }
      packet.data[i] = workerV2;
    }
    return queuePacket(packet);
  });

  // Handler task factory — device packets route to the matching device task
  const addHandlerTask = (id, priority, deviceId) => {
    let workQueue = null;
    let deviceQueue = null;
    // 3 device packets per handler (matching Octane)
    const queue = createPacket(createPacket(createPacket(null, deviceId, KIND_DEVICE), deviceId, KIND_DEVICE), deviceId, KIND_DEVICE);
    addTask(id, priority, queue, STATE_SUSPENDED_RUNNABLE, (packet) => {
      if (packet !== null) {
        if (packet.kind === KIND_WORK) {
          workQueue = addPacketTo(packet, workQueue);
        } else {
          deviceQueue = addPacketTo(packet, deviceQueue);
        }
      }
      if (workQueue !== null) {
        const count = workQueue.datum;
        if (count < DATA_SIZE) {
          if (deviceQueue !== null) {
            const v = deviceQueue;
            deviceQueue = v.link;
            v.datum = workQueue.data[count];
            workQueue.datum = count + 1;
            return queuePacket(v);
          }
        } else {
          const v = workQueue;
          workQueue = v.link;
          return queuePacket(v);
        }
      }
      return suspendCurrent();
    });
  };
  addHandlerTask(ID_HANDLER_A, 2000, ID_DEVICE_A);
  addHandlerTask(ID_HANDLER_B, 3000, ID_DEVICE_B);

  // Device task factory
  const addDeviceTask = (id, priority) => {
    let pending = null;
    addTask(id, priority, null, STATE_SUSPENDED, (packet) => {
      if (packet === null) {
        if (pending === null) { return suspendCurrent(); }
        const v = pending;
        pending = null;
        return queuePacket(v);
      }
      pending = packet;
      return holdCurrent();
    });
  };
  addDeviceTask(ID_DEVICE_A, 4000);
  addDeviceTask(ID_DEVICE_B, 5000);

  // Run scheduler
  currentTcb = list;
  for (const _ of forever) {
    if (currentTcb === null) { break; }
    if (isHeldOrSuspended(currentTcb)) {
      currentTcb = currentTcb.link;
    } else {
      currentId = currentTcb.id;
      currentTcb = tcbRun(currentTcb);
    }
  }

  return { queueCount, holdCount };
};

suite("richards scheduler", () => {
  bench("full simulation (1000 iterations)", {
    run: () => {
      runRichards();
    },
  });

  bench("simulation with verification", {
    run: () => {
      const result = runRichards();
      if (result.queueCount !== EXPECTED_QUEUE_COUNT) {
        throw new Error("Bad queue count: " + result.queueCount + " expected: " + EXPECTED_QUEUE_COUNT);
      }
      if (result.holdCount !== EXPECTED_HOLD_COUNT) {
        throw new Error("Bad hold count: " + result.holdCount + " expected: " + EXPECTED_HOLD_COUNT);
      }
    },
  });
});

/*---
description: JSON processing pipeline benchmarks — parse, transform, serialize
---*/

const SMALL_DOC = JSON.stringify({
  id: 1,
  name: "Alice",
  email: "alice@example.com",
  age: 30,
  active: true,
  scores: [95, 87, 92, 78, 88],
  address: {
    street: "123 Main St",
    city: "Springfield",
    state: "IL",
    zip: "62701",
  },
  tags: ["admin", "user", "premium"],
});

// Build a large document programmatically
const buildLargeDoc = () => {
  const users = Array.from({ length: 50 }, (_, i) => ({
    id: i + 1,
    name: "User" + (i + 1),
    email: "user" + (i + 1) + "@example.com",
    age: 20 + (i % 50),
    active: i % 3 !== 0,
    department: ["engineering", "sales", "marketing", "support", "hr"][i % 5],
    salary: 50000 + (i * 1000),
    scores: Array.from({ length: 5 }, (_, j) => 60 + ((i * 7 + j * 13) % 40)),
    address: {
      street: (100 + i) + " Street " + String.fromCharCode(65 + (i % 26)),
      city: ["New York", "Chicago", "Houston", "Phoenix", "Philadelphia"][i % 5],
      state: ["NY", "IL", "TX", "AZ", "PA"][i % 5],
    },
    metadata: {
      created: "2024-01-" + String(i + 1).padStart(2, "0"),
      lastLogin: "2024-06-" + String((i % 28) + 1).padStart(2, "0"),
      loginCount: i * 5 + 10,
    },
  }));
  return JSON.stringify({ users, total: users.length, page: 1 });
};

const LARGE_DOC = buildLargeDoc();

// Transform: filter active users, compute derived fields
const transformUsers = (jsonStr) => {
  const data = JSON.parse(jsonStr);
  const transformed = {
    ...data,
    users: data.users
      .filter((u) => u.active)
      .map((u) => ({
        ...u,
        avgScore: u.scores.reduce((a, b) => a + b, 0) / u.scores.length,
        fullAddress: u.address.street + ", " + u.address.city + ", " + u.address.state,
        seniorityLevel: u.salary >= 70000 ? "senior" : u.salary >= 55000 ? "mid" : "junior",
      })),
  };
  transformed.total = transformed.users.length;
  return JSON.stringify(transformed);
};

// Aggregate: group by department, compute stats
const aggregateByDepartment = (jsonStr) => {
  const data = JSON.parse(jsonStr);
  const groups = new Map();
  for (const user of data.users) {
    const dept = user.department;
    if (!groups.has(dept)) {
      groups.set(dept, { count: 0, totalSalary: 0, totalScoreSum: 0, scoreCount: 0 });
    }
    const g = groups.get(dept);
    g.count = g.count + 1;
    g.totalSalary = g.totalSalary + user.salary;
    const scoreSum = user.scores.reduce((a, b) => a + b, 0);
    g.totalScoreSum = g.totalScoreSum + scoreSum;
    g.scoreCount = g.scoreCount + user.scores.length;
  }
  const result = {};
  for (const [dept, stats] of groups) {
    result[dept] = {
      count: stats.count,
      avgSalary: stats.totalSalary / stats.count,
      avgScore: stats.totalScoreSum / stats.scoreCount,
    };
  }
  return JSON.stringify(result);
};

suite("JSON processing", () => {
  bench("parse small document", {
    run: () => {
      JSON.parse(SMALL_DOC);
    },
  });

  bench("parse + stringify small document", {
    run: () => {
      const obj = JSON.parse(SMALL_DOC);
      JSON.stringify(obj);
    },
  });

  bench("parse large document (50 users)", {
    run: () => {
      JSON.parse(LARGE_DOC);
    },
  });

  bench("transform pipeline (filter + map + serialize)", {
    run: () => {
      transformUsers(LARGE_DOC);
    },
  });

  bench("aggregate by department", {
    run: () => {
      aggregateByDepartment(LARGE_DOC);
    },
  });

  bench("full roundtrip: parse + transform + aggregate + serialize", {
    run: () => {
      const transformed = transformUsers(LARGE_DOC);
      aggregateByDepartment(transformed);
    },
  });
});

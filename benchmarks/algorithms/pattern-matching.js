/*---
description: Multi-pattern regex scanning benchmarks — inspired by Octane RegExp
---*/

const SAMPLE_TEXT = `const handler = (request, response) => {
  const userId = Number.parseInt(request.params.id);
  const email = request.body.email;
  const isValid = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$/.test(email);
  if (!isValid) {
    response.status(400).json({ error: "Invalid email: " + email });
    return;
  }
  const ip = request.headers["x-forwarded-for"] || "127.0.0.1";
  const timestamp = "2024-06-15T14:30:00Z";
  const price = 1299.99;
  const hex = "#ff6600";
  const url = "https://api.example.com/v2/users/12345?format=json&limit=100";
  const phone = "+1-555-867-5309";
  const version = "v2.14.3";
  const uuid = "550e8400-e29b-41d4-a716-446655440000";
  console.log("Processing user " + userId + " from " + ip + " at " + timestamp);
  response.json({ success: true, price: price, ref: uuid });
};`;

const PATTERNS = [
  { name: "numbers", pattern: /\b\d+(\.\d+)?\b/g },
  { name: "strings", pattern: /"[^"]*"|'[^']*'/g },
  { name: "identifiers", pattern: /\b[a-zA-Z_$][a-zA-Z0-9_$]*\b/g },
  { name: "hexColors", pattern: /#[0-9a-fA-F]{3,8}\b/g },
  { name: "urls", pattern: /https?:\/\/[^\s"']+/g },
  { name: "emails", pattern: /[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}/g },
  { name: "ipAddresses", pattern: /\b\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\b/g },
  { name: "dates", pattern: /\d{4}-\d{2}-\d{2}(T\d{2}:\d{2}:\d{2}Z?)?/g },
  { name: "semver", pattern: /v?\d+\.\d+\.\d+/g },
  { name: "uuids", pattern: /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/g },
];

// Scan text against all patterns, collect match counts
const scanAllPatterns = (text, patterns) => {
  const results = new Map();
  for (const { name, pattern } of patterns) {
    const matches = [...text.matchAll(pattern)];
    results.set(name, matches.length);
  }
  return results;
};

// Scan and collect actual matches with positions
const scanWithPositions = (text, patterns) => {
  const allMatches = [];
  for (const { name, pattern } of patterns) {
    for (const match of text.matchAll(pattern)) {
      allMatches.push({
        category: name,
        value: match[0],
        index: match.index,
      });
    }
  }
  return allMatches.toSorted((a, b) => a.index - b.index);
};

// Classify and aggregate matches
const classifyAndAggregate = (text, patterns) => {
  const matches = scanWithPositions(text, patterns);
  const stats = new Map();
  for (const m of matches) {
    if (!stats.has(m.category)) {
      stats.set(m.category, { count: 0, totalLength: 0 });
    }
    const s = stats.get(m.category);
    s.count = s.count + 1;
    s.totalLength = s.totalLength + m.value.length;
  }
  return stats;
};

// Doubled text for larger scale
const LARGE_TEXT = SAMPLE_TEXT + "\n" + SAMPLE_TEXT;

suite("pattern matching", () => {
  bench("scan 10 patterns (count only)", {
    run: () => {
      scanAllPatterns(SAMPLE_TEXT, PATTERNS);
    },
  });

  bench("scan with positions (sorted)", {
    run: () => {
      scanWithPositions(SAMPLE_TEXT, PATTERNS);
    },
  });

  bench("classify and aggregate", {
    run: () => {
      classifyAndAggregate(SAMPLE_TEXT, PATTERNS);
    },
  });

  bench("large text scan (2x)", {
    run: () => {
      scanAllPatterns(LARGE_TEXT, PATTERNS);
    },
  });
});

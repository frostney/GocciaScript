import { describe, expect, test } from "bun:test";
import { formatStars, pickPrecedenceVersions } from "@/lib/github";

describe("pickPrecedenceVersions — release-pin selection", () => {
  test("current repo (0.x line, no major bump) → patch + previous-minor patches", () => {
    // Current GocciaScript history. Expects:
    //   patch  → 0.6.1 (overall newest)
    //   minor  → 0.5.1 (latest patch in a different (major,minor))
    //   major  → 0.4.0 (different (major,minor) than both pins)
    const tags = [
      "0.6.1",
      "0.6.0",
      "0.5.1",
      "0.5.0",
      "0.4.0",
      "0.3.0",
      "0.2.0",
      "0.1.0",
    ];
    expect(pickPrecedenceVersions(tags)).toEqual(["0.6.1", "0.5.1", "0.4.0"]);
  });

  test("post-1.0 line → patch + 0.6.x + previous-minor cycle", () => {
    // Latest is 1.0.2, so the major-pin candidate (any tag with major != 1)
    // is 0.6.1, but that collides with the minor pin → fall through to
    // 0.5.1.
    const tags = [
      "1.0.2",
      "1.0.1",
      "1.0.0",
      "0.6.1",
      "0.6.0",
      "0.5.1",
      "0.5.0",
    ];
    expect(pickPrecedenceVersions(tags)).toEqual(["1.0.2", "0.6.1", "0.5.1"]);
  });

  test("rich post-2.x line → patch / minor / major all distinct", () => {
    const tags = ["2.1.3", "2.1.2", "2.0.0", "1.5.4", "1.5.0", "1.0.0"];
    expect(pickPrecedenceVersions(tags)).toEqual(["2.1.3", "2.0.0", "1.5.4"]);
  });

  test("only one minor cycle → only the patch pin", () => {
    expect(pickPrecedenceVersions(["0.1.1", "0.1.0"])).toEqual(["0.1.1"]);
  });

  test("two minor cycles → patch + previous-minor's patch", () => {
    expect(
      pickPrecedenceVersions(["0.2.1", "0.2.0", "0.1.1", "0.1.0"]),
    ).toEqual(["0.2.1", "0.1.1"]);
  });

  test("single release degrades gracefully to one pin", () => {
    expect(pickPrecedenceVersions(["0.1.0"])).toEqual(["0.1.0"]);
  });

  test("`v` prefix is preserved through normalization", () => {
    expect(pickPrecedenceVersions(["v0.6.1", "v0.5.1"])).toEqual([
      "v0.6.1",
      "v0.5.1",
    ]);
  });

  test("invalid tags are filtered out", () => {
    expect(
      pickPrecedenceVersions(["0.1.0", "nightly", "garbage", "0.0.1"]),
    ).toEqual(["0.1.0", "0.0.1"]);
  });

  test("empty input → empty output", () => {
    expect(pickPrecedenceVersions([])).toEqual([]);
  });

  test("input order doesn't affect output (sorting is by semver, not arrival)", () => {
    const a = pickPrecedenceVersions(["0.5.1", "0.6.1", "0.6.0", "0.5.0"]);
    const b = pickPrecedenceVersions(["0.6.0", "0.5.0", "0.6.1", "0.5.1"]);
    expect(a).toEqual(b);
    expect(a).toEqual(["0.6.1", "0.5.1"]);
  });
});

describe("formatStars — GitHub-button style", () => {
  test("under 1k → exact integer", () => {
    expect(formatStars(0)).toBe("0");
    expect(formatStars(1)).toBe("1");
    expect(formatStars(42)).toBe("42");
    expect(formatStars(999)).toBe("999");
  });

  test("integer multiples of 1k drop the `.0` suffix", () => {
    expect(formatStars(1_000)).toBe("1k");
    expect(formatStars(2_000)).toBe("2k");
    expect(formatStars(10_000)).toBe("10k");
  });

  test("non-integer thousands carry one decimal", () => {
    expect(formatStars(2_400)).toBe("2.4k");
    expect(formatStars(85_300)).toBe("85.3k");
    expect(formatStars(123_400)).toBe("123.4k");
  });

  test("rounds to one decimal at the boundary", () => {
    // 1099 / 1000 = 1.099 → rounds to 1.1
    expect(formatStars(1_099)).toBe("1.1k");
    // 1049 / 1000 = 1.049 → rounds to 1.0 → strips trailing .0
    expect(formatStars(1_049)).toBe("1k");
    // 1050 / 1000 = 1.05 → rounds to 1.1 (toFixed half-away)
    expect(formatStars(1_050)).toBe("1.1k");
  });

  test("millions follow the same rules with `m` suffix", () => {
    expect(formatStars(1_000_000)).toBe("1m");
    expect(formatStars(1_200_000)).toBe("1.2m");
    expect(formatStars(85_300_000)).toBe("85.3m");
  });
});

import { describe, expect, test } from "bun:test";
import { DEFAULT_LOCALE, parseAcceptLanguage } from "@/lib/locale";

describe("parseAcceptLanguage", () => {
  test("returns the default locale when the header is absent", () => {
    expect(parseAcceptLanguage(null)).toBe(DEFAULT_LOCALE);
    expect(parseAcceptLanguage(undefined)).toBe(DEFAULT_LOCALE);
    expect(parseAcceptLanguage("")).toBe(DEFAULT_LOCALE);
  });

  test("returns a single language tag verbatim (canonicalized)", () => {
    expect(parseAcceptLanguage("en-US")).toBe("en-US");
    expect(parseAcceptLanguage("fr-FR")).toBe("fr-FR");
    expect(parseAcceptLanguage("de-DE")).toBe("de-DE");
  });

  test("canonicalizes case (en-us → en-US)", () => {
    expect(parseAcceptLanguage("en-us")).toBe("en-US");
    expect(parseAcceptLanguage("FR-fr")).toBe("fr-FR");
  });

  test("picks the highest-quality candidate, not the first listed", () => {
    // First-listed has implicit q=1, but the explicit q=0.5 fr-FR loses
    // to the explicit q=0.9 de-DE.
    expect(parseAcceptLanguage("fr-FR;q=0.5,de-DE;q=0.9")).toBe("de-DE");
  });

  test("treats missing `q=` as 1.0", () => {
    expect(parseAcceptLanguage("en-US,fr-FR;q=0.9")).toBe("en-US");
  });

  test("breaks ties in header order (stable sort)", () => {
    expect(parseAcceptLanguage("en-GB;q=0.8,en-US;q=0.8")).toBe("en-GB");
  });

  test("ignores the `*` wildcard tag", () => {
    expect(parseAcceptLanguage("*;q=0.5")).toBe(DEFAULT_LOCALE);
    expect(parseAcceptLanguage("en-US,*")).toBe("en-US");
  });

  test("falls through to the next candidate on an invalid first tag", () => {
    expect(parseAcceptLanguage("not_a_tag,en-US;q=0.9")).toBe("en-US");
  });

  test("real-world Chrome header", () => {
    // Chrome with a US locale preference.
    expect(parseAcceptLanguage("en-US,en;q=0.9")).toBe("en-US");
  });

  test("real-world multi-locale header", () => {
    expect(parseAcceptLanguage("fr-CA,fr;q=0.8,en-US;q=0.6,en;q=0.4")).toBe(
      "fr-CA",
    );
  });

  test("falls back to default when nothing parses", () => {
    expect(parseAcceptLanguage("not_a_tag,also_bad")).toBe(DEFAULT_LOCALE);
  });
});

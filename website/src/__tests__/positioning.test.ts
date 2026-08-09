import { describe, expect, test } from "bun:test";
import { buildGocciaApiSkillMd, buildLlmsTxt } from "@/lib/agent-discovery";
import {
  buildHomeStructuredData,
  COMPILER_SUPPORT_ANSWER,
  ECMASCRIPT_SCOPE_ANSWER,
  ECMASCRIPT_SCOPE_QUESTION,
  GOCCIASCRIPT_SUMMARY,
  NODE_COMPATIBILITY_ANSWER,
  POSITIONING_FAQS,
  TYPE_ANNOTATIONS_ANSWER,
  VITEST_COMPATIBILITY_ANSWER,
} from "@/lib/positioning";

/**
 * Claims the compatibility copy may never make: the drop-in audit has not
 * closed, and no repository evidence compares this runner's wall-clock
 * against Vitest.
 *
 * Each alternative carries its own boundaries because `%` is not a word
 * character — a single trailing `\b` around the whole alternation silently
 * stopped `100%` from ever matching when followed by a space.
 */
const FORBIDDEN_CLAIM_PATTERNS: readonly RegExp[] = [
  /\b100\s*%/i,
  /\bexact(?:ly)?\s+compatible\b/i,
  /\bfully\s+compatible\b/i,
  /\bcomplete\s+drop-in\b/i,
  /\b(?:faster|quicker|speedier|snappier)\s+than\s+Vitest\b/i,
  /\b(?:outperforms|outpaces|outruns|beats)\s+Vitest\b/i,
];

function expectConcepts(text: string, concepts: readonly RegExp[]) {
  for (const concept of concepts) {
    expect(text).toMatch(concept);
  }
}

describe("GocciaScript positioning", () => {
  test("keeps the canonical summary centered on the runtime purpose", () => {
    expectConcepts(GOCCIASCRIPT_SUMMARY, [
      /sandbox-first ECMAScript runtime/i,
      /AI agents/i,
      /hosts define.+capabilities.+runtime surface.+execution limits/i,
      /embedded in native applications/i,
    ]);
    expect(GOCCIASCRIPT_SUMMARY).toMatch(
      /AI agents[\s\S]+embedded in native applications/i,
    );
  });

  test("distinguishes the implementation from the recommended profile", () => {
    expectConcepts(ECMASCRIPT_SCOPE_ANSWER, [
      /implements core ECMAScript/i,
      /recommended profile/i,
      /product policy.+implementation ceiling/i,
      /traditional for loops/i,
      /for\.\.\.in/i,
      /while\/do\.\.\.while/i,
      /non-strict Script semantics/i,
      /private test262 host.+conformance/i,
    ]);
  });

  test("keeps type annotations standards-first", () => {
    expectConcepts(TYPE_ANNOTATIONS_ANSWER, [
      /TC39 Type Annotations proposal/i,
      /types-as-comments runtime model/i,
      /optional --strict-types.+runtime enforcement/i,
      /interpreter and bytecode modes/i,
      /not a replacement.+static structural type checker/i,
    ]);
  });

  test("distinguishes the Node host from the sandbox fs API", () => {
    expectConcepts(NODE_COMPATIBILITY_ANSWER, [
      /not a complete Node\.js host/i,
      /Node-compatible fs API/i,
      /virtual filesystem/i,
      /synchronous, callback, and promise-based methods/i,
      /does not expose the ambient host filesystem/i,
    ]);
  });

  test("frames Vitest compatibility as a direction with its gaps named", () => {
    expectConcepts(VITEST_COMPATIBILITY_ANSWER, [
      /Vitest and Jest test API/i,
      /pinned Vitest release/i,
      /semantics oracle/i,
      /direction, not a finished claim/i,
      /no vi namespace/i,
      /rather than about raw engine throughput/i,
    ]);
  });

  test("never upgrades the compatibility direction into an absolute claim", () => {
    for (const pattern of FORBIDDEN_CLAIM_PATTERNS)
      expect(VITEST_COMPATIBILITY_ANSWER).not.toMatch(pattern);
  });

  // A guard that cannot match the phrasing it forbids is worse than none: it
  // reads as coverage. `100%` ended in a non-word character, so a trailing
  // \b required a word character after the percent sign and `100% compatible`
  // slipped through; the performance rule only knew one verb.
  test("the forbidden-claim guard matches the phrasings it exists to stop", () => {
    const forbidden = [
      "100% compatible with Vitest.",
      "100%-compatible today.",
      "It is 100 % compatible.",
      "The runner is exactly compatible with Vitest.",
      "The runner is exactly compatible.",
      "It is fully compatible with Vitest.",
      "A complete drop-in for Vitest.",
      "It is faster than Vitest.",
      "It is much faster than Vitest on every suite.",
      "It outperforms Vitest.",
      "The runner beats Vitest on wall-clock.",
      "It outpaces Vitest.",
      "Runs quicker than Vitest.",
    ];

    for (const claim of forbidden)
      expect(
        FORBIDDEN_CLAIM_PATTERNS.some((pattern) => pattern.test(claim)),
      ).toBe(true);

    // The guard must not fire on the copy the answer is allowed to make.
    const allowed = [
      "Vitest is the semantics oracle for this runner.",
      "Compatibility is a direction, not a finished claim.",
      "About 90 tests exercise the matchers.",
    ];

    for (const claim of allowed)
      expect(
        FORBIDDEN_CLAIM_PATTERNS.some((pattern) => pattern.test(claim)),
      ).toBe(false);
  });

  test("states the complete Delphi support contract", () => {
    expectConcepts(COMPILER_SUPPORT_ANSWER, [
      /Win32 and Win64 application matrix/i,
      /all applicable Pascal and JavaScript tests/i,
      /shared runtime semantics across both compilers/i,
    ]);
  });

  test("rejects compressed claims and historical pass rates", () => {
    const canonicalCopy = POSITIONING_FAQS.map(
      ({ question, answer }) => `${question}\n${answer}`,
    ).join("\n");

    expect(canonicalCopy).not.toContain("code you didn't write");
    expect(canonicalCopy).not.toContain("Arrow functions only");
    expect(canonicalCopy).not.toContain("No traditional loops");
    expect(canonicalCopy).not.toContain("parsed and discarded");
    expect(canonicalCopy).not.toContain("there is no separate type-checker");
    // No trailing \b: `%` is not a word character, so requiring a boundary
    // after it means the pattern only matches a percentage glued to a word.
    expect(canonicalCopy).not.toMatch(/\b\d{2,3}(?:\.\d+)?\s*%/);
  });

  test("structured data reuses the visible positioning FAQ", () => {
    const [software, faq] = buildHomeStructuredData("https://example.test");

    expect(software).toMatchObject({
      "@type": "SoftwareApplication",
      name: "GocciaScript",
      url: "https://example.test/",
    });
    expect(faq).toMatchObject({
      "@type": "FAQPage",
    });
    expect(faq.mainEntity).toHaveLength(POSITIONING_FAQS.length);
    expect(faq.mainEntity).toContainEqual(
      expect.objectContaining({
        name: ECMASCRIPT_SCOPE_QUESTION,
      }),
    );
  });

  test("machine-facing summaries reuse the canonical distinctions", () => {
    const origin = "https://example.test";

    for (const output of [
      buildLlmsTxt(origin),
      buildGocciaApiSkillMd(origin),
    ]) {
      expect(output).toContain(GOCCIASCRIPT_SUMMARY);
      expect(output).toContain(ECMASCRIPT_SCOPE_ANSWER);
      expect(output).toContain(TYPE_ANNOTATIONS_ANSWER);
      expect(output).toContain(NODE_COMPATIBILITY_ANSWER);
      expect(output).toContain(VITEST_COMPATIBILITY_ANSWER);
      expect(output).toContain(COMPILER_SUPPORT_ANSWER);
    }
  });
});

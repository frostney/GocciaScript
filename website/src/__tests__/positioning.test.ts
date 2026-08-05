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
} from "@/lib/positioning";

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
    expect(canonicalCopy).not.toMatch(/\b\d{2,3}(?:\.\d+)?%\b/);
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
      expect(output).toContain(COMPILER_SUPPORT_ANSWER);
    }
  });
});

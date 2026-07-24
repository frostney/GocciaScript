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

describe("GocciaScript positioning", () => {
  test("keeps the canonical summary centered on the runtime purpose", () => {
    expect(GOCCIASCRIPT_SUMMARY).toContain(
      "sandbox-first ECMAScript runtime and toolchain",
    );
    expect(GOCCIASCRIPT_SUMMARY).toContain("AI agents");
    expect(GOCCIASCRIPT_SUMMARY).toContain("Hosts define");
    expect(GOCCIASCRIPT_SUMMARY).toContain("embedded in native applications");
    expect(GOCCIASCRIPT_SUMMARY.indexOf("AI agents")).toBeLessThan(
      GOCCIASCRIPT_SUMMARY.indexOf("embedded"),
    );
  });

  test("distinguishes the implementation from the recommended profile", () => {
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain("implements core ECMAScript");
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain(
      "product policy, not the implementation ceiling",
    );
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain("traditional for loops");
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain("for...in");
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain("while/do...while");
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain("non-strict Script semantics");
    expect(ECMASCRIPT_SCOPE_ANSWER).toContain(
      "private test262 host exposes it",
    );
  });

  test("keeps type annotations standards-first", () => {
    expect(TYPE_ANNOTATIONS_ANSWER).toContain("TC39 Type Annotations proposal");
    expect(TYPE_ANNOTATIONS_ANSWER).toContain(
      "types-as-comments runtime model",
    );
    expect(TYPE_ANNOTATIONS_ANSWER).toContain(
      "optional --strict-types GocciaScript extension",
    );
    expect(TYPE_ANNOTATIONS_ANSWER).toContain("interpreter and bytecode modes");
    expect(TYPE_ANNOTATIONS_ANSWER).toContain("not a replacement");
  });

  test("distinguishes the Node host from the sandbox fs API", () => {
    expect(NODE_COMPATIBILITY_ANSWER).toContain("not a complete Node.js host");
    expect(NODE_COMPATIBILITY_ANSWER).toContain("Node-compatible fs API");
    expect(NODE_COMPATIBILITY_ANSWER).toContain(
      "synchronous, callback, and promise-based",
    );
    expect(NODE_COMPATIBILITY_ANSWER).toContain("virtual filesystem");
  });

  test("states the complete Delphi support contract", () => {
    expect(COMPILER_SUPPORT_ANSWER).toContain("Win32 and Win64");
    expect(COMPILER_SUPPORT_ANSWER).toContain(
      "all applicable Pascal and JavaScript tests",
    );
    expect(COMPILER_SUPPORT_ANSWER).toContain("shared runtime semantics");
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

import { describe, expect, test } from "bun:test";
import {
  buildHomeStructuredData,
  ECMASCRIPT_SCOPE_QUESTION,
  GOCCIASCRIPT_SUMMARY,
  POSITIONING_FAQS,
} from "@/lib/positioning";

describe("GocciaScript positioning", () => {
  test("keeps the canonical summary centered on the runtime purpose", () => {
    expect(GOCCIASCRIPT_SUMMARY).toContain("sandbox-first ECMAScript runtime");
    expect(GOCCIASCRIPT_SUMMARY).toContain("host-controlled capabilities");
    expect(GOCCIASCRIPT_SUMMARY).toContain(
      "embedding portable JavaScript in applications",
    );
    expect(GOCCIASCRIPT_SUMMARY).not.toContain("generated code");
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
});

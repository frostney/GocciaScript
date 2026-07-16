import { describe, expect, test } from "bun:test";
import {
  buildHomeStructuredData,
  ECMASCRIPT_SCOPE_QUESTION,
  GOCCIASCRIPT_SUMMARY,
  POSITIONING_FAQS,
} from "@/lib/positioning";

describe("GocciaScript positioning", () => {
  test("keeps the canonical summary explicit about runtime and profile", () => {
    expect(GOCCIASCRIPT_SUMMARY).toContain(
      "FreePascal-native, embeddable ECMAScript runtime",
    );
    expect(GOCCIASCRIPT_SUMMARY).toContain("recommended profile");
    expect(GOCCIASCRIPT_SUMMARY).toContain("compatibility flags");
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

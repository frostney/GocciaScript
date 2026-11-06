import { describe, expect, test } from "bun:test";
import { renderToStaticMarkup } from "react-dom/server";
import { Landing } from "@/components/landing";
import type { Test262ModeBreakdown } from "@/lib/test262-dashboard";

function conformanceSentence(
  annexBIncluded: boolean,
  byMode?: Test262ModeBreakdown,
): string {
  const text = renderToStaticMarkup(
    <Landing
      compatibility={{ passed: 10, totalRun: 20, annexBIncluded, byMode }}
      release={null}
      locale="en-US"
    />,
  ).replace(/<[^>]+>/g, "");
  const start = text.indexOf("The latest published run");
  return text.slice(start, text.indexOf("); see results", start));
}

describe("landing conformance sentence", () => {
  test("names the Annex B exclusion while the run leaves annexB out", () => {
    expect(conformanceSentence(false)).toBe(
      "The latest published run, Annex B excluded, has 10 of 20 runs passing (50.0%",
    );
  });

  test("drops the exclusion once the run includes annexB", () => {
    expect(conformanceSentence(true)).toBe(
      "The latest published run has 10 of 20 runs passing (50.0%",
    );
  });

  test("counts runs and names each mode's rate when the report records modes", () => {
    const counts = { failed: 0, wrapperInfra: 0, timeouts: 0 };
    expect(
      conformanceSentence(true, {
        "non-strict": { run: 10, passed: 6, ...counts },
        strict: { run: 10, passed: 4, ...counts },
      }),
    ).toBe(
      "The latest published run has 10 of 20 runs passing (50.0%; non-strict 60.0%, strict 40.0%",
    );
  });
});

import { describe, expect, test } from "bun:test";
import { renderToStaticMarkup } from "react-dom/server";
import { Landing } from "@/components/landing";

function conformanceSentence(annexBIncluded: boolean): string {
  const text = renderToStaticMarkup(
    <Landing
      compatibility={{ passed: 10, totalRun: 20, annexBIncluded }}
      release={null}
      locale="en-US"
    />,
  ).replace(/<[^>]+>/g, "");
  const start = text.indexOf("The latest published run");
  return text.slice(start, text.indexOf("cases passing", start));
}

describe("landing conformance sentence", () => {
  test("names the Annex B exclusion while the run leaves annexB out", () => {
    expect(conformanceSentence(false)).toBe(
      "The latest published run, Annex B excluded, has 10 of 20 ",
    );
  });

  test("drops the exclusion once the run includes annexB", () => {
    expect(conformanceSentence(true)).toBe(
      "The latest published run has 10 of 20 ",
    );
  });
});

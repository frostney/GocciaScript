/*---
description: >
  A text asset's line breaks read as LF: a CRLF or a lone CR in the file is one
  LF in the imported content, whatever the platform wrote.
features: [text-assets]
---*/

import { content } from "./helpers/crlf-note.txt";

describe("text asset line breaks", () => {
  test("CRLF and CR read as LF", () => {
    expect(content).toBe("first line\nsecond line\nthird line\n");
  });
});

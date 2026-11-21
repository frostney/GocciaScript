/*---
description: Blob.prototype.stream and textStream are not implemented
features: [Blob]
---*/

describe("Blob.prototype.stream", () => {
  test("stream and textStream are absent: the engine has no ReadableStream", () => {
    expect("stream" in Blob.prototype).toBe(false);
    expect("textStream" in Blob.prototype).toBe(false);
  });
});

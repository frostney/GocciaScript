/*---
description: Testing emoji support in property names
features: [unicode-properties, emoji]
---*/

test("emoji property names", () => {
    const obj = {
      "🌟": "star emoji",
      "🎉": "party emoji",
      "💻": "computer emoji"
    };
  
    expect(obj["🌟"]).toBe("star emoji");
    expect(obj["🎉"]).toBe("party emoji");
    expect(obj["💻"]).toBe("computer emoji");
  });
  
  
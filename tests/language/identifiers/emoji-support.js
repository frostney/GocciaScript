/*---
description: Testing emoji support in identifiers
features: [unicode-identifiers, emoji]
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
  
  test("emoji identifiers", () => {
    const 🚀 = "rocket";
    const 🎯 = "target";
    
    expect(🚀).toBe("rocket");
    expect(🎯).toBe("target");
  }); 
/*---
features: [unicode-identifiers, emoji]
---*/

test("emoji identifiers", () => {
  // @ts-ignore
    const 🚀 = "rocket";
    // @ts-ignore
    const 🎯 = "target";
    
    // @ts-ignore
    expect(🚀).toBe("rocket");
    // @ts-ignore
    expect(🎯).toBe("target");
  }); 
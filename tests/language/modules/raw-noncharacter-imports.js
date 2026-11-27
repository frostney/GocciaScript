/*---
description: >
  An imported module or JSON file that holds a raw U+FFFE or U+FFFF in its
  UTF-8 text gives the importer that code unit.
features: [modules, json-modules]
---*/

import { noncharacters } from "./helpers/raw-noncharacter-export.js";
import data from "./helpers/raw-noncharacter.json";

describe("raw noncharacters in imported files", () => {
  test("a module's string literal keeps U+FFFF and U+FFFE", () => {
    expect(noncharacters.length).toBe(2);
    expect(noncharacters.charCodeAt(0)).toBe(0xffff);
    expect(noncharacters.charCodeAt(1)).toBe(0xfffe);
  });

  test("a JSON module's string keeps U+FFFF and U+FFFE", () => {
    expect(data.noncharacters.length).toBe(2);
    expect(data.noncharacters.charCodeAt(0)).toBe(0xffff);
    expect(data.noncharacters.charCodeAt(1)).toBe(0xfffe);
  });
});

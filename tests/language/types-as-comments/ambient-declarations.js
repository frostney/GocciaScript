/*---
description: ambient declarations (`declare`) are parsed and ignored at runtime
features: [types-as-comments]
---*/

declare const env: {
  mode: string;
  flags: Record<string, boolean>;
};
declare let counter: number;
declare var legacy: string;
declare function greet(name: string): void;
declare class Widget<T extends { id: string }> {
  render(): void;
}
declare abstract class Shape {}
declare enum Color { Red, Green }
declare const enum Size { Small, Large }
declare module "virtual:assets" {
  const url: string;
  export default url;
}
declare module "side-effect";
declare global {
  interface Window {
    app: unknown;
  }
}
declare namespace Lib.Inner {
  const version: string;
}
declare type Alias = string;
declare interface Shaped {
  area(): number;
}

test("every ambient form is skipped and binds nothing", () => {
  expect(typeof env).toBe("undefined");
  expect(typeof greet).toBe("undefined");
  expect(typeof Widget).toBe("undefined");
  expect(typeof Color).toBe("undefined");
  expect(typeof Lib).toBe("undefined");
});

test("code after ambient declarations runs", () => {
  const value = 41;
  expect(value + 1).toBe(42);
});

test("declare stays an identifier when no declaration follows it", () => {
  const declare = (input) => input * 2;
  expect(declare(21)).toBe(42);
  const table = { declare: 1 };
  expect(table.declare).toBe(1);
});

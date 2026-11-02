// Function declarations that read a top-level using binding.
export default function () {
  return resource.name;
}

export function readResource() {
  return resource.name;
}

using resource = {
  name: "imported",
  [Symbol.dispose]() {},
};

// Run: ./build/GocciaSandboxRunner /sandbox.js \
//   --seed=examples/_experimental/sandbox.js=/ --source-type=module \
//   --diff --diff-format=unified
import fs from "fs";
import { $ } from "goccia";

fs.writeFileSync("/greeting.txt", "Hello from the sandbox!");
console.log((await $`cat /greeting.txt`.text()).trim());

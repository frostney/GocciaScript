// Run: ./build/GocciaRunner examples/_experimental/sandbox.js --sandbox \
//   --source-type=module --diff=unified
import fs from "fs";
import { $ } from "goccia";

fs.writeFileSync("/greeting.txt", "Hello from the sandbox!");
console.log((await $`cat /greeting.txt`.text()).trim());

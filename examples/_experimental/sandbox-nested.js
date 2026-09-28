// Run: ./build/GocciaRunner examples/_experimental/sandbox-nested.js \
//   --copy examples/_experimental/sandbox-child.js --source-type=module \
//   --diff=unified
import fs from "fs";
import { runScript } from "goccia";

fs.writeFileSync("/message.txt", "hello from the parent");

const child = runScript("/sandbox-child.js", {
  sandbox: true,
  copy: ["/sandbox-child.js", "/message.txt"],
  diff: "unified",
});

console.log(child.stdout.trim());
console.log(child.diff.trim());
console.log("child write leaked?", fs.existsSync("/child-only.txt"));
fs.writeFileSync("/parent-only.txt", "the parent keeps running");

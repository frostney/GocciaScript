// Run: ./build/GocciaSandboxRunner /sandbox-nested.js \
//   --seed=examples/sandbox-nested.js=/ \
//   --seed=examples/sandbox-child.js=/ --source-type=module \
//   --diff --diff-format=unified
import fs from "fs";
import { runScript } from "goccia";

fs.writeFileSync("/message.txt", "hello from the parent");

const child = runScript("/sandbox-child.js", {
  sandbox: true,
  seed: ["/sandbox-child.js", "/message.txt"],
  diff: true,
  diffFormat: "unified",
});

console.log(child.stdout.trim());
console.log(child.diff.trim());
console.log("child write leaked?", fs.existsSync("/child-only.txt"));
fs.writeFileSync("/parent-only.txt", "the parent keeps running");

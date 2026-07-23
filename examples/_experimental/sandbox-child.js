// This experimental file runs inside the isolated child sandbox.
import fs from "fs";

console.log(fs.readFileSync("/message.txt", "utf8"));
fs.writeFileSync("/child-only.txt", "this stays in the child");

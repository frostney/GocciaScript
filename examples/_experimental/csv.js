// Run: ./build/GocciaScriptLoader examples/_experimental/csv.js
import { parse, stringify } from "goccia:csv";

const source = `speaker,talk
Ada,"Analytical Engine, 1843"
Grace,The Compiler`;

const talks = parse(source);
console.table(talks);
console.log(stringify(talks));

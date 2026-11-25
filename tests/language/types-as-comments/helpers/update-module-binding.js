// Updates a typed binding at a module's top level, whose bindings live in the
// module environment rather than in registers.
export let label: string = "1";

let postfixThrew = false;
try { label++; } catch (e) { postfixThrew = e instanceof TypeError; }

let prefixThrew = false;
try { --label; } catch (e) { prefixThrew = e instanceof TypeError; }

export let count: number = 1;
const countBefore = count++;

export const results = { postfixThrew, prefixThrew, countBefore };

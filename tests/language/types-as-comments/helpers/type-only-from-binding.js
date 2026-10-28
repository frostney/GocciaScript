// None of these modules exists. A type-only import is erased whole, so this
// file loads without trying to resolve any of them.
import type from from "./nonexistent-from-binding.js";
import type Named from "./nonexistent-named-binding.js";
import type { Member } from "./nonexistent-members.js";
import type * as Everything from "./nonexistent-namespace.js";

export const kinds = [
  typeof from,
  typeof Named,
  typeof Member,
  typeof Everything,
  typeof type,
];

import { invariant } from "es-toolkit/invariant";

const marker = "GocciaEsToolkitResult:";

try {
  invariant(true, "a true invariant must not throw");
  console.log(marker + JSON.stringify({ id: "invariant-true", status: "pass" }));
} catch (error) {
  console.log(marker + JSON.stringify({ id: "invariant-true", status: "fail", error: String(error) }));
}

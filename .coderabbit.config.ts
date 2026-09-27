// CodeRabbit configuration: the central frostney/coderabbit settings and the
// web-UI settings (inherited), minus review of vendored Agent Skills. The
// function is shared from frostney/coderabbit (lib/skills.ts); it needs this
// repository's lock, which only a file here can read — through
// skills-lock.yaml, a symlink to skills-lock.json (see that repository's
// README, "Shared functions").
import { defineConfig, includeRemote } from "@coderabbitai/config"
import lock from "./skills-lock.yaml"

const { excludeVendoredSkills } = includeRemote({
  path: "lib/skills.ts",
}) as unknown as { excludeVendoredSkills(lock: unknown): object }

export default defineConfig(excludeVendoredSkills(lock))

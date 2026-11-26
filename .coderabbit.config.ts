// CodeRabbit configuration: keep the central and web settings inherited, skip
// vendored skills, and spend PR-review allowance only at deliberate review
// points.
import { defineConfig, includeRemote } from "@coderabbitai/config"
import lock from "./skills-lock.yaml"

const { excludeVendoredSkills } = includeRemote({
  path: "lib/skills.ts",
}) as unknown as {
  excludeVendoredSkills(lock: unknown): {
    inheritance: true
    reviews: { path_filters: string[] }
  }
}

const reviewScope = excludeVendoredSkills(lock)

export default defineConfig({
  ...reviewScope,
  reviews: {
    ...reviewScope.reviews,
    auto_review: {
      // This repository opens many small and stacked PRs. Spend a review only
      // when the author marks a coherent change ready for feedback.
      enabled: false,
      labels: ["review-ready"],
      auto_incremental_review: false,
      ignore_usernames: ["github-actions[bot]"],
    },
  },
})

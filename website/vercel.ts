const ignoreCommand = `
base="\${VERCEL_GIT_PREVIOUS_SHA:-HEAD^}"
target="\${VERCEL_GIT_COMMIT_SHA:-HEAD}"

ensure_commit() {
  git cat-file -e "$1^{commit}" 2>/dev/null && return 0
  git fetch --depth=1 origin "$1" >/dev/null 2>&1 &&
    git cat-file -e "$1^{commit}" 2>/dev/null &&
    return 0

  if [ "$(git rev-parse --is-shallow-repository 2>/dev/null)" = "true" ]; then
    git fetch --unshallow >/dev/null 2>&1 &&
      git cat-file -e "$1^{commit}" 2>/dev/null &&
      return 0
  fi

  return 1
}

if ! ensure_commit "$base"; then
  echo "Previous commit $base is not available; building."
  exit 1
fi

if ! ensure_commit "$target"; then
  echo "Target commit $target is not available; building."
  exit 1
fi

git diff --quiet "$base" "$target" -- ./
`.trim();

export const config = {
  framework: "nextjs",
  buildCommand: "bun run build",
  installCommand: "bun install --frozen-lockfile",
  ignoreCommand,
  functions: {
    "src/app/api/run/route.ts": {
      maxDuration: 15,
      memory: 1024,
    },
  },
} as const;

#!/bin/sh
#
# Lints the staged Markdown files with markdownlint-cli2: the pre-commit
# lint-markdown check in lefthook.yml.
#
# Usage: sh scripts/lint-staged-markdown.sh
#
# The files come from git's index, not from Lefthook's staged-file list,
# which leaves out a type change (a symlink replaced by a regular file) and
# reads each path from the worktree, following symlinks, before it filters.
# A file is linted when its staged entry is a regular file (mode 100644 or
# 100755), so a staged symlink is skipped even where it was checked out as a
# plain file (core.symlinks=false). Lefthook still skips the whole job when
# its own list holds no regular file, so a commit that stages only type
# changes or symlinks is not linted here; CI's docs job lints the tree.
#
# Each file goes to the linter as a literal path (":<path>"), which
# markdownlint-cli2 neither reads as a glob nor drops for matching
# .gitignore. --no-globs keeps the config's "**/*.md", which CI's
# whole-tree run uses, from adding every Markdown file in the worktree.

set -eu

newline='
'
staged=$(git -c core.quotePath=false diff --cached --name-only \
  --diff-filter=ACMRT -- '*.md')

set -f
IFS=$newline
set --
for file in $staged; do
  case $(git --literal-pathspecs ls-files --stage -- "$file") in
    100644\ * | 100755\ *) set -- "$@" ":$file" ;;
  esac
done
unset IFS
set +f

[ $# -gt 0 ] || exit 0
exec npx markdownlint-cli2 --no-globs "$@"

# Exact file scope

When the user supplies a file list:

- accept exact repository-relative file paths only; do not expand directories
  or glob patterns;
- reject absolute paths, paths outside the repository, directories, ambiguous
  expansions, and entries that cannot be tied to the current worktree or
  comparison history;
- allow tracked files that were renamed or deleted in the comparison range;
- print the effective file list before judging the change; and
- locate every new finding in a listed file.

The list is a strict finding scope, not an inspection sandbox. Read the minimum
directly related source, tests, configuration, project instructions, and history
needed to understand the listed files, and run relevant probes. Disclose that
supporting context separately. Do not turn an issue found only in supporting
context into a finding; report a limitation only when it prevents a conclusion
about a listed file.

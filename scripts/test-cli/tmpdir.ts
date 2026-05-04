/**
 * Temp-directory helpers shared across the scripts/test-cli*.ts harnesses.
 */

import { mkdtempSync, rmSync } from "fs";
import { join } from "path";
import { tmpdir } from "os";

/** Creates a fresh temp directory under the OS tmpdir for a given prefix. */
export const mkdtemp = (prefix: string): string =>
  mkdtempSync(join(tmpdir(), prefix));

/**
 * Returns a 0-arg function that calls {@link mkdtemp} with `prefix` each
 * time. Bind once per harness when every block in the file shares the
 * same prefix; use {@link mkdtemp} directly when prefixes differ per call.
 */
export const makeTmpFactory = (prefix: string) => (): string => mkdtemp(prefix);

/** Recursively deletes a directory, ignoring missing paths. */
export const clean = (dir: string): void =>
  rmSync(dir, { recursive: true, force: true });

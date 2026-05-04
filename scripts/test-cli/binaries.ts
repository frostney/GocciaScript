/**
 * Build-output paths for the Goccia CLI binaries, with the `.exe` suffix
 * added on Windows. Shared across the scripts/test-cli*.ts smoke harnesses.
 */

const ext = process.platform === "win32" ? ".exe" : "";

export const LOADER = `./build/GocciaScriptLoader${ext}`;
export const BARE = `./build/GocciaScriptLoaderBare${ext}`;
export const REPL = `./build/GocciaREPL${ext}`;
export const TESTRUNNER = `./build/GocciaTestRunner${ext}`;
export const BUNDLER = `./build/GocciaBundler${ext}`;
export const BENCHRUNNER = `./build/GocciaBenchmarkRunner${ext}`;

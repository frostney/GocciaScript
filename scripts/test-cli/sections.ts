/**
 * Named sections for a scripts/test-cli*.ts harness, with --section and
 * --list selection. scripts/test-cli-apps.ts runs every check through
 * section(); scripts/test-cli-apps-selection.ts tests this module.
 */

import { parseArgs } from "util";

export type SectionOptions = { filters: string[]; list: boolean };

/**
 * Reads repeatable --section=<text> options and --list from `args`. Returns
 * an error message for an unknown option, a positional argument or an empty
 * --section text.
 */
export function parseSectionArgs(args: string[]): SectionOptions | { error: string } {
  let values: { section?: string[]; list?: boolean };
  try {
    ({ values } = parseArgs({
      args,
      options: { section: { type: "string", multiple: true }, list: { type: "boolean" } },
    }));
  } catch (error) {
    return { error: error instanceof Error ? error.message : String(error) };
  }
  const filters = values.section ?? [];
  if (filters.some((text) => text === "")) return { error: "--section needs a non-empty text" };
  return { filters, list: values.list === true };
}

/**
 * Runs named sections, recording a failure instead of aborting the run.
 *
 * A harness is a straight sequence of assertions that throw, so the first
 * failure used to end the run and every later section went unexamined. On
 * platforms exercised only in CI that meant one defect per round: during the
 * 0.11.0 stack the Windows job reported a coverage assertion, then — after a
 * fix and another twenty-minute round — a second one 550 lines further down.
 * Collecting failures turns those rounds into one.
 *
 * Output on a green run is unchanged: the header prints, the body runs, and
 * nothing is recorded. A failing section prints its error where it happened
 * and the run continues; every failure is repeated at the end and the run
 * exits non-zero.
 *
 * With --section texts, only the sections whose name contains one of them,
 * ignoring case, print and run; with --list, every name prints and no body
 * runs. A text that matches no section fails the run.
 */
export class SectionRunner {
  private readonly filters: string[];
  private readonly matched = new Set<string>();
  private readonly failures: { name: string; error: unknown }[] = [];
  private run = 0;
  private skipped = 0;

  constructor(
    private readonly options: SectionOptions,
    private readonly log: (line: string) => void = console.log,
    private readonly logError: (line: string) => void = console.error,
  ) {
    this.filters = options.filters.map((text) => text.toLowerCase());
  }

  async section(name: string, body: () => Promise<void> | void): Promise<void> {
    if (this.options.list) {
      this.log(name);
      return;
    }
    if (this.filters.length > 0) {
      const matching = this.filters.filter((text) => name.toLowerCase().includes(text));
      if (matching.length === 0) {
        this.skipped++;
        return;
      }
      for (const text of matching) this.matched.add(text);
    }
    this.run++;
    this.log(name);
    try {
      await body();
    } catch (error) {
      this.failures.push({ name, error });
      this.log(`  FAILED: ${messageOf(error)}`);
    }
  }

  /**
   * Prints the run's summary and returns its exit code: 1 when a section
   * failed or a --section text matched no section, 0 otherwise.
   */
  finish(script: string): number {
    if (this.options.list) return 0;
    const unmatched = this.options.filters.filter((text) => !this.matched.has(text.toLowerCase()));
    if (unmatched.length > 0)
      this.logError(`\nNo section matched ${unmatched.map((text) => JSON.stringify(text)).join(", ")}; --list prints the names.`);
    if (this.failures.length > 0) {
      this.logError(`\n${this.failures.length} section(s) failed:`);
      for (const failure of this.failures) {
        this.logError(`  - ${failure.name}`);
        this.logError(`      ${messageOf(failure.error)}`);
      }
    }
    if (unmatched.length > 0 || this.failures.length > 0) return 1;
    this.log(
      this.skipped === 0
        ? `\nAll ${script} tests passed.`
        : `\nAll ${this.run} selected ${script} sections passed; --section skipped ${this.skipped}.`,
    );
    return 0;
  }
}

const messageOf = (error: unknown): string => (error instanceof Error ? error.message : String(error));

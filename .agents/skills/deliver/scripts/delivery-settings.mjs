#!/usr/bin/env node
// Reads a project's delivery settings from the frontmatter of ORCHESTRATION.md.
// Usage: node delivery-settings.mjs [project-root]
import { readFile } from "node:fs/promises";
import { join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

export const ORCHESTRATION_FILE = "ORCHESTRATION.md";
export const ENDPOINTS = ["ready-to-merge", "merged", "deployed"];
export const ENTRY_POINTS = ["deliver", "stop"];
export const DEFAULT_SETTINGS = { endpoint: "ready-to-merge", "entry-points": "deliver" };
const ALLOWED = { endpoint: ENDPOINTS, "entry-points": ENTRY_POINTS };

function scalar(raw) {
  const value = raw.trim();
  const quoted = value.match(/^(["'])(.*)\1(\s+#.*)?$/);
  if (quoted) return quoted[2];
  const comment = value.search(/\s#/);
  return (comment === -1 ? value : value.slice(0, comment)).trim();
}

// Missing keys take their defaults; an unknown value or unterminated
// frontmatter is an error, so a typo never silently widens or narrows delivery.
export function parseDeliverySettings(source) {
  const settings = { ...DEFAULT_SETTINGS };
  const defaulted = new Set(Object.keys(DEFAULT_SETTINGS));
  if (source === null) return { ...settings, file: null, defaulted: [...defaulted] };
  const lines = source.replace(/^﻿/, "").split(/\r?\n/);
  if (lines[0] === "---") {
    const end = lines.indexOf("---", 1);
    if (end === -1) throw new Error(`${ORCHESTRATION_FILE} frontmatter has no closing ---`);
    for (const line of lines.slice(1, end)) {
      const match = line.match(/^([A-Za-z0-9_-]+):(.*)$/);
      if (!match || !(match[1] in ALLOWED)) continue;
      const key = match[1];
      const value = scalar(match[2]);
      if (!ALLOWED[key].includes(value)) {
        throw new Error(`${ORCHESTRATION_FILE} sets ${key} to "${value}"; use ${ALLOWED[key].join(", ")}`);
      }
      settings[key] = value;
      defaulted.delete(key);
    }
  }
  return { ...settings, file: ORCHESTRATION_FILE, defaulted: [...defaulted] };
}

export async function readDeliverySettings(projectRoot) {
  let source = null;
  try {
    source = await readFile(join(projectRoot, ORCHESTRATION_FILE), "utf8");
  } catch (error) {
    if (error.code !== "ENOENT") throw error;
  }
  return parseDeliverySettings(source);
}

if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  try {
    const settings = await readDeliverySettings(resolve(process.argv[2] ?? "."));
    process.stdout.write(`${JSON.stringify(settings)}\n`);
  } catch (error) {
    process.stderr.write(`${error.message}\n`);
    process.exitCode = 1;
  }
}

#!/usr/bin/env bun
/**
 * test-cli-embedded-resources.ts
 *
 * Smoke-checks that generated resource payloads are linked into the shipped
 * CLI binary and that the same binary can use the runtime features backed by
 * those resources.
 */

import { readFileSync } from "fs";

import { LOADER } from "./test-cli/binaries";
import { runLoaderJson } from "./test-cli/assertions";

type EmbeddedResourceMarker = {
  label: string;
  magic: string;
  resourceName: string;
};

const embeddedResources: EmbeddedResourceMarker[] = [
  { label: "timezone data", magic: "GOCCIATZ", resourceName: "GOCCIA_TZDATA" },
  { label: "CLDR data", magic: "GOCCIACL", resourceName: "GOCCIA_CLDR" },
  { label: "Unicode data", magic: "GOCCIAUC", resourceName: "GOCCIA_UCD" },
];

function assertBinaryContains(buffer: Buffer, marker: string, label: string): void {
  if (buffer.indexOf(Buffer.from(marker, "ascii")) < 0) {
    throw new Error(`${LOADER} is missing ${label} marker ${marker}`);
  }
}

function assertLoaderReturnsTrue(source: string, label: string, extraArgs: string[] = []): void {
  const { exitCode, json, stderr } = runLoaderJson(source, extraArgs);
  if (exitCode !== 0) {
    throw new Error(`${label} exited ${exitCode}: ${stderr}`);
  }

  const result = json.files?.[0]?.result;
  if (result !== true) {
    throw new Error(`${label} expected true, got ${JSON.stringify(result)}`);
  }
}

console.log("Embedded resource payload markers...");
{
  const loaderBytes = readFileSync(LOADER);
  for (const resource of embeddedResources) {
    assertBinaryContains(loaderBytes, resource.magic, `${resource.label} payload`);
    assertBinaryContains(loaderBytes, resource.resourceName, `${resource.label} resource name`);
  }
}

const behaviorProbes = [
  {
    label: "Temporal named timezone data",
    source: String.raw`
const summer = Temporal.ZonedDateTime.from("2024-07-01T12:00-04:00[America/New_York]");
const winter = Temporal.ZonedDateTime.from("2024-01-01T12:00-05:00[America/New_York]");
summer.timeZoneId === "America/New_York" &&
  summer.offset === "-04:00" &&
  winter.offset === "-05:00";
`,
  },
  {
    label: "Intl locale CLDR data",
    source: String.raw`
const usZones = new Intl.Locale("en-US").getTimeZones();
Array.isArray(usZones) &&
  usZones[0] === "America/Adak" &&
  usZones.includes("America/New_York") &&
  new Intl.Locale("ar").getTextInfo().direction === "rtl" &&
  new Intl.PluralRules("pl").select(2) === "few";
`,
  },
  {
    label: "RegExp Unicode property data",
    source: String.raw`
const greek = new RegExp("\\p{Script=Greek}", "u");
const emoji = new RegExp("\\p{Emoji}", "u");
greek.test("\u03A9") &&
  !greek.test("A") &&
  emoji.test("\u231A") &&
  !emoji.test("A");
`,
  },
];

for (const probe of behaviorProbes) {
  console.log(`${probe.label} (interpreted)...`);
  assertLoaderReturnsTrue(probe.source, probe.label);

  console.log(`${probe.label} (bytecode)...`);
  assertLoaderReturnsTrue(probe.source, `${probe.label} bytecode`, ["--mode=bytecode"]);
}

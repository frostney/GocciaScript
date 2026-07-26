export const GOCCIASCRIPT_SUMMARY =
  "GocciaScript is a sandbox-first ECMAScript runtime and toolchain for AI agents. Hosts define the available capabilities, runtime surface, and execution limits. It uses modern recommended defaults while tracking ECMAScript compatibility through generated test262 reports. GocciaScript is implemented in FreePascal, supports Delphi, and can also be embedded in native applications.";

export const GOCCIASCRIPT_META_DESCRIPTION =
  "Sandbox-first ECMAScript runtime and toolchain for AI agents, with host-defined capabilities, test262-tracked compatibility, and native application embedding.";

export const ECMASCRIPT_SCOPE_QUESTION =
  "Does GocciaScript implement only a subset of JavaScript?";

export const ECMASCRIPT_SCOPE_ANSWER =
  "GocciaScript implements core ECMAScript and publishes generated test262 results. Its recommended profile keeps var, traditional function syntax, loose equality, labels, traditional for loops, for...in, while/do...while, ASI, arguments, and non-strict Script semantics disabled by default, but each has an explicit compatibility path. Those defaults are product policy, not the implementation ceiling. Normal hosts do not install eval; the private test262 host exposes it for conformance. Broad Annex B browser compatibility is not a pre-1.0 target.";

export const TYPE_ANNOTATIONS_QUESTION =
  "How do type annotations and --strict-types relate?";

export const TYPE_ANNOTATIONS_ANSWER =
  "GocciaScript implements the TC39 Type Annotations proposal and its types-as-comments runtime model: supported annotations have no runtime effect by default. The optional --strict-types GocciaScript extension adds runtime enforcement for supported annotations and inferred primitive contracts in interpreter and bytecode modes. It is not a replacement for a static structural type checker such as tsc.";

export const NODE_COMPATIBILITY_QUESTION = "Is GocciaScript Node-compatible?";

export const NODE_COMPATIBILITY_ANSWER =
  "GocciaScript is not a complete Node.js host: it does not provide CommonJS, npm package resolution, process, Buffer, or the general node: module set. GocciaSandboxRunner does provide a Node-compatible fs API over its virtual filesystem, with synchronous, callback, and promise-based methods, Stats objects, and Node-shaped errors. The documented method set stays inside the sandbox and does not expose the ambient host filesystem.";

export const COMPILER_SUPPORT_QUESTION =
  "Does GocciaScript support FreePascal and Delphi?";

export const COMPILER_SUPPORT_ANSWER =
  "FreePascal is GocciaScript's cross-platform toolchain. The repository also includes Delphi 12 Community Edition projects for the complete shipped Win32 and Win64 application matrix. The Delphi support contract covers every shipped application and all applicable Pascal and JavaScript tests, with shared runtime semantics across both compilers.";

export const POSITIONING_FAQS = [
  {
    question: "What is GocciaScript?",
    answer: GOCCIASCRIPT_SUMMARY,
  },
  {
    question: ECMASCRIPT_SCOPE_QUESTION,
    answer: ECMASCRIPT_SCOPE_ANSWER,
  },
  {
    question: TYPE_ANNOTATIONS_QUESTION,
    answer: TYPE_ANNOTATIONS_ANSWER,
  },
  {
    question: NODE_COMPATIBILITY_QUESTION,
    answer: NODE_COMPATIBILITY_ANSWER,
  },
  {
    question: COMPILER_SUPPORT_QUESTION,
    answer: COMPILER_SUPPORT_ANSWER,
  },
] as const;

export function buildHomeStructuredData(origin: string) {
  const homepage = new URL("/", origin).toString();
  return [
    {
      "@context": "https://schema.org",
      "@type": "SoftwareApplication",
      name: "GocciaScript",
      applicationCategory: "DeveloperApplication",
      description: GOCCIASCRIPT_META_DESCRIPTION,
      url: homepage,
      codeRepository: "https://github.com/frostney/GocciaScript",
      license: "https://github.com/frostney/GocciaScript/blob/main/LICENSE",
      isAccessibleForFree: true,
    },
    {
      "@context": "https://schema.org",
      "@type": "FAQPage",
      mainEntity: POSITIONING_FAQS.map((item) => ({
        "@type": "Question",
        name: item.question,
        acceptedAnswer: {
          "@type": "Answer",
          text: item.answer,
        },
      })),
    },
  ] as const;
}

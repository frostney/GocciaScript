export const GOCCIASCRIPT_SUMMARY =
  "GocciaScript is a sandbox-first ECMAScript runtime with explicit host-controlled capabilities, designed for embedding portable JavaScript in applications. It is implemented in FreePascal, uses safer recommended defaults, and tracks language compatibility through generated test262 reports.";

export const GOCCIASCRIPT_META_DESCRIPTION =
  "Sandbox-first ECMAScript runtime with explicit host-controlled capabilities, portable application embedding, and test262-tracked language compatibility.";

export const ECMASCRIPT_SCOPE_QUESTION =
  "Does GocciaScript implement only a subset of JavaScript?";

export const ECMASCRIPT_SCOPE_ANSWER =
  "Calling GocciaScript merely a JavaScript-like subset is misleading. It implements a broad core ECMAScript surface and tracks current behavior with generated test262 reports. Its recommended default profile disables selected legacy or high-risk forms; compatibility flags enable many of those standard forms. Separately, it is not a Node.js, npm, or browser host, and broad Annex B browser compatibility is not a pre-1.0 target.";

export const NODE_COMPATIBILITY_QUESTION = "Is GocciaScript Node-compatible?";

export const NODE_COMPATIBILITY_ANSWER =
  "No. GocciaScript is intentionally not a Node.js host environment: it does not provide CommonJS, node: built-ins, npm-style package resolution, or Node globals such as process and Buffer.";

export const COMPILER_SUPPORT_QUESTION =
  "Does GocciaScript support FreePascal and Delphi?";

export const COMPILER_SUPPORT_ANSWER =
  "FreePascal is the supported compiler and embedding toolchain today. The source uses Delphi-compatible language mode, but compiling and embedding GocciaScript with the Delphi compiler is currently untested.";

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

import path from "node:path";
import { Blockquote } from "@astryxdesign/core/Blockquote";
import { Code, CodeBlock } from "@astryxdesign/core/CodeBlock";
import Link from "next/link";
import type { ComponentProps, ReactElement, ReactNode } from "react";
import { isValidElement } from "react";
import { MermaidBlock } from "@/components/mermaid-block";
import { type DocsPage, docsSource } from "@/lib/docs-source";

const REPOSITORY_BLOB_URL =
  "https://github.com/frostney/GocciaScript/blob/main";

function resolveRepositoryHref(href: string, page: DocsPage): string {
  if (
    href.startsWith("#") ||
    href.startsWith("/") ||
    /^[a-z][a-z\d+.-]*:/i.test(href)
  ) {
    return docsSource.resolveHref(href, page);
  }

  const match = href.match(/^([^?#]*)(\?[^#]*)?(#.*)?$/);
  const pathname = match?.[1] ?? href;
  const suffix = `${match?.[2] ?? ""}${match?.[3] ?? ""}`;
  const repositoryPath = path.posix.normalize(
    path.posix.join(path.posix.dirname(page.path), pathname),
  );
  const candidates = [
    repositoryPath,
    `${repositoryPath}.md`,
    path.posix.join(repositoryPath, "README.md"),
  ];
  const target = docsSource
    .getPages()
    .find((candidate) => candidates.includes(candidate.path));

  if (target) return `${target.url}${suffix}`;

  // The docs collection intentionally exposes README.md and docs/**/*.md.
  // Keep links to other repository files useful by sending them to GitHub
  // instead of inventing website routes that cannot resolve.
  return `${REPOSITORY_BLOB_URL}/${repositoryPath}${suffix}`;
}

function textContent(value: ReactNode): string {
  if (typeof value === "string" || typeof value === "number") {
    return String(value);
  }
  if (Array.isArray(value)) return value.map(textContent).join("");
  if (isValidElement<{ children?: ReactNode }>(value)) {
    return textContent(value.props.children);
  }
  return "";
}

function codeDetails(children: ReactNode): { code: string; language?: string } {
  if (!isValidElement<{ children?: ReactNode; className?: string }>(children)) {
    return { code: textContent(children) };
  }
  const language = children.props.className?.match(/language-([^\s]+)/)?.[1];
  return {
    code: textContent(children.props.children).replace(/\n$/, ""),
    language,
  };
}

export function docsMdxComponents(page: DocsPage) {
  return {
    a({ href = "", children, ...props }: ComponentProps<"a">) {
      const resolved = resolveRepositoryHref(href, page);
      const isInternal = resolved.startsWith("/") || resolved.startsWith("#");
      return isInternal ? (
        <Link href={resolved} {...props}>
          {children}
        </Link>
      ) : (
        <a href={resolved} rel="noreferrer" {...props}>
          {children}
        </a>
      );
    },
    blockquote({ children, ...props }: ComponentProps<"blockquote">) {
      return <Blockquote {...props}>{children}</Blockquote>;
    },
    code({ children, className }: ComponentProps<"code">) {
      return <Code className={className}>{children}</Code>;
    },
    img({ src, alt = "", ...props }: ComponentProps<"img">) {
      const resolvedSrc =
        typeof src === "string"
          ? src === "./logo.png"
            ? "/logo.png"
            : src
          : (src as { src?: string } | undefined)?.src;
      // Repository docs can contain arbitrary image sizes, so a semantic img
      // is the honest rendering primitive here; authors still own alt text.
      // biome-ignore lint/performance/noImgElement: repository Markdown has no fixed dimensions
      return <img src={resolvedSrc} alt={alt} loading="lazy" {...props} />;
    },
    pre({ children }: ComponentProps<"pre">) {
      const { code, language } = codeDetails(children);
      if (language === "mermaid") return <MermaidBlock code={code} />;
      return (
        <CodeBlock
          code={code}
          language={language}
          hasLanguageLabel={Boolean(language)}
          hasCopyButton
          width="100%"
        />
      );
    },
  } satisfies Record<string, (props: never) => ReactElement>;
}

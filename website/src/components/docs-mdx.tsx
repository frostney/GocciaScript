import { Blockquote } from "@astryxdesign/core/Blockquote";
import { Code, CodeBlock } from "@astryxdesign/core/CodeBlock";
import Link from "next/link";
import type { ComponentProps, ReactElement, ReactNode } from "react";
import { isValidElement } from "react";
import { MermaidBlock } from "@/components/mermaid-block";
import { resolveRepositoryHref } from "@/lib/docs-links";
import type { DocsPage } from "@/lib/docs-source";

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
    img({ src, alt = "", className, ...props }: ComponentProps<"img">) {
      const isRepositoryLogo =
        page.path === "README.md" && alt === "GocciaScript logo";
      const resolvedSrc =
        typeof src === "string"
          ? src === "./logo.png"
            ? "/logo.png"
            : src
          : (src as { src?: string } | undefined)?.src;
      // Repository docs can contain arbitrary image sizes, so a semantic img
      // is the honest rendering primitive here; authors still own alt text.
      return (
        // biome-ignore lint/performance/noImgElement: repository Markdown has no fixed dimensions
        <img
          src={resolvedSrc}
          alt={alt}
          className={[
            "docs-markdown-image",
            isRepositoryLogo ? "docs-logo-image" : undefined,
            className,
          ]
            .filter(Boolean)
            .join(" ")}
          loading="lazy"
          {...props}
        />
      );
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

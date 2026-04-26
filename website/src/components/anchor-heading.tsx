import type { ReactNode } from "react";

function slugify(text: string): string {
  return text
    .toLowerCase()
    .normalize("NFKD")
    .replace(/[̀-ͯ]/g, "")
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");
}

function extractText(node: ReactNode): string {
  if (typeof node === "string") return node;
  if (typeof node === "number") return String(node);
  if (Array.isArray(node)) return node.map(extractText).join("");
  if (
    node &&
    typeof node === "object" &&
    "props" in node &&
    node.props &&
    typeof node.props === "object" &&
    "children" in node.props
  ) {
    return extractText((node.props as { children: ReactNode }).children);
  }
  return "";
}

type Props = { id?: string; children: ReactNode; className?: string };

function AnchorLink({ slug }: { slug: string }) {
  return (
    <a
      href={`#${slug}`}
      className="heading-anchor"
      aria-label="Link to this section"
    >
      #
    </a>
  );
}

export function AnchorH2({ id, children, className }: Props) {
  const slug = id ?? slugify(extractText(children));
  return (
    <h2
      id={slug}
      className={`anchor-heading${className ? ` ${className}` : ""}`}
    >
      {children}
      <AnchorLink slug={slug} />
    </h2>
  );
}

export function AnchorH3({ id, children, className }: Props) {
  const slug = id ?? slugify(extractText(children));
  return (
    <h3
      id={slug}
      className={`anchor-heading${className ? ` ${className}` : ""}`}
    >
      {children}
      <AnchorLink slug={slug} />
    </h3>
  );
}

export { extractText, slugify };

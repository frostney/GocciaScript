// Heap snapshot workload: a markdown / MDX-style pipeline that suspends.
//
// Parses a generated document set (frontmatter, headings, lists, fenced code,
// paragraphs with emphasis, code spans and links) into an mdast-like tree,
// runs transform passes (heading slugs, a table of contents, link
// collection), then waits on the host to resolve the component props an
// MDX-style <Chart /> embed needs. While it waits, the trees, the regular
// expressions, the slug registry and the pass results are live.

const HEADING = /^(#{1,6})\s+(.*)$/;
const LIST_ITEM = /^\s*[-*]\s+(.*)$/;
const FENCE = /^```(\w*)$/;
const EMBED = /^<(\w+)\s+([^>]*)\/>$/;
const ATTR = /(\w+)="([^"]*)"/g;
const INLINE = /(\*\*[^*]+\*\*|\*[^*]+\*|`[^`]+`|\[[^\]]+\]\([^)]+\))/g;

const parseFrontmatter = (lines) => {
  const data = {};
  if (lines[0] !== "---") return { data, body: lines };
  const end = lines.indexOf("---", 1);
  for (const line of lines.slice(1, end)) {
    const at = line.indexOf(":");
    data[line.slice(0, at).trim()] = line.slice(at + 1).trim();
  }
  return { data, body: lines.slice(end + 1) };
};

const parseInline = (text) => {
  const nodes = [];
  let last = 0;
  for (const match of text.matchAll(INLINE)) {
    if (match.index > last) nodes.push({ type: "text", value: text.slice(last, match.index) });
    const token = match[0];
    if (token.startsWith("**")) nodes.push({ type: "strong", children: [{ type: "text", value: token.slice(2, -2) }] });
    else if (token.startsWith("*")) nodes.push({ type: "emphasis", children: [{ type: "text", value: token.slice(1, -1) }] });
    else if (token.startsWith("`")) nodes.push({ type: "inlineCode", value: token.slice(1, -1) });
    else {
      const close = token.indexOf("](");
      nodes.push({ type: "link", url: token.slice(close + 2, -1), children: [{ type: "text", value: token.slice(1, close) }] });
    }
    last = match.index + token.length;
  }
  if (last < text.length) nodes.push({ type: "text", value: text.slice(last) });
  return nodes;
};

const parseBlocks = (lines) => {
  const root = { type: "root", children: [] };
  let paragraph = [];
  let list = null;
  let code = null;
  const flush = () => {
    if (paragraph.length > 0) root.children.push({ type: "paragraph", children: parseInline(paragraph.join(" ")) });
    paragraph = [];
    list = null;
  };
  for (const line of lines) {
    if (code) {
      if (line === "```") { root.children.push(code); code = null; } else code.value += (code.value ? "\n" : "") + line;
      continue;
    }
    const fence = FENCE.exec(line);
    if (fence) { flush(); code = { type: "code", lang: fence[1], value: "" }; continue; }
    const heading = HEADING.exec(line);
    if (heading) { flush(); root.children.push({ type: "heading", depth: heading[1].length, children: parseInline(heading[2]) }); continue; }
    const embed = EMBED.exec(line);
    if (embed) {
      flush();
      const attributes = {};
      for (const m of embed[2].matchAll(ATTR)) attributes[m[1]] = m[2];
      root.children.push({ type: "mdxJsxFlowElement", name: embed[1], attributes });
      continue;
    }
    const item = LIST_ITEM.exec(line);
    if (item) {
      if (paragraph.length > 0) flush();
      if (!list) { list = { type: "list", children: [] }; root.children.push(list); }
      list.children.push({ type: "listItem", children: parseInline(item[1]) });
      continue;
    }
    if (line.trim() === "") { flush(); continue; }
    paragraph.push(line.trim());
  }
  flush();
  return root;
};

const textOf = (node) => node.value ?? (node.children ?? []).map(textOf).join("");
const slugs = new Map();
const slugify = (text) => {
  const base = text.toLowerCase().replace(/[^a-z0-9]+/g, "-").replace(/^-|-$/g, "");
  const n = slugs.get(base) ?? 0;
  slugs.set(base, n + 1);
  return n === 0 ? base : `${base}-${n}`;
};

const visit = (node, fn) => { fn(node); for (const child of node.children ?? []) visit(child, fn); };

const TOPICS = ["Runtime", "Parser", "Collector", "Modules", "Promises", "Strings", "Temporal", "Intl"];
const makeDoc = (i) => {
  const topic = TOPICS[i % TOPICS.length];
  const lines = ["---", `title: ${topic} notes ${i}`, `order: ${i}`, "---", `# ${topic} notes ${i}`, ""];
  for (const s of [1, 2, 3, 4]) {
    lines.push(`## Section ${s} of ${topic}`, "");
    lines.push(`The **${topic.toLowerCase()}** keeps *state* in \`slot ${s}\` and links to [the guide](https://example.com/${topic}/${s}).`);
    lines.push("Each paragraph continues on a second line with more words about the design.", "");
    lines.push("- first point about " + topic, "- second point with `code`", "- third point with a [link](https://example.com/p)", "");
    if (s % 2 === 0) lines.push("```js", `const x${s} = ${i} * ${s};`, "print(x" + s + ");", "```", "");
    if (s === 3) lines.push(`<Chart series="${topic}" points="${8 + (i % 5)}" />`, "");
  }
  return lines.join("\n");
};

const docs = Array.from({ length: 60 }, (_, i) => makeDoc(i)).map((source) => {
  const { data, body } = parseFrontmatter(source.split("\n"));
  const tree = parseBlocks(body);
  const toc = [];
  const links = [];
  visit(tree, (node) => {
    if (node.type === "heading") { node.id = slugify(textOf(node)); toc.push({ depth: node.depth, id: node.id, text: textOf(node) }); }
    if (node.type === "link") links.push(node.url);
  });
  return { data, tree, toc, links };
});

const embeds = [];
for (const doc of docs) visit(doc.tree, (node) => { if (node.type === "mdxJsxFlowElement") embeds.push(node); });
print("parsed", docs.length, "docs", embeds.length, "embeds", slugs.size, "slugs");

const escape = (s) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
const toHtml = (node) => {
  switch (node.type) {
    case "root": return node.children.map(toHtml).join("\n");
    case "heading": return `<h${node.depth} id="${node.id}">${node.children.map(toHtml).join("")}</h${node.depth}>`;
    case "paragraph": return `<p>${node.children.map(toHtml).join("")}</p>`;
    case "list": return `<ul>${node.children.map(toHtml).join("")}</ul>`;
    case "listItem": return `<li>${node.children.map(toHtml).join("")}</li>`;
    case "strong": return `<strong>${node.children.map(toHtml).join("")}</strong>`;
    case "emphasis": return `<em>${node.children.map(toHtml).join("")}</em>`;
    case "inlineCode": return `<code>${escape(node.value)}</code>`;
    case "code": return `<pre><code class="language-${node.lang}">${escape(node.value)}</code></pre>`;
    case "link": return `<a href="${node.url}">${node.children.map(toHtml).join("")}</a>`;
    case "mdxJsxFlowElement": return `<figure data-component="${node.name}">${node.rendered}</figure>`;
    default: return escape(node.value ?? "");
  }
};

const resolveEmbeds = async () => {
  const scale = await hostCall("chartScale", embeds.length);
  for (const node of embeds) {
    const points = Number(node.attributes.points);
    node.rendered = Array.from({ length: points }, (_, k) => Math.round(Math.sin(k) * scale * 10) / 10).join(",");
  }
};

resolveEmbeds().then(() => {
  const html = docs.map((d) => toHtml(d.tree)).join("\n");
  let hash = 0;
  for (const ch of html) hash = (hash * 31 + ch.charCodeAt(0)) % 1000000007;
  print("html", html.length, hash);
  print("toc", docs[7].toc.map((t) => t.id).join(","));
  print("links", docs.reduce((n, d) => n + d.links.length, 0));
  print("embed", embeds[3].rendered);
});

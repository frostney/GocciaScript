// Heap snapshot workload: a React-like server render that suspends.
//
// A small createElement / renderToString pair renders a product catalogue
// page. One async component (the account header) waits on the host for the
// session; while it waits, the element tree, the rendered chunks of every
// other component and a memo cache are live. Once the host answers, the
// suspended component renders and the page is assembled.

const h = (type, props, ...children) => ({
  type,
  props: props ?? {},
  children: children.flat(Infinity).filter((c) => c !== null && c !== false),
});

const ESCAPES = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" };
const escapeHtml = (text) =>
  String(text).replace(/[&<>"]/g, (c) => ESCAPES[c]);

const renderAttrs = (props) =>
  Object.entries(props)
    .filter(([k, v]) => k !== "key" && v !== undefined && v !== false)
    .map(([k, v]) => (v === true ? ` ${k}` : ` ${k === "className" ? "class" : k}="${escapeHtml(v)}"`))
    .join("");

const memo = new Map();
const suspended = [];

const render = (node, out) => {
  if (typeof node === "string" || typeof node === "number") {
    out.push(escapeHtml(node));
    return;
  }
  if (typeof node.type === "function") {
    const result = node.type({ ...node.props, children: node.children });
    if (result instanceof Promise) {
      const slot = out.length;
      out.push("");
      suspended.push(result.then((tree) => {
        const inner = [];
        render(tree, inner);
        out[slot] = inner.join("");
      }));
      return;
    }
    render(result, out);
    return;
  }
  out.push(`<${node.type}${renderAttrs(node.props)}>`);
  for (const child of node.children) render(child, out);
  out.push(`</${node.type}>`);
};

const Price = ({ cents, currency }) => {
  const key = `${currency}:${cents}`;
  if (!memo.has(key)) memo.set(key, `${(cents / 100).toFixed(2)} ${currency}`);
  return h("span", { className: "price" }, memo.get(key));
};

const Product = ({ product }) =>
  h("li", { className: "product", "data-id": product.id },
    h("h3", null, product.name),
    h("p", { className: "desc" }, product.description),
    h(Price, { cents: product.cents, currency: "EUR" }),
    product.tags.length > 0
      ? h("ul", { className: "tags" }, product.tags.map((t) => h("li", { key: t }, t)))
      : null);

const AccountHeader = async ({ userId }) => {
  const session = await hostCall("session", userId);
  return h("header", { className: "account" },
    h("span", null, `Signed in as user ${userId}, session ${session}`));
};

const Page = ({ products, userId }) =>
  h("html", null,
    h("head", null, h("title", null, "Catalogue")),
    h("body", null,
      h(AccountHeader, { userId }),
      h("main", null,
        h("ul", { className: "products" },
          products.map((p) => h(Product, { key: p.id, product: p }))))));

const WORDS = ["alpine", "brass", "cedar", "delta", "ember", "fjord", "garnet",
  "harbor", "indigo", "juniper", "kelp", "lumen", "maple", "nectar"];
const products = Array.from({ length: 400 }, (_, i) => ({
  id: `p${i}`,
  name: `${WORDS[i % WORDS.length]} ${WORDS[(i * 7) % WORDS.length]} #${i}`,
  description: Array.from({ length: 12 }, (_, j) => WORDS[(i + j * 3) % WORDS.length]).join(" "),
  cents: 499 + ((i * 7919) % 20000),
  tags: WORDS.filter((_, j) => (i + j) % 5 === 0),
}));

const out = ["<!doctype html>"];
const tree = h(Page, { products, userId: 42 });
render(tree, out);
print("rendered before suspend", out.length, suspended.length, memo.size);

Promise.all(suspended).then(() => {
  const html = out.join("");
  let hash = 0;
  for (const ch of html) hash = (hash * 31 + ch.charCodeAt(0)) % 1000000007;
  print("html", html.length, hash);
  print("head", html.slice(0, 120));
  print("header", html.slice(html.indexOf("<header"), html.indexOf("</header>") + 9));
});

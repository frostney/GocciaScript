// Heap snapshot workload: JSON-heavy data processing that suspends.
//
// Builds an order export, round-trips it through JSON.stringify and
// JSON.parse, indexes it by customer and by product in Maps, aggregates
// totals, then waits on the host for exchange rates. While it waits, the
// parsed records, the indexes and the partial aggregates are live.

const REGIONS = ["eu-west", "eu-central", "us-east", "ap-south"];
const SKUS = Array.from({ length: 120 }, (_, i) => `SKU-${String(i).padStart(4, "0")}`);
const CURRENCIES = ["EUR", "USD", "GBP", "JPY"];

const orders = Array.from({ length: 4000 }, (_, i) => ({
  id: `ord_${i}`,
  customer: `cus_${(i * 37) % 900}`,
  region: REGIONS[i % REGIONS.length],
  currency: CURRENCIES[(i >> 3) % CURRENCIES.length],
  placedAt: `2026-0${1 + (i % 9)}-${String(1 + (i % 28)).padStart(2, "0")}`,
  lines: Array.from({ length: 1 + (i % 5) }, (_, j) => ({
    sku: SKUS[(i * 13 + j * 7) % SKUS.length],
    quantity: 1 + ((i + j) % 4),
    unitCents: 199 + ((i * 31 + j * 17) % 5000),
  })),
  flags: { gift: i % 11 === 0, express: i % 7 === 0 },
}));

const exported = JSON.stringify(orders);
const records = JSON.parse(exported);

const byCustomer = new Map();
const bySku = new Map();
for (const order of records) {
  const list = byCustomer.get(order.customer) ?? [];
  list.push(order);
  byCustomer.set(order.customer, list);
  for (const line of order.lines) {
    const entry = bySku.get(line.sku) ?? { quantity: 0, cents: {} };
    entry.quantity += line.quantity;
    entry.cents[order.currency] = (entry.cents[order.currency] ?? 0) + line.quantity * line.unitCents;
    bySku.set(line.sku, entry);
  }
}

const totalsByCurrency = {};
for (const order of records) {
  const cents = order.lines.reduce((sum, l) => sum + l.quantity * l.unitCents, 0);
  totalsByCurrency[order.currency] = (totalsByCurrency[order.currency] ?? 0) + cents;
}

print("parsed", records.length, "orders", exported.length, "bytes", byCustomer.size, "customers", bySku.size, "skus");

const report = async () => {
  const usdPerEur = await hostCall("rate", "USD/EUR");
  const rates = { EUR: 1, USD: 1 / usdPerEur, GBP: 1.17, JPY: 0.0061 };
  const totalEur = Object.entries(totalsByCurrency)
    .reduce((sum, [currency, cents]) => sum + (cents / 100) * rates[currency], 0);
  const top = [...byCustomer.entries()]
    .map(([customer, list]) => [customer, list.reduce((s, o) => s + o.lines.reduce((t, l) => t + l.quantity * l.unitCents * rates[o.currency], 0), 0)])
    .sort((a, b) => b[1] - a[1] || (a[0] < b[0] ? -1 : 1))
    .slice(0, 5)
    .map(([c, v]) => `${c}=${(v / 100).toFixed(2)}`);
  const bestSku = [...bySku.entries()].sort((a, b) => b[1].quantity - a[1].quantity || (a[0] < b[0] ? -1 : 1))[0];
  return { totalEur: totalEur.toFixed(2), top, bestSku: `${bestSku[0]}:${bestSku[1].quantity}` };
};

report().then((r) => {
  print("totalEur", r.totalEur);
  print("top", r.top.join(" "));
  print("bestSku", r.bestSku);
  print("roundtrip", JSON.stringify(records) === exported);
});

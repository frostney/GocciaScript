// Run: ./build/GocciaScriptLoader examples/_experimental/tc39-ahead.js
// Compare: node examples/_experimental/tc39-ahead.js
const leaders = Iterator.zip([
  ["Ada", "Grace", "Linus"],
  [100, 98, 60],
]).map(([name, score]) => ({ name, score }));

for (const leader of leaders) {
  const badge = match (leader) {
    { score: 100 }: "🏆";
    { score: >= 90 }: "⭐";
    default: "🛠️";
  };
  console.log(`${badge} ${leader.name}: ${leader.score}`);
}

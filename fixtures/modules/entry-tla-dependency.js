globalThis.entryTlaOrder = [...(globalThis.entryTlaOrder ?? []), "dependency start"];

export let later = "before await";

await Promise.resolve();
await null;

later = "after await";
globalThis.entryTlaOrder = [...globalThis.entryTlaOrder, "dependency end"];

let hit = 0;
export default class {};
if (true) { hit = 1; }

let { destructuredHit } = { destructuredHit: 0 };
if (true) { destructuredHit = 1; }

export { destructuredHit, hit };

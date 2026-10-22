/*---
description: >
  A private name whose constant index does not fit in one byte still names the
  private element, and a property with the same text stays an ordinary property (#1326)
features: [classes, private-fields, private-methods, bytecode]
---*/

// Each test lists more than 255 constants before the private accesses, so the
// private names cannot use the one-byte constant-name instructions.

test("private reads, writes and `in` past the one-byte constant operand", () => {
  const padding = [
    "p000", "p001", "p002", "p003", "p004", "p005", "p006", "p007", "p008", "p009",
    "p010", "p011", "p012", "p013", "p014", "p015", "p016", "p017", "p018", "p019",
    "p020", "p021", "p022", "p023", "p024", "p025", "p026", "p027", "p028", "p029",
    "p030", "p031", "p032", "p033", "p034", "p035", "p036", "p037", "p038", "p039",
    "p040", "p041", "p042", "p043", "p044", "p045", "p046", "p047", "p048", "p049",
    "p050", "p051", "p052", "p053", "p054", "p055", "p056", "p057", "p058", "p059",
    "p060", "p061", "p062", "p063", "p064", "p065", "p066", "p067", "p068", "p069",
    "p070", "p071", "p072", "p073", "p074", "p075", "p076", "p077", "p078", "p079",
    "p080", "p081", "p082", "p083", "p084", "p085", "p086", "p087", "p088", "p089",
    "p090", "p091", "p092", "p093", "p094", "p095", "p096", "p097", "p098", "p099",
    "p100", "p101", "p102", "p103", "p104", "p105", "p106", "p107", "p108", "p109",
    "p110", "p111", "p112", "p113", "p114", "p115", "p116", "p117", "p118", "p119",
    "p120", "p121", "p122", "p123", "p124", "p125", "p126", "p127", "p128", "p129",
    "p130", "p131", "p132", "p133", "p134", "p135", "p136", "p137", "p138", "p139",
    "p140", "p141", "p142", "p143", "p144", "p145", "p146", "p147", "p148", "p149",
    "p150", "p151", "p152", "p153", "p154", "p155", "p156", "p157", "p158", "p159",
    "p160", "p161", "p162", "p163", "p164", "p165", "p166", "p167", "p168", "p169",
    "p170", "p171", "p172", "p173", "p174", "p175", "p176", "p177", "p178", "p179",
    "p180", "p181", "p182", "p183", "p184", "p185", "p186", "p187", "p188", "p189",
    "p190", "p191", "p192", "p193", "p194", "p195", "p196", "p197", "p198", "p199",
    "p200", "p201", "p202", "p203", "p204", "p205", "p206", "p207", "p208", "p209",
    "p210", "p211", "p212", "p213", "p214", "p215", "p216", "p217", "p218", "p219",
    "p220", "p221", "p222", "p223", "p224", "p225", "p226", "p227", "p228", "p229",
    "p230", "p231", "p232", "p233", "p234", "p235", "p236", "p237", "p238", "p239",
    "p240", "p241", "p242", "p243", "p244", "p245", "p246", "p247", "p248", "p249",
    "p250", "p251", "p252", "p253", "p254", "p255", "p256", "p257", "p258", "p259"
  ];
  class Box {
    #value = 1;
    "#slot:0$value" = "public";
    bump() {
      this.#value += 1;
      return [this.#value, this["#slot:0$value"], #value in this];
    }
    static has(value) {
      return #value in value;
    }
  }
  const box = new Box();
  expect(padding.length).toBe(260);
  expect(box.bump()).toEqual([2, "public", true]);
  expect(Box.has({ "#slot:0$value": 1 })).toBe(false);
  expect(Object.keys(box)).toEqual(["#slot:0$value"]);
});

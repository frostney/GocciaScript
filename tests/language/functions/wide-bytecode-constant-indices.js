/*---
description: Bytecode supports constant-pool indices above 255 in ABC operands
features: [object-literals, getters]
---*/

test("wide bytecode operands preserve property constant indices", () => {
  const run = () => {
    const values = [
      "value000", "value001", "value002", "value003", "value004", "value005", "value006", "value007",
      "value008", "value009", "value010", "value011", "value012", "value013", "value014", "value015",
      "value016", "value017", "value018", "value019", "value020", "value021", "value022", "value023",
      "value024", "value025", "value026", "value027", "value028", "value029", "value030", "value031",
      "value032", "value033", "value034", "value035", "value036", "value037", "value038", "value039",
      "value040", "value041", "value042", "value043", "value044", "value045", "value046", "value047",
      "value048", "value049", "value050", "value051", "value052", "value053", "value054", "value055",
      "value056", "value057", "value058", "value059", "value060", "value061", "value062", "value063",
      "value064", "value065", "value066", "value067", "value068", "value069", "value070", "value071",
      "value072", "value073", "value074", "value075", "value076", "value077", "value078", "value079",
      "value080", "value081", "value082", "value083", "value084", "value085", "value086", "value087",
      "value088", "value089", "value090", "value091", "value092", "value093", "value094", "value095",
      "value096", "value097", "value098", "value099", "value100", "value101", "value102", "value103",
      "value104", "value105", "value106", "value107", "value108", "value109", "value110", "value111",
      "value112", "value113", "value114", "value115", "value116", "value117", "value118", "value119",
      "value120", "value121", "value122", "value123", "value124", "value125", "value126", "value127",
      "value128", "value129", "value130", "value131", "value132", "value133", "value134", "value135",
      "value136", "value137", "value138", "value139", "value140", "value141", "value142", "value143",
      "value144", "value145", "value146", "value147", "value148", "value149", "value150", "value151",
      "value152", "value153", "value154", "value155", "value156", "value157", "value158", "value159",
      "value160", "value161", "value162", "value163", "value164", "value165", "value166", "value167",
      "value168", "value169", "value170", "value171", "value172", "value173", "value174", "value175",
      "value176", "value177", "value178", "value179", "value180", "value181", "value182", "value183",
      "value184", "value185", "value186", "value187", "value188", "value189", "value190", "value191",
      "value192", "value193", "value194", "value195", "value196", "value197", "value198", "value199",
      "value200", "value201", "value202", "value203", "value204", "value205", "value206", "value207",
      "value208", "value209", "value210", "value211", "value212", "value213", "value214", "value215",
      "value216", "value217", "value218", "value219", "value220", "value221", "value222", "value223",
      "value224", "value225", "value226", "value227", "value228", "value229", "value230", "value231",
      "value232", "value233", "value234", "value235", "value236", "value237", "value238", "value239",
      "value240", "value241", "value242", "value243", "value244", "value245", "value246", "value247",
      "value248", "value249", "value250", "value251", "value252", "value253", "value254", "value255",
      "value256", "value257", "value258", "value259"
    ];
    const object = {
      get result() {
        return values[259];
      },
    };

    return object.result;
  };

  expect(run()).toBe("value259");
});

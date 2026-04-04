import {
  "0" as firstDoc,
  "1" as tagList,
  "2" as status,
} from "./helpers/multi-document-stream.yaml";
import {
  firstDoc as reExportedFirstDoc,
  tagList as reExportedTagList,
  status as reExportedStatus,
} from "./helpers/multi-document-re-exporter.js";

describe("YAML multi-document module imports", () => {
  test("imports each document by string index", () => {
    expect(firstDoc.name).toBe("first");
    expect(firstDoc.enabled).toBe(true);
    expect(tagList.length).toBe(2);
    expect(tagList[0]).toBe("alpha");
    expect(tagList[1]).toBe("beta");
    expect(status).toBe("done");
  });

  test("re-exports indexed YAML documents through JavaScript modules", () => {
    expect(reExportedFirstDoc.name).toBe("first");
    expect(reExportedTagList[1]).toBe("beta");
    expect(reExportedStatus).toBe("done");
  });
});

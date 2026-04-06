import {
  content as noteContent,
  metadata as noteMetadata,
} from "./helpers/note.txt";
import {
  content as markdownContent,
  metadata as markdownMetadata,
} from "./helpers/guide.md";
import { content as extensionlessContent } from "./helpers/note";
import * as noteModule from "./helpers/note.txt";

describe("text asset import", () => {
  test("exports text file content as a string", () => {
    expect(noteContent).toBe("Caf\u00e9 asset\nSecond line\n");
    expect(markdownContent).toBe(
      "# Asset Import\n\n- named exports\n- namespace imports\n",
    );
  });

  test("supports extensionless text asset imports", () => {
    expect(extensionlessContent).toBe(noteContent);
  });

  test("exports frozen metadata for text assets", () => {
    expect(noteMetadata.kind).toBe("text");
    expect(noteMetadata.path.endsWith("note.txt")).toBe(true);
    expect(noteMetadata.fileName).toBe("note.txt");
    expect(noteMetadata.extension).toBe(".txt");
    expect(noteMetadata.byteLength).toBeGreaterThan(0);
    expect(Object.isFrozen(noteMetadata)).toBe(true);
    expect(markdownMetadata.fileName).toBe("guide.md");
    expect(markdownMetadata.extension).toBe(".md");
  });

  test("supports namespace imports for text assets", () => {
    expect(noteModule.content).toBe(noteContent);
    expect(noteModule.metadata).toBe(noteMetadata);
    expect(Object.keys(noteModule)).toEqual(["metadata", "content"]);
    expect(Object.getPrototypeOf(noteModule)).toBeNull();
    expect(Object.isFrozen(noteModule)).toBe(true);
  });
});

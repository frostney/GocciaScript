/*---
description: URL constructor with absolute and relative URLs
features: [URL]
---*/

describe("URL constructor", () => {
  test("basic absolute URL normalizes href", () => {
    const url = new URL("https://example.com");
    expect(url.href).toBe("https://example.com/");
  });

  test("URL with all components", () => {
    const url = new URL("https://user:pass@example.com:8080/path?q=1#hash");
    expect(url.protocol).toBe("https:");
    expect(url.username).toBe("user");
    expect(url.password).toBe("pass");
    expect(url.hostname).toBe("example.com");
    expect(url.port).toBe("8080");
    expect(url.pathname).toBe("/path");
    expect(url.search).toBe("?q=1");
    expect(url.hash).toBe("#hash");
  });

  test("relative URL with string base", () => {
    const url = new URL("/path", "https://example.com");
    expect(url.href).toBe("https://example.com/path");
  });

  test("relative URL with base URL object", () => {
    const base = new URL("https://example.com/foo/baz");
    const url = new URL("../bar", base);
    expect(url.href).toBe("https://example.com/bar");
  });

  test("relative path resolves against base", () => {
    const url = new URL("page.html", "https://example.com/dir/");
    expect(url.href).toBe("https://example.com/dir/page.html");
  });

  test("throws TypeError for invalid URL without base", () => {
    expect(() => new URL("not-a-url")).toThrow(TypeError);
  });

  test("throws TypeError for invalid base", () => {
    expect(() => new URL("/path", "not-valid")).toThrow(TypeError);
  });

  test("throws TypeError with no arguments", () => {
    expect(() => new URL()).toThrow(TypeError);
  });

  test("URL preserves trailing slash", () => {
    const url = new URL("https://example.com/path/");
    expect(url.pathname).toBe("/path/");
  });

  test("URL without path gets slash added", () => {
    const url = new URL("https://example.com");
    expect(url.pathname).toBe("/");
  });

  test("non-ASCII path, query and fragment are percent-encoded as UTF-8", () => {
    const url = new URL("http://h/café/../x?q=é#é");
    expect(url.href).toBe("http://h/x?q=%C3%A9#%C3%A9");
    expect(new URL("http://h/café").pathname).toBe("/caf%C3%A9");
  });

  test("a code point past U+00FF encodes as its UTF-8 bytes instead of failing", () => {
    expect(new URL("http://h/Ā").pathname).toBe("/%C4%80");
    expect(new URL("http://h/€?€#€").href).toBe(
      "http://h/%E2%82%AC?%E2%82%AC#%E2%82%AC",
    );
  });

  test("a surrogate pair is one code point and a lone surrogate is U+FFFD", () => {
    expect(new URL("http://h/\u{1F600}?\u{1F600}#\u{1F600}").href).toBe(
      "http://h/%F0%9F%98%80?%F0%9F%98%80#%F0%9F%98%80",
    );
    expect(new URL("http://h/\ud800").pathname).toBe("/%EF%BF%BD");
    expect(new URL("http://h/a\udc00b?\ud800#\ud800\ud800").href).toBe(
      "http://h/a%EF%BF%BDb?%EF%BF%BD#%EF%BF%BD%EF%BF%BD",
    );
  });

  test("a code point whose low byte is a delimiter is not a delimiter", () => {
    // U+012F, U+013F, U+0123 and U+015C have the low bytes of / ? # and \.
    const url = new URL("http://h/aįbĿcģdŜe");
    expect(url.pathname).toBe("/a%C4%AFb%C4%BFc%C4%A3d%C5%9Ce");
    expect(url.search).toBe("");
    expect(url.hash).toBe("");
  });

  test("userinfo is percent-encoded as UTF-8", () => {
    const url = new URL("http://üser:päss\u{1F600}@h/");
    expect(url.username).toBe("%C3%BCser");
    expect(url.password).toBe("p%C3%A4ss%F0%9F%98%80");
    expect(url.href).toBe("http://%C3%BCser:p%C3%A4ss%F0%9F%98%80@h/");
  });

  test("an opaque path and a non-special query are percent-encoded as UTF-8", () => {
    expect(new URL("foo:é?é#é").href).toBe("foo:%C3%A9?%C3%A9#%C3%A9");
    expect(new URL("foo://h/é?é").href).toBe("foo://h/%C3%A9?%C3%A9");
  });

  test("a non-special host is percent-encoded as UTF-8", () => {
    expect(new URL("foo://café/x").href).toBe("foo://caf%C3%A9/x");
  });

  test("a relative reference with non-ASCII resolves against its base", () => {
    expect(new URL("../é?é#é", "http://h/a/b/c").href).toBe(
      "http://h/a/%C3%A9?%C3%A9#%C3%A9",
    );
  });

  test("already-encoded input is not encoded twice", () => {
    expect(new URL("http://h/%C3%A9?%C3%A9#%C3%A9").href).toBe("http://h/%C3%A9?%C3%A9#%C3%A9");
  });

  test("the query's searchParams read it back as UTF-8", () => {
    expect(new URL("http://h/?q=é€").searchParams.get("q")).toBe("é€");
  });

  test("removing tabs and newlines does not join two unpaired surrogates", () => {
    expect(new URL("http://h/\ud83d\t\ude00").pathname).toBe("/%EF%BF%BD%EF%BF%BD");
    expect(new URL("http://h/?\ud83d\n\ude00").search).toBe("?%EF%BF%BD%EF%BF%BD");
  });
});

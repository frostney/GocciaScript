/*---
description: JSX expression containers holding comments
features: [jsx]
---*/

const Fragment = Symbol("Fragment");
const createElement = (tag, props, ...children) => ({ tag, props, children });

describe("JSX expression comments", () => {
  test("a comment-only container contributes no child", () => {
    const el = <div>{/* a comment */}</div>;
    expect(el.children.length).toBe(0);
  });

  test("a comment-only container between children contributes no child", () => {
    const el = (
      <div>
        {/* a comment */}
        <span>inner</span>
      </div>
    );
    expect(el.children.length).toBe(1);
    expect(el.children[0].tag).toBe("span");
  });

  test("a comment spanning lines contributes no child", () => {
    const el = (
      <div>
        {/* a comment
           that runs on */}
      </div>
    );
    expect(el.children.length).toBe(0);
  });

  test("adjacent comment-only containers contribute no children", () => {
    const el = <div>{/* one */}{/* two */}</div>;
    expect(el.children.length).toBe(0);
  });

  test("a line-comment-only container contributes no child", () => {
    const el = (
      <div>
        {
          // a comment
        }
      </div>
    );
    expect(el.children.length).toBe(0);
  });

  test("an empty container contributes no child", () => {
    const el = <div>{}</div>;
    expect(el.children.length).toBe(0);
  });

  test("a comment before an expression keeps the expression", () => {
    const value = 42;
    const el = <div>{/* the answer */ value}</div>;
    expect(el.children[0]).toBe(42);
  });

  test("a comment after an expression keeps the expression", () => {
    const value = 42;
    const el = <div>{value /* the answer */}</div>;
    expect(el.children[0]).toBe(42);
  });

  test("a comment containing a quote keeps the expression", () => {
    const value = "kept";
    const el = <div>{/* it's a comment */ value}</div>;
    expect(el.children[0]).toBe("kept");
  });

  test("a regex child is still a regex, not a comment", () => {
    const el = <div>{/ab+/.source}</div>;
    expect(el.children[0]).toBe("ab+");
  });

  test("a line comment between attributes is not an attribute", () => {
    const el = (
      <div
        // why this attribute is here
        id="one"
        className="two"
      />
    );
    expect(el.props.id).toBe("one");
    expect(el.props.className).toBe("two");
  });

  test("a block comment between attributes is not an attribute", () => {
    const el = <div id="one" /* between */ className="two" />;
    expect(el.props.id).toBe("one");
    expect(el.props.className).toBe("two");
  });

  test("a comment before the first attribute is not an attribute", () => {
    const el = (
      <div
        /* leading */
        id="one"
      />
    );
    expect(el.props.id).toBe("one");
  });

  test("a comment after the last attribute is not an attribute", () => {
    const el = (
      <div
        id="one"
        // trailing
      />
    );
    expect(el.props.id).toBe("one");
  });

  test("a comment inside an attribute expression keeps the value", () => {
    const el = <div value={/* the answer */ 42} />;
    expect(el.props.value).toBe(42);
  });

  test("a comment-only container is no child of a fragment", () => {
    const el = (
      <>
        {/* a comment */}
        <span>inner</span>
      </>
    );
    expect(el.children.length).toBe(1);
    expect(el.children[0].tag).toBe("span");
  });
});

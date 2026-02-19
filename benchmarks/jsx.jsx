/*---
description: JSX transformation and createElement benchmarks
---*/

const createElement = (tag, props, ...children) => ({ tag, props, children });
const Fragment = Symbol("Fragment");

suite("JSX element creation", () => {
  bench("simple element", () => {
    const el = <div>hello</div>;
  });

  bench("self-closing element", () => {
    const el = <br />;
  });

  bench("element with string attribute", () => {
    const el = <div className="container"></div>;
  });

  bench("element with multiple attributes", () => {
    const el = <div id="main" className="active" tabIndex={0} role="button"></div>;
  });

  bench("element with expression attribute", () => {
    const count = 42;
    const el = <span data-count={count + 1}></span>;
  });
});

suite("JSX children", () => {
  bench("text child", () => {
    const el = <p>Hello World</p>;
  });

  bench("expression child", () => {
    const name = "Alice";
    const el = <p>{name}</p>;
  });

  bench("mixed text and expression", () => {
    const name = "Bob";
    const el = <p>Hello {name}, welcome!</p>;
  });

  bench("nested elements (3 levels)", () => {
    const el = <div><section><p>deep</p></section></div>;
  });

  bench("sibling children", () => {
    const el = <ul><li>one</li><li>two</li><li>three</li></ul>;
  });
});

suite("JSX components", () => {
  bench("component element", () => {
    const Button = (props) => props;
    const el = <Button label="ok" />;
  });

  bench("component with children", () => {
    const Card = (props) => props;
    const el = <Card title="Test"><p>body</p></Card>;
  });

  bench("dotted component", () => {
    const UI = { Button: (props) => props };
    const el = <UI.Button variant="primary">Click</UI.Button>;
  });
});

suite("JSX fragments", () => {
  bench("empty fragment", () => {
    const el = <></>;
  });

  bench("fragment with children", () => {
    const el = <><span>a</span><span>b</span><span>c</span></>;
  });
});

suite("JSX spread and shorthand", () => {
  bench("spread attributes", () => {
    const props = { id: "item", className: "active", role: "listitem" };
    const el = <div {...props}></div>;
  });

  bench("spread with overrides", () => {
    const base = { className: "base", tabIndex: 0 };
    const el = <div {...base} className="override" id="main"></div>;
  });

  bench("shorthand props", () => {
    const className = "active";
    const id = "main";
    const el = <div {className} {id}></div>;
  });
});

suite("JSX complex trees", () => {
  bench("nav bar structure", () => {
    const el = (
      <nav>
        <ul>
          <li><a href="/">Home</a></li>
          <li><a href="/about">About</a></li>
          <li><a href="/contact">Contact</a></li>
        </ul>
      </nav>
    );
  });

  bench("card component tree", () => {
    const title = "Product";
    const price = 29.99;
    const el = (
      <div className="card">
        <div className="card-header">
          <h2>{title}</h2>
        </div>
        <div className="card-body">
          <p className="price">${price}</p>
          <button className="btn">Add to cart</button>
        </div>
      </div>
    );
  });

  bench("10 list items via Array.from", () => {
    const items = Array.from({ length: 10 }, (_, i) => (
      <li key={i} className="item">{i}</li>
    ));
  });
});

// goccia:yaml (parse, parseDocuments), goccia:toml (parse) and .yaml, .yml and .toml module imports for the CLI host.
// Goccia.YAML and Goccia.TOML parse in the host (n.dataParse) into Goccia.DataTree's JSON: an entry list in
// which a container names its children by index, so a node a YAML alias shares is one value here, cycles included.
// Objects have no prototype and a YAML tag outside the core schema keeps its tagName, as in the CLIs. Called once
// per realm with the global object, the host's natives (n) and the preludes' shared internals; registers the modules
// and the file formats.
(function (G, n, shared) {
  'use strict';
  const parseJSON = JSON.parse, create = Object.create, defineProperty = Object.defineProperty, setProto = Object.setPrototypeOf;
  const toObject = Object, reflectGet = Reflect.get, ownKeys = Reflect.ownKeys, ProxyCtor = Proxy;
  const ErrorCtor = G.Error, TypeErrorCtor = G.TypeError, WeakSetCtor = WeakSet;
  const uncurryThis = Function.prototype.bind.bind(Function.prototype.call);
  const WeakSetPrototypeAdd = uncurryThis(WeakSet.prototype.add), WeakSetPrototypeHas = uncurryThis(WeakSet.prototype.has);
  const StringCtor = String;
  // a list append that defines the index (a setter on Array.prototype must not see it)
  const append = (list, v) => { defineProperty(list, list.length, { __proto__: null, value: v, writable: true, enumerable: true, configurable: true }); };
  const isTagged = new WeakSetCtor();
  const addTagged = (v) => { WeakSetPrototypeAdd(isTagged, v); return v; };
  const SPECIAL_NUMBERS = { NaN: NaN, Infinity: Infinity, '-Infinity': -Infinity, '-0': -0 };
  const KIND_YAML = 0, KIND_YAML_DOCUMENTS = 1, KIND_TOML = 2;

  // a tagged node: the value itself with tagName and value added (an object or array behind a proxy, a scalar boxed;
  // the CLIs' tagged value takes the scalar's typeof, which a script value cannot)
  function tagged(tagName, value) {
    if (value !== null && typeof value === 'object') {
      return addTagged(new ProxyCtor(value, {
        get: (target, key) => key === 'tagName' ? tagName : key === 'value' ? value : reflectGet(target, key),
      }));
    }
    const box = value === null ? create(null) : toObject(value);
    defineProperty(box, 'tagName', { __proto__: null, value: tagName });
    defineProperty(box, 'value', { __proto__: null, value });
    return addTagged(box);
  }

  const field = (value) => ({ __proto__: null, value, writable: true, enumerable: true, configurable: true });

  // the value of entry 0 of Goccia.DataTree's JSON; a container is made empty when first named and filled in a
  // second pass, so a cycle finds it and the nesting needs no recursion
  function rebuild(json) {
    const entries = parseJSON(json);
    // the caches have no prototype: a read of an index not filled yet finds nothing a script put on Array.prototype
    const values = setProto([], null), made = setProto([], null), unfilled = [];
    const valueOf = (i) => {
      if (made[i] === true) return values[i];
      const e = entries[i];
      let v;
      if (e === null || typeof e !== 'object') v = e;
      else if (e[0] === 0) v = SPECIAL_NUMBERS[e[1]];
      else if (e[0] === 1) { v = []; append(unfilled, i); }
      else if (e[0] === 2) { v = create(null); append(unfilled, i); }
      else v = tagged(e[1], valueOf(e[2]));
      defineProperty(values, i, field(v));
      defineProperty(made, i, field(true));
      return v;
    };
    const root = valueOf(0);
    for (let k = 0; k < unfilled.length; k++) {
      const i = unfilled[k], e = entries[i], v = values[i];
      if (e[0] === 1) for (let j = 1; j < e.length; j++) defineProperty(v, j - 1, field(valueOf(e[j])));
      else for (let j = 1; j < e.length; j += 2) defineProperty(v, e[j], field(valueOf(e[j + 1])));
    }
    return root;
  }

  const stringArgument = (args, name) => {
    if (args.length < 1) throw new ErrorCtor(`${name} expects at least 1 argument but got 0`);
    if (typeof args[0] !== 'string') throw new TypeErrorCtor(`${name}: argument must be a string`);
    return args[0];
  };

  // YAML.parse(text), as Bun.YAML.parse: null for no document, the document itself for one, the documents for more
  function parse(text) {
    return rebuild(n.dataParse(KIND_YAML, stringArgument(arguments, 'YAML.parse')));
  }
  function parseDocuments(text) {
    return rebuild(n.dataParse(KIND_YAML_DOCUMENTS, stringArgument(arguments, 'YAML.parseDocuments')));
  }
  const yaml = { parse, parseDocuments };
  const toml = {
    parse: function parse(text) {
      return rebuild(n.dataParse(KIND_TOML, stringArgument(arguments, 'TOML.parse')));
    },
  };
  shared.hostModules['goccia:yaml'] = () => yaml;
  shared.hostModules['goccia:toml'] = () => toml;

  // a YAML file of several documents imports each under its index ("0", "1", ...); of one document, a mapping's
  // keys or a sequence's own properties (its indices and length), anything else as the default export
  const yamlModule = (text, key, specifier) => {
    const documents = rebuild(n.dataParse(KIND_YAML_DOCUMENTS, text));
    if (documents.length === 0)
      throw new ErrorCtor(`YAML module "${specifier}" must contain at least one top-level document.`);
    const out = [];
    if (documents.length > 1) {
      for (let i = 0; i < documents.length; i++) { append(out, StringCtor(i)); append(out, documents[i]); }
      return shared.exportList(out);
    }
    const document = documents[0];
    if (document === null || typeof document !== 'object' || WeakSetPrototypeHas(isTagged, document)) {
      return shared.exportList(['default', document]);
    }
    const keys = ownKeys(document);
    for (let i = 0; i < keys.length; i++) { append(out, keys[i]); append(out, document[keys[i]]); }
    return shared.exportList(out);
  };
  shared.dataFormats['.yaml'] = yamlModule;
  shared.dataFormats['.yml'] = yamlModule;
  // a TOML file imports as its top-level table's keys
  shared.dataFormats['.toml'] = (text) => rebuild(n.dataParse(KIND_TOML, text));
})

# 0195 - YAML is parsed by a port of saphyr's scanner and event parser

**Date:** 2026-11-27
**Area:** `yaml`, `data-formats`
**Related:** [#2247](https://github.com/frostney/GocciaScript/issues/2247)

## Context

`goccia:yaml` and `.yaml` imports read YAML through `TGocciaYAMLParser`, which
matched lines of text: it stripped comments, searched for top-level `:` and
compared indentation counts. Against the yaml-test-suite, with values compared
to each case's `in.json`, it accepted invalid YAML, rejected valid YAML and
gave wrong values for about a quarter of the cases. The failures spread over
block scalars, folding, tabs, comments, flow collections, node properties and
directives, so fixing one case in that structure risked another.

YAML 1.2.2 defines the language as a grammar of context-dependent productions.
The parsers that pass the whole suite read it as libyaml does: a scanner that
tracks indentation, flow nesting and possible implicit keys, and a parser that
turns its tokens into the suite's event stream. saphyr-parser (Rust, MIT or
Apache-2.0) is one, with no case excluded.

## Decision

`Goccia.YAML.Scanner` and `Goccia.YAML.Events` are a port of saphyr-parser's
scanner and parser, and `Goccia.YAML` composes their events into a
`Goccia.DataTree` tree. The composer owns what YAML leaves to the
application: the core schema with the extensions Bun also reads, tags, merge
keys, anchors as shared nodes, string keys, and the depth and work guards.

`GocciaYAMLComplianceRunner` checks the event stream against each case's
`test.event`, the values against `in.json`, and the rejection of each error
case. Pull-request and full CI fail on any case that does not pass.

## Consequences

- A suite bump that adds a failing case fails CI until the parser handles it.
- The port keeps saphyr's structure, so a fix saphyr makes can be carried over.
  Where the port differs (UTF-16 positions, a depth guard instead of a flow
  nesting limit, a block-level key that goes stale inside a flow collection),
  the unit says so.
- Directives need the `---` that starts their document, a tab after the
  indentation of a block scalar line is content, and `!!binary` keeps its
  base64 text, as YAML 1.2.2 and the suite require.

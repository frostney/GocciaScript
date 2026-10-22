# 0149 - A fetch request's URL and a redirect's Location go through the URL parser

**Date:** 2026-10-19
**Area:** `runtime`, `embedding`
**Related:** [ADR 0146](0146-embedder-fetch-transport.md), [#1786](https://github.com/frostney/GocciaScript/issues/1786), [#1805](https://github.com/frostney/GocciaScript/issues/1805)

## Context

The built-in client resolved a path-relative redirect `Location` against the
host root instead of the redirecting response's URL, and sent request paths
with `.` and `..` segments in them. [ADR 0146](0146-embedder-fetch-transport.md)
handed a transport the URL as `fetch()` received it, unnormalized. WHATWG
Fetch (HTTP-redirect fetch) parses `Location` with the URL parser, using the
response's URL as the base, and a request's URL is the parsed one.

## Decision

**The request URL is the parsed one, on both paths.** `NormalizeHTTPRequestURL`
(`HTTPClient`) parses an absolute http(s) URL with the URL Standard's parser
(`Goccia.URL.Parser`) and serializes it without its fragment, so the host is
lowercase and dot segments are removed. `fetch()` applies it once, before the
URL reaches either the built-in client or a transport, so a transport's
`Request.URL` is the URL the client would send. This supersedes the
"unnormalized" consequence of ADR 0146.

**A redirect's `Location` is resolved against the redirecting response's URL.**
`ResolveHTTPRedirectURL` parses it with that URL as the base, so relative,
`./`, `../`, absolute-path, query-only and scheme-relative forms resolve as
the URL Standard says. The client uses it for every hop, and so does
`FetchOverNetwork`, which runs the same client. A `Location` that does not
parse is a network error.

**What the client refuses today stays refused.** A URL with a control
character, a scheme other than http and https, a non-ASCII host or one the
parser refuses is handed on as given for the client's own checks. Non-ASCII
path and query characters reach the parser percent-encoded as UTF-8; the
bytes of a `Location` header value are percent-encoded one by one.

## Consequences

- A host that keys recordings by `Request.URL` sees one spelling of a URL.
- Host parsing is still the engine's simplified one, so a host form the URL
  Standard accepts and the engine's parser does not is handed on as given for
  a request and refused for a `Location`.

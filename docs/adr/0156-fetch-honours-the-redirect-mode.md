# 0156 - fetch honours the redirect mode, and "manual" answers with the redirect response

**Date:** 2026-10-22
**Area:** `runtime`, `embedding`
**Related:** [ADR 0146](0146-embedder-fetch-transport.md), [ADR 0149](0149-fetch-urls-go-through-the-url-parser.md), [#1870](https://github.com/frostney/GocciaScript/issues/1870)

## Context

`fetch()` ignored its `redirect` option and followed every redirect. After
20 redirects the built-in client stopped following and answered with the
twenty-first redirect response, which a script that checks only `status`
cannot tell from success. It also treated 306 as a redirect status. WHATWG
Fetch decides a redirect by the request's redirect mode (HTTP fetch): "error"
is a network error, "manual" answers with an opaque-redirect filtered
response, and "follow" runs HTTP-redirect fetch, whose step for a request
already redirected 20 times returns a network error. A redirect status is
301, 302, 303, 307 or 308.

The opaque-redirect filter exists for a browser's origin model: a page must
not read where another origin redirects it. GocciaScript has no origin model
and no page, and a script asks for "manual" precisely to read the `Location`.

## Decision

**`redirect` is read as the RequestRedirect enum.** `fetch()` converts the
option to a string; `"follow"` (the default when absent or undefined),
`"error"` and `"manual"` are accepted, and anything else throws `TypeError`
synchronously, as an unsupported method does.

**The mode travels with the request.** The built-in client takes it in
`THTTPRequestPolicy.RedirectMode` (`HTTPTypes.THTTPRedirectMode`), the fetch
manager hands it to the worker, and a transport receives it as
`TGocciaFetchTransportRequest.Redirect`. `FetchOverNetwork` honours it, so a
forwarding transport reads as the built-in client does. A transport that
follows redirects itself must honour it; the engine does not second-guess its
answer.

**"manual" answers with the redirect response itself.** Its status, status
text, headers and body reach the script, `redirected` is false and `url` is
the request's URL, as in Node.js and Bun. It is not an opaque-redirect
filtered response.

**"error" refuses every redirect status, one without a `Location` too,**
from its status line and headers, before its body is read or any further
request is made, with a network error (`TypeError`).

**"follow" fails on the twenty-first redirect.** Twenty redirects are
followed; the twenty-first redirect response, once its `Location` resolves to
an http(s) URL, is a network error (`TypeError: redirect limit exceeded: more
than 20 redirects`). A redirect without a `Location` is still the answer.

**Only 301, 302, 303, 307 and 308 are redirect statuses,** and a 303 turns
a request into GET only when it is neither GET nor HEAD (HTTP-redirect fetch),
so a HEAD request stays HEAD; it used to become GET.

## Consequences

- Scripts and embedders that relied on the 21st redirect response, or on
  306 being followed, now see a network error or the 306 response, and a
  HEAD request redirected by a 303 gets a HEAD response.
- Package and module loads that use the built-in client keep the default
  "follow" mode and gain the same limit.
- The credential rule is unchanged: `Authorization`, `Cookie` and
  `Proxy-Authorization` stop on the first redirect to another origin,
  an HTTPS-to-HTTP redirect included, under "follow".

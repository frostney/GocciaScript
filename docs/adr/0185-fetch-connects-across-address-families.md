# 0185 - fetch connects across address families

**Date:** 2026-11-24
**Area:** `runtime`, `embedding`
**Related:** [ADR 0122](0122-unified-capability-model.md), [ADR 0170](0170-fetch-reuses-the-connection-within-an-origin.md)

## Context

The built-in HTTP client asked the host resolver for IPv4 and dialed only
the first answer. Hosts with AAAA records alone could not be reached. An
IPv6 route may be absent or stalled even when DNS gives both families.

## Decision

Resolve both families through the platform resolver. Check the hostname
before lookup and check every returned address before the connection worker
gets it. The worker dials the checked numeric address and never resolves
the hostname. TLS uses the requested hostname. Reused connections keep the
winning address for later redirect checks.

Keep the first approved address in resolver order and alternate families
among the rest. Keep pending attempts alive while staggering later ones by
up to 250 ms, using the connection cadence in [RFC 8305 section 8](https://www.rfc-editor.org/rfc/rfc8305.html#section-8).
When there is less than 500 ms left, give the next attempt at least half
the remaining budget. A failed attempt advances immediately. The first
successful socket wins and the rest close. Each connection and any redirects
share the original request deadline.

## Consequences

AAAA-only hosts work with an IPv6 route. A missing route fails at connect
time, and a stalled route does not consume the entire deadline before the
other family is tried. Hostname grants exclude non-public IPv6 addresses
as well as private IPv4 addresses. Explicit network grants still govern
permission to use those destinations.

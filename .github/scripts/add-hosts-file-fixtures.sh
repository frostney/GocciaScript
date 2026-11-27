#!/usr/bin/env bash

# Adds the host names that only the hosts file lists, so the resolver checks
# run instead of skipping: HTTPClient.Test.pas and test-cli-permissions.ts
# resolve goccia-hosts-file.test, and fetch-e2e.sh fetches
# goccia-hosts-ipv6.test, whose only address is ::1. Fetch resolves names
# through the system resolver (getaddrinfo), which reads the hosts file, so
# these names reach it on Linux and macOS alike (issues #1565 and #1688).
#
# Linux lists the IPv4 name under 127.0.0.2, an address the default resolver
# would never return for a name, so finding it proves the hosts file was read.
# macOS configures only 127.0.0.1 on its loopback interface, so a connect to
# 127.0.0.2 waits 75 seconds for a timeout instead of being refused at once:
# there the name gets 127.0.0.1.

set -euo pipefail

case "$(uname -s)" in
  Linux)  ipv4_address=127.0.0.2 ;;
  Darwin) ipv4_address=127.0.0.1 ;;
  *)
    echo "add-hosts-file-fixtures.sh: unsupported OS $(uname -s)" >&2
    exit 1
    ;;
esac

printf '%s\n' "$ipv4_address goccia-hosts-file.test" '::1 goccia-hosts-ipv6.test' |
  sudo tee -a /etc/hosts > /dev/null

# The system resolver must answer from the file before any check relies on
# it: getaddrinfo, the call fetch makes, raises for a name it cannot resolve.
python3 - <<'PYTHON'
import socket

for name in ("goccia-hosts-file.test", "goccia-hosts-ipv6.test"):
    found = socket.getaddrinfo(name, None, proto=socket.IPPROTO_TCP)
    print(name, " ".join(sorted({entry[4][0] for entry in found})))
PYTHON

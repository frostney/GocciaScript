# APT repository for GocciaScript

End-user install instructions live on https://gocciascript.dev/install
and in the project's `README.md`. This directory is for **maintainers**
publishing the repository.

## What ships here

- `gocciascript.list` — the apt sources line users add to
  `/etc/apt/sources.list.d/gocciascript.list`. Pinned to the
  `signed-by=` keyring path produced by the user-side `dearmor` step.
- `build-deb.sh` — packages the release archive's `build/` payload
  into a Debian binary package (`.deb`) named
  `gocciascript_<version>_<arch>.deb`. Run once per arch per release.

## Repository layout

The hosted repo at `https://gocciascript.dev/repo/` follows the
standard flat-layout APT structure:

```text
repo/
  key.gpg                  # public signing key (downloaded by users)
  dists/
    stable/
      InRelease            # signed Release file
      Release
      Release.gpg
      main/
        binary-amd64/Packages.gz
        binary-arm64/Packages.gz
  pool/
    main/g/gocciascript/
      gocciascript_0.6.1_amd64.deb
      gocciascript_0.6.1_arm64.deb
```

The `key.gpg` served at `https://gocciascript.dev/repo/key.gpg` is
the public half of the repo's signing key. Users dearmor it into a
keyring under `/usr/share/keyrings/gocciascript.gpg` and reference
that keyring in their `sources.list.d/` entry — modern Debian/Ubuntu
practice (`signed-by=` instead of the deprecated `apt-key`).

## Publishing flow (for the release pipeline)

```sh
# 1. Build .debs from the released artifacts.
./packaging/apt/build-deb.sh 0.6.1 amd64
./packaging/apt/build-deb.sh 0.6.1 arm64

# 2. Stage them into the pool.
mkdir -p repo/pool/main/g/gocciascript
mv gocciascript_0.6.1_*.deb repo/pool/main/g/gocciascript/

# 3. Regenerate the index files (requires apt-utils).
cd repo
apt-ftparchive packages pool/main > dists/stable/main/binary-amd64/Packages
gzip -kf dists/stable/main/binary-amd64/Packages
# (repeat per arch)

# 4. Generate Release + sign it.
apt-ftparchive release dists/stable > dists/stable/Release
gpg --default-key gocciascript@frostney.dev \
  -abs -o dists/stable/Release.gpg dists/stable/Release
gpg --default-key gocciascript@frostney.dev \
  --clearsign -o dists/stable/InRelease dists/stable/Release

# 5. Upload to the host serving https://gocciascript.dev/repo/.
```

Until the repo is live, the install commands referenced from the
website point at endpoints that 404 — they're the *intended* layout,
documented here so the publishing pipeline has a target to aim at.

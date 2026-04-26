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

## One-time setup

1. **Generate the GPG signing key.** Keep the private half in a
   password-managed secret (or a GitHub Actions repo secret named
   `APT_SIGNING_KEY`). The public half becomes `repo/key.gpg`.

   ```sh
   gpg --quick-generate-key \
     'GocciaScript releases <gocciascript@frostney.dev>' rsa4096 sign 5y
   gpg --armor --export gocciascript@frostney.dev > key.gpg
   ```

2. **Stand up the host.** Any static file host serving
   `gocciascript.dev/repo/*` works — S3 + CloudFront, Vercel, GitHub
   Pages on a sibling repo, plain Nginx. Until this exists, the
   install commands on `/install` reach a 404.

## Per-release publishing (every tag push)

```sh
# 3. Build .debs from the released linux archives.
./packaging/apt/build-deb.sh 0.6.1 amd64
./packaging/apt/build-deb.sh 0.6.1 arm64

# 4. Stage into the pool.
mkdir -p repo/pool/main/g/gocciascript
mv gocciascript_0.6.1_*.deb repo/pool/main/g/gocciascript/

# 5. Regenerate the index per arch (requires apt-utils).
cd repo
for arch in amd64 arm64; do
  apt-ftparchive packages pool/main \
    > dists/stable/main/binary-$arch/Packages
  gzip -kf dists/stable/main/binary-$arch/Packages
done

# 6. Generate Release + sign it (clearsigned `InRelease` is what
#    modern apt clients prefer; the detached `Release.gpg` is kept
#    around for older clients).
apt-ftparchive release dists/stable > dists/stable/Release
gpg --default-key gocciascript@frostney.dev \
  -abs -o dists/stable/Release.gpg dists/stable/Release
gpg --default-key gocciascript@frostney.dev \
  --clearsign -o dists/stable/InRelease dists/stable/Release

# 7. Upload the repo/ tree to the host.
#    Whichever transport the host accepts: rsync, aws s3 sync, scp, etc.
```

Steps 3–7 are intended to live in a GitHub Actions workflow triggered
on tag push. The signing key gets imported via
`echo "$APT_SIGNING_KEY" | gpg --import`; the host upload uses
whichever credentials the host needs (S3 keys, SSH key, deploy hook).
The same workflow job can also mirror the Homebrew formula into the
tap repo (see `packaging/homebrew/README.md`) — both publishing
surfaces fire off the same release artifact.

Until the host is live, the install commands referenced from
`/install` point at endpoints that 404 — they're the **intended**
layout, documented here so the publishing pipeline has a target to
aim at.

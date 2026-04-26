# Homebrew tap for GocciaScript

`brew tap` requires the destination repo to be named `homebrew-<name>` —
this is a hard rule from Homebrew, not a convention. So the canonical
install command

```sh
brew install frostney/tap/gocciascript
```

requires a separate repo at **`github.com/frostney/homebrew-tap`**, with
the formula at `Formula/gocciascript.rb`.

We **don't** maintain that file directly in the tap repo. The
source-of-truth is `packaging/homebrew/gocciascript.rb` in this repo;
release CI mirrors it into `frostney/homebrew-tap` on every tag push.
Means there's exactly one place to edit and the tap repo is fully
generated.

## Per-release sync

The release workflow does, after the binaries are uploaded:

1. Bump `version` in `packaging/homebrew/gocciascript.rb` to the new tag.
2. Compute SHA-256 for each of the four release archives and substitute
   them in:

   ```sh
   for arch in arm64 x64; do
     for os in macos linux; do
       ext=$([ "$os" = linux ] && echo tar.gz || echo zip)
       url="https://github.com/frostney/GocciaScript/releases/download/v${VERSION}/gocciascript-${VERSION}-${os}-${arch}.${ext}"
       sha=$(curl -fsSL "$url" | shasum -a 256 | cut -d' ' -f1)
       echo "$os $arch $sha"
     done
   done
   ```

3. Commit the updated formula into `frostney/homebrew-tap` via a
   deploy-key-authenticated push (or a fine-grained PAT scoped to that
   repo). The tap repo's `main` is the public surface — every commit on
   it is what users see.

4. Tag the tap repo with the same version so `brew install` resolves
   reproducibly if anyone pins a sha.

## Alternatives we considered

- **Two-argument tap** (`brew tap user/name <url>`) lets the source repo
  have any name, but the install command becomes
  `brew tap frostney/goccia https://github.com/frostney/GocciaScript`
  followed by `brew install frostney/goccia/gocciascript` — two steps
  users won't remember.
- **Direct-URL install** (`brew install <https url to .rb>`) works
  once but has no `brew upgrade`, no auto-update, and is
  [explicitly discouraged](https://docs.brew.sh/Installation) by the
  Homebrew docs.
- **Submitting to homebrew-core** is the eventual goal. Until
  GocciaScript hits the notability threshold (~10k stars or
  documented wide usage), homebrew-core would reject the PR.

The auto-synced tap repo is the path that works today and stays
the path even after a hypothetical homebrew-core merge — the tap
just becomes redundant rather than wrong.

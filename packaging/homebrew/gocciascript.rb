# Homebrew formula for GocciaScript.
#
# Lives in the tap repo at:
#   github.com/frostney/homebrew-tap/Formula/gocciascript.rb
#
# Users install with:
#   brew install frostney/tap/gocciascript
#
# **Status: NOT YET SHIPPABLE.** The four `sha256` values below are
# placeholders — the release workflow that's supposed to compute them
# from the actual archives and mirror this file into the tap repo is
# not wired up yet (see `packaging/homebrew/README.md` for the planned
# flow). The formula compiles as a Ruby file, but `brew install` will
# refuse to use it until the hashes match real artifacts.
#
# To bump for a new release (manually, until CI handles it):
#   1. Update `version` to the released tag (without the leading `v`).
#   2. Recompute each `sha256` from the published archive:
#        shasum -a 256 gocciascript-<version>-<os>-<arch>.{zip,tar.gz}
#      and replace the `REPLACE_WITH_*` placeholders below.
#   3. Commit the change to the tap repo.
#
# Releases ship a single archive per OS/arch — once extracted, Homebrew's
# default cellar layout puts us inside the archive's top-level directory
# (`gocciascript-<version>-<os>-<arch>/`), and the binaries live directly
# alongside `tests/` / `benchmarks/` / `examples/`. See
# `.github/scripts/stage-build-artifacts.sh` for the staging that produces
# the layout — no `build/` subdirectory.

class Gocciascript < Formula
  desc "Sandbox-first ECMAScript runtime — drop of JavaScript, no Node.js"
  homepage "https://gocciascript.dev"
  version "0.6.1"
  license "MIT"

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/frostney/GocciaScript/releases/download/#{version}/gocciascript-#{version}-macos-arm64.zip"
      sha256 "REPLACE_WITH_MACOS_ARM64_SHA256"
    else
      url "https://github.com/frostney/GocciaScript/releases/download/#{version}/gocciascript-#{version}-macos-x64.zip"
      sha256 "REPLACE_WITH_MACOS_X64_SHA256"
    end
  end

  on_linux do
    if Hardware::CPU.arm?
      url "https://github.com/frostney/GocciaScript/releases/download/#{version}/gocciascript-#{version}-linux-arm64.tar.gz"
      sha256 "REPLACE_WITH_LINUX_ARM64_SHA256"
    else
      url "https://github.com/frostney/GocciaScript/releases/download/#{version}/gocciascript-#{version}-linux-x64.tar.gz"
      sha256 "REPLACE_WITH_LINUX_X64_SHA256"
    end
  end

  def install
    bin.install "GocciaScriptLoader"
    bin.install "GocciaTestRunner"
    bin.install "GocciaREPL"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/GocciaScriptLoader --version")
  end
end

# Homebrew formula for GocciaScript.
#
# Lives in the tap repo at:
#   github.com/frostney/homebrew-tap/Formula/gocciascript.rb
#
# Users install with:
#   brew install frostney/tap/gocciascript
#
# To bump for a new release:
#   1. Update `version` to the released tag (without the leading `v`).
#   2. Recompute each `sha256` from the published archive:
#        shasum -a 256 gocciascript-<version>-<os>-<arch>.{zip,tar.gz}
#   3. Commit the change to the tap repo.
#
# Releases ship a single archive per OS/arch with the toolchain laid
# out under `build/` — `bin.install` walks into that directory.

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
    bin.install "build/GocciaScriptLoader"
    bin.install "build/GocciaTestRunner"
    bin.install "build/GocciaREPL"
  end

  test do
    assert_match version.to_s, shell_output("#{bin}/GocciaScriptLoader --version")
  end
end

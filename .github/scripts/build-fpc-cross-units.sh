#!/usr/bin/env bash
#
# Builds the FPC RTL and the package units GocciaScript needs for one target,
# using a compiler already installed in the cached cross toolchain, and
# installs them under $PREFIX/lib/fpc/$FPC_VERSION/units/<cpu>-<os>.
# toolchain.yml runs it for each target that a toolchain host builds.
#
#   build-fpc-cross-units.sh <compiler> <cpu> <os> [CROSSOPT=...] [BINUTILSPREFIX=...]
#
# <compiler> names a compiler in $PREFIX/lib/fpc/$FPC_VERSION, such as
# ppcrossx64. CROSSOPT and BINUTILSPREFIX are passed to `make rtl` and to the
# package compiles. Environment: PREFIX (the toolchain prefix), FPC_VERSION,
# and FPC_SOURCE (the unpacked FPC source tree).

set -euo pipefail

if [ "$#" -lt 3 ]; then
  echo "Usage: $0 <compiler> <cpu> <os> [CROSSOPT=...] [BINUTILSPREFIX=...]" >&2
  exit 1
fi

compiler="$1"
cpu="$2"
os="$3"
shift 3
make_vars=("$@")
target="${cpu}-${os}"

: "${PREFIX:?PREFIX must name the toolchain prefix}"
: "${FPC_VERSION:?FPC_VERSION must be set}"
: "${FPC_SOURCE:?FPC_SOURCE must name the FPC source tree}"

echo ""
echo "========================================"
echo "Building RTL + packages for ${target}"
echo "========================================"

cd "$FPC_SOURCE"

cross_fpc="$PREFIX/lib/fpc/${FPC_VERSION}/${compiler}"
if [ ! -x "$cross_fpc" ]; then
  echo "::error::Compiler ${cross_fpc} not found"
  exit 1
fi

# FPC's Makefiles add -P$(CPU_TARGET) to FPCOPT, but -P is only understood by
# the fpc driver, not by bare compilers such as ppcrossx64. `make rtl` calls
# this wrapper, which drops -P flags.
wrapper="$PREFIX/bin/${compiler}-wrapper"
mkdir -p "$PREFIX/bin"
cat > "$wrapper" << WRAP
#!/bin/bash
args=()
for arg in "\$@"; do
  case "\$arg" in
    -P*) ;;
    *)   args+=("\$arg") ;;
  esac
done
exec "${cross_fpc}" "\${args[@]}"
WRAP
chmod +x "$wrapper"

# Translate the make variables into flags for the package compiles below.
# CROSSOPT holds a single compiler option. The ${array[@]+...} expansions keep
# empty arrays working under set -u in macOS's bash 3.2.
pkg_extra_flags=()
for var in ${make_vars[@]+"${make_vars[@]}"}; do
  case "$var" in
    BINUTILSPREFIX=*) pkg_extra_flags+=("-XP${var#BINUTILSPREFIX=}") ;;
    CROSSOPT=*)       pkg_extra_flags+=("${var#CROSSOPT=}") ;;
    *)
      echo "::error::Unsupported make variable: ${var}"
      exit 1
      ;;
  esac
done

make rtl PP="$wrapper" CPU_TARGET="$cpu" OS_TARGET="$os" \
  OPT="-dFPC_SOFT_FPUX80" ${make_vars[@]+"${make_vars[@]}"}

# Install RTL units
rtl_dir="$PREFIX/lib/fpc/${FPC_VERSION}/units/${target}/rtl"
mkdir -p "$rtl_dir"
cp "rtl/units/${target}"/* "$rtl_dir/"

# Determine varutils source (Windows uses win/, others use common/)
case "$os" in
  win32|win64)
    varutils_src="packages/rtl-objpas/src/win/varutils.pp"
    varutils_fi=(-Fipackages/rtl-objpas/src/inc -Fipackages/rtl-objpas/src/win)
    process_fi=(-Fipackages/fcl-process/src/win)
    ;;
  *)
    varutils_src="packages/rtl-objpas/src/common/varutils.pp"
    varutils_fi=(-Fipackages/rtl-objpas/src/inc)
    process_fi=(-Fipackages/fcl-process/src/unix)
    ;;
esac

# Build rtl-objpas (varutils, variants, strutils, dateutils)
echo "--- rtl-objpas for ${target} ---"
objpas_dir="$PREFIX/lib/fpc/${FPC_VERSION}/units/${target}/rtl-objpas"
mkdir -p "$objpas_dir"

"$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
  -Fu"$rtl_dir" -Fu"$objpas_dir" -FU"$objpas_dir" \
  "${varutils_fi[@]}" "$varutils_src"

for unit in variants strutils dateutils; do
  "$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
    -Fu"$rtl_dir" -Fu"$objpas_dir" -FU"$objpas_dir" \
    "packages/rtl-objpas/src/inc/${unit}.pp"
done

# Build rtl-generics (needs variants from rtl-objpas)
echo "--- rtl-generics for ${target} ---"
gen_dir="$PREFIX/lib/fpc/${FPC_VERSION}/units/${target}/rtl-generics"
mkdir -p "$gen_dir"
for unit in generics.hashes generics.strings generics.defaults \
            generics.helpers generics.memoryexpanders generics.collections; do
  src="packages/rtl-generics/src/${unit}.pas"
  if [ -f "$src" ]; then
    "$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
      -Fu"$rtl_dir" -Fu"$objpas_dir" -Fu"$gen_dir" -FU"$gen_dir" "$src"
  fi
done

# Build fcl-process (provides Process unit)
echo "--- fcl-process for ${target} ---"
fcl_dir="$PREFIX/lib/fpc/${FPC_VERSION}/units/${target}/fcl-process"
mkdir -p "$fcl_dir"
"$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
  -Fu"$rtl_dir" -FU"$fcl_dir" \
  "${process_fi[@]}" "packages/fcl-process/src/pipes.pp"
"$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
  -Fu"$rtl_dir" -Fu"$fcl_dir" -FU"$fcl_dir" \
  "${process_fi[@]}" "packages/fcl-process/src/process.pp"

# Build socket units from packages/rtl-extra (not produced by cross make rtl)
# Sockets for Unix/Darwin, WinSock2 for Windows
rtl_extra="packages/rtl-extra/src"
echo "--- socket units for ${target} ---"
case "$os" in
  linux)
    if [ ! -f "$rtl_dir/sockets.ppu" ]; then
      "$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
        -Fu"$rtl_dir" -FU"$rtl_dir" \
        -Fi"${rtl_extra}/inc" -Fi"${rtl_extra}/linux" \
        "${rtl_extra}/unix/sockets.pp"
    fi
    ;;
  darwin)
    if [ ! -f "$rtl_dir/sockets.ppu" ]; then
      "$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
        -Fu"$rtl_dir" -FU"$rtl_dir" \
        -Fi"${rtl_extra}/inc" -Fi"${rtl_extra}/bsd" -Fi"${rtl_extra}/darwin" \
        "${rtl_extra}/unix/sockets.pp"
    fi
    ;;
  win32|win64)
    if [ ! -f "$rtl_dir/winsock2.ppu" ]; then
      "$cross_fpc" -T"${os}" -O2 -dFPC_SOFT_FPUX80 ${pkg_extra_flags[@]+"${pkg_extra_flags[@]}"} \
        -Fu"$rtl_dir" -FU"$rtl_dir" \
        "${rtl_extra}/win/winsock2.pp"
    fi
    ;;
esac

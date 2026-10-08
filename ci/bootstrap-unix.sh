#!/usr/bin/env bash
set -euo pipefail

tpx_lazarus="$RUNNER_TEMP/lazarus"
tpx_lazarus_commit=62c14a4d18c81f222127d42ce9b89b922c63fcbf
tpx_fpc_commit=0eeb544f39332bfe6e468c8cae18bbf6270c7e1e
tpx_compiler="$(command -v fpc)"

if [[ "$RUNNER_OS" == macOS ]]; then
  tpx_fpc_source="$RUNNER_TEMP/fpc-source"
  tpx_fpc_prefix="$RUNNER_TEMP/tpx-fpc"
  git init "$tpx_fpc_source"
  git -C "$tpx_fpc_source" remote add origin https://gitlab.com/freepascal.org/fpc/source.git
  git -C "$tpx_fpc_source" fetch --depth 1 origin "$tpx_fpc_commit"
  git -C "$tpx_fpc_source" checkout --detach FETCH_HEAD
  tpx_sdk="$(xcrun --sdk macosx --show-sdk-path)"
  make -C "$tpx_fpc_source" -j3 all FPC="$tpx_compiler" \
    OPT="-XR$tpx_sdk" FPMAKE_BUILD_OPT="-XR$tpx_sdk"
  make -C "$tpx_fpc_source" install INSTALL_PREFIX="$tpx_fpc_prefix"
  mkdir -p "$tpx_fpc_prefix/etc"
  "$tpx_fpc_prefix/bin/fpcmkcfg" \
    -d "basepath=$tpx_fpc_prefix/lib/fpc/3.2.3" \
    -d "sharepath=$tpx_fpc_prefix/share/fpc/3.2.3" \
    -o "$tpx_fpc_prefix/etc/fpc.cfg"
  ln -s ../lib/fpc/3.2.3/ppca64 "$tpx_fpc_prefix/bin/ppca64"
  export PATH="$tpx_fpc_prefix/bin:$PATH"
  export PPC_CONFIG_PATH="$tpx_fpc_prefix/etc"
  tpx_compiler="$tpx_fpc_prefix/bin/fpc"
  echo "PPC_CONFIG_PATH=$PPC_CONFIG_PATH" >> "$GITHUB_ENV"
  echo "TPX_COMPILER=$tpx_fpc_prefix/lib/fpc/3.2.3/ppca64" >> "$GITHUB_ENV"
  echo "FPC_SOURCE=$tpx_fpc_commit" >> "$GITHUB_ENV"
  echo "$tpx_fpc_prefix/bin" >> "$GITHUB_PATH"
fi

git init "$tpx_lazarus"
git -C "$tpx_lazarus" remote add origin https://gitlab.com/freepascal.org/lazarus/lazarus.git
git -C "$tpx_lazarus" fetch --depth 1 origin "$tpx_lazarus_commit"
git -C "$tpx_lazarus" checkout --detach FETCH_HEAD
git -C "$tpx_lazarus" apply "$GITHUB_WORKSPACE/ci/patches/lazarus-4.8-opendocument.patch"
git -C "$tpx_lazarus" apply "$GITHUB_WORKSPACE/ci/patches/lazarus-4.8-cocoa-font-cancel.patch"
git -C "$tpx_lazarus" apply "$GITHUB_WORKSPACE/ci/patches/lazarus-4.8-cocoa-text-shortcuts.patch"
make -C "$tpx_lazarus" lazbuild LCL_PLATFORM="$WIDGETSET" FPC="$tpx_compiler"
echo "LAZARUS_DIR=$tpx_lazarus" >> "$GITHUB_ENV"
echo "LAZBUILD=$tpx_lazarus/lazbuild" >> "$GITHUB_ENV"
echo "FPC=$tpx_compiler" >> "$GITHUB_ENV"
echo "LAZARUS_SOURCE=$tpx_lazarus_commit" >> "$GITHUB_ENV"

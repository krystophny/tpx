#!/usr/bin/env bash
set -euo pipefail

tpx_sam2p_commit=9b0f8b77b5fb0af42e8faf8fb63a0760e214757b
tpx_sam2p_source="$RUNNER_TEMP/sam2p"
tpx_sam2p_bin="$RUNNER_TEMP/sam2p-bin"
git init "$tpx_sam2p_source"
git -C "$tpx_sam2p_source" remote add origin https://github.com/pts/sam2p.git
git -C "$tpx_sam2p_source" fetch --depth 1 origin "$tpx_sam2p_commit"
git -C "$tpx_sam2p_source" checkout --detach FETCH_HEAD
(cd "$tpx_sam2p_source" && sh compile.sh)
mkdir -p "$tpx_sam2p_bin"
install -m 755 "$tpx_sam2p_source/sam2p" "$tpx_sam2p_bin/sam2p"
"$tpx_sam2p_bin/sam2p" --version
echo "$tpx_sam2p_bin" >> "$GITHUB_PATH"
echo "SAM2P_SOURCE=$tpx_sam2p_commit" >> "$GITHUB_ENV"

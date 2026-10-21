#!/usr/bin/env bash
set -xeuo pipefail

cargo build --release

PREFIX="${PREFIX:-/usr/local}"
mkdir -p "$PREFIX" 2>/dev/null || true

SUDO=""
if [ ! -w "$PREFIX" ] && command -v sudo >/dev/null 2>&1; then
    SUDO="sudo"
fi

$SUDO install -d "$PREFIX/bin" "$PREFIX/zern/std"
$SUDO install -m 755 target/release/zern "$PREFIX/zern/zern"
$SUDO cp -r std/. "$PREFIX/zern/std/"
$SUDO install -m 644 LICENSE "$PREFIX/zern/LICENSE"
$SUDO ln -sf ../zern/zern "$PREFIX/bin/zern"

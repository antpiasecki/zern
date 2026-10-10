#!/usr/bin/env bash
set -xeou pipefail

PREFIX="${PREFIX:-/usr/local}"

cargo build --release

install -d "$PREFIX/bin" "$PREFIX/zern/std"
install -m 755 target/release/zern "$PREFIX/zern/zern"
cp -r std/. "$PREFIX/zern/std/"
install -m 644 LICENSE "$PREFIX/zern/LICENSE"
ln -sf ../zern/zern "$PREFIX/bin/zern"

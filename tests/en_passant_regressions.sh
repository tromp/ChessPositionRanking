#!/bin/sh
set -eu

ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
BIN=${TMPDIR:-/tmp}/chess-position-ranking-en-passant-$$
trap 'rm -f "$BIN"' EXIT HUP INT TERM

ghc -O -isrc --make "$ROOT/tests/EnPassantRegressions.hs" -o "$BIN"
"$BIN"

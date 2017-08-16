#!/usr/bin/env bash

set -eu

if [[ $# -ne 2 ]]; then
    echo "Usage: $0 <iterations> <path-to-scheme-benchmark>"
    echo
    echo "Run the given scheme benchmark (for example, nboyer.scm) the given"
    echo "number times, and print the resulting times (in milliseconds) as a"
    echo "histogram."
    exit 1
fi

export RUST_BACKTRACE=1

REPO_DIR="$(dirname "$0")/../.."

pushd "$REPO_DIR/lisp" > /dev/null
    cargo build --release
popd > /dev/null

if test ! $(which histo); then
    cargo install histo
fi

for ((i=0; i<"$1"; i += 1)); do
    # Pull out the "real" seconds from time's output. Note that this probably
    # relies on OSX's `time` formatting, and will probably not work on
    # Linux. Patches welcome :)
    sample=$($(which time) "$REPO_DIR/target/release/lisp" "$2" 2>&1 1>/dev/null | cut -f 1 -d "r")

    # Transform the sample from seconds with decimals into whole milliseconds.
    sample=$(echo "$sample * 1000" | bc | cut -f 1 -d '.')

    # Echo the sample to both stderr, so we have something to look at while
    # benches are being run, and know that they aren't stuck, and to stdout for
    # `histo` to take as input.
    echo "Iteration $i: $sample milliseconds" 1>&2
    echo "$sample"
done \
    | histo

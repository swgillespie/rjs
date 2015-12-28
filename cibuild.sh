#!/bin/bash

rm -rf target
mkdir -p target

for crate in rjs
do
    CARGO_TARGET_DIR=./target cargo build --manifest-path=./src/${crate}/Cargo.toml || exit 1
done

for crate in librjs_syntax librjs_runtime librjs
do
    CARGO_TARGET_DIR=./target cargo test --manifest-path=./src/${crate}/Cargo.toml || exit 1
done

source run_tests.sh

echo "Build successful!"

#!/bin/bash

rm -rf target
mkdir -p target

for crate in rjs
do
    CARGO_TARGET_DIR=./target cargo build --manifest-path=./src/${crate}/Cargo.toml || exit 1
done

for crate in librjs librjs_syntax
do
    CARGO_TARGET_DIR=./target cargo test --manifest-path=./src/${crate}/Cargo.toml || exit 1
done


echo "Build successful!"

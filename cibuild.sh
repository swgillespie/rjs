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

export JS_TEST_COMMAND=./target/debug/rjs
cd test
bundle install
cd ..
ruby test/run_tests.rb || exit 1


echo "Build successful!"

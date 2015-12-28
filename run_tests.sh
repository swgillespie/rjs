export JS_TEST_COMMAND=./target/debug/rjs
cd test
bundle install
cd ..
export RUST_BACKTRACE=1
ruby test/run_tests.rb --no-use-color || exit 1

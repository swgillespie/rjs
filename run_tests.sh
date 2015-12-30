export JS_TEST_COMMAND=./target/debug/rjs
cd test/rjs_tests
bundle install
cd ../..
export RUST_BACKTRACE=1
export TEST262_DIR=test/test262_tests/test262

ruby test/rjs_tests/run_tests.rb || exit 1

if [ -z "$RUN_TEST262" ]; then
  echo "not running test262, RUN_TEST262 not set"
else
  sh test/test262_tests/run_test262.sh || exit 1
fi

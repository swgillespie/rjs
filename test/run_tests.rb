require 'test/unit'

TEST_FOLDERS = ['ecmascript_src']
TEST_COMMAND = ENV['JS_TEST_COMMAND']
TEST_DIR = File.dirname(File.expand_path $0)

puts "working dir #{TEST_DIR}"

raise "JS_TEST_COMMAND variable is required" if TEST_COMMAND.nil?

puts "Using test command #{TEST_COMMAND}"

ESTest = Struct.new(:name, :filename, :output, :ignore_reason)

def create_test_cases
  tests = []
  TEST_FOLDERS.each do |folder|
    Dir.glob(File.join TEST_DIR, folder, '*.js').each do |file|
      tests << create_test_case(file)
    end
  end

  return tests
end

def create_test_case(filename)
  # read the file and look for the output comment.
  contents = File.read(filename)
  name = File.basename(filename, '.js')
  match = %r{.*/\*===\n(?<output>[^=]*)===\*/.*}.match(contents)
  ignore_reason = nil
  if match
    output = match["output"]
    ignore_match = %r{<ignore\(\"(?<reason>[^\"]*)\"\)>}.match(output)
    if ignore_match
      ignore_reason = ignore_match["reason"]
    end
  else
    output = ""
  end
  return ESTest.new(name, filename, output, ignore_reason)
end

Class.new Test::Unit::TestCase do
  create_test_cases().each do |test|
    define_method "test_" + test.name do
      omit("skipping the test because: #{test.ignore_reason}") unless test.ignore_reason.nil?
      stdout = `#{TEST_COMMAND} #{test.filename} 2>&1`
      assert_equal test.output, stdout
    end
  end
end

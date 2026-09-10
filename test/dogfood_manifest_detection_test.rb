# SPDX-License-Identifier: MPL-2.0
require 'yaml'
require 'tmpdir'
require 'fileutils'
require 'open3'

workflow = YAML.load_file('.github/workflows/dogfood-gate.yml')
detect = workflow.fetch('jobs').fetch('a2ml-validate').fetch('steps').find { |step| step['id'] == 'detect' }.fetch('run')
[
  [[], 0],
  [['manifest.deed'], 1],
  [['manifest.a2ml'], 1],
  [["line\nbreak.deed", 'legacy.a2ml'], 2],
  [['.git/ignored.deed', 'audits/assail-classifications.a2ml'], 0]
].each do |files, expected|
  Dir.mktmpdir('dogfood-detect-') do |dir|
    files.each do |file|
      path = File.join(dir, file)
      FileUtils.mkdir_p(File.dirname(path))
      File.write(path, '')
    end
    output = File.join(dir, 'outputs')
    log, status = Open3.capture2e({ 'GITHUB_OUTPUT' => output }, 'bash', '-e', '-o', 'pipefail', '-c', detect, chdir: dir)
    abort log unless status.success?
    abort "wrong candidate count for #{files.inspect}" unless File.read(output) == "count=#{expected}\n"
  end
end
puts 'PASS: empty, DEED-only, A2ML-only, mixed, newline filenames, and excluded manifests'

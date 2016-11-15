
require 'rspec/core/rake_task'
require 'rubocop/rake_task'

task :default => :start

RSpec::Core::RakeTask.new(:test)

RuboCop::RakeTask.new do |task|
  task.requires << 'rubocop-rspec'
end

task :start do
  appname = File.join(Dir.pwd, '/lib/webapp/main.rb')
  exec("ruby #{appname}")
end

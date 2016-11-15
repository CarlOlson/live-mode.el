
require 'rspec/core/rake_task'
require 'rubocop/rake_task'

task default: :'sinatra:start'

RSpec::Core::RakeTask.new(:test)

RuboCop::RakeTask.new do |task|
  task.requires << 'rubocop-rspec'
end

namespace :sinatra do
  desc 'Starts Sinatra webapp'
  task :start do
    appname = File.join(Dir.pwd, '/lib/webapp/main.rb')
    exec("ruby #{appname}")
  end

  desc 'Starts Sinatra webapp with auto-reload via rerun gem'
  task :rerun do
    exec("bundler exec rerun --pattern '{lib/**/*.rb}' rake")
  end
end

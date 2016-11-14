require './lib/state_channel'

module Helpers
  extend self

  # Languages included with highlight.js
  COMMON_LANGUAGES = [
    "apache",
    "bash",
    "cs",  # C#
    "cpp", # C++
    "css",
    "coffeescript",
    "diff",
    "html",
    "xml",
    "http",
    "ini",
    "json",
    "java",
    "javascript",
    "makefile",
    "markdown",
    "nginx",
    "objectivec",
    "php",
    "perl",
    "python",
    "ruby",
    "sql"
  ]

  def common_mode? mode
    mode.nil? or
      mode.empty? or
      COMMON_LANGUAGES.include? mode
  end

  def html_safe? str
    str !~ /[&<>"'`=\/]/
  end

  def new_channel
    StateChannel.new({ event: 'set', text: '' })
  end

  def path_to_buffer path
    # TODO escape characters
    if path =~ /^\//
      path[1..-1]
    else
      path
    end
  end
end

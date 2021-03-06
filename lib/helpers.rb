require './lib/buffer'
require './lib/state_channel'

module Helpers
  module_function

  # Languages included with highlight.js
  COMMON_LANGUAGES = [
    'apache',
    'bash',
    'cs',  # C#
    'cpp', # C++
    'css',
    'coffeescript',
    'diff',
    'html',
    'xml',
    'http',
    'ini',
    'json',
    'java',
    'javascript',
    'makefile',
    'markdown',
    'nginx',
    'objectivec',
    'php',
    'perl',
    'python',
    'ruby',
    'sql'
  ].freeze

  def common_mode?(mode)
    mode.nil? ||
      mode.empty? ||
      COMMON_LANGUAGES.include?(mode)
  end

  def html_safe?(str)
    str !~ %r{[&<>"'`=/]}
  end

  def new_channel
    StateChannel.new(Buffer.new) do |buffer, update|
      if update['event'] == 'set'
        Buffer.new update['text']
      else
        buffer.update(update['start']  || 0,
                      update['length'] || 0,
                      update['text']   || '')
      end
    end
  end

  def path_to_buffer(path)
    # TODO: escape characters
    if path =~ %r{^/}
      path[1..-1]
    else
      path
    end
  end
end

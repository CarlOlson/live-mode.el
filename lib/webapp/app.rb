require 'thin'
require 'sinatra/base'
require 'json'

require './lib/helpers'

class App < Sinatra::Base
  helpers Helpers

  def initialize(app = nil)
    super(app)
    @channels      = settings.channels || {}
    @channel_modes = {}
  end

  def erb_locals
    mode = defined?(@buffer) ? @channel_modes[@buffer] : ''
    { uncommon_mode: common_mode?(mode),
      mode: mode,
      buffer_list: @channels.keys }
  end

  configure do
    # NOTE: takes requests on reactor thread
    set :threaded, false
  end

  get '/' do
    # TODO: auto refresh
    erb :index, locals: erb_locals
  end

  before '/:buffer' do
    @buffer = path_to_buffer(params['buffer'])
  end

  get '/:buffer' do
    erb :buffer, locals: erb_locals
  end

  post '/:buffer' do
    halt 'External IPs not allowed' unless request.ip =~ /^127.0.0.1$/

    request.body.rewind
    event = JSON.parse(request.body.read)

    (@channels[@buffer] ||= new_channel)
      .update(event)

    @channel_modes[@buffer] = event['mode']

    status 200
  end
end

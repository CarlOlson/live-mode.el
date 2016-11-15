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

  configure do
    # NOTE: takes requests on reactor thread
    set :threaded, false
  end

  get '/' do
    # TODO: auto refresh
    erb :index, locals: { buffer_list: @channels.keys }
  end

  before '/:buffer' do
    @buffer = path_to_buffer(params['buffer'])
  end

  get '/:buffer' do
    mode = @channel_modes[@buffer]

    if common_mode? mode
      erb :buffer, locals: { uncommon_mode: false }
    else
      erb :buffer, locals: { uncommon_mode: true,
                             mode: mode }
    end
  end

  post '/:buffer' do
    unless request.ip =~ /^127.0.0.1$/
      halt 'External IPs not allowed'
    end

    request.body.rewind
    event = JSON.parse(request.body.read)

    (@channels[@buffer] ||= new_channel)
      .update(event)

    @channel_modes[@buffer] = event['mode']

    status 200
  end
end

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

  get '/' do
    erb :index, locals: { buffer_list: @channels.keys }
  end

  before '/:buffer' do
    @buffer = path_to_buffer(params['buffer'])
  end

  get '/:buffer' do
    mode = @channel_modes[@buffer]

    if Helpers.common_mode? mode
      erb :buffer, locals: { uncommon_mode: false }
    else
      erb :buffer, locals: { uncommon_mode: true,
                             mode: mode }
    end
  end

  post '/:buffer' do
    request.body.rewind
    event  = JSON.parse(request.body.read)

    (@channels[@buffer] ||= new_channel)
      .update(event)

    @channel_modes[@buffer] = event['mode']

    status 200
  end
end

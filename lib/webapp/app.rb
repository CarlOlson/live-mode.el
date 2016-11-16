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
    { common_mode: common_mode?(mode),
      mode: mode,
      buffer_list: @channels.keys,
      style: @style }
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

    @style = request.cookies['style'] || 'default'
    response.set_cookie 'style', value: @style
  end

  get '/:buffer' do
    redirect to('/') unless @channels.key? @buffer
    erb :buffer, locals: erb_locals
  end

  post '/:buffer' do
    halt 'External IPs not allowed' unless request.ip =~ /^127.0.0.1$/

    request.body.rewind
    events = JSON.parse(request.body.read)
    events = events.is_a?(Array) ? events : [events]

    events.each do |event|
      (@channels[@buffer] ||= new_channel)
        .update(event)

      @channel_modes[@buffer] = event['mode'] if event['mode']

      puts event if event['event'] == 'update'
    end

    status 200
  end
end

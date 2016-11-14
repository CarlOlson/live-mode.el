require 'thread'
require 'thin'
require 'em-websocket'
require 'sinatra/base'
require 'json'
require 'uri'

require './lib/helpers'

EM.run do

  # TODO check if threadsafe in EM
  CHANNELS      = Hash.new
  CHANNEL_MODES = Hash.new

  class App < Sinatra::Base
    helpers Helpers

    get '/' do
      erb :index, locals: { buffer_list: CHANNELS.keys }
    end

    get '/:buffer' do
      buffer = path_to_buffer(request.path)
      mode   = CHANNEL_MODES[buffer]

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
      buffer = path_to_buffer(request.path)

      (CHANNELS[buffer] ||= new_channel).
        update(event)

      CHANNEL_MODES[buffer] = event['mode']

      status 200
    end
  end

  EM::WebSocket.start(:host => '0.0.0.0', :port => '3001') do |ws|
    ws.onopen do |handshake|
      buffer  = Helpers.path_to_buffer(handshake.path)
      channel =
        (CHANNELS[buffer] ||= Helpers.new_channel)

      sid = channel.subscribe do |cmd|
        ws.send(cmd.to_json)
      end

      ws.onclose do
        channel.unsubscribe(sid)
      end
    end
  end

  App.run! :port => 3000
end

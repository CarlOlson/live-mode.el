require 'thread'
require 'thin'
require 'em-websocket'
require 'sinatra/base'
require 'json'
require 'uri'

require_relative '../state_channel'

EM.run do

  def new_channel
    StateChannel.new({ event: 'set', text: '' })
  end

  # TODO check if threadsafe in EM
  Channels = Hash.new

  class App < Sinatra::Base
    get '/' do
      erb :index, locals: { buffer_list: Channels.keys }
    end

    get '/:buffer' do
      erb :buffer
    end

    post '/:buffer' do
      request.body.rewind
      event = JSON.parse(request.body.read)
      (Channels[request.path] ||= new_channel).
        update(event)
      status 200
    end
  end

  EM::WebSocket.start(:host => '0.0.0.0', :port => '3001') do |ws|
    ws.onopen do |handshake|
      channel =
        (Channels[handshake.path] ||= new_channel)

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

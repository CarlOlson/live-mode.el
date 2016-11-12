
require 'thread'
require 'thin'
require 'em-websocket'
require 'sinatra/base'
require 'json'

require_relative '../state_channel'

EM.run do
  Channel = StateChannel.new({ event: 'set', text: '' })

  class App < Sinatra::Base
    get '/' do
      erb :index
    end

    post '/' do
      request.body.rewind
      event = JSON.parse(request.body.read)
      Channel.update(event)
      status 200
    end
  end

  EM::WebSocket.start(:host => '0.0.0.0', :port => '3001') do |ws|
    ws.onopen do
      sid = Channel.subscribe do |cmd|
        puts cmd.to_json
        ws.send(cmd.to_json)
      end

      ws.onclose do
        Channel.unsubscribe(sid)
      end
    end
  end

  App.run! :port => 3000
end

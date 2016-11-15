require 'eventmachine'
require 'em-websocket'

require './lib/helpers'
require './lib/webapp/app'

EM.run do
  channels = {}

  EM::WebSocket.start(host: '0.0.0.0', port: '3001') do |ws|
    ws.onopen do |handshake|
      buffer  = Helpers.path_to_buffer(handshake.path)
      channel =
        (channels[buffer] ||= Helpers.new_channel)

      sid = channel.subscribe do |cmd|
        ws.send(cmd.to_json)
      end

      ws.onclose do
        channel.unsubscribe(sid)
      end
    end
  end

  # TODO: move to configuration file
  App.run!(host: '0.0.0.0', port: 3000, channels: channels)
end
require 'eventmachine'
require 'em-websocket'

require './lib/helpers'
require './lib/webapp/app'

EM.run do
  channels = {}

  EM::WebSocket.start(host: '0.0.0.0', port: '3001') do |ws|
    ws.onopen do |handshake|
      buffer  = Helpers.path_to_buffer(handshake.path)
      channel = channels[buffer]

      if channel.nil?
        ws.close
      else
        sid = channel.subscribe do |cmd|
          ws.send(cmd.to_json)
        end

        ws.onclose do
          channel.unsubscribe(sid)
        end
      end
    end
  end

  # TODO: move to configuration file
  App.run!(host: '0.0.0.0',
           port: 3000,
           channels: channels)

  # NOTE: is last to everride other traps
  trap(:INT)  { EventMachine.stop }
  trap(:TERM) { EventMachine.stop }
end


require 'socket'
require 'state_server'

TIMEOUT = 0.25

def select_and_gets socket
  if IO.select([socket], nil, nil, TIMEOUT)
    socket.gets
  else
    fail
  end
end

describe StateServer do
  before do
    @port = 5065
    @host = '127.0.0.1'
    @server = StateServer.new @host, @port

    # NOTE must sleep to allow listener thread to start
    sleep 0.1
  end
  
  after do
    @server.close if @server
  end
  
  it "should send state on connection" do
    @server.state = "test"
    @socket = TCPSocket.new @host, @port
    expect(select_and_gets(@socket)).to eq %Q{(state "test")\n}
    @socket.close
  end

  it "should send updates to connections" do
    @server.state = "1234567890"
    @socket = TCPSocket.new @host, @port
    select_and_gets(@socket) # STATE 1234567890
    cmd = [6, 5, "!"]
    @server.update *cmd
    expect(select_and_gets(@socket)).to start_with [:update, *cmd].to_lisp
    @socket.close
  end

  it "should escape newlines" do
    @server.state = "line1\nline2"
    @socket = TCPSocket.new @host, @port
    expect(select_and_gets(@socket)).to eq %Q{(state "line1\\nline2")\n}
    @socket.close
  end

  it "should apply updates to state" do
    @server.state = "1234567890"
    @server.update 6, 5, "!"
    @server.force

    @socket = TCPSocket.new @host, @port
    expect(select_and_gets(@socket)).to start_with %Q{(state "12345!")}
    @socket.close
  end
end

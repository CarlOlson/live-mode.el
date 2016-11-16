
require 'socket'
require 'state_server'

TIMEOUT = 0.25

def select_and_gets(socket)
  unless IO.select([socket], nil, nil, TIMEOUT)
    raise StandardError, 'IO.select timeout'
  end
  socket.gets
end

describe StateServer do
  let(:port) { 5065 }
  let(:host) { '127.0.0.1' }
  let(:server) do
    # NOTE must sleep to allow listener thread to start
    described_class.new(host, port).tap { sleep 0.1 }
  end
  let(:socket) { TCPSocket.new host, port }

  after do
    server.close
    socket.close
  end

  it 'sends state on connection' do
    server.state = 'test'
    expect(select_and_gets(socket)).to start_with '(state "test")'
  end

  it 'sends updates to connections' do
    server.state = '1234567890'
    select_and_gets(socket) # STATE 1234567890
    cmd = [6, 5, '!']
    server.update(*cmd)
    expect(select_and_gets(socket)).to start_with [:update, *cmd].to_lisp
  end

  it 'escapes newlines' do
    server.state = "line1\nline2"
    expect(select_and_gets(socket)).to start_with '(state "line1\nline2")'
  end

  it 'applys updates to state' do
    server.state = '1234567890'
    server.update 6, 5, '!'
    server.force
    expect(select_and_gets(socket)).to start_with '(state "12345!")'
  end
end

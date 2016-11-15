require 'thread'
require 'socket'
require 'hamster'

require 'enumerable'
require 'buffer'

class Client
  attr_accessor :conn, :updates

  def initialize(conn)
    @conn    = conn
    @updates = 0
  end
end

class StateServer
  WAIT_TIME = 0.1

  def initialize(host, port)
    @server  = TCPServer.new host, port
    @mutex   = Mutex.new
    @clients = Hamster::Vector[]

    @state = Hamster::Hash[
      queue:  Hamster::Vector[],
      buffer: Buffer.new,
      updates: 0
    ]

    listen
    process
  end

  def state=(text)
    @mutex.synchronize do
      @state = @state
                 .put(:queue,
                      Hamster::Vector[text])
                 .put(:updates,
                      @state.get(:updates) +
                      @state.get(:queue).size +
                      # increment each update, including new state
                      1)
                 .put(:buffer,
                      Buffer.new(text))
    end
  end

  def update(start, length, insert)
    cmd = [start, length, insert]
    @mutex.synchronize do
      @state = @state
                 .put(:queue,
                      @state.get(:queue).push(cmd))
                 .put(:buffer,
                      @state.get(:buffer).update(*cmd))
    end
  end

  def force
    return if @state.get(:queue).size <= 1
    @mutex.synchronize do
      @state = @state
                 .put(:queue,
                      Hamster::Vector[@state.get(:buffer).text])
                 .put(:updates,
                      # don't increment, force isn't a real update
                      @state.get(:updates) +
                      @state.get(:queue).size)
    end
  end

  def close
    @listener.kill  if @listener
    @processor.kill if @processor
    @server.close   if @server
  end

  private

  def process
    @processor ||= Thread.new do
      loop do
        begin
          state = @state
          unless state.get(:queue).empty?
            @clients.each do |client|
              process_client client, state
            end
          end
          sleep WAIT_TIME
        rescue => e
          puts e.inspect
          exit 1
        end
      end
    end
  end

  def process_client(client, state)
    # return if closed
    return if client.conn.closed?
    return if IO.select(nil, [client.conn], nil, 0).nil?

    base_update = state.get(:updates)
    queue = state.get(:queue)
    updates = base_update + queue.size

    # send base state
    if client.updates < base_update
      client.conn.puts [:state, queue.first].to_lisp
      client.updates = base_update
    end

    # send updates
    if client.updates < updates
      queue[1..-1].each do |s|
        client.conn.puts [:update, *s].to_lisp
      end
      client.updates = updates
    end
  end

  def listen
    @listener ||= Thread.new do
      loop do
        begin
          new_client = Client.new @server.accept_nonblock
          @clients = @clients
                       .push(new_client)
                       .reject { |client| client.conn.closed? }
        rescue IO::WaitReadable
          IO.select([@server])
        rescue => e
          puts e.inspect
          exit 1
        end
      end
    end
  end
end

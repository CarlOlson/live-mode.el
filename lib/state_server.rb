
require 'thread'
require 'socket'
require 'hamster'

require_relative 'enumerable'
require_relative 'buffer'

class Client < Struct.new(:conn, :index, :iter)
  def initialize conn
    super conn, -1, 0
  end
end

class StateServer
  WAIT_TIME = 0.1
  
  def initialize host, port
    @server = TCPServer.new host, port
    @mutex  = Mutex.new
    @clients  = Hamster::Vector[]
    
    @state = Hamster::Hash[
      queue:  Hamster::Vector[],
      buffer: Buffer.new,
      iteration: 0
    ]
    
    listen
    process
  end

  def state= text
    @mutex.synchronize do
      @state = @state.
                 put(:queue,
                     Hamster::Vector[text]).
                 put(:iteration,
                     @state.get(:iteration) + 1).
                 put(:buffer,
                     Buffer.new(text))
    end
  end

  def update start, length, insert
    cmd = [start, length, insert]
    @mutex.synchronize do
      @state = @state.
                 put(:queue,
                     @state.get(:queue).push(cmd)).
                 put(:buffer,
                     @state.get(:buffer).update(*cmd))
    end
  end

  def force
    return if @state.get(:queue).size <= 1
    @mutex.synchronize do
      @state = @state.put(:queue,
                          Hamster::Vector[@state.get(:buffer).text])
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
          if not state.get(:queue).empty?
            @clients.each do |client|
              process_client client, state
            end
          end
          sleep WAIT_TIME
        rescue => e
          puts e.inspect
        end
      end
    end
  end

  def process_client client, state
    # return if closed
    return if client.conn.closed?
    return if IO.select(nil, [client.conn], nil, 0).nil?

    iter  = state.get(:iteration)
    queue = state.get(:queue)
    
    # send current state
    if client.iter < iter
      client.iter = iter
      client.conn.puts [:state, queue.first].to_lisp
      client.index = 1
    end

    # send updates
    if client.index < queue.size
      queue[1..-1].each do |s|
        client.conn.puts [:update, *s].to_lisp
      end
      client.index = queue.size
    end
  end
  
  def listen
    @listener ||= Thread.new do
      loop do
        begin
          client = Client.new @server.accept_nonblock
          @clients = @clients.
                       push(client).
                       reject { |client|
            client.conn.closed?
          }
        rescue IO::WaitReadable
          IO.select([@server])
        rescue => e
          puts e.inspect
        end
      end
    end
  end
end

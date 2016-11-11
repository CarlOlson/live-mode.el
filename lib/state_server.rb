
require 'thread'
require 'socket'
require 'hamster'

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
      queue: Hamster::Vector[],
      iteration: 0
    ]
    
    listen
    process
  end

  def state= value
    @mutex.synchronize do
      @state = @state.
                 put(:queue,
                     Hamster::Vector[value]).
                 put(:iteration,
                     @state.get(:iteration) + 1)
    end
  end

  def update cmd
    @mutex.synchronize do
      @state = @state.
                 put(:queue,
                     @state.get(:queue).push(cmd))
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
          @clients.each do |client|
            process_client client, state
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

    iter  = state.get(:iteration)
    queue = state.get(:queue)
    
    # send current state
    if client.iter < iter
      client.iter = iter
      client.conn.puts "STATE #{queue.first}"
      client.index = 1
    end

    # send updates
    if client.index < queue.size
      queue[1..-1].each do |s|
        client.conn.puts "UPDATE #{s}"
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

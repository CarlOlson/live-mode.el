
require 'thread'
require 'socket'
require 'hamster'

class Connection < Struct.new(:conn, :index, :iter)
  def initialize conn
    super conn, -1, 0
  end
end

class StateServer
  WAIT_TIME = 0.1
  
  def initialize host, port
    @server  = TCPServer.new host, port
    @mutex   = Mutex.new
    @conns   = Hamster::Vector[]
    
    @queue = Hamster::Vector[]
    @dirty = false

    @iteration = 0
    
    listen
    process
  end

  def state= value
    @mutex.synchronize do
      @queue = Hamster::Vector[value]
      @iteration += 1
      @dirty = true
    end
  end
  
  def close
    @listener.kill  if @listener
    @processor.kill if @processor
    @server.close   if @server
  end

  def update cmd
    @mutex.synchronize do
      @queue = @queue.push cmd
    end
  end
  
  private
  def process
    @processor ||= Thread.new do
      queue = nil
      iter  = nil
      loop do
        begin
          @mutex.synchronize do
            queue = @queue
            iter = @iteration
          end
          @conns.each do |conn|
            process_connection conn, queue, iter
          end
          sleep WAIT_TIME
        rescue => e
          puts e.inspect
        end
      end
    end
  end

  def process_connection client, queue, iter
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
          conn = Connection.new @server.accept_nonblock
          @conns = @conns.push conn
        rescue IO::WaitReadable
          IO.select([@server])
        rescue => e
          puts e.inspect
        end
      end
    end
  end
end

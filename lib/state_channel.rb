
require 'securerandom'
require 'eventmachine'

class StateChannel
  def initialize(state = nil, &block)
    @state   = state
    @updates = []
    @subs    = {}
    @uid     = 0
    @aggregate = block
  end

  def subscribe(&block)
    raise ArgumentError, 'No block given' unless block_given?

    unique_id.tap do |id|
      EM.schedule { @subs[id] = block }
      sync_subscriber(id)
    end
  end

  def unsubscribe(id)
    EM.schedule do
      @subs.delete id
    end
  end

  def state=(value)
    EM.schedule do
      @state   = value
      @updates = []
      @subs.each do |id, _|
        sync_subscriber id
      end
    end
  end

  def update(*args)
    EM.schedule do
      @updates << args
      @subs.each do |_, block|
        block.call(*args)
      end

      aggregate
    end
  end

  def aggregate(&block)
    EM.schedule do
      @aggregate = block if block_given?

      if @aggregate
        @updates.each do |update|
          @state = @aggregate.call(@state, *update)
        end
        @updates = []
      end
    end
  end

  private

  def unique_id
    # good enough
    SecureRandom.hex
  end

  def sync_subscriber(id)
    EM.schedule do
      @subs[id].call @state
      @updates.each do |args|
        @subs[id].call(*args)
      end
    end
  end
end

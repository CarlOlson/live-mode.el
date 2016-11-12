
require 'em-spec/rspec'
require 'state_channel'

describe StateChannel do
  include EM::SpecHelper

  before do
    @channel = StateChannel.new
  end

  describe '#initialize' do
    it 'should accept an initial state' do
      expect(StateChannel.new 'test state')
    end
  end

  describe '#subscribe' do
    it 'should require a block' do
      expect { @channel.subscribe }.to raise_error ArgumentError
    end

    it 'should return a subscription id' do
      id = @channel.subscribe do
      end
      expect(id).to be
    end
  end

  describe '#update' do
    it 'should send subscribers its arguments' do
      em {
        @channel.subscribe do |arg1, arg2|
          @arg1, @arg2 = arg1, arg2
        end
        @channel.update(1, 2)
        expect(@arg1).to eq 1
        expect(@arg2).to eq 2
        done
      }
    end
  end

  describe '#unsubscribe' do
    it 'should remove subscribers' do
      em {
        id = @channel.subscribe do |arg1|
          @arg1 = arg1
        end
        @channel.update 1
        @channel.unsubscribe id
        @channel.update 2
        expect(@arg1).to eq 1
        done
      }
    end
  end

  describe '#state=' do
    it 'should update the state' do
      em {
        @channel.state = 'test'
        @channel.subscribe do |state|
          expect(state).to eq 'test'
          done
        end
      }
    end

    it 'should send state to subscribers' do
      em {
        args = []
        @channel.subscribe do |arg|
          args << arg
        end
        @channel.state = 1
        expect(args).to eq [nil, 1]
        done
      }
    end

    it 'shoud clear updates' do
      em {
        @channel.update 1
        @channel.state = 2
        args = []
        @channel.subscribe do |arg|
          args << arg
        end
        expect(args).to eq [2]
        done
      }
    end
  end

  it 'should send new subscribers the starting state' do
    em {
      @channel = StateChannel.new 'state'
      @state = nil
      @channel.subscribe do |state|
        @state = state
      end
      expect(@state).to eq 'state'
      done
    }
  end

  it 'should send new subscribes past updates' do
    em {
      @channel = StateChannel.new 0
      @channel.update 1
      @channel.update 2
      @updates = []
      @channel.subscribe do |arg|
        @updates << arg
      end
      expect(@updates).to eq [0, 1, 2]
      done
    }
  end

end

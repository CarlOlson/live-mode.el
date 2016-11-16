
require 'em-spec/rspec'
require 'state_channel'
require 'buffer'

describe StateChannel do
  include EM::SpecHelper

  let(:channel) { described_class.new }

  describe '#initialize' do
    it 'accepts an initial state' do
      expect(described_class.new('test state'))
    end
  end

  describe '#subscribe' do
    it 'requires a block' do
      expect { channel.subscribe }.to raise_error ArgumentError
    end

    it 'returns a subscription id' do
      id = channel.subscribe {}
      expect(id).to be
    end
  end

  describe '#update' do
    it 'sends subscribers its arguments' do
      em do
        args = []
        channel.subscribe do |arg1, arg2|
          args = [arg1, arg2]
        end
        channel.update(1, 2)
        expect(args).to eq [1, 2]
        done
      end
    end
  end

  describe '#unsubscribe' do
    it 'removes subscribers' do
      em do
        arg1 = nil
        id = channel.subscribe do |arg|
          arg1 = arg
        end
        channel.update 1
        channel.unsubscribe id
        channel.update 2
        expect(arg1).to eq 1
        done
      end
    end
  end

  describe '#state=' do
    it 'updates the state' do
      em do
        channel.state = 'test'
        channel.subscribe do |state|
          expect(state).to eq 'test'
          done
        end
      end
    end

    it 'sends state to subscribers' do
      em do
        args = []
        channel.subscribe do |arg|
          args << arg
        end
        channel.state = 1
        expect(args).to eq [nil, 1]
        done
      end
    end

    it 'clears updates' do
      em do
        channel.update 1
        channel.state = 2
        args = []
        channel.subscribe do |arg|
          args << arg
        end
        expect(args).to eq [2]
        done
      end
    end
  end

  it 'sends new subscribers the starting state' do
    em do
      channel = described_class.new 'state'
      state = nil
      channel.subscribe do |arg|
        state = arg
      end
      expect(state).to eq 'state'
      done
    end
  end

  context 'without an aggregate function' do
    it 'sends new subscribes past updates' do
      em do
        channel = described_class.new 0
        channel.update 1
        channel.update 2
        updates = []
        channel.subscribe do |arg|
          updates << arg
        end
        expect(updates).to eq [0, 1, 2]
        done
      end
    end
  end

  context 'with an aggregate function' do
    it 'aggregates the updates into the state' do
      em do
        channel.aggregate do |state, update|
          state.update(update['start'],
                       update['length'],
                       update['text'])
        end
        channel.state = Buffer.new('1234567890!')
        channel.update('event'  => 'update',
                       'start'  => 6,
                       'length' => 5,
                       'text'   => '!')
        state = nil
        channel.subscribe do |arg|
          state = arg
        end
        expect(state.text).to eq '12345!!'
        done
      end
    end
  end
end

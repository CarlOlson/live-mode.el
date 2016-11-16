require 'buffer'

describe Buffer do
  before do
    # NOTE: let won't work with reassignment
    @buffer = described_class.new
  end

  it 'stores text' do
    @buffer = @buffer.set('test')
    expect(@buffer.text).to eq 'test'
  end

  it 'updates text' do
    @buffer = @buffer.set('1234567890!')
    @buffer = @buffer.update(6, 5, '!')
    expect(@buffer.text).to eq '12345!!'
  end

  it 'is immutable' do
    @buffer.text = 'test'
    expect(@buffer.text).to eq ''
  end
end

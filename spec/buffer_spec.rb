
require 'buffer'

describe Buffer do
  before do
    @buffer = Buffer.new
  end

  it "should store text" do
    @buffer = @buffer.set("test")
    expect(@buffer.text).to eq "test"
  end

  it "should update text" do
    @buffer = @buffer.set("1234567890!")
    @buffer = @buffer.update(6, 5, "!")
    expect(@buffer.text).to eq "12345!!"
  end

  it "should be immutable" do
    @buffer.text = "test"
    expect(@buffer.text).to eq ""
  end
end

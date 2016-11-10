
class Buffer
  attr_reader :text
  
  def initialize text = ""
    @text = text
  end

  def set text
    Buffer.new text
  end
  alias :text= :set
  
  def update start, length, insert
    # buffers are indexed from 1
    start -= 1
    text = @text[0...start] +
           insert +
           @text[start + length..-1]

    Buffer.new text
  end
end


module Enumerable
  def to_lisp
    items = self.map do |item|
      if item.is_a? String
        item.dump
      elsif item.is_a? Symbol
        item.to_s
      elsif item.is_a? Numeric
        item
      elsif item.respond_to? :to_lisp
        item.to_lisp
      else
        raise StandardError.new 'connot convert to lisp'
      end
    end
    "(#{items.join(' ')})"
  end
end


module Enumerable
  def to_lisp
    items = map do |item|
      case item.class
      when String  then item.dump
      when Symbol  then item.to_s
      when Numeric then item
      else
        item.to_lisp
      end
    end
    "(#{items.join(' ')})"
  end
end

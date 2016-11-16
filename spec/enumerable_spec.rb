
require 'enumerable'

describe Enumerable do
  it 'converts arrays to lisp lists' do
    arr = [:progn, [:format, '%s', 5]]
    expect(arr.to_lisp).to eq '(progn (format "%s" 5))'
  end

  it 'raises error on unknown types' do
    arr = [proc {}]
    expect { arr.to_lisp }.to raise_error StandardError, /lisp/
  end

  it 'escapes string characters' do
    arr = [:list, "line1\nline2"]
    expect(arr.to_lisp).to eq '(list "line1\\nline2")'
  end
end

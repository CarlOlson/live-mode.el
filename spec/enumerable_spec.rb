
require 'enumerable'

describe Enumerable do
  it "should convert arrays to lisp lists" do
    arr = [:progn, [:format, "%s", 5]]
    expect(arr.to_lisp).to eq %Q{(progn (format "%s" 5))}
  end

  it "should raise error on unknown types" do
    arr = [Proc.new {}]
    expect { arr.to_lisp }.to raise_error StandardError, /lisp/
  end

  it "should escape string characters" do
    arr = [:list, "line1\nline2"]
    expect(arr.to_lisp).to eq %Q{(list "line1\\nline2")}
  end
end

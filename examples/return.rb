class Main
  def initialize ()
    self.fact(0).puts();
    self.fact(10).puts();
  end

  def fact (n)
    case n
      when 0 
        return 1
    end
    "This line should...";
    n * self.fact(n-1);
  end
end

class Fact
  def fact (n)
    case n
      when 0
        1
      when x
        n * self.fact(n-1)
    end
  end
end

class Main
  def initialize ()
    f = new Fact();
    f.fact(0).puts();
    f.fact(10).puts();
  end
end


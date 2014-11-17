class Fact
  def fact (n)
    case n
      when 0
        1;
      when x 
        n * self.fact(n-1); 
    end
  end
end

class Proxy
  def initialize (c, log)
    @receiver = c;
    @log = log;
  end

  def method_missing (msg)
    case @log
      when true()
        "Method call:".puts();
        msg.puts();
    end
    send(@receiver, msg);
  end
end

class Main
  def initialize ()
    f = new Fact();
    f.fact(10).puts();
    p1 = new Proxy(f, false());
    p1.fact(10).puts();
    p2 = new Proxy(f, true());
    p2.fact(10).puts();
  end
end

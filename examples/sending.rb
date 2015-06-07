class Main
  def initialize (value)
    jens = new Person();

    y = 3

    @some_value = 4 + 2 - 20 / (7 + y) + 1 * 2 

    jens.send("say","1")
    jens.send("run","fast")

    switch("password")
    self.switch("invalid")

    x = test_return()
    puts(x)

    ivar()

    list = new Cons("A",new Cons("B", new Cons("C", new Cons(7,0))))

    puts(list.get(3))
    list.put(3,8)
    puts(list.get(3))

    h = True
    puts(h)
  end

  def switch(x)
    case x
      when "password"
        puts("3")
      when x
        puts("4")
    end
  end

  def test_return()
    return 5
    puts(0)
  end

  def ivar()
    puts(@some_value)
  end
end

class Cons
  def initialize(x,xs)
    @element = x
    @rest = xs
  end

  def get(i)
    case i
      when 0
        return @element
      when x
        return @rest.get(i-1)
    end
  end

  def put(i, value)
    case i
      when 0
        @rest = new Cons(@element, @rest)
        @element = value
      when x
        return @rest.put(i-1, value)
    end
  end
end

class Person
  def say(x)
    puts(x)
  end

  def method_missing(methodName,x)
    puts("2")
  end
end

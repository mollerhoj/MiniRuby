class Main
  def initialize (value)
    2 
    jens = new Person();

    jens.send("yell","hello","goodbye")
    jens.send("tell","hello","goodbye")

    hello("dude")
  end

  def hello(x)
    puts(x)
  end
end

class Person

  def yell(x,y)
    puts(x)
    puts(y)
  end

  def method_missing(methodName,x,y)
    puts(methodName)
    puts(x)
    puts(y)
  end

end

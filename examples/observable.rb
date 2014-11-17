class Observable
  def initialize (value)
    @value = value;
    @observers = nil();
  end

  def addObserver (obj, cookie)
    @observers = cons(observer(obj, cookie), @observers);
  end

  def setValue (value)
    @value = value;
    self.notifyObservers(@observers);
  end

  def notifyObservers (xs)
    case xs
      when cons(x, xs)
        "Note - new xs shadows the old one";
        case x
          when observer(obj, cookie)
            obj.notify(cookie, @value);
        end
        self.notifyObservers(xs);
    end
  end 
end

class Observer
  def notify(cookie, initializeval)
    "Changed:".puts();
    cookie.puts();
    "New value".puts();
    initializeval.puts();
  end
end

class Main
  def initialize ()
    box = new Observable(0);
    obs1 = new Observer();
    obs2 = new Observer();
    box.setValue(1);
    box.addObserver(obs1, obs1());
    box.setValue(2);
    "".puts();
    box.addObserver(obs2, obs2());
    box.setValue(3);
  end
end

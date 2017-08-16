def power(x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i > 0) {
    r = r * x
    i = i - 1
  }
  r
}

def power2(x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  WHILE(i > 0) {
    r = r * x
    i = i - 1
  }
  r
}

def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition)(command)
  } else {
    ()
  }
}

def REPEAT(condition: => Boolean)(command: => Unit) {
  command
  if (condition) ()
  else {
    REPEAT(condition)(command)
  }
}

power(32, 2)
power2(32, 2)
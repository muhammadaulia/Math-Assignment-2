
f <- function(x) {
  return((2*x) - sin(x))
}

trapezoid <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop
  }
   
  h <- b - a
   
  fxdx <- (h / 2) * (f(a) + f(b))
   
  return(fxdx)
}

trapezoid(f, 1, 10)

f <- function(x) {
  return((4*x) * (2*(sin(x))))
}

simpsons.rule <- function(f, a, b) {
  if (is.function(f) == FALSE) {
    stop
  }
    
  h <- (b - a) / 2
  x0 <- a
  x1 <- a + h
  x2 <- b
   
  s <- (h / 3) * (f(x0) + 4 * f(x1) + f(x2))
   
  return(s)
}
simpsons.rule(f, 0, pi/4)


#EXCERCISE
f<- function(x) {
    result <- sin(x)
    return(result)
}
input <- 30:120
plot(input,sapply( input , f ) ,
type= "l" , xlab = "x" ,ylab = "f(x)" )

#EXCERCISE
f<- function(x) {
    result <- log(x)
    return(result)
}
input <- 30:120
plot(input,sapply( input , f ) ,
type= "l" , xlab = "x" ,ylab = "f(x)" )

#EXCERCISE
f<- function(x) {
    result <- sqrt(x)-2
    return(result)
}
input <- 10 : 25
plot(input,sapply( input , f ) ,
type= "l" , xlab = "x" ,ylab = "f(x)" )

#EXCERCISE
f<- function(x) {
    result <- sqrt(x-2)
    return(result)
}

input <- 10 : 25
plot(input,sapply( input , f ) ,
type= "l" , xlab = "x" ,ylab = "f(x)" )

#CONSTANT FUNCTION
#FUNGSI : f(x) = a, nilai a kita ganti dengan 10

f <- function(x) {
    fx <- 10
    return (fx)
}

input <- 10:25
plot(input,sapply( input , f ) ,
type= "l" , xlab = "x" ,ylab = "f(x)" )

#LINEAR FUNCTION
#FUNGSI : f(x) = ax + b, nilai a diganti dengan 10 dan nilai b diganti dengan 10

f <- function(x) {
    fx <- (10*x) + 10
    return(fx)
}

input <- -2:15
plot(input,sapply(input,f),type="l",xlab="x",ylab="f(x)")

#QUADRATIC FUNCTION
#FUNGSI : f (x) = ax^2 + bx + c , nilai a diganti dengan 5, nilai b diganti dengan 7, dan nilai c diganti dengan 11

f <- function(x) {
    fx <- 5*x^2 + 7*x + 11
    return(fx)
}

input <- -15:15
plot (input,sapply(input,f),type="l",xlab="x",ylab="f(x)")

#POLYNOMIAL FUNCTION
#FUNGSI : f (x) = an x^n + an−1 x^n−1 + . . . + a1x + a0

f <- function(x) {
    fx <- x^3 - (5*x^2) + x-2
    return (fx)
}
input <- seq(-10,20, by=2)
plot (input,sapply(input , f),type="l",xlab="x",ylab="f(x)")

#RATIONAL FUNCTIONS
#FUNGSI : f (x) = p(x)/q(x)

f <- function (x) {
    fx <- 20/x
    return(fx)
}
input <- seq (1,10,by = 2)
plot (input , sapply(input , f),type="l",xlab="x",ylab="f(x)")

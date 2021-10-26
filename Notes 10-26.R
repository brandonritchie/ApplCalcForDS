fun1 <- function(x){
  sqrt((8/x)^2 + x^2)
}

x <- seq(-50,50)

plot(x, fun1(x), type = "l")

f <- function(x){
  ((0.5*((x^2+64*x^-2)^-0.5))*(-128/x^3 + 2*x))
}

uniroot(f, interval = c(1,100))$root

sqrt(8)

f <- function(x){x*exp(-x)}
Df <- function(x){1*exp(-x) - x*(exp(-x))}
D2f <- function(x){-2*exp(-x) + x*exp(-x)}

uniroot(Df, c(-10,10))$root

plot(seq(0,5,.001), f(seq(0,5,.001)), type = "l")
plot(seq(-10,10), Df(seq(-10,10)), type = "l")
p
lot(seq(-10,10), D2f(seq(-10,10)), type = "l")

g <- function(x){x*(1-x)}
Dg <- function(x){1-2*x}
D2g <- function(x){0*x-2} #Is the zero important?

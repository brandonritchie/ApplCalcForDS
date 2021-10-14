## Uniroot practice

uniroot(function(x){3*x-5}, c(-10,10))$root

uniroot(function(x){3*x-12}, c(-10,10))$root

uniroot(function(x){exp(-x)-3*x+5}, c(-10,10))$root

# Error
uniroot(function(x){exp(x)-3*x+5}, c(0,5))$root

uniroot(function(x){x^2+x-6}, c(0,10))$root

x <- seq(-50,50)
f <- function(x){exp(x)-3*x+5}
plot(x = x, y = f(x))

uniroot.all(function(x){x^2+x-6}, c(-10,10))


p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(0, lambda = 1)

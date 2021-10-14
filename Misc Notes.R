f <- function(x){3^x-17}
interval <- c(2,3)
my_root <- uniroot(f, interval)$root

#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")

#1
f1 <- function(x){3^x-17}
interval <- c(2,4)
uniroot(f1, interval)$root

# 24
f24 <- function(b){-10*exp(2-2*b)+60}
interval <- c(-10,10)

uniroot(f24,interval)$root

#26
f26 <- function(x){6*exp(5*x-6) - 54}
interval <- c(-10,10)
uniroot(f26, interval)$root

f <- function(x){-x^2+15}
interval <- c(-5,5)

uniroot(f,interval)

uniroot(function(x){3*exp(2*x)-2}, interval = c(-5,5))$root

f1 <- function(x,n=0.003,p=0.5){
  # x must be an whole number between 0 and n, endpoints included
  factorial(n)/(factorial(x)*factorial(n-x))*p^x*(1-p)^(n-x)
}

plot(x = seq(-1,10,0.1),f1(seq(-1,10,0.1)))

x <- seq(-10,10, .1)

f4 <- function(x,lambda=1){
  
  out <- rep(0,length(x))
  out[(x > 0)] <- 1 - exp(-lambda*x[(x > 0)])
  
  return(out)
}

plot(x,f4(x))

f5 <- function(x,a=0,b=1){
  
  out <- rep(0,length(x))
  out[(a <= x) & (x <= b)] <- (x[(a <= x) & (x <= b)]-a)/(b-a)
  out[(x > b)] <- 1
  
  return(out)
}

plot(x,f5(x))

library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)

###
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)
f1 <- function(x,a0=0,a1=0){ a0 + a1*x }

x <- seq(-10,800001,2)
yM <- f1(x,a0=100,a1=0.0006)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)

yM <- f4()

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)

sqrt((15.38)^2 / sum((cars$speed - mean(cars$speed))^2))

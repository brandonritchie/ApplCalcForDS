par(mar=c(4,4,0.25,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y)

par(mar=c(4,4,2,0.25))
x <- mtcars$wt
y <- mtcars$mpg
plot(x,y,
     pch=16,
     xlab='weight (1000 lbs)',
     ylab='Miles per US gallon',main='Our 1st Scatter Plot')
install.packages("devtools")

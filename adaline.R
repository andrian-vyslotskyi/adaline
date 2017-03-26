#install.packages("ggplot2")
library("ggplot2")

input <- read.csv("brain-body.csv",header = T)
wholeDataset <- qplot(brain, body, data=input, geom=c("point"))

x <- input$brain
y <- input$body
n <- length(x)

###ls###
z <- n * sum(x^2) - sum(x)^2
b <- (sum(y)*sum(x^2) - sum(y*x)*sum(x))/z
a <- (n * sum(x*y) - sum(y)*sum(x))/z

yrLs <- a*x+b

sigmaY <- sum(yrLs - y)^2/(n-2)
sigmaA <- (sigmaY*sum(x^2))/z
sigmaB <- sigmaY*n/z

withLs <- wholeDataset + 
  geom_path(aes(x, y, colour="less squares"), data = data.frame(x=x, y=yrLs))

###adaline###
adaline <- function(x, w) x*w

train <- function(epoch, x, y, w, q){
  for(e in (1:epoch)) {
    for(i in (1: n)){
      yr[i] <<- adaline(x[i], w)
      dw <- q * x[i] * (y[i] - yr[i])
      w <- w + dw
    }
  }
  return(w)
}

w0 <- runif(1, 0.0, 1.0)
epoch <- 1000
q <- 0.00000005
yr <- 0

w <- train(epoch, x, y, w0, q)
yr <- sapply(x, function(xi) adaline(xi, w))
yr

withAdaline <- wholeDataset + 
  geom_path(aes(x, y, colour="adaline"), data=data.frame(x=x, y=yr))

all <- withLs + geom_path(aes(x, y, colour="adaline"), data=data.frame(x=x, y=yr))

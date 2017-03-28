library(shiny)
library(ggplot2)

generateInputData <- function(f = "csv" , n = 100, step=1) {
  if(f == "csv") {
    res <- read.csv("brain-body.csv",header = F)
  } else {
    x <- seq(0, n, step)
    y <- x^2 + 2*x
    res <- data.frame(V1=x, V2=y)
  }
  res
}

###adaline###
adaline <- function(x, w) x*w

train <- function(epoch, x, y, w, q) {
  yr <- 0
  for(e in seq(1, epoch, 1) ) {
    for(i in seq(1, length(x), 1) ) {
      yr[i] <- adaline(x[i], w)
      dw <- q * x[i] * (y[i] - yr[i])
      w <- w + dw
    }
  }
  return(w)
}
###

###ls###




###server###
shinyServer(function(input, output) {
  
  output$plot <- renderPlot({
    input_data <- generateInputData(f = input$f)
    wholeDataset <- qplot(V1, V2, data=input_data, geom=c("point"))
    
    x <- input_data$V1
    y <- input_data$V2
    n <- length(x)
    ################################
    
    z <- n * sum(x^2) - sum(x)^2
    b <- (sum(y)*sum(x^2) - sum(y*x)*sum(x))/z
    a <- (n * sum(x*y) - sum(y)*sum(x))/z
    
    yrLs <- a*x+b
    
    sigmaY <- sum((yrLs - y)^2)/(n-2)
    sigmaA <- (sigmaY*sum(x^2))/z
    sigmaB <- sigmaY*n/z
    
    withLs <- wholeDataset + 
      geom_path(aes(x, y, colour="less squares"), data = data.frame(x=x, y=yrLs))
    #################################
    
    w <- train(input$epoch, x, y, input$w0, input$q)
    yr <- sapply(x, function(xi) adaline(xi, w))
    
    sigmaY_adaline <- sum((yr - y)^2)/(n-2)
    
    withAdaline <- wholeDataset + 
      geom_path(aes(x, y, colour="adaline"), data=data.frame(x=x, y=yr))
    
    all <- withLs + geom_path(aes(x, y, colour="adaline"), data=data.frame(x=x, y=yr))
    
    output$mse_ls <- renderText({sigmaY})
    output$mse_a <- renderText({sigmaY_adaline})
    
    all
  })
  
  
})

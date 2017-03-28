library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Less square and Adaline regressions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("w0",
                   "W0:",
                   min = 0,
                   max = 1,
                   value = 0.5),
      numericInput("q",
                   "q:",
                   max = 1,
                   value = 0.00001),
      numericInput("epoch",
                   "train epochs:",
                   value = 100),
      selectInput("f",
                  "input data",
                  choices = list("brain/body weight dataset" = "csv", "y=x^2+2x" = "par"),
                  selected = "csv")
    ),
    
    mainPanel(
      h3("Result lines:"),
      plotOutput("plot"),
      h5("Mean squared error for less square:"),
      textOutput("mse_ls"),
      h5("Mean squared error for adaline:"),
      textOutput("mse_a")
    )
  )
))

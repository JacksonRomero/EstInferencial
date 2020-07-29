
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Demostración del Teorema Central del Límite"),
  

  sidebarLayout(
    sidebarPanel(
      helpText("Selccione el tipo de distribución,  número de simulaciones y tamaño de muestra
              "),
      selectInput("distribution", "Distribución:", 
                  c("Lanzamiento de dados", "Lanzamiento de moneda", "Exponencial", "Poisson")),
      sliderInput("nosim", "Número de Simulaciones:", 100, 10000, 100, step = 100),
      sliderInput("samples", "Número de muestras:", 1, 40, 10, step = 1, animate = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot")
    )
  )
))
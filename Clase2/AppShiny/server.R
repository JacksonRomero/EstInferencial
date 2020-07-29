

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$plot  <- renderPlot({
    nosim <- input$nosim
    n <- input$samples
    
    if(input$distribution == "Lanzamiento de dados") {
      set.seed(2604)
      sim <- sample(1:6, nosim * n, replace = TRUE)
      sims <- matrix(sim, nosim)
      media<-round(apply(sims, 1,mean),2)
      
    }
    else if(input$distribution == "Lanzamiento de moneda") {
      set.seed(2604)
      sim <- sample(0:1, nosim * n, replace = TRUE)
      sims <- matrix(sim, nosim)
      media<-round(apply(sims, 1,mean),2)
    }
    else if(input$distribution == "Exponencial") {
      set.seed(2604)
      sim <- rexp(n*nosim)
      sims <- matrix(sim, nosim)
      media<-round(apply(sims, 1,mean),2)
    }
    else if(input$distribution == "Poisson") {
      set.seed(2604)
      sim <- rpois(n*nosim, lambda = 1)
      sims <- matrix(sim, nosim)
      media<-round(apply(sims, 1,mean),2)
    }
    plot(table(media)/nosim, type='h', lwd=10, xlab = '', ylab='', col='grey')
    
  })
})

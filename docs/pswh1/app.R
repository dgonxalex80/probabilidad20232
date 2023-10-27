#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


# Cargar las bibliotecas necesarias
library(shiny)
library(pwr)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("mu0", "Media hipotética (H0):", min = -3, max = 3, value = 0, step = 0.1),
      sliderInput("sigma", "Desviación estándar poblacional:", min = 0.1, max = 2, value = 1, step = 0.1),
      sliderInput("n", "Tamaño de la muestra:", min = 10, max = 100, value = 50, step = 10),
      sliderInput("alpha", "Nivel de significancia (α):", min = 0.01, max = 0.10, value = 0.05, step = 0.01)
    ),
    mainPanel(
      plotOutput("power_plot")
    )
  )
)

server <- function(input, output) {
  output$power_plot <- renderPlot({
    mu0 <- input$mu0
    sigma <- input$sigma
    n <- input$n
    alpha <- input$alpha
    
    # Cálculo de la potencia
    mu_alt <- seq(mu0, mu0 + 2, by = 0.1) # Valores de la media alternativa
    power <- sapply(mu_alt, function(mu) {
      z <- (mu - mu0) / (sigma / sqrt(n))
      power <- 1 - pnorm(qnorm(1 - alpha) - (z)) # Corregido
      return(power)
    })
    
    plot(mu_alt, power, type = "l", xlab = "Media Alternativa", ylab = "Potencia", main = "Curva de Potencia")
  })
}

shinyApp(ui = ui, server = server)

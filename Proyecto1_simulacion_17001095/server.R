#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("problema1.R")
library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$problem1plot <- renderPlot({

        # Do simulations
        average_waiting_time <- simulation_problem_one(nsim=input$nsim)

        # plot
        servers <- c(1:7)
        
        dataset <- data.frame(servers, average_waiting_time)
        ggplot(dataset, aes(x=servers, average_waiting_time)) +
          geom_col(fill="steelblue") +
          labs(
            x="Número de Servidores",
            y="Tiempo Promedio (minutos)",
            title="Tiempo Promedio de Espera vs No. Servidores"
          ) + 
          geom_text(
            aes(label=sprintf("%0.2f", round(average_waiting_time, digits = 2))),
            vjust=-0.3,
            color="black",
            size=6
          ) +
          theme_minimal()
    })
})

#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("problema1.R")
source("problema2.R")
source("problema3.R")

library(shiny)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  ### Rendering Area ###
  output$problem1plot <- renderPlot({
    # Do simulations
    average_waiting_time_per_server <- simulation_problem_one(nsim=input$nsim)
    
    # plot
    servers <- c(1:7)
    
    # Creating Temporal Dataset
    dataset <- data.frame(servers, average_waiting_time_per_server)
    
    View(dataset)
    # Plotting
    ggplot(
      dataset,
      aes(x=servers, average_waiting_time_per_server)
    ) +
      geom_col(fill="steelblue") +
      labs(
        x="Número de Servidores",
        y="Tiempo Promedio (minutos)",
        title="Tiempo Promedio de Espera vs No. Servidores"
      ) + 
      geom_text(
        aes(label=sprintf("%0.2f", round(average_waiting_time_per_server, digits = 2))),
        vjust=-0.3,
        color="black",
        size=6
      ) +
      theme_minimal()
  })
  
  output$problem2plot <- renderPlot({
    
    # Do simulations
    results <- simulation_problem_two(
      nsim=input$nsim2,
      time_limit = input$time_limit
    )
    
    View(results)
    
    # plot
    ggplot(results, aes(x=servers, average_waiting_time)) +
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
  
  output$problem3plot <- renderPlot({
    
    # Do simulations
    results <- simulation_problem_three(
      nsim=input$nsim3,
      queue_limit = input$queue_limit,
      servers = input$servers
    )
    
    View(results)
    
    # plot
    ggplot(results, aes(x=servers, average_not_queued)) +
      geom_col(fill="steelblue") +
      labs(
        x="Número de Servidores",
        y="Cantidad de personas no aceptadas",
        title="Cantidad promedio de personas no aceptadas vs No. Servidores"
      ) +
      geom_text(
        aes(label=sprintf("%0.2f", round(average_not_queued, digits = 2))),
        vjust=-0.3,
        color="black",
        size=6
      ) +
      theme_minimal()
  })
})

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage(title='Proyecto 1 - 17001095',
             tabPanel('Problema 1',
                      sidebarLayout(
                        sidebarPanel(
                          numericInput('nsim',label = 'Numero de simulaciones:',
                                       value = 5,
                                       min=1, 
                                       step = 1),
                          submitButton('Aplicar cambios')
                        ),
                        mainPanel(
                          plotOutput('problem1plot'),
                        )
                      )
             ),
             tabPanel('Problema 2',
                      sidebarLayout(
                        sidebarPanel(
                          numericInput('time_limit',label = 'Tiempo de espera máximo (minutos)',
                                       value = 15,
                                       min=1, 
                                       step = 1),
                          numericInput('nsim2',label = 'Numero de simulaciones:',
                                       value = 5,
                                       min=1, 
                                       step = 1),
                          submitButton('Aplicar cambios')
                        ),
                        mainPanel(
                          plotOutput('problem2plot'),
                        )
                      )
             ),
             tabPanel('Problema 3',
                      sidebarLayout(
                        sidebarPanel(
                          numericInput('queue_limit',label = 'Tamaño máximo de la cola',
                                       value = 15,
                                       min=1, 
                                       step = 1),
                          numericInput('servers',label = 'Número máximo de servidores',
                                       value = 10,
                                       min=1, 
                                       step = 1),
                          numericInput('nsim3',label = 'Numero de simulaciones:',
                                       value = 5,
                                       min=1, 
                                       step = 1),
                          submitButton('Aplicar cambios')
                        ),
                        mainPanel(
                          plotOutput('problem3plot'),
                        )
                      )
             )
  )
))

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
                          verbatimTextOutput("problem1info")
                        )
                      )
             ),
             tabPanel('plot',
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput('N2', label = 'Tamaño del Grupo:',
                                      min = 5,
                                      max=150,
                                      value = 20,
                                      step = 1),
                          numericInput('n2',label = "Numero de persona la misma fecha de cumpleaños:",
                                       value = 2,step = 1),
                          sliderInput('range_nsim',label = 'Rango simulacion',
                                      value = c(500,2000),min=10,max=5000),
                          numericInput('step',label = 'Step',value = 500),
                          submitButton('Aplicar cambios')
                        ),
                        mainPanel(plotOutput('plotxy'))
                        
                      )
             )
  )
))

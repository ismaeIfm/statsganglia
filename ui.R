
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel(h1("Accounting")),
  fluidRow(
    
    column(3,
           dateRangeInput("dates", label = h3("Rango de Fechas"), start= "2014-02-10", min = "2014-02-10")),
    
    column(3, 
           textInput("text", label = h3("Búsqueda"), 
                     value = "Enter text..."))
  ),  
  sidebarLayout(
    sidebarPanel(  checkboxGroupInput("checkGroup", 
                                      label = h3("Record Type"), 
                                      choices = list("Abort" = 1, 
                                                     "Checkpoint" = 2, 
                                                     "Delete" = 3, 
                                                     "Exit" = 4, 
                                                     "Queue" = 5, 
                                                     "Rerun" = 6, 
                                                     "Start" = 7, 
                                                     "Restart" = 8 ), selected = c(1,2,3,4,5,6,7,8)),  
                   selectInput("select", label = h3("Usuario"), 
                                                                  choices = list("Todos" = 1, "Usuario 1" = 2, "Usuario 2" = 3,
                                                                                 "Usuario 3" = 4), selected = 1)),
    mainPanel(  plotOutput("text1"))
  )

  
))
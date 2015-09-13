
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
           dateRangeInput("dates", label = h3("Rango de Fechas"), start= "2014-02-10", min = "2014-02-10"))
    #,column(3, textInput("text", label = h3("Busqueda"), value = "Enter text..."))
  ),  


    mainPanel(
      tabsetPanel(
        tabPanel("Resumen" ,     
                 fluidRow(column(3,  checkboxGroupInput("checkGroup", 
                                                                      label = h3("Record Type"), 
                                                                      choices = list("Abort" = "A", 
                                                                                     "Checkpoint" = "C", 
                                                                                     "Delete" = "D", 
                                                                                     "Exit" = "E", 
                                                                                     "Queue" = "Q", 
                                                                                     "Rerun" = "R", 
                                                                                     "Start" = "S", 
                                                                                     "Restart" = "T" ), selected = c("A","C","D","E","Q","R","S","T"))
                 ), column(9, plotOutput("summary"))),
                 tableOutput("jobsnames")),
        tabPanel("Actividad Usuarios", plotOutput("users"), selectInput("select_users", label = h3("Usuario"), choices=c(" " = 1)), selectInput("select_jobs", label = h3("Trabajos"), choices=c(" " = 1)))#, plotOutput("jobs")))
        
        )
  )

  
))
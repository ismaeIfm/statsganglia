
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
           dateRangeInput("dates", label = h3("Rango de Fechas"), 
                          start= "2014-02-10", min = "2014-02-10")
           )
  ),  
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("Resumen",
               plotOutput("summary"),
               strong("Trabajos Activos:"), textOutput("activeJobs"),
               h3("Usuarios Activos: "),
               plotOutput("usersActivity")
      ), 
      tabPanel("Usuarios",
               selectInput("select_users", label = h3("Usuario"), choices=c(" " = 1)), 
               h3(textOutput("actualUser")),
               h4("Actividad del usuario: "),
               plotOutput("activityByUser"), 
               h4(strong("Número de trabajos:"), textOutput("activeJobsByUser")),
               h4("Información de los trabajos del usuario: "),
               tableOutput("jobInformationByUser")
      ),
      tabPanel("Trabajos",
               selectInput("select_jobs", label = h3("Trabajo"), choices=c(" " = 1))
      )
    ) 
  )
  
  
))
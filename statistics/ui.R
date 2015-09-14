
library(shiny)

shinyUI(fluidPage(
  titlePanel(h1("Accounting")),
  fluidRow(
    
    column(3,
           dateRangeInput("dates", label = h3("Rango de Fechas"), start= "2014-02-10", min = "2014-02-10")
    )
  ),  
    mainPanel(
        tabsetPanel(
            tabPanel("Actividad General",
                        h4("Resumen estadistico"),
                        verbatimTextOutput("summary"),
                        checkboxGroupInput("select_check",label = h3("Record"),choices =c(" ")),
                        checkboxGroupInput("select_checkusers",label = h3("Users"),choices =c(" ")),
                        checkboxGroupInput("select_checkncpu",label = h3("Num Cpu"),choices=c(" ")),
                        #plotOutput("timeserieI"),
                        plotOutput("histogramI")
                        ),
            tabPanel("Actividad Usuarios", 
                  selectInput("select_users", label = h3("Usuario"), choices=c(" " = 1)),
                  selectInput("select_jobs", label = h3("Trabajos"), choices=c(" " = 1)),
                  selectInput("select_cpu", label = h3("Cpu"), choices=c(" " = 1)),
                  selectInput("select_record",label = h3("Record"),choices=c(" " = 1)),
                  plotOutput("timeserieA"),
                  #plotOutput("histogram"),
                  plotOutput("timeserieB")
            )
                         
        )
  )
))
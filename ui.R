
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
           dateRangeInput("dates", label = h3("Date range"))),
    
    column(3, 
           textInput("text", label = h3("Text input"), 
                     value = "Enter text..."))
  ),  
  sidebarLayout(
    sidebarPanel(  checkboxGroupInput("checkGroup", 
                                      label = h3("Checkbox group"), 
                                      choices = list("Choice 1" = 1, 
                                                     "Choice 2" = 2, "Choice 3" = 3),
                                      selected = 1),  
                   selectInput("select", label = h3("Select box"), 
                                                                  choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                                                 "Choice 3" = 3), selected = 1)),
    mainPanel("main panel")
  )

  
))
library(shiny)
library(ggplot2)
source("utils.R")

accounting <- ReadDirectory("../accounting/")
preprocessedData <- GetPreprocessedData(accounting)

shinyServer(function(input, output,session) {
  initialDate <- reactive({as.POSIXct(input$dates[1], format = "%Y-%m-%d")})
  finalDate <- reactive({as.POSIXct(paste(as.character(input$dates[2]), "23:59:59"), format = "%Y-%m-%d %H:%M:%S")})
  sessionData <- reactive({ GetDataByRangeOfDate(accounting, initialDate(), finalDate())})
  preprocessedSessionData <- reactive({ GetPreprocessedDataByDate(preprocessedData, initialDate(), finalDate())})
  getUsers <- reactive({GetUsers(preprocessedSessionData())})
  preprocessedSessionDataByUser <- reactive({
    dataWithUser <-preprocessedSessionData()[!is.na(preprocessedSessionData()$User), ]
    dataWithUser[dataWithUser$User == input$select_users, ]
  }) 
  
  observe({
    updateSelectInput(session, "select_users", choices = as.character(getUsers()))
  })
  
  output$summary <- renderPlot({ 
    PlotDataSummary(sessionData())
    })
  
  output$activeJobs <- renderText({ 
    CountNumberOfJobs(preprocessedSessionData())
  })
  
  output$usersActivity <- renderPlot({
    PlotUserActivity(preprocessedSessionData())
  })
  
  output$activityByUser <- renderPlot({ 
    PlotDataSummary(GetDataByName(sessionData(), input$select_users))
  })
  
  output$actualUser <- renderText({
    input$select_users
  })
  
  output$activeJobs <- renderText({ 
    CountNumberOfJobs(preprocessedSessionData())
  })
  
  output$jobInformationByUser <- renderTable({
    preprocessedSessionDataByUser()
  })
  
  output$activeJobsByUser <- renderText({ 
    CountNumberOfJobs(preprocessedSessionDataByUser())
  })
  
})
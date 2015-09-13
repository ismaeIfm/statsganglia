
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
source("utils.R")


#Reads the files in the directory
accounting <- ReadDirectory("accounting/")


shinyServer(function(input, output,session) {
  
  output$summary <- renderPlot({ 
    sessionData <- GetDataByType(accounting, input$checkGroup)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[1]), `>=`)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[2]), `<=`)
    PlotDataSummary(sessionData)#Plots the data by message type
   
  })

  output$jobsnames <- renderTable({
    sessionData <- GetDataByType(accounting, input$checkGroup)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[1]), `>=`)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[2]), `<=`)
    GetJobsnames(sessionData)
  })
  
  output$users <- renderPlot({ 
    sessionData <- GetDataByType(accounting, input$checkGroup)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[1]), `>=`)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[2]), `<=`)
    users <- GetUsers(sessionData)
    updateSelectInput(session, "select", choices = as.character(unique(users)$mensaje))
    PlotHistUsers(users)
  })

  output$jobs <- renderPlot({ 
    sessionData <- GetDataByType(accounting, input$checkGroup)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[1]), `>=`)
    sessionData <- GetDataByDate(sessionData, as.POSIXct(input$dates[2]), `<=`)
    PlotHistJobs(sessionData)
  })
  
  output$seriedetiempo <- renderPlot({
    data<-GetDateMessageUserJobNcupsPpnAndMemByRange(accounting,input$dates[1],input$dates[2])
    updateSelectInput(session, "select_jobs", choices = as.character( unique( subset(data, user == input$select_users[1], jobname) ) ))
    dateUserJobnameNcpuPlot(data,input$select_users,input$select_jobs,input$select_cpu,)
    
  })
  
})

#Creates a date
lastChristmasDate <- as.POSIXct("12/25/2014 08:32:07", format = "%m/%d/%Y %H:%M:%S")

#Gets data which date is minor to the date specified
messagesSinceLastChristmas <- GetDataByDate(accounting, lastChristmasDate, `<`)

PlotHistRequestors(accounting)#Plots the data my requestors
PlotHistUsers(accounting)#Plots the data by users
PlotHistJobs(accounting)#Plots the data by job

GetUserJobs("natorro", accounting)#Gets the jobs of the user natorro in a table

GetJobsnames(accounting)#Gets the jobs of the data in a table

GetDataByType(accounting, "S")#Filters the data by record marker


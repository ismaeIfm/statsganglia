library(shiny)
library(ggplot2)
source("utils.R")


#Reads the files in the directory
#accounting <- ReadDirectory("accounting/")
data<-GetDateMessageUserJobNcupsAndPpnByRange(accounting,as.character(accounting[1,1]),as.character(accounting[length(accounting$date),1]))

shinyServer(function(input, output,session) {
  
  observe({
    name<-unique(data$Record)
    updateCheckboxGroupInput(session,"select_check",choices = as.character(name))
  })
  
  observe({
    name<-unique(data$user)
    updateCheckboxGroupInput(session,"select_checkusers",choices = as.character(name))
  })
  
  observe({
    name<-unique(data$Ncpu)
    updateCheckboxGroupInput(session,"select_checkncpu",choices = as.character(name))
  })
  
  observe({
    name<-unique(data$user)
    updateSelectInput(session,"select_users",choices = as.character(name))
    })
  
  observe({
    name<-unique(subset(data, user == input$select_users, jobname))
    updateSelectInput(session, "select_jobs", choices = as.character(name$jobname))
  })
  
  observe({
    name<-unique(subset(data,user == input$select_users & jobname == input$select_jobs,Ncpu))
    updateSelectInput(session, "select_cpu", choices = as.numeric(name$Ncpu))
  })
  
  observe({
    name<-unique(subset(data,user == input$select_users & jobname == input$select_jobs & Ncpu == input$select_cpu,Record))
    updateSelectInput(session,"select_record", choices = as.character(name$Record))
  })
  
  output$timeserieI<-renderPlot({
    dateUserJobnameNcpuPlot(data,input$select_checkusers,NULL,input$select_checkncpu,input$select_check,as.POSIXct(input$dates[1]),as.POSIXct(input$dates[2]))
  })
  
  output$histogramI<-renderPlot({
    userJobnameCountRecordPlot(data,input$select_checkusers,NULL,input$select_check,as.POSIXct(input$dates[1]),as.POSIXct(input$dates[2]))
  })
  
  output$timeserieA <- renderPlot({
   dateUserJobnameNcpuPlot(data,input$select_users,input$select_jobs,input$select_cpu,input$select_record,as.POSIXct(input$dates[1]),as.POSIXct(input$dates[2]))
  })
  
  output$histogram <- renderPlot({
    userJobnameCountRecordPlot(data,input$select_users,input$select_jobs,input$select_record,as.POSIXct(input$dates[1]),as.POSIXct(input$dates[2]))
  })
  
  output$timeserieB <-renderPlot({
    dateNcpuRecordPlot(data,input$select_cpu,input$select_record,as.POSIXct(input$dates[1]),as.POSIXct(input$dates[2]))
  })
  
})

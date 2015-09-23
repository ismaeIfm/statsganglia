library(shiny)
library(ggplot2)
source("utils.R")

#
accounting <- ReadDirectory("../accounting/")
#
preprocessedData <- GetPreprocessedData(accounting)

shinyServer(function(input, output,session) {
  initialDate <- reactive({as.POSIXct(input$dates[1], format = "%Y-%m-%d")})
  finalDate <- reactive({as.POSIXct(paste(as.character(input$dates[2]), "23:59:59"), format = "%Y-%m-%d %H:%M:%S")})
  sessionData <- reactive({ GetDataByRangeOfDate(accounting, initialDate(), finalDate())})
  preprocessedSessionData <- reactive({ GetPreprocessedDataByDate(preprocessedData, initialDate(), finalDate())})
  getUsers <- reactive({GetUsers(preprocessedSessionData())})
  preprocessedSessionDataByUser <- reactive({
    dataWithUser <- preprocessedSessionData()[!is.na(preprocessedSessionData()$User), ]
    dataWithUser[dataWithUser$User == input$select_users, ]
  }) 
  getJobsIds <- reactive(GetJobsIDsFromPreprocessedData(preprocessedSessionData()))
  
  
  observe({
    updateSelectInput(session, "select_users", choices = as.character(getUsers()))
    updateSelectInput(session, "select_jobs", choices = as.character(getJobsIds()))
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
  
  output$jobnameById <- renderText({ 
    jobname <- preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$Jobname
    if(is.na(jobname)){
      return("No se encuentran registros de nombre")
    }
    return(jobname)
  })
  
  output$usernameById <- renderText({ 
    username <- preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$User
    if(is.na(username)){
      return("No disponible")
    }
    return(username)
  })
  
  output$groupnameById <- renderText({ 
    groupname <- preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$Group
    if(is.na(groupname)){
      return("No disponible")
    }
    return(groupname)
  })
  
  output$exitStatusById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$exitStatus
  })
  
  output$listNCPUsById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$listNCPUs
  })
  
  output$execHostById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$execHost
  })
  
  output$startById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$Start
  })
  
  output$ctimeById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$ctime
  })
  
  output$qtimeById <- renderText({ 
    as.character(preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$qtime)
  })
  
  output$etimeById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$etime
  })
  
  output$endById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$End
  })
  
  output$dtimeById <- renderText({ 
    as.character(preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$dtime)
  })
  
  output$dtimeById <- renderText({ 
    as.character(preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$dtime)
  })
  
  output$listMemById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$listMem
  })
  
  output$usedMemById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$usedMem
  })
  
  output$usedVMemById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$usedVMem
  })
  
  output$listCPUTById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$listCPUT
  })
  
  output$listWalltimeById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$listWalltime
  })
  
  output$usedCPUTById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$usedCPUT
  })
  
  output$usedWalltimeById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$listWalltime
  })
  
  output$requestorById <- renderText({ 
    preprocessedSessionData()[preprocessedSessionData()$Jobid == input$select_jobs,]$listWalltime
  })
  
  
})
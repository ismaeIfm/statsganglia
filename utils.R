library(ggplot2)

ReadDirectory <- function(name.directory) {
  # Reads all the files of a directory. Each file is readed in table format to 
  # create a data frame from it. All the data frames are combined into one.
  # The directory must contain only accounting logs from torque separated 
  # by a semicolon ";".
  # 
  # Args:
  #   name.directory: The path of the directory to be readed.
  #
  # Returns:
  #   A data frame that contains all the files of the directory name.directory 
  #   readed in table format and combined.
  dataDirectory <- data.frame()
  files <- list.files(name.directory)
  for (i in files){
    nameFile <- paste(name.directory,i, sep = "")
    dataFile <- read.table(nameFile, sep = ";", flush = TRUE)
    dataDirectory <- rbind(dataDirectory, dataFile, deparse.level = 1)
  }
  return(dataDirectory)
}


GetDataByDate <- function(data, date, fun) {
  # Filters data by date.
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #   date: A date as a starting point for comparison.
  #   fun: A comparable function.
  #
  # Returns:
  #   A data frame that contains all the data that satisfies comparing it with the date using fun.
  return(data[fun(as.POSIXct(data$V1, format = "%m/%d/%Y %H:%M:%S"), date), ])
}

GetDateByRange<-function(data,initialDate,finalDate){
  # Filters data by range.
  #
  # Args:
  #   data: Data from accounting logs of torque.
  #   initialDate: Start range.
  #   finalDate: Complete range.
  #
  # Returns:
  #   A data frame that contains all the data contained in the range
  data<-as.data.frame(subset(as.POSIXct(data$V1, format = "%m/%d/%Y %H:%M:%S"), 
                             as.POSIXct(data$V1, format = "%m/%d/%Y %H:%M:%S") > 
                             as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S")&
                             as.POSIXct(data$V1, format = "%m/%d/%Y %H:%M:%S") < 
                             as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S")
                             )       
                      )
  colnames(data)<-c("V1")
  return(data)
}

GetDateAndMessageByRange<-function(data,initialDate,finalDate){
  # Filters data by time range, return dates and messages
  #
  # Args:
  #   data: Data from accounting logs of torque.
  #   initialDate: Start range.
  #   finalDate: Complete range.
  #
  # Returns:
  #   A data frame that contains all the data and messages contained in the range
  data$V1<-as.POSIXct(data$V1, format = "%m/%d/%Y %H:%M:%S")
  data<-as.data.frame(subset(data,
                               data$V1 > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S")&
                               data$V1 < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"), 
                               select = c(V1,V4))
                     )
  return(data)
}

GetUserFromMessage <- function(message) {
  # Extracts the username from a message 
  # 
  # Args:
  #   message: A message from accounting logs from torque. The fourth column
  #           of a line from a accounting log file.
  #
  # Returns:
  #   The string of he username if it is present
  userExpr <- "user=[a-z]+"
  m <- regexpr(userExpr, message, perl = TRUE)
  match <- regmatches(message, m)
  return(substring(match, nchar("user=") + 1))
}

GetRequestorFromMessage <- function(message) {
  # Extracts the requestor user from a message 
  # 
  # Args:
  #   message: A message from accounting logs from torque. The fourth column
  #           of a line from a accounting log file.
  #
  # Returns:
  #   The string of he requestor user if it is present
  userExpr <- "requestor=[a-z]+"
  m <- regexpr(userExpr, message, perl = TRUE)
  match <- regmatches(message, m)
  return(substring(match, nchar("requestor=") + 1))
}

GetJobnameFromMessage <- function(message) {
  # Extracts the jobname from a message 
  # 
  # Args:
  #   message: A message from accounting logs from torque. The fourth column
  #           of a line from a accounting log file.
  #
  # Returns:
  #   The string of he jobname if it is present
  jobnameExpr <- "jobname=[a-zA-Z.0-9]+"#Falta añadir la diagonal
  m <- regexpr(jobnameExpr, message, perl = TRUE)
  match <- regmatches(message, m)
  return(substring(match, nchar("jobname=") + 1))
}

PlotDataSummary <- function(data) {
  # Plots a histogram of the data classified by message type. The plot is 
  # generated using ggplot2.
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #
  # Returns:
  #   The plot.
  qplot(V2, data=data, geom = "histogram")
}

PlotHistRequestors <- function(data) {
  # Plots a histogram of the requestors appearing in the data. The plot is 
  # generated using ggplot2.
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #
  # Returns:
  #   The plot.
  requestors <- data.frame(lapply(data[4], GetRequestorFromMessage))
  qplot(V4, data=requestors, geom = "histogram")
}

PlotHistUsers <- function(data) {
  # Plots a histogram of the users appearing in the data. The plot is 
  # generated using ggplot2.
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #
  # Returns:
  #   The plot.
  users <- data.frame(lapply(data[4], GetUserFromMessage))
  qplot(V4, data=users, geom="histogram")
}


PlotHistJobs <- function(data) {
  # Plots a histogram of the jobs appearing in the data. The plot is 
  # generated using ggplot2.
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #
  # Returns:
  #   The plot.
  jobsnames <- data.frame(lapply(data[4], GetJobnameFromMessage))
  qplot(V4, data=jobsnames, geom = "histogram")
}

GetUserJobs <- function (user.name, data) {
  # Get all the jobs belonging to a user
  # 
  # Args:
  #   user.name: Name of the user.
  #   data: Data from accounting logs of torque.
  #
  # Returns:
  #   A table that contains all the jobs belonging to the user.
  userString <- paste("user=", user.name, sep = "")
  userJobs <- data[grep(userString, data$V4), ]
  userJobsname <- table(lapply(userJobs[4], GetJobnameFromMessage))
 return(userJobsname)
}

GetJobsnames <- function(data) {
  # Get all the jobs from the data
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #
  # Returns:
  #   A table that contains all the jobs from the data.
  return(table(lapply(data[4], GetJobnameFromMessage)))
}

GetDataByType <- function(data, type) {
  # Filters the data by type of Record Marker
  # 
  # Args:
  #   data: Data from accounting logs of torque.
  #   type: Record marker type. 
  #
  # Returns:
  #   A table that contains all the data of a single type.s
  dataByType <- switch(type, "A" = data[data$V2 == "A", ],
                       "C" = data[data$V2 == "C", ], "D" = data[data$V2 == "D", ],
                       "E" = data[data$V2 == "E", ], "Q" = data[data$V2 == "Q", ], 
                       "R" = data[data$V2 == "R", ], "S" = data[data$V2 == "S", ],
                       "T" = data[data$V2 == "T", ])
  return(dataByType)
}


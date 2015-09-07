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
  colnames(data)<-c("date")
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
                             select = c(V1,V2,V3,V4))
  )
  colnames(data)<-c("date","status","Nmingus","message")
  return(data)
}

GetDateMessageAndUserByRange<-function(data,initialDate,finalDate){
  
  data<-GetDateAndMessageByRange(data,initialDate,finalDate)
  userExpr <- "user=[a-zA-Z.0-9]+"
  m <- regexpr(userExpr,data$message, perl = TRUE)
  data <- cbind(data,m)
  date<-subset(data, data$m != (-1),select = c("date","status","Nmingus","message","m"))
  user <- regmatches(data$message, data$m)
  user <- substring(user, nchar("user=") + 1)
  data <- cbind(date,user)
  data$m<-NULL
  return(data)
}

GetDateMessageUserAndJobByRange<-function(data,initialDate,finalDate){
  
  data<-GetDateMessageAndUserByRange(data,initialDate,finalDate)
  jobnameExpr <- "jobname=[a-zA-Z.0-9]+"
  m <- regexpr(jobnameExpr, data$message, perl = TRUE)
  data <- cbind(data,m)
  date <- subset(data, data$m != (-1),select = c("date","status","Nmingus","message","user","m"))
  jobname <- regmatches(data$message, data$m)
  jobname <- substring(jobname, nchar("jobname=") + 1)
  data <- cbind(date,jobname)
  data$m <- NULL
  return(data)
}

GetDateMessageUserJobAndNcupsByRange<-function(data,initialDate,finalDate){
  data<-GetDateMessageUserAndJobByRange(data,initialDate,finalDate)
  ncpuExpr <- "source[_]List[.]ncpus=[0-9]{1,3}"
  m <- regexpr(ncpuExpr,data$message, perl = TRUE)
  data <- cbind(data,m)
  date <- subset(data, data$m != (-1),select = c(date,status,Nmingus,message,user,jobname,m))
  Ncpu <- regmatches(data$message, data$m)
  Ncpu <- substring(Ncpu, nchar("source_List.ncpus=") + 1)
  data <- cbind(date,Ncpu)
  data$m <- NULL
  return(data)
}

GetDateMessageUserJobNcupsAndPpnByRange<-function(data,initialDate,finalDate){
  data<-GetDateMessageUserJobAndNcupsByRange(data,initialDate,finalDate)
  exehostExpr="exec[_]host=node[0-9]{1,3}[/][0-9]{1,3}"
  m <- regexpr(exehostExpr,data$message, perl = TRUE) 
  data<-cbind(data,m)
  date <- subset(data, data$m != (-1),select = c(date,status,Nmingus,message,user,jobname,Ncpu,m))
  Node <- regmatches(data$message,data$m)
  Node <- substring(Node, nchar("exec_host=node")+1)
  x<-regexpr("[/]",Node)
  Ppn<-substring(Node,(x+1))
  Node<-substring(Node,1,(x-1))
  data<-cbind(date,Node)
  data<-cbind(date,Ppn)
  data$m<-NULL
  return(data)
}

GetDateMessageUserJobNcupsPpnAndMemByRange<-function(data,initialDate,finalDate){
  data<-GetDateMessageUserJobNcupsAndPpnByRange(data,initialDate,finalDate)
  memExpr<-"resources[_]used.mem=[0-9]+[a-zA-Z]+"
  m<-regexpr(memExpr,data$message,perl=TRUE)
  data<-cbind(data,m)
  date <- subset(data, data$m != (-1),select = c(date,status,Nmingus,message,user,jobname,Ncpu,Ppn))
  MemUsedkb <- regmatches(data$message,data$m)
  MemUsedkb <- substring(MemUsedkb, nchar("resources_used.mem=")+1)
  x<-regexpr("[a-zA-Z]+",MemUsedkb)
  MemUsedkb<-substring(MemUsedkb,1,(x-1))
  data<-cbind(date,MemUsedkb)
  data$m<-NULL
  data$message<-NULL
  return(data)
}

dateNcpuStatusPlot<-function(dataset,number,letter)
{
  dataset<-subset(dataset, Ncpu == number & status == letter)
  return(qplot(date,Ncpu,data = dataset,color = status))
}

dateUserJobnameNcpuPlot<-function(dataset,nuser,jname,ncup,lstatus)
{
  dataset<-subset(dataset,user == nuser & jobname == jname & Ncpu == ncpu & status = lstatus)
  return(qplot(date,paste(data$user,data$jobname),data = dataset,color = Ncpu,ylab ="user and jobname"))
}


userJobnameCountStatusPlot<-function(dataset,nuser,jnamen,lstatus)
{
  dataset<-subset(dataset,user == nuser & jobname == jname & Ncpu == ncpu & status = lstatus)
  return(qplot(paste(data$user,data$jobname), data = dataset, geom = "bar",xlab="user and jobname",color=status))
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


GetUsers <- function(data){
  users <- data.frame(lapply(data[4], GetUserFromMessage))
}

PlotHistUsers <- function(users) {
  # Plots a histogram of the users appearing in the data. The plot is 
  # generated using ggplot2.
  # 
  # Args:
  #   users: Users from accounting logs of torque.
  #
  # Returns:
  #   The plot.
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
  #   type: List that contains record marker type. 
  #
  # Returns:
  #   A table that contains all the data of a single type.s
  dataByType <- data[data$V2 == type, ]
  return(dataByType)
}


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
  
  colnames(dataDirectory) <- c("date", "Record" ,"Nmingus", "mensaje")
  return(dataDirectory)
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
  data$date<-as.POSIXct(data$date, format = "%m/%d/%Y %H:%M:%S")
  data<-as.data.frame(subset(data,
                             data$date > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S")&
                               data$date < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"), 
                             select = c(date,Record,Nmingus,mensaje))
  )
  colnames(data)<-c("date","Record","Nmingus","message")
  return(data)
}

GetDateMessageAndUserByRange<-function(data,initialDate,finalDate){
  
  data<-GetDateAndMessageByRange(data,initialDate,finalDate)
  userExpr <- "user=[a-zA-Z.0-9]+"
  m <- regexpr(userExpr,data$message, perl = TRUE)
  data <- cbind(data,m)
  date<-subset(data, data$m != (-1),select = c("date","Record","Nmingus","message","m"))
  user <- regmatches(data$message, data$m)
  user <- substring(user, nchar("user=") + 1)
  data <- cbind(date,user)
  data$m<-NULL
  return(data)
}

GetDateMessageUserAndJobByRange<-function(data,initialDate,finalDate){
  
  data<-GetDateMessageAndUserByRange(data,initialDate,finalDate)
  jobnameExpr <- "jobname=[a-zA-Z.0-9\\-\\/]+"
  m <- regexpr(jobnameExpr, data$message, perl = TRUE)
  data <- cbind(data,m)
  date <- subset(data, data$m != (-1),select = c("date","Record","Nmingus","message","user","m"))
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
  date <- subset(data, data$m != (-1),select = c(date,Record,Nmingus,message,user,jobname,m))
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
  date <- subset(data, data$m != (-1),select = c(date,Record,Nmingus,message,user,jobname,Ncpu,m))
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
  date <- subset(data, data$m != (-1),select = c(date,Record,Nmingus,message,user,jobname,Ncpu,Ppn))
  MemUsedkb <- regmatches(data$message,data$m)
  MemUsedkb <- substring(MemUsedkb, nchar("resources_used.mem=")+1)
  x<-regexpr("[a-zA-Z]+",MemUsedkb)
  MemUsedkb<-substring(MemUsedkb,1,(x-1))
  data<-cbind(date,MemUsedkb)
  data$m<-NULL
  data$message<-NULL
  return(data)
}

dateNcpuRecordPlot<-function(dataset,number,letter,initialDate,finalDate)
{
  dataset<-subset(dataset, Ncpu == number & Record == letter & date > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S") & date < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"))
  return(qplot(date,Ncpu,data = dataset,color = Record))
}

dateUserJobnameNcpuPlot<-function(dataset,nuser,jname,ncpu,lRecord,initialDate,finalDate)
{
  if(!is.null(jname))
  {
    dataset<-subset(dataset,user == nuser & jobname == jname & Ncpu == ncpu & Record == lRecord & date > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S") & date < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"))
  }
  else
  {
    dataset<-subset(dataset,user == nuser & Ncpu == ncpu & Record == lRecord & date > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S") & date < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"))
  }
  
  return(qplot(date,paste(dataset$user,dataset$jobname),data = dataset,color = Ncpu,ylab ="user and jobname"))
}


userJobnameCountRecordPlot<-function(dataset,nuser,jname,lRecord,initialDate,finalDate)
{
  if(!is.null(jname))
  {
    dataset<-subset(dataset,user == nuser & jobname == jname & Record == lRecord & date > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S") & date < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"))
  }
  else
  {
    dataset<-subset(dataset,user == nuser & Record == lRecord & date > as.POSIXct(initialDate, format = "%m/%d/%Y %H:%M:%S") & date < as.POSIXct(finalDate, format = "%m/%d/%Y %H:%M:%S"))
  }
  
  return(qplot(paste(dataset$user,dataset$jobname), data = dataset, geom = "bar",xlab="user and jobname",color=Record))
}



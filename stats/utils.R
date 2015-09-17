kNameExpression <- "[a-zA-Z.0-9\\_\\-@]+"
kFileExpression <- "[a-zA-Z.0-9\\_\\-\\/@]+"
kNumberExpression <- "[0-9]+"
KTimeExpression <- "[0-9]+:[0-9]+:[0-9]+"
KMemAmountExpression <- "[0-9]+(m|M|k|K|g|G|t|T)*(b|B)"

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
  
  colnames(dataDirectory) <- c("Date", "Record" ,"JobID", "Message")
  return(dataDirectory)
}



GetJobsIDsFromData <- function(data){
  unique(data$JobID)
}

GetDataByJobID <- function(data, jobid){
  dataByJob <- data[data$JobID == jobid, ]
  return(dataByJob)
}

GetDataByName <- function (data, user.name) {
  userString <- paste("user=", user.name, sep = "")
  dataByUser <- data[grep(userString, data$Message), ]
  
  requestorString <- paste("requestor=", user.name, sep = "")
  dataByRequestor <- data[grep(requestorString, data$Message), ]
  
  dataByUsername <- rbind(dataByUser, dataByRequestor, deparse.level = 1)
}

GetDataByRangeOfDate <- function(data, date.1, date.2){
  data[date.1 <=  as.POSIXct(data$Date, format = "%m/%d/%Y %H:%M:%S") 
       & date.2 >=  as.POSIXct(data$Date, format = "%m/%d/%Y %H:%M:%S"), ]
}

GetValueFromMessage <- function(message, string.of.value, regular.expression){
  valueExpr <- paste(string.of.value, regular.expression, sep = "")
  m <- regexpr(valueExpr, message, perl = TRUE)
  match <- regmatches(message, m)
  
  return(substring(match, nchar(string.of.value) + 1))
  
}

GetValueTimeFromMessage <- function(message, string.of.value, regular.expression){
  valueTime <- GetValueFromMessage(message, string.of.value, regular.expression)
  as.character(as.POSIXct(strtoi(valueTime, base = 0L), origin= "1970-01-01"))  }

GetUserFromMessage <- function(message) {
  GetValueFromMessage(message, "user=", kNameExpression)
}

GetGroupFromMessage <- function(message){
  GetValueFromMessage(message, "group=", kNameExpression)
}

GetJobnameFromMessage <- function(message) {
  GetValueFromMessage(message, "jobname=", kFileExpression)
}

GetQueueFromMessage <- function(message) {
  GetValueFromMessage(message, "queue=", kNameExpression)
}

GetCTimeFromMessage <- function(message) {
  GetValueTimeFromMessage(message, "ctime=", kNumberExpression)
}

GetQTimeFromMessage <- function(message) {
  GetValueTimeFromMessage(message, "qtime=", kNumberExpression)
}

GetETimeFromMessage <- function(message) {
  GetValueTimeFromMessage(message, "etime=", kNumberExpression)
}

GetStartTimeFromMessage <- function(message) {
  GetValueTimeFromMessage(message, "start=", kNumberExpression)
}

GetEndTimeFromMessage <- function(message) {
  GetValueTimeFromMessage(message, "end=", kNumberExpression)
}

GetOwnerFromMessage <- function(message) {
  GetValueFromMessage(message, "owner=", kNameExpression)
}

GetExecHostFromMessage <- function(message) {
  GetValueFromMessage(message, "exec_host=", kFileExpression)
}

GetListCPUTFromMessage <- function(message) {
  GetValueFromMessage(message, "Resource_List.cput=", KTimeExpression)
}

GetListMemFromMessage <- function(message) {
  GetValueFromMessage(message, "Resource_List.mem=", KMemAmountExpression)
}

GetListNCPUsFromMessage <- function(message) {
  GetValueFromMessage(message, "Resource_List.ncpus=", kNumberExpression)
}

GetListNeedNodesFromMessage <- function(message) {
  GetValueFromMessage(message, "Resource_List.neednodes=", kNameExpression)
}

GetListNodeCTFromMessage <- function(message) {
  GetValueFromMessage(message, "Resource_List.nodect=", kNumberExpression)
}

GetListWallTimeFromMessage <- function(message) {
  GetValueFromMessage(message, "Resource_List.walltime=", KTimeExpression)
}

GetSessionFromMessage <- function(message) {
  GetValueFromMessage(message, "session=", kNumberExpression)
}

GetExitStatusFromMessage <- function(message) {
  GetValueFromMessage(message, "Exit_status=", kNumberExpression)
}

GetUsedCPUTFromMessage <- function(message) {
  GetValueFromMessage(message, "resources_used.cput=", KTimeExpression)
}

GetUsedMemFromMessage <- function(message) {
  GetValueFromMessage(message, "resources_used.mem=", KMemAmountExpression)
}

GetUsedVMemFromMessage <- function(message) {
  GetValueFromMessage(message, "resources_used.vmem=", KMemAmountExpression)
}

GetUsedWalltimeFromMessage <- function(message) {
  GetValueFromMessage(message, "resources_used.walltime=", KTimeExpression)
}

GetRequestorFromMessage <- function(message){
  GetValueFromMessage(message, "requestor=", kNameExpression)
}

GetValuesFromData <- function(data, fun.values){
  values <- unique(lapply(data$Message, fun.values))
  values <- values[lapply(values,length)>0]
  if(length(values) == 0){
    return(list(NA))
  }
  
  return(values)#Tomamos el valor mas reciente
}

GetUsersFromData <- function(data){
  GetValuesFromData(data, GetUserFromMessage)
}

GetGroupsFromData <- function(data){
  GetValuesFromData(data, GetGroupFromMessage)
}

GetJobNamesFromData <- function(data){
  GetValuesFromData(data, GetJobnameFromMessage)
}

GetQueuesFromData <- function(data){
  GetValuesFromData(data, GetQueueFromMessage)
}

GetCTimesFromData <- function(data){
  GetValuesFromData(data, GetCTimeFromMessage)
}

GetQTimesFromData <- function(data){
  GetValuesFromData(data, GetQTimeFromMessage)
}

GetETimesFromData <- function(data){
  GetValuesFromData(data, GetETimeFromMessage)
}

GetStartTimesFromData <- function(data){
  GetValuesFromData(data, GetStartTimeFromMessage)
}

GetOwnersFromData <- function(data){
  GetValuesFromData(data, GetOwnerFromMessage)
}

GetExecHostsFromData <- function(data){
  GetValuesFromData(data, GetExecHostFromMessage)
}

GetListCPUTsFromData <- function(data){
  GetValuesFromData(data, GetListCPUTFromMessage)
}

GetListMemsFromData <- function(data){
  GetValuesFromData(data, GetListMemFromMessage)
}

GetListNCPUsFromData <- function(data){
  GetValuesFromData(data, GetListNCPUsFromMessage)
}

GetListNeedNodesFromData <- function(data){
  GetValuesFromData(data, GetListNeedNodesFromMessage)
}

GetListNodeCTsFromData <- function(data){
  GetValuesFromData(data, GetListNodeCTFromMessage)
}

GetListWallTimesFromData <- function(data){
  GetValuesFromData(data, GetListWallTimeFromMessage)
}

GetSessionsFromData <- function(data){
  GetValuesFromData(data, GetSessionFromMessage)
}

GetEndTimesFromData <- function(data){
  GetValuesFromData(data, GetEndTimeFromMessage)
}

GetExitStatusFromData <- function(data){
  GetValuesFromData(data, GetExitStatusFromMessage)
}

GetUsedCPUTFromData  <- function(data){
  GetValuesFromData(data, GetUsedCPUTFromMessage)
}

GetUsedMemFromData <- function(data){
  GetValuesFromData(data, GetUsedMemFromMessage)
}

GetUsedVMemFromData <- function(data){
  GetValuesFromData(data, GetUsedVMemFromMessage)
}

GetUsedWalltimeFromData <- function(data){
  GetValuesFromData(data, GetUsedWalltimeFromMessage)
}

GetRequestorsFromData <- function(data){
  GetValuesFromData(data, GetRequestorFromMessage)
}

GetDeletedTimesFromData <- function(data){
  dataDeleted <- data[data$Record == "D", ]
  deletedTimes <- lapply(unique(dataDeleted$Date), as.POSIXct, format= "%m/%d/%Y %H:%M:%S" )
  if(length(deletedTimes) == 0){
    return(list(NA))
  }
  return(deletedTimes)
}

GetQueueTimesFromData <- function(data){
  dataQueue <- data[data$Record == "Q", ]
  queueTimes <- lapply(unique(dataQueue$Date), as.POSIXct, format= "%m/%d/%Y %H:%M:%S" )
  if(length(queueTimes) == 0){
    return(list(NA))
  }
  return(queueTimes)
}


GetInformationByJobID <- function(data, jobid){
  dataByJobID <- GetDataByJobID(data, jobid)
  
  jobname <- GetJobNamesFromData(dataByJobID)
  #print("jobname: ")
  #print(jobname)
  
  user <- GetUsersFromData(dataByJobID)
  #print("username: ")
  #print(user)
  
  group <- GetGroupsFromData(dataByJobID)
  #print("group: ")
  #print(group)
  
  
  owner <- GetOwnersFromData(dataByJobID)
  #print("owner:  ")
  #print(owner)
  
  
  queue <- GetQueuesFromData(dataByJobID)
  #print("queue: ")
  #print(queue)
  
  
  start <- GetStartTimesFromData(dataByJobID)[1]
  #print("start: ")
  #print(start)
  
  
  end <- GetEndTimesFromData(dataByJobID)
  #print("end: ")
  #print(end)
  
  
  
  ctime <- GetCTimesFromData(dataByJobID)
  
  #print("ctime: ")
  #print(ctime)
  
  
  qtime <- GetQTimesFromData(dataByJobID)
  if(is.na(qtime)){
    qtime <- GetQueueTimesFromData(dataByJobID)
  }
  #print("qtime: ")
  #print(qtime)
  
  
  etime <- GetETimesFromData(dataByJobID)
  #print("etime: ")
  #print(etime)
  
  
  execHost <- GetExecHostsFromData(dataByJobID)
  #print("exech: ")
  #print(execHost)
  
  
  listCPUT <- GetListCPUTsFromData(dataByJobID)
  #print("listCpuT: ")
  #print(listCPUT)
  
  
  listMem <- GetListMemsFromData(dataByJobID)
  #print("listMem: ")
  #print(listMem)
  
  
  listNCPUs <- GetListNCPUsFromData(dataByJobID)
  #print("listNCPUs: ")
  #print(listNCPUs)
  
  
  listNeedNode <- GetListNeedNodesFromData(dataByJobID)
  #print("listNeedNode: ")
  #print(listNeedNode)
  
  
  listNodeCT <- GetListNodeCTsFromData(dataByJobID)
  #print("listNodeCT: ")
  #print(listNodeCT)
  
  
  listWalltime <- GetListWallTimesFromData(dataByJobID)
  #print("listWalltime: ")
  #print(listWalltime)
  
  
  session <- GetSessionsFromData(dataByJobID)
  #print("session: ")
  #print(session)
  
  
  exitStatus <- GetExitStatusFromData(dataByJobID)
  #print("exitStatus: ")
  #print(exitStatus)
  
  
  usedCPUT <- GetUsedCPUTFromData(dataByJobID)
  #print("usedCPUT: ")
  #print(usedCPUT)
  
  
  usedMem <- GetUsedMemFromData(dataByJobID)
  #print("usedMem: ")
  #print(usedMem)
  
  
  usedVMem <- GetUsedVMemFromData(dataByJobID)
  #print("usedVMem: ")
  #print(usedVMem)
  
  
  usedWalltime <- GetUsedWalltimeFromData(dataByJobID)
 # print("usedWalltime: ")
#  print(usedWalltime)
  
  
  requestor <- GetRequestorsFromData(dataByJobID)
  
  dtime <- GetDeletedTimesFromData(dataByJobID)[1]

  information <- data.frame(jobid,jobname, user, group, owner, queue, start, end, ctime, qtime, etime, execHost, listCPUT, listMem,listNCPUs, listNeedNode, listNodeCT, listWalltime, session, exitStatus, usedCPUT, usedMem, usedVMem, usedWalltime, requestor, dtime)
  colnames(information) <- c("Jobid","Jobname", "User", "Group", "Owner", "Queue", "Start", "End", "ctime", "qtime", "etime", "execHost", "listCPUT", "listMem","listNCPUs", "listNeedNode", "listNodeCT", "listWalltime", "session", "exitStatus", "usedCPUT", "usedMem", "usedVMem", "usedWalltime", "requestor", "dtime")
  return(information)
}

GetPreprocessedData <- function(data){
  ids <- GetJobsIDsFromData(data)
  preprocessedData <- data.frame()
  for (i in ids) {
    preprocessedData <- rbind(preprocessedData, GetInformationByJobID(data, i), deparse.level = 1)
  }
  return(preprocessedData)
}

StringDateIsOnTheRange <- function(string.date, initial.date, final.date){
  date <- as.POSIXct(string.date, "%Y-%m-%d %H:%M:%S")
  return(date >= initial.date & date <= final.date)
  
}

GetPreprocessedDataByDate <- function(preprocessed.data, initial.date, final.date){
  preprocessedDataByDate <- preprocessed.data[StringDateIsOnTheRange(preprocessed.data$Start,
                                                                     initial.date, final.date)
                                              | StringDateIsOnTheRange(preprocessed.data$dtime,
                                                                       initial.date, final.date)
                                              | StringDateIsOnTheRange(preprocessed.data$qtime,
                                                                       initial.date, final.date)
                                              | StringDateIsOnTheRange(preprocessed.data$End,
                                                                       initial.date, final.date), ]
}

CountNumberOfJobs <- function(preprocessed.data){
  length(unique(preprocessed.data$Jobid))
}

GetUsers <- function(preprocessed.data){
  return(sort(unique(preprocessed.data$User)))
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
  qplot(Record, data=data, geom = "histogram")
}

PlotUserActivity <- function(preprocessed.data){
  users <-na.exclude(preprocessed.data$User)
  
  qplot(users, data = data.frame(users), geom = "histogram")
}


#fecha1 <- as.POSIXct("10/02/2014 00:00:00", format = "%d/%m/%Y %H:%M:%S")
#fecha2 <- as.POSIXct("09/14/2014 23:59:59", format = "%m/%d/%Y %H:%M:%S")

#accounting <- ReadDirectory("Desktop/accounting/")
#predata <- GetPreprocessedData(accounting)
#D <- GetPreprocessedDataByDate(predata, fecha1, fecha2)


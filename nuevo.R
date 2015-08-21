library(ggplot2)

read_file <- function(name_file){
  data_f <- read.table(name_file, sep=";", flush=TRUE)
  return(data_f)
}

read_directory <- function(name_directory){
  data_directory <- data.frame()
  files <- list.files(name_directory)
  for(i in files){
    data_file <- read_file(paste(name_directory,i, sep= ""))
    data_directory <- rbind(data_directory, data_file, deparse.level = 1)#, make.row.names = TRUE)
  }
  return(data_directory)
}

get_data_by_date <- function(data, date, fun){
  messages[fun(as.POSIXct(data$V1, format = "%m/%d/%Y %H:%M:%S"), date),]
}

get_user_from_message <- function(message){
 user_expr <- "user=[a-z]+"
 m <- regexpr(user_expr, message, perl=TRUE)
 match <- regmatches(message, m)
 return(substring(match, nchar("user=") + 1))
}

get_requestor_from_message <- function(message){
  user_expr <- "requestor=[a-z]+"
  m <- regexpr(user_expr, message, perl=TRUE)
  match <- regmatches(message, m)
  return(substring(match, nchar("requestor=") + 1))
}

get_jobname_from_message <- function(message){
  jobname_expr <- "jobname=[a-zA-Z.0-9]+"#Falta añadir la diagonal
  m <- regexpr(jobname_expr, message, perl=TRUE)
  match <- regmatches(message, m)
  return(substring(match, nchar("jobname=") + 1))
}

plot_data_summary <- function(messages){
  qplot(V2, data=messages, geom="histogram")
  
}

plot_hist_requestors <- function(data){
  requestors <- data.frame(lapply(data[4],get_requestor_from_message))
  qplot(V4, data=requestors, geom="histogram")
}

plot_hist_users <- function(data){
  users <- data.frame(lapply(data[4],get_user_from_message))
  qplot(V4, data=users, geom="histogram")#Add log
}


plot_hist_jobs <- function(data){
  jobsnames <- data.frame(lapply(data[4],get_jobname_from_message))
  qplot(V4, data=jobsnames, geom="histogram")
}

plot_data_summary(accounting)
plot_hist_requestors(accounting)
plot_hist_users(accounting)
plot_hist_jobs(accounting)



prueba <- data.frame(lapply(data[4],get_user_from_message))


get_user_jobs <- function (user_name, all_jobs){
  user_string <- paste("user=", user_name, sep="")
  user_jobs <- all_jobs[grep(user_string, all_jobs$V4),]
  user_jobsname <- table(lapply(user_jobs[4],get_jobname_from_message))
 return(user_jobsname)
}

get_jobsnames <- function(messages){
  return(table(lapply(messages[4],get_jobname_from_message)))
}

get_messages_by_type <- function(messages, type){
  messages_type <- switch(type, "A" = messages[messages$V2=="A",], "C" = messages[messages$V2=="C",], "D" = messages[messages$V2=="D",], "E" = messages[messages$V2=="E",], "Q" = messages[messages$V2=="Q",], "R" = messages[messages$V2=="R",], "S" = messages[messages$V2=="S",], "T" = messages[messages$V2=="T",])
  return(messages_type)
}

get_users_by_message_type <- function(messages, type){
  messages_by_type <- get_messages_by_type(messages, type)
  users_names <- table(lapply(messages_by_type[4],get_user_from_message)) 
  return(users_names)
}


#prueba <- data[,c('V2', 'V4')]


accounting <- read_directory("/home/ismael/Desktop/accounting/")

data <- set_user_to_message(data)

plot_summary(data)

started_jobs <- get_messages_by_type(data, "S")


started_jobs
num_users_started_jobs <- get_users_by_message_type(data, "S")

prueba <- data.frame(started_jobs, user= get_user_from_message(started_jobs$V4))
qplot(user, data=prueba, geom="histogram")



num_users_started_jobs

get_user_jobs("natorro", data)
names <- get_jobsnames(data)

last_christmas_date <- as.POSIXct("12/25/2014 08:32:07", format = "%m/%d/%Y %H:%M:%S")

messages_since_last_christmas <- get_messages_by_date(data, last_christmas_date, `<`)



prueba <- as.data.frame(occurences, row.names = NULL,responseName = "Freq", stringsAsFactors = TRUE,sep = "", base = list(LETTERS))
ggplot(data=prueba, aes(x=Var1, y=Freq)) + geom_line()

qplot(x = Var1,  data=prueba)
qplot(V2, data=data, geom="histogram")



get_user

prueba

midataframe <- data.frame(a=c(1,2,3), b=c(2,4,6))
data.frame(midataframe, c=midataframe$a)

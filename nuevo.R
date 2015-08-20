


read_file <- function(name_file){
  data_f <- read.table(name_file, sep=";", flush=TRUE)
  return(data_f)
}

read_directory <- function(name_directory){
  data_directory <- data.frame()
  files <- list.files(name_directory)
  for(i in files){
    data_file <- read_file(paste(name_directory,i, sep= ""))
    data_directory <- rbind(data_directory, data_file, deparse.level = 1, make.row.names = TRUE)

  }
  return(data_directory)
}

get_summary <- function(messages){
  return(table(unlist(messages[2])))
}

get_user_from_message <- function(message){
 user_expr <- "user=[a-z]+"
 m <- regexpr(user_expr, message, perl=TRUE)
 match <- regmatches(message, m)
 return(substring(match, nchar("user=") + 1))
}

get_jobname_from_message <- function(message){
  jobname_expr <- "jobname=[a-zA-Z.0-9]+"#Falta añadir la diagonal
  m <- regexpr(jobname_expr, message, perl=TRUE)
  match <- regmatches(message, m)
  return(substring(match, nchar("jobname=") + 1))
}

get_user_jobs <- function (user_name, all_jobs){
  user_string <- paste("user=", user_name, sep="")
  user_jobs <- all_jobs[grep(user_string, all_jobs$V4),]
  user_jobsname <- table(lapply(user_jobs[4],get_jobname_from_message))
 return(user_jobsname)
}

get_jobsnames <- function(messages){
  return(table(lapply(messages[4],get_jobname_from_message)))
}

get_messages_from_date <- function(messages, date){
  
}


data <- read_directory("/home/ismael/Desktop/accounting/")
occurences <- get_summary(data)

occurences


#jobs <- table(unlist(data[3]))

#prueba <- data[,c('V2', 'V4')]

#jobs

started_jobs <- data[data$V2=="S",]
tail(num_users_started_jobs)

num_users_started_jobs <- table(lapply(started_jobs[4],get_user_from_message))
get_user_jobs("natorro", data)
names <- get_jobsnames(data)



strptime("02/10/2014 16:07:58", format = "%d/%m/%Y %H:%M:%S")

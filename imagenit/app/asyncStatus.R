# async file status helpers --------------------

# inspired by  http://blog.fellstat.com/?p=407
# Status File

# setup status keeping environment and pass data object (list)
# to store required identifying information:
# for now, nothing but location of the file 
statusSetup <- function(){
  status_file <- tempfile()
  print(paste("async status stored in:",status_file))
  write ("initializing",status_file)
  list(status_file = status_file)
}


get_status <- function(statInfo){
  scan(statInfo$status_file, what = "character",sep="\n", quiet=TRUE)
}

set_status <- function(statInfo, msg){
  #print(msg)
  write(msg,statInfo$status_file)
  statInfo
}

fire_interrupt <- function(statInfo){
  set_status(statInfo, "interrupt")
  statInfo
}

fire_ready <- function(statInfo){
  set_status(statInfo,"Ready")
  statInfo
}

fire_running <- function(statInfo, perc_complete){
  if(missing(perc_complete))
    msg <- "Running..."
  else
    msg <- paste0("Running... ", perc_complete, "% Complete")
  set_status(statInfo, msg)
}

interrupted <- function(statInfo){
  get_status(statInfo) == "interrupt"
}


# Delete file at end of session
statusCleanUp <- function(statInfo){
  print(paste("statusCleanUp, deleting:", statInfo$status_file))
  if(file.exists(statInfo$status_file))
    unlink(statInfo$status_file)
}


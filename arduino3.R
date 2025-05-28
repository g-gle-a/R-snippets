library(serial)

initCon <- function() {
  port <- listPorts()[1]
  cat('Opening port', port, '\n')
  gsenser <-""
  road <-""
  con <- serialConnection(name = "testcon", port = port
                           ,mode = "9600,n,8,1"
                           ,newline = 1
                           ,translation = "crlf")
  
  open(con)
  cat('Attempting to check GSenser...')
  while (!grepl("GSenser.",gsenser) )  {gsenser <-read.serialConnection(con)}
  cat('DONE')
  return(con)
}

# cmd  The string command to send to the arduino
# onSuccess The function which will be called if the send command succeed
# onError A function called in case of an error happens
createCommand <- function(cmd, onSuccess, onError = function(...) {print(paste('[ERROR]', list(...)))}) {
   return(list( cmd = cmd, onSuccess = onSuccess, onError = onError))
}

# send a command to the arduino
# commands are created with the createCommand function
sendCommand <- function(command, con) {
  tryCatch({
    cat('Sending command:', command$cmd, '\n')
    write.serialConnection (con, command$cmd)
    data <- ''
    while ( data == '' )  {      # read until we get something
      data <-read.serialConnection(con)
      Sys.sleep(0.2)
    }
    cat('Data read:', data, '\n')
     #command$onSuccess(data)
    b<<- grepl (command$cmd, commands)
    len <- length(commands)
    if (!b[len])
    {
       for (i in 1: (len-1)){
         cat("i", i, "b[",i,"]=", b[i], '\n')
        if (b[i])
        {
          sendCommand (commands[[i+1]],con)
          return()
          i<-len
        }  
       }
    }  
    command$onSuccess(data)
    return()
  
  }, 
  error = function(e) { 
    cat('---From sendCommand()---\n'); 
    command$onError(e)
  }
  )
}

sendListOfCommands <- function(cmdList, con) {
  commands <<- lapply(cmdList, createCommand, 
                      onSuccess = function(...) {
                       # sendCommand(commands[[1]], con)
                        print(paste('Last command result:', list(...)))
                      })
  sendCommand(commands[[1]], con)
  #cat('cmd[ 1 ] =', commands[[1]]$cmd, ', ')
  #for (k in 1:(length(commands) - 1)) {
    #cat
   # nextIter <- paste(k + 1)
    #commands[[k]]$onSuccess <- function(...) {
     # cat('Calling command =>', commands[[k + 1]]$cmd, '\n')
    #  sendCommand(commands, con)
    #}
 # }
  
  # call the first to trigger the sequential execution
  #sendCommand(commands[[1]])
  #closecommands
}

con <- initCon()
sendListOfCommands (c("POT","chans: 2 1 3", "delay: 2000", "points: 100", "START"),con)

ds.login <- function(loginfile,variables=NULL,label="D"){

  # if the user does not specify variables (default behaviour)
  # display a message telling the user that the whole dataset
  # will be assigned since he did not specify variables
  if(is.null(variables)){
    cat("\n  No variables have been specified. \n  All the variables in the opal datasource \n  (the whole dataset) will be assigned to R!\n\n")
    var <- 0
  }else{
    var <- 1
  }

  # load the required libraries
  # login the file that holds the login details
  # to do anything in datashield you need to load 
  # this library so the login stage is the right place
  library(opal)
  
  # load the file that contains the login details
  logins <- read.table(loginfile, header=T, sep=",")
  
  # URLs 
  urls <- as.character(logins[,2])
  
  # usernames
  userids <- as.character(logins[,3])
  
  # passwords
  pwds <- as.character(logins[,4])
  
  # opal directories where the microdata is stored
  paths <- as.character(logins[,5])
  
  # put the server names in a list
  opals <- as.list(as.character(logins$servers))
  
  # login to the oplas keeping the server names as 
  # specified in the login file
  cat("\nLog in to the collaborating servers\n\n")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  for(i in 1:length(opals)) {
     opals[[i]] <- opal.login(userids[i], pwds[i], urls[i],
                   opts=list(ssl.verifyhost=0,ssl.verifypeer=0))
  }
  
  # assign data to the opal server you logged in to
  # if no variables are specified the whole dataset is assigned
  # i.e. all the variables in the opal database are assigned
  cat("Assigining data\n\n")
  for(i in 1:length(opals)) {
    if(var==0){
      datashield.assign(opals[[i]], label, paths[i])
    }else{
      datashield.assign(opals[[i]], label, paths[i], variables)
    }
  }
  
  # display the name of the variable that have been assigned
  # since variables have the same name in all servers just display 
  # names for one server only
  cat("Variables assigned:\n")
  var.names <- datashield.aggregate(opals[[1]], quote(colnames(as.name(label))))
  cat(paste(var.names,collapse=", "),"\n\n")
  # return the 'opals' object
  return(opals)
}

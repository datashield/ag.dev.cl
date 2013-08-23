
#' @title Checks if all variables do exist and are not empty
#' @description This function check that the variables to analyse are (1) available from all 
#' the studies and (2) that they do not contain only missing values (NAs). It excludes studies 
#' that fail any of these two checks
#' @param opals a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources.
#' @param variables the variables to check 
#' @return the opal objects which passed the checks
#' @author Gaye, A.

ag.ds.checkvar <- function(opals, variables){
  
  # print a message for the user informing of checks
  cat("\nChecks are carried out on the variables used for the analysis\nto ensure they are available from the dataset(s) and not empty\n\n")
  # get the names of the opal servers/studies
   stdname <- names(opals)
   
  # a list that keeps the results of the checks for each datasets and each variable
  keeptrack <- vector("list", length(opals))
  
  # loop through the dataset(s) and the variable(s)
  for(i in 1: length(opals)){
    for(j in 1: length(variables)){
      cally <- call("ag.checkvar1.ds", quote(D), variables[j]) 
      checkres <- datashield.aggregate(opals[i], cally)
      if(checkres[[1]] == 1) { cat("The variable", variables[j], "is missing from", stdname[i],"!\n") }
      if(checkres[[1]] == 2) { cat("The variable", variables[j], "in", stdname[i], "is empty (NAs only)!\n") }
      keeptrack[[i]][j] <- checkres[[1]]
    }
    if(sum(keeptrack[[i]]) > 0){ cat(stdname[i], "will not be included in the analysis\n\n") }
  }
  
  # remove studies which contain one or more variables that failed the checks
  idx1 <- c()
  for(i in 1:length(keeptrack)){
    if(sum(keeptrack[[i]]) > 0){
      idx1 <- append(idx1, 1)
    }else{
      idx1 <- append(idx1, 0)
    }
  }
  idx2 <- which(idx1 == 1)
  if(length(idx2) > 0){ opals <- opals[-idx2] }
  
  # If none of the datasets passed the checks stop the process
  # ortherwise return the opal objects that passed the checks
  if(length(opals) == 0){
    stop("The variables specified in the arguments are not available or contain only missing values!")
  }else{
    return(opals)
  }
}
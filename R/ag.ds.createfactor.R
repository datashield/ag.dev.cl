#' 
#' @title create and combines factor vectors
#' @description This function calls a server function that produces  
#' vectors into \code{factor} vectors. The factors can be processed separately or
#' concatenated into one vectors.
#' @param opals a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources. 
#' @param xvect a numeric of character vector
#' @param type tells if the factor vectors should be combined into one vector 
#' (\code{type} = "combine") or not (\code{type} = "split")
#' @return a list of \code{factor} vectors or one factor vector
#' @author Gaye, A.
#  @export
#'
ag.ds.createfactor <- function(opals=NULL, xvect=NULL, type="split"){
  
  if(is.null(opals)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric of character vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # get the names of the studies/opals and the name of the variable   
  stdnames <- names(opals)
  var <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  
  # call the function that checks the variables are available and not empty
  opals <- ag.ds.checkvar(opals, var)
  
  # call the server side function and generate the factors
  valid.opals <- vector("list")
  for(i in 1:length(opals)){
    cally <- call("ag.createfactor.ds", xvect) 
    out <- datashield.aggregate(opals[1], cally)
    
    # the function 'createfactor.ds' outputs only valid factors
    # if the factor produced is not valid a string is returned
    # so here we check if a factor was return and if yes we store it
    # otherwise a message is displayed to tell that the study/opal was ignored
    if(is.factor(out)){
      valid.opals[[i]] <- out
    }else{
      cat("\nthe variable", var, "in", stdnames[i], "is not a valid factor vector\nand will hence not be included in the output\n")
    }
  }
  
  # return the right output depending on the value of the argument 'type'
  if(type=="split"){
    return(valid.opals)
  }else{
    global.var <- unlist(valid.opals)
    return(global.var)
  }
  
}
#'
#' @title Checks that all studies have the variables included in the glm \code{formula}
#' @description The combined glm fit fails if the outcome variable or one of the covariates
#' is missing from any of the studies. This function is called by the function \code{ds.glm}
#' to ensure that only studies/opals that contains all the required variables are included in
#' the analysis. If a study is excluded an alert is displayed to inform the user.
#' This function is internal (not available to users) as it is only required by \code{ds.glm}.
#' @param opals a character strings that represent the URL of the servers where 
#' the study datasets are stored.
#' @param formula an object of class \code{formula} which describes the model to be fitted
#' @return an object of class \code{opal}, the input object (if no study has been excluded)
#' or an updated one if some study(ies) has been excluded due to missing variables.
#' @author Gaye, A.
#' 
ag.ds.varexist1 <- function(opals, formula){

   # get the names of the variables from the formula
   xx <- all.vars(formula)
   variables <- xx[-1]
   
   # check if these variables are available from each study and keep records
   misngvar <- vector("list", length(opals))
   
   for(i in 1:length(opals)){
     # get names of the variables that have been assigned from this opal
     var.names <- datashield.aggregate(opals[[i]], quote(colnames(D)))
     tagtemp <- vector("numeric", (length(variables)))
     for(j in 1:length(variables)){
       tagtemp[j] <- variables[j] %in% var.names
       if(!(tagtemp[j])){ misngvar[[i]] <- append(misngvar[[i]], variables[j]) }
     }
     # if any of the variables in the formula is missing tell and removed the study
     # which has missing variables
     if(sum(tagtemp) < length(variables)){ 
       stdname <- names(opals)[i]
       mm <- paste(misngvar[[i]], collapse=", ")
       if(length(misngvar[[i]]) > 1){
         cat("The variables '", mm, "' are missing from ", stdname[i],"\n")
       }else{
         cat("The variable '", mm, "' is missing from ", stdname[i],"\n")       
       }
       cat("This study will not included in the analysis\n")
       # removed the study from the list of opals
       opals <- opals[-i]       
     }
   }
   return(opals)
}


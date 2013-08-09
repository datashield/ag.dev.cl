ag.ds.varexist <- function(opals, formula){

   # get the names of the variables from the formula
   xx <- all.vars(formula)
   variables <- xx[-1]
   
   # check if these variables are available from each study
   misngvar <- vector("list", length(opals))
   for(i in 1:length(opals)){
     # get names of the variables that have been assigned from this opal
     var.names <- datashield.aggregate(opals[[i]], quote(colnames(D)))
     tagtemp <- vector("numeric", (length(variables)))
     for(j in 1:length(variables)){
       tagtemp[j] <- variables[j] %in% var.names
       if(!(tagtemp[j])){ misngvar[[i]] <- append(misngvar[[i]], variables[j]) }
     }
     # if any of the variables in the formula is missing tell
     if(sum(tagtemp) > 0){ 
       stdname <- names(opals)[i]
       mm <- paste(misngvar[[i]], collapse=", ")
       if(length(misngvar[[i]]) > 1){
         cat("The variables '", mm, "' are missing from ", stdname[i],"\n")
       }else{
         cat("The variable '", mm, "' is missing from ", stdname[i],"\n")       
       }
       cat("This study was not included in the analysis\n")
     }
   }
}


#' 
#' @title Plots a histogram 
#' @description This function plots histogram of the given data values.
#' It calls a datashield server side function that produces the
#' histogram objects to plot. The objects to plot do not contain bins with
#' counts < 5. The function allows for the user to plot disctinct histograms
#' (one for each study) or a combine histogram that merges the single plots.
#' @param opals a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources. 
#' @param xvect vector of values for which the histogram is desired.
#' @param type a character which represent the type of graph to display. 
#' If \code{type} is set to 'combine', a histogram that merges the single 
#' plot is displayed. Each histogram is plotted separately if If \code{type} 
#' is set to 'split'.
#' @return one or more histogram plot depending on the argument \code{type}
#' @author Gaye, A.
#' @export
#' @examples {
#' # load that contains the login details
#' data(logindata)
#' 
#' # login and assign specific variable(s)
#' myvar <- list("LAB_TSC")
#' opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)
#' 
#' # Example 1: plot a combined histogram of the variable 'LAB_TSC' - default behaviour
#' ag.ds.histogram(opals=opals, xvect=quote(D$LAB_TSC))
#' 
#' # Example 2: Plot the histograms separately (one per study)
#'  ag.ds.histogram(opals=opals, xvect=quote(D$LAB_TSC), type="split")
#'  
#' # Example 3: Plot the histograms of the first and second study
#'  ag.ds.histogram(opals=opals[1:2], xvect=quote(D$LAB_TSC), type="split")
#'
#' # Example 4: Plot the histogram of the third study only
#'  ag.ds.histogram(opals=opals[3], xvect=quote(D$LAB_TSC), type="split")
#' }

ag.ds.histogram <- function(opals=opals, xvect=NULL, type="combine"){


  if(is.null(opals)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are logged in to valid opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(xvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # a list to hold histogram objects and their break points and densities
  hist.objects <- vector("list", length(opals))
  breaks.all <- c()
  density.all <- c()
  
  # get the name of the variable used for the histogram
  variables <-  strsplit(deparse(xvect), "\\$", perl=TRUE)[[1]][2]
  
  # call the function that checks the variables are available and not empty
  opals <- ag.ds.checkvar(opals, variables)
  
  # call the server side function and generate the histogram objects
  cally <- call("ag.histogram.ds", xvect) 
  output <- datashield.aggregate(opals, cally)
  for(i in 1:length(opals)){
    obj <- output[[i]]
    hist.objects[[i]] <- obj
    breaks.all <- append(breaks.all, hist.objects[[i]]$breaks)
    density.all <- append(density.all, hist.objects[[i]]$density)  
  }

  # get the largest range and align the break points across the individual histograms
  # aligning break points is not just 'nicer' it allows easy visual comparison of graphs
  xlim <- range(breaks.all)
  ylim <- range(0,density.all)  

  # plot the individual histograms on the same graph 
  # some bins might have been combined causing wider class intervals, so the breaks might not be equidistant;
  # if the breaks are not equidistant it is the relative frequencies (probabilities i.e., x$density)
  # that should be plotted. Hence the combined histogram y-axis is the density. If we plot the x$counts
  # (x-axis labelled 'frequency'), the areas/spikes of the histogram will not be proportional to the relative 
  # relative frequency. furthermore frequency is relative to sample size whilst density is not.
  
  # if the argument 'type'="combine" plot a combined histogram and if 'type'="split" plot single histograms separately
  if(type=="combine"){
    colour <- "black"
    par(mfrow=c(1,1))
    plot(hist.objects[[1]],xlim = xlim, ylim = ylim, col = colour,xlab = 'lengths', freq = FALSE, main = 'Histogram of the pooled data')
    if(length(opals) > 1){
      for(i in 2:length(opals)){
        plot(hist.objects[[i]],xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n', col = colour, add = TRUE, freq = FALSE)     
      }
    }
  }else{  
    if(type=="split"){
      # set the graph area and plot
      ll <- length(opals)
      colour <- rainbow(ll)
      if(ll > 1){
        if((ll %% 2) == 0){ numr <- ll/2 }else{ numr <- (ll+1)/2}
        numc <- 2
        par(mfrow=c(numr,numc))
        for(i in 1:ll){
          plot(hist.objects[[i]],xlim = xlim, ylim = ylim, col = colour[i],xlab = 'lengths', freq = FALSE, main = names(opals)[i])
        }
      }else{
        par(mfrow=c(1,1))
        plot(hist.objects[[1]],xlim = xlim, ylim = ylim, col = colour[1],xlab = 'lengths', freq = FALSE, main = names(opals)[1])
      }
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}


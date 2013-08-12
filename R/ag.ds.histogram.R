

ag.ds.histogram <- function(opals=NULL, numvect=NULL, type="combine"){

  if(is.null(opals)){
    cat("\n\n ALERT!\n")
    cat(" No valid opal object(s) provided.\n")
    cat(" Make sure you are looged in opal server(s).\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  if(is.null(numvect)){
    cat("\n\n ALERT!\n")
    cat(" Please provide a valid numeric vector\n")
    stop(" End of process!\n\n", call.=FALSE)
  }
  
  # a list to hold histogram objects and their break points and densities
  hist.objects <- vector("list", length(opals))
  breaks.all <- c()
  density.all <- c()
  
  # call the server side function and generate the histogram objects
  cally <- call("ag.histogram.ds", numvect) 
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
    plot(hist.objects[[1]],xlim = xlim, ylim = ylim, col = colour,xlab = 'lengths', freq = FALSE, main = 'Pooled Data')
    if(length(opals) > 1){
      for(i in 2:length(opals)){
        plot(hist.objects[[i]],xlim = xlim, ylim = ylim, xaxt = 'n', yaxt = 'n', col = colour, add = TRUE, freq = FALSE)     
      }
    }
  }
  
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
  }
}

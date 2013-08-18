#' 
#' @title Generates a heatmap plot for merged datasets
#' @param opals a list of opal object(s) obtained after login in to opal servers;
#' these objects hold also the data assign to R, as \code{dataframe}, from opal 
#' datasources.
#' @param xvect a numerical vector
#' @param yvect a numerical vector
#' @return a heatmap plot
#' @author Isaeva, J.
#' @export
#' 
ag.ds.heatmapplot <- function(opals, xvect, yvect)
{
  
  # define the min and max of the variables across all datasets
  cally <- call("ag.MinMax.ds", xvect, yvect) 
  MinMax.obj <- datashield.aggregate(opals, cally)
  
  num.sources <- length(MinMax.obj)
  
  x.global.min = NULL
  x.global.max = NULL
  y.global.min = NULL
  y.global.max = NULL
  
  for (i in 1:num.sources) {
    x.global.min = c(x.global.min, MinMax.obj[[i]][1,1])
    x.global.max = c(x.global.max, MinMax.obj[[i]][2,1])
    y.global.min = c(y.global.min, MinMax.obj[[i]][1,2])
    y.global.max = c(y.global.max, MinMax.obj[[i]][2,2])
  }
  
  x.global.min = min(x.global.min)
  x.global.max = max(x.global.max)
  y.global.min = min(y.global.min)
  y.global.max = max(y.global.max)
  

  
  # generate the grid density object to plot
  cally <- call("ag.griddensitylim.ds", xvect, yvect, x.global.min, x.global.max, y.global.min, y.global.max) 
  grid.density.obj <- datashield.aggregate(opals, cally)
  
  num.sources <- length(grid.density.obj)
  
  numcol<-dim(grid.density.obj[[1]])[2]
  
  Global.grid.density = matrix(0, dim(grid.density.obj[[1]])[1], numcol-2)
  for (i in 1:num.sources){
    Global.grid.density = Global.grid.density + grid.density.obj[[i]][,1:(numcol-2)]
  }
  
  par(mfrow=c(1,1))
  
  x<-grid.density.obj[[1]][,(numcol-1)]
  y<-grid.density.obj[[1]][,(numcol)]
  z<-Global.grid.density
  
  #   library('fields')
  image.plot(x,y,z, col=heat.colors(50))
  
}

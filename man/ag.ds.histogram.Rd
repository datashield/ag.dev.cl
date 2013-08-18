\name{ag.ds.histogram}
\alias{ag.ds.histogram}
\title{Plots a histogram}
\usage{
  ag.ds.histogram(opals = opals, xvect = NULL,
    type = "combine")
}
\arguments{
  \item{opals}{a list of opal object(s) obtained after
  login in to opal servers; these objects hold also the
  data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{vector of values for which the histogram is
  desired.}

  \item{type}{a character which represent the type of graph
  to display. If \code{type} is set to 'combine', a
  histogram that merges the single plot is displayed. Each
  histogram is plotted separately if If \code{type} is set
  to 'split'.}
}
\value{
  one or more histogram plot depending on the argument
  \code{type}
}
\description{
  This function plots histogram of the given data values.
  It calls a datashield server side function that produces
  the histogram objects to plot. The objects to plot do not
  contain bins with counts < 5. The function allows for the
  user to plot disctinct histograms (one for each study) or
  a combine histogram that merges the single plots.
}
\examples{
{
# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# show the name of the studies/servers
names(opals)

# Example 1: plot a combined histogram of the variable 'LAB_TSC' - default behaviour
ag.ds.histogram(opals=opals, xvect=quote(D$LAB_TSC))

# Example 2: Plot the histograms separately (one per study)
 ag.ds.histogram(opals=opals, xvect=quote(D$LAB_TSC), type="split")

# Example 3: Plot the histograms of the first and second study
 ag.ds.histogram(opals=opals[1:2], xvect=quote(D$LAB_TSC), type="split")

# Example 4: Plot the histogram of the third study only
 ag.ds.histogram(opals=opals[3], xvect=quote(D$LAB_TSC), type="split")
}
}
\author{
  Gaye, A.
}


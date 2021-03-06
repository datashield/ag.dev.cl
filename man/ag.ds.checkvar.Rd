\name{ag.ds.checkvar}
\alias{ag.ds.checkvar}
\title{Checks if all variables do exist and are not empty}
\usage{
  ag.ds.checkvar(opals, variables)
}
\arguments{
  \item{opals}{a list of opal object(s) obtained after
  login in to opal servers; these objects hold also the
  data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{variables}{a character vector, the names of the
  variable(s) to check}
}
\value{
  the opal objects which passed the checks
}
\description{
  This function check that the variables to analyse are (1)
  available from all the studies and (2) that they do not
  contain only missing values (NAs). It excludes studies
  that fail any of these two checks
}
\examples{
{

# load that contains the login details
data(logindata)

# login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvar)

# run checks for the variable LAB_TSC
ag.ds.checkvar(opals=opals, variables=list(quote(D$LAB_TSC)))
}
}
\author{
  Gaye, A.
}


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

  \item{variables}{the variables to check}
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
\author{
  Gaye, A.
}


\name{ag.ds.createfactor}
\alias{ag.ds.createfactor}
\title{create and combines factor vectors}
\usage{
  ag.ds.createfactor(opals = NULL, xvect = NULL,
    type = "split")
}
\arguments{
  \item{opals}{a list of opal object(s) obtained after
  login in to opal servers; these objects hold also the
  data assign to R, as \code{dataframe}, from opal
  datasources.}

  \item{xvect}{a numeric of character vector}

  \item{type}{tells if the factor vectors should be
  combined into one vector (\code{type} = "combine") or not
  (\code{type} = "split")}
}
\value{
  a list of \code{factor} vectors or one factor vector
}
\description{
  This function calls a server function that produces
  vectors into \code{factor} vectors. The factors can be
  processed separately or concatenated into one vectors.
}
\author{
  Gaye, A.
}


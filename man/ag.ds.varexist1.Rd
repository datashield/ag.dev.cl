\name{ag.ds.varexist1}
\alias{ag.ds.varexist1}
\title{Checks that all studies have the variables included in the glm \code{formula}}
\usage{
  ag.ds.varexist1(opals, formula)
}
\arguments{
  \item{opals}{a character strings that represent the URL
  of the servers where the study datasets are stored.}

  \item{formula}{an object of class \code{formula} which
  describes the model to be fitted}
}
\value{
  an object of class \code{opal}, the input object (if no
  study has been excluded) or an updated one if some
  study(ies) has been excluded due to missing variables.
}
\description{
  The combined glm fit fails if the outcome variable or one
  of the covariates is missing from any of the studies.
  This function is called by the function \code{ds.glm} to
  ensure that only studies/opals that contains all the
  required variables are included in the analysis. If a
  study is excluded an alert is displayed to inform the
  user. This function is internal (not available to users)
  as it is only required by \code{ds.glm}.
}
\author{
  Gaye, A.
}


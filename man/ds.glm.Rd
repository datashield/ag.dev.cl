\name{ds.glm}
\alias{ds.glm}
\title{Runs a combined GLM analysis of non-pooled data}
\usage{
  ds.glm(opals, formula, family, maxit = 10)
}
\arguments{
  \item{opals}{a character strings that represent the URL
  of the servers where the study datasets are stored}

  \item{formula}{an object of class \code{formula} which
  describes the model to be fitted}

  \item{family}{a description of the error distribution
  function to use in the model}

  \item{maxit}{the number of iterations of IWLS used}
}
\value{
  coefficients a named vector of coefficients

  residuals the 'working' residuals, that is the residuals
  in the final iteration of the IWLS fit.

  fitted.values the fitted mean values, obtained by
  transforming the linear predictors by the inverse of the
  link function.

  rank the numeric rank of the fitted linear model.

  family the \code{family} object used.

  linear.predictors the linear fit on link scale.

  aic A version of Akaike's An Information Criterion, which
  tells how well the model fits
}
\description{
  This function allows for the fitting of generalized
  linear models to non-pooled data.
}
\details{
  It enables a parallelized analysis of individual-level
  data sitting on distinct servers by sending instructions
  to each computer requesting non-disclosing summary
  statistics. The sumaries are then combined to estimate
  the parameters of the model; these parameters are the
  same as those obtained if the data were 'physically'
  pooled.
}
\examples{
{
# put example here
}
}
\author{
  Burton, P.; Laflamme, P.; Gaye, A.
}


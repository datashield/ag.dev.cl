\name{ds.login}
\alias{ds.login}
\title{Log in and assign variables to R}
\usage{
  ds.login(logins, variables = NULL)
}
\arguments{
  \item{logins}{a dataframe that holds login details}

  \item{variables}{specific variables to assign. If no
  variables are specified (default) the whole dataset is
  assigned.}
}
\value{
  an object of class opal
}
\description{
  This function allows for clients to login to opal servers
  and assign all the data or specific variables from Opal
  datasources to R. The assigned dataframes (one for each
  opal server) are named "D".
}
\details{
  login function required at the start of a DataSHIELD
  process
}
\examples{
{
# load that contains the login details
data(logindata)

# Example 1: login and assign the whole dataset (default)
opals <- ds.login(logins=logindata)

# Example 2: login and assign specific variable(s)
myvariables <- list("LAB_TSC")
opals <- ds.login(logins=logindata,variables=,myvariables)}
}
\author{
  Gaye, A.
}


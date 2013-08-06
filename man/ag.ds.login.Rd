\name{ag.ds.login}
\alias{ag.ds.login}
\title{Log in and assign variables to R}
\usage{
  ag.ds.login(logins, assign = FALSE, variables = NULL)
}
\arguments{
  \item{logins}{a dataframe that holds login details}

  \item{assign}{a boolean which tells whether or not data
  should from the opal datasource to R after login into the
  server(s).}

  \item{variables}{specific variables to assign. If
  \code{assign} is set to FALSE this argument is ignored
  otherwise the specified variables are assign to R. If no
  variables are specified (default) the whole dataset is
  assigned.}
}
\value{
  an object of class opal
}
\description{
  This function allows for clients to login to opal servers
  and (optionaly) assign all the data or specific variables
  from Opal datasources to R. The assigned dataframes (one
  for each opal server) are named "D".
}
\details{
  login function required at the start of a DataSHIELD
  process
}
\examples{
{
# load that contains the login details
data(logindata)

# Example 1: just login (default)
opals <- ag.ds.login(logins=logindata)

# Example 2: login and assign the whole dataset
opals <- ag.ds.login(logins=logindata,assign=TRUE)

# Example 3: login and assign specific variable(s)
myvariables <- list("LAB_TSC")
opals <- ag.ds.login(logins=logindata,assign=TRUE,variables=myvariables)
}
}
\author{
  Gaye, A.
}


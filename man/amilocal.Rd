% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{amilocal}
\alias{amilocal}
\title{Am I running local or on some Server?}
\usage{
amilocal(myuser = "delt")
}
\arguments{
\item{myuser}{character string of system user name.}
}
\value{
TRUE if only 'myuser' is logged in the current system. FALSE otherwise.
}
\description{
If running on some server where other users than the specified username
are logged in, this function returns FALSE.
If running on a system with only the specified user, it returns TRUE.
}

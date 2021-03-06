% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harmonization.R
\name{detectUndefinedValue}
\alias{detectUndefinedValue}
\title{Check if any value has the string UNDEFINED}
\usage{
detectUndefinedValue(df, newvar, oldvar)
}
\arguments{
\item{df}{data frame to be checked}

\item{newvar}{string new variable name with values which have the prefix "UNDEFINED" are expected}

\item{oldvar}{string old variable name before merging}
}
\value{
raises warning if appropriate.
}
\description{
In the process of harmonization, values which were not defined,
receive the prefix "UNDEFINED: ".
This function checks if there are any values with "UNDEFINED" and
raises a warning and prints the undefined values.
}

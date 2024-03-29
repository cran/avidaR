% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_db_summary.R
\name{get_db_summary}
\alias{get_db_summary}
\title{Get database summary}
\usage{
get_db_summary(triplestore)
}
\arguments{
\item{triplestore}{Object of class triplestore_access which manages database
access.}
}
\value{
Data frame: Columns: "data type", "value".
}
\description{
Get a summary of the data stored.
}
\examples{

avidaDB <- triplestore_access$new()

avidaDB$set_access_options(
  url = "https://graphdb.fortunalab.org",
  user = "public_avida",
  password = "public_avida",
  repository = "avidaDB_test"
)

get_db_summary(triplestore = avidaDB)

}

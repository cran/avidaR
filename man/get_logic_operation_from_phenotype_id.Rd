% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_logic_operation_from_phenotype_id.R
\name{get_logic_operation_from_phenotype_id}
\alias{get_logic_operation_from_phenotype_id}
\title{Get the logic operations computed by a digital organism whose genome
encodes a specific phenotype}
\usage{
get_logic_operation_from_phenotype_id(
  phenotype_id = FALSE,
  phenotype_binary = FALSE,
  triplestore
)
}
\arguments{
\item{phenotype_id}{Integer, a vector of integers (from 0 to 511), or a
logical value (if FALSE, the function returns the entire phenotype space).}

\item{phenotype_binary}{Logical value (TRUE/FALSE) to show/hide
phenotype_id in binary notation (FALSE by default).}

\item{triplestore}{Object of class triplestore_access which manages database
access.}
}
\value{
Data frame. Columns: "phenotype_id", "phenotype_binary" (optional),
"equals", "exclusive-or", "not-or", "and-not", "or", "orn-not", "and",
"not-and", "not"
}
\description{
Get the logic operations encoded by a digital organism having
the requested phenotype.
}
\examples{

# Create triplestore object
avidaDB <- triplestore_access$new()

# Set access options
avidaDB$set_access_options(
  url = "https://graphdb.fortunalab.org",
  user = "public_avida",
  password = "public_avida",
  repository = "avidaDB_test"
)

# One phenotype
get_logic_operation_from_phenotype_id(
  phenotype_id = 1,
  phenotype_binary = TRUE,
  triplestore = avidaDB
)

# More than one phenotype
get_logic_operation_from_phenotype_id(
  phenotype_id = c(1,2,3),
  phenotype_binary = TRUE,
  triplestore = avidaDB
)

# All phenotypes
get_logic_operation_from_phenotype_id(triplestore = avidaDB)

}

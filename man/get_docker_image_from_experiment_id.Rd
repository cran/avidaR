% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_docker_image_from_experiment_id.R
\name{get_docker_image_from_experiment_id}
\alias{get_docker_image_from_experiment_id}
\title{Get docker image from experiment}
\usage{
get_docker_image_from_experiment_id(avida_experiment_id, triplestore)
}
\arguments{
\item{avida_experiment_id}{Integer or a vector of integer values.}

\item{triplestore}{Object of class triplestore_access which manages database
access.}
}
\value{
Data frame. Columns: "avida_experiment_id" "docker_image_id" "id" "repo_digest"
}
\description{
get the docker image built to run the experiment, which
guarantees reproducibility .
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

# Single paper
get_docker_image_from_experiment_id(
  avida_experiment_id = 1,
  triplestore = avidaDB
)

# More than one experiment
get_docker_image_from_experiment_id(
  avida_experiment_id = c(1, 2, 3, 4),
  triplestore = avidaDB
)

}

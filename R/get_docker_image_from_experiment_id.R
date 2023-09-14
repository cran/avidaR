#' Get docker image from experiment
#'
#' @description get the docker image built to run the experiment, which
#' guarantees reproducibility .
#'
#' @param avida_experiment_id Integer or a vector of integer values.
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame. Columns: "avida_experiment_id" "docker_image_id" "id" "repo_digest"
#'
#' @examples
#'
#' # Create triplestore object
#' avidaDB <- triplestore_access$new()
#'
#' # Set access options
#' avidaDB$set_access_options(
#'   url = "https://graphdb.fortunalab.org",
#'   user = "public_avida",
#'   password = "public_avida",
#'   repository = "avidaDB_test"
#' )
#'
#' # Single paper
#' get_docker_image_from_experiment_id(
#'   avida_experiment_id = 1,
#'   triplestore = avidaDB
#' )
#'
#' # More than one experiment
#' get_docker_image_from_experiment_id(
#'   avida_experiment_id = c(1, 2, 3, 4),
#'   triplestore = avidaDB
#' )
#'
#' @export

get_docker_image_from_experiment_id <- function(avida_experiment_id, triplestore) {
  # validata params
  validate_param(param = "avida_experiment", value = avida_experiment_id, types = 2)

  # Build query
  query <- paste0(
    "PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
    "PREFIX rdf: <", rdf_prefix(), ">\n",
    "SELECT distinct ?avida_experiment_id ?docker_image_id (concat('https://hub.docker.com/r/', STRBEFORE(str(?repo_tag), ':'), '/tags') AS ?docker_repository_url) (STRAFTER(str(?repo_tag), ':') as ?docker_image_tag) WHERE {\n",
    "  # avida experiment\n",
    "  #avida_experiment#\n",
    "  # docker image\n",
    "  ?docker_image_id ONTOAVIDA:00000196 ?avida_experiment_id .\n",
    "  ?docker_image_id ONTOAVIDA:00000198 ?repo_tag .\n",
    "  ?docker_image_id ONTOAVIDA:00000199 ?digest .\n",
    "}"
  )

  # Replace params
  query <- replace_data(param = "avida_experiment", value = avida_experiment_id, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
  }

  # Return response
  return(response)
}

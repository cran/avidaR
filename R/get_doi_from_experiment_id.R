#' Get doi from experiment
#'
#' @description Get the DOI of the scientific publication that documents the
#' experiment carried out.
#'
#' @param avida_experiment_id Integer or a list of integer values.
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame. Columns: "doi" "avida_experiment_id"
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
#' get_doi_from_experiment_id(
#'   avida_experiment_id = 1,
#'   triplestore = avidaDB
#' )
#'
#' # More than one experiment
#' get_doi_from_experiment_id(
#'   avida_experiment_id = c(1, 2, 4),
#'   triplestore = avidaDB
#' )
#'
#' @export

get_doi_from_experiment_id <- function(avida_experiment_id, triplestore) {
  # validata params
  validate_param(param = "avida_experiment_id", value = avida_experiment_id, types = 2)

  # Build query
  query <- paste0(
    "PREFIX RO: <", ro_prefix(), ">\n",
    "PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
    "SELECT distinct (concat('https://doi.org/', ?doi_path) as ?doi)  ?avida_experiment_id WHERE {\n",
    "  # experiment\n",
    "  #avida_experiment#\n",
    "  ?paper_id RO:0002353 ?avida_experiment_id .\n",
    "  ?paper_id ONTOAVIDA:00000015 ?doi_path .\n",
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

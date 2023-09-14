#' Get experiment from organism
#'
#' @description Get the experiment identifier and description from which a
#' digital organism is derived.
#'
#' @param organism_id Integer or a list of integer values.
#'
#' @param description Logical value (TRUE/FALSE) to show/hide the description of
#' the experiment (FALSE by default).
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame. Columns: "organism_id" "avida_experiment_id"
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
#' # Single organism
#' get_experiment_id_from_organism_id(organism_id = 1, triplestore = avidaDB)
#'
#' # More than one organism
#' get_experiment_id_from_organism_id(
#'   organism_id = c(1, 2, 3),
#'   triplestore = avidaDB
#' )
#'
#' @export

get_experiment_id_from_organism_id <- function(organism_id, triplestore, description = FALSE) {
  # validata params
  validate_param(param = "organism_id", value = organism_id, types = 2)
  validate_param(param = "description", value = description, types = 1)

  # Build query
  query <- paste0(
    "PREFIX IAO: <http://purl.obolibrary.org/obo/IAO_>\n",
    "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
    "PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
    "SELECT distinct ?organism_id ?avida_experiment_id #description# WHERE {\n",
    "  #organism\n",
    "  #organism#\n\n",
    "  #experiment\n",
    "  ?organism_id RO:0002354 ?avida_experiment_id .\n",
    "  ?avida_experiment_id IAO:0000115 ?description .\n",
    "}")

  # Replace params
  query <- replace_data(param = "organism", value = organism_id, query = query)
  query <- replace_data(param = "description", value = description, query = query)

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

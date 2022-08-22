#' Get phenotype from logic operations
#'
#' @description Get the phenotype encoded by the genome of a digital organism that is specified by a unique combination of logic operations.
#'
#' @param logic_operation List of logical functions from the following set:
#' "equals", "exclusive-or", "not-or", "and-not", "or", "orn-not", "and", 
#' "not-and", "not".
#' 
#' @param phenotype_binary Logical value (TRUE/FALSE) to show/hide the phenotype in binary notation (FALSE by default).
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#' 
#' @return Data frame. Columns: "phenotype_id", "phenotype_binary" (optional).
#' 
#' @examples 
#' 
#' # Create triplestore object
#' triplestore <- triplestore_access$new()
#' 
#' # Set access options
#' triplestore$set_access_options(
#'   url = "https://graphdb.fortunalab.org",
#'   user = "public_avida",
#'   password = "public_avida",
#'   repository = "avidaDB_test"
#' )
#' 
#' # Single logic operation
#' get_phenotype_id_from_logic_operation(
#'   logic_operation = "not-or",
#'   triplestore = triplestore
#' )
#' 
#' # More than one logic operation
#' ops <- c("equals", "exclusive or", "not-or", "and-not", "or", "orn-not")
#' 
#' get_phenotype_id_from_logic_operation(
#'   logic_operation = ops,
#'   phenotype_binary = TRUE,
#'   triplestore = triplestore
#' )
#'
#' @export

get_phenotype_id_from_logic_operation <- function(logic_operation, phenotype_binary = FALSE, triplestore) {
  # Validate logic_operation
  validate_logic_operation(logic_operation)

  # Validate params
  validate_param(param = "phenotype_binary", value = phenotype_binary, types = 1)

  p_binary <- logic_operation_to_binary(logic_operation)

  p_id <- paste0("ONTOAVIDA:phenotype_", logic_operation_to_integer(logic_operation))

  # Build sparql query
   query <- paste0("
     PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">
     PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
     PREFIX RO: <http://purl.obolibrary.org/obo/RO_>
     select distinct ?phenotype_id ('", p_binary, "' as ?phenotype_binary)
     where {
         # phenotype
         values ?phenotype_id { ", p_id, " } .
         #logic_operation#
   }")

  # Replace params
  query <- replace_data(param = "logic_operation", logic_operation, query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefix
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("phenotype_binary" = phenotype_binary), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

#' Get tandem repeat from logic operations
#'
#' @description Get the tandem repeat contained in the transcriptome of a
#' digital organism that executes a specific combination of logic operations.
#'
#' @param logic_operation List of logical functions from the following set:
#' "equals", "exclusive or", "not-or", "and-not", "or", "orn-not", "and",
#' "not-and", "not".
#'
#' @param seed_id Integer (from 1 to 1000) or a vector of integer values. This
#' integer is used for starting the pseudo-random number generator that
#' represents the environment experiencing a digital organism. If seed_id value
#' is not specified, it returns data for a single randomly chosen seed_id value
#' (between 1 and 1000).
#'
#' @param tandem_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#'
#' @param tandem_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame. Columns: "seed_id" (optional), "tandem_id", "tandem_seq"
#' (optional), "tandem_pos" (optional).
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
#' # Single logic operation
#' get_tandem_id_from_logic_operation(
#'   logic_operation = "not",
#'   triplestore = avidaDB
#' )
#'
#' # More than one logic operation
#' get_tandem_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   tandem_seq = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seed_1 and seed_2
#' get_tandem_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   tandem_seq = TRUE,
#'   tandem_pos = TRUE,
#'   seed_id = c(1,2),
#'   triplestore = avidaDB
#' )
#'
#' @export

get_tandem_id_from_logic_operation <- function(logic_operation, seed_id = sample(1:1000, 1), tandem_seq = FALSE, tandem_pos = FALSE, triplestore) {
  # Validate logic_opretations
  validate_logic_operation(logic_operation)

  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = 2)
  validate_param(param = "tandem_pos", value = tandem_pos, types = 1)
  validate_param(param = "tandem_seq", value = tandem_seq, types = 1)

  # Get phenotype_id
  phenotype_id <- logic_operation_to_integer(logic_operation)

  # Get tandem from phenotype
  response <- get_tandem_id_from_phenotype_id(phenotype_id = phenotype_id, tandem_seq = tandem_seq, tandem_pos = tandem_pos, seed_id = seed_id, triplestore = triplestore)
  
  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Show/Hide columns
    response <- show_hide_columns(c("phenotype_id" = FALSE), response)
  }

  # Return response
  return(response)
}

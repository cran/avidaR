#' Get transcriptome from logic operations
#'
#' @description Get the transcriptome of a digital organism that executes a
#' specific combination of logic operations for a list of seeds used for
#' starting the pseudo-random number generator (i.e., a set of environments).
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
#' @param transcriptome_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#'
#' @param transcriptome_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
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
#' get_transcriptome_id_from_logic_operation(
#'   logic_operation = "not",
#'   triplestore = avidaDB
#' )
#'
#' # More than one logic operation
#' get_transcriptome_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   transcriptome_seq = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seed_1 and seed_2
#' get_transcriptome_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   seed_id = c(1,2),
#'   transcriptome_seq = TRUE,
#'   transcriptome_pos = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional), "transcriptome_id",
#' "transcriptome_seq" (optional), "transcriptome_pos" (optional)
#'
#' @export

get_transcriptome_id_from_logic_operation <- function(logic_operation, seed_id = sample(1:1000, 1), transcriptome_seq = FALSE, transcriptome_pos = FALSE, triplestore) {
  # Validate logic_operation
  validate_logic_operation(logic_operation)

  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = 2)
  validate_param(param = "transcriptome_pos", value = transcriptome_pos, types = 1)
  validate_param(param = "transcriptome_seq", value = transcriptome_seq, types = 1)

  # Get phenotype_id
  phenotype_id <- logic_operation_to_integer(logic_operation)

  # Submit query by calling get_transcriptome_id_from_phenotype_id() function
  response <- get_transcriptome_id_from_phenotype_id(phenotype_id = phenotype_id, seed_id = seed_id, transcriptome_seq = transcriptome_seq, transcriptome_pos = transcriptome_pos, phenotype_binary = FALSE, triplestore = triplestore)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Show/Hide columns
    response <- show_hide_columns(c("phenotype_id" = FALSE), response)
  }

  # Return response
  return(response)
}

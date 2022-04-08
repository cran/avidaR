#' Get tandem repeat from logic operations
#'
#' @description Get the tandem repeat contained in the transcriptome of a
#' digital organism that executes a specific combination of logic operations.
#'
#' @param logic_operation List of logical functions from the following set:
#' "equals", "exclusive or", "not-or", "and-not", "or", "orn-not", "and",
#' "not-and", "not".
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
#' @param tandem_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' @param tandem_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @return Data frame. Columns: "seed_id" (optional), "tandem_id", "tandem_seq"
#' (optional), "tandem_pos" (optional).
#' 
#' @examples
#' 
#' # Single logic operation
#' get_tandem_id_from_logic_operation(logic_operation = "not")
#' 
#' # More than one logic operation
#' get_tandem_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   tandem_seq = TRUE
#' )
#' 
#' # At seed_1 and seed_2
#' get_tandem_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   tandem_seq = TRUE,
#'   tandem_pos = TRUE,
#'   seed_id = c(1,2)
#' )
#'
#' @export

get_tandem_id_from_logic_operation <- function(logic_operation, seed_id = FALSE, tandem_seq = FALSE, tandem_pos = FALSE) {
  # Validate logic_opretations
  validate_logic_operation(logic_operation)

  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "tandem_pos", value = tandem_pos, types = 1)
  validate_param(param = "tandem_seq", value = tandem_seq, types = 1)

  # Get phenotype_id
  phenotype_id <- logic_operation_to_integer(logic_operation)
  
  # Get tandem from phenotype
  response <- get_tandem_id_from_phenotype_id(phenotype_id = phenotype_id, tandem_seq = tandem_seq, tandem_pos = tandem_pos, seed_id = seed_id)
  
  # Show/Hide columns
  response <- show_hide_columns(c("phenotype_id" = FALSE), response)

  # Return response
  return(response)
}

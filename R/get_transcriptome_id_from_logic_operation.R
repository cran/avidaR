#' Get transcriptome from logic operations
#'
#' @description Get the transcriptome of a digital organism that executes a
#' specific combination of logic operations for a list of seeds used for
#' starting the pseudo-random number generator (i.e., a set of environments).
#'
#' @param logic_operation List of logical functions from the following set:
#' "equals", "exclusive or", "not-or", "and-not", "or", "orn-not", "and",
#' "not-and", "not".
#' @param seed_id Integer (from 1 to 1000), a vector of integer
#' values, or a logical value. This integer is used for starting the
#' pseudo-random number generator that represents the environment experiencing a
#' digital organism. If a logical value is used, TRUE returns data found in all
#' environments and FALSE (by default) returns only distinct data regardless of
#' the seed.
#' @param transcriptome_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' @param transcriptome_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @examples 
#' 
#' # Single logic operation
#' get_transcriptome_id_from_logic_operation(logic_operation = "not")
#' 
#' # More than one logic operation
#' get_transcriptome_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   transcriptome_seq = TRUE
#' )
#' 
#' # At seed_1 and seed_2
#' get_transcriptome_id_from_logic_operation(
#'   logic_operation = c("not", "and"),
#'   seed_id = c(1,2),
#'   transcriptome_seq = TRUE,
#'   transcriptome_pos = TRUE
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional), "transcriptome_id",
#' "transcriptome_seq" (optional), "transcriptome_pos" (optional)
#'
#' @export

get_transcriptome_id_from_logic_operation <- function(logic_operation, seed_id = FALSE, transcriptome_seq = FALSE, transcriptome_pos = FALSE) {
  # Validate logic_operation
  validate_logic_operation(logic_operation)

  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "transcriptome_pos", value = transcriptome_pos, types = 1)
  validate_param(param = "transcriptome_seq", value = transcriptome_seq, types = 1)
  
  # Get phenotype_id
  phenotype_id <- logic_operation_to_integer(logic_operation)
  
  # Submit query by calling get_transcriptome_id_from_phenotype_id() function
  response <- get_transcriptome_id_from_phenotype_id(phenotype_id = phenotype_id, seed_id = seed_id, transcriptome_seq = transcriptome_seq, transcriptome_pos = transcriptome_pos, phenotype_binary = FALSE)

  # Show/Hide columns
  response <- show_hide_columns(c("phenotype_id" = FALSE), response)
  
  # Return response
  return(response)
}

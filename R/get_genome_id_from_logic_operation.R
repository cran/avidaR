#' Get genome from logic operations
#'
#' @description Get the genome of a digital organism that encodes a unique
#' combination of logic operations for a list of seeds used for starting the
#' pseudo-random number generator (i.e., a set of environments).
#'
#' @param logic_operation List of logical operations from the following set:
#' "equals", "exclusive-or", "not-or", "and-not", "or", "orn-not", "and", 
#' "not-and", "not".
#' @param seed_id Integer (from 1 to 1000) or a vector of integer values. This
#' integer is used for starting the pseudo-random number generator that
#' represents the environment experiencing a digital organism. If seed_id value
#' is not specified, it returns data for a single randomly chosen seed_id value
#' (between 1 and 1000).
#' @param genome_seq Logical value (TRUE/FALSE) to show/hide this column
#' ("FALSE" by default).
#' 
#' @examples
#' 
#' # Single logic operation
#' get_genome_id_from_logic_operation(logic_operation = "not-or")
#' 
#' # More than one logic operation
#' get_genome_id_from_logic_operation(logic_operation = c("not", "not-and"))
#' 
#' # At seed_1
#' get_genome_id_from_logic_operation(
#'   logic_operation = c("or", "equals", "and"),
#'   seed_id = 1,
#'   genome_seq = TRUE
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional), "genome_id", "genome_seq" (optional).
#'
#' @export

get_genome_id_from_logic_operation <- function(logic_operation, seed_id = sample(1:1000, 1), genome_seq = FALSE) {
  # Validate logic_opretation
  validate_logic_operation(logic_operation)

  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = 2)
  validate_param(param = "genome_seq", value = genome_seq, types = 1)

  # Get phenotype_id
  phenotype_id <- logic_operation_to_integer(logic_operation)
  
  # Submit query by calling get_genome_id_from_phenotype_id() function
  response <- get_genome_id_from_phenotype_id(phenotype_id = phenotype_id, seed_id = seed_id, genome_seq = genome_seq)
  
  response <- show_hide_columns(c("phenotype_id" = FALSE), response)

  # Return response
  return(response)
}

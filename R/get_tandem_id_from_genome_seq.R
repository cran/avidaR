#' Get tandem repeat from genome sequence
#'
#' @description Get the tandem repeat contained in the transcriptome of a
#' digital organism having a specific linear string of letters representing the
#' instruction codes that make up its genome.
#'
#' @param genome_seq String of letters or a list of strings.
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
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
#' @return Data frame. Column: "seed_id" (optional), "genome_id", "tandem_id",
#' "tandem_seq" (optional), "tandem_pos (optional)."
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
#' # Get sequences for genomes 1 and 2
#' sequence1 <- get_genome_seq_from_genome_id(
#'   1,
#'   triplestore = avidaDB
#' )$genome_seq
#'
#' sequence2 <- get_genome_seq_from_genome_id(
#'   2,
#'   triplestore = avidaDB
#' )$genome_seq
#'
#' # Single genome
#' get_tandem_id_from_genome_seq(
#'   genome_seq = sequence1,
#'   triplestore = avidaDB
#'  )
#'
#' # More than one genome
#' get_tandem_id_from_genome_seq(
#'   genome_seq = c(sequence1, sequence2),
#'   tandem_seq = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seed_1 and seed_2
#' get_tandem_id_from_genome_seq(
#'   genome_seq = sequence2,
#'   seed_id = c(1,2),
#'   tandem_seq = TRUE,
#'   tandem_pos = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' @export

get_tandem_id_from_genome_seq <- function(genome_seq, seed_id = FALSE, tandem_seq = FALSE, tandem_pos = FALSE, triplestore) {
  # Validate params
  validate_param(param = "genome_seq", value = genome_seq, types = 3)
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "tandem_pos", value = tandem_pos, types = 1)
  validate_param(param = "tandem_seq", value = tandem_seq, types = 1)

  # Get genome identifier
  response <- get_genome_id_from_genome_seq(genome_seq, triplestore = triplestore)
  
  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Get genome instruction sequences
    genome_id <- as.integer(gsub("genome_", "", response$genome_id))
  
    # Get tandems
    response <- get_tandem_id_from_genome_id(genome_id = genome_id, tandem_seq = tandem_seq, tandem_pos = tandem_pos, seed_id = seed_id, triplestore = triplestore)
  
    if (is.null(response))
      return(invisible(NULL))
  }
  
  # Return response
  return(as.data.frame(response))
}

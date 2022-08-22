#' Get genome from genome sequence
#'
#' @description Get the genome of a digital organism from the linear string of
#' letters representing the instruction codes that make up its genome.
#'
#' @param genome_seq String of letters or a list of strings
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#' 
#' @return Data frame. Columns: "genome_id" "genome_seq"
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
#' # Get sequence for genome_1
#' sequence <- get_genome_seq_from_genome_id(
#'   genome_id = 1,
#'   triplestore = triplestore
#' )$genome_seq[1]
#' 
#' # Get genome id from sequence
#' get_genome_id_from_genome_seq(
#'   genome_seq = sequence,
#'   triplestore = triplestore
#' )
#' 
#' @export

get_genome_id_from_genome_seq <- function(genome_seq, triplestore) {
  # validata params
  validate_param(param = "genome_seq", value = genome_seq, types = 3)

  # Build query
  query <- paste0("
    PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">
    SELECT distinct ?genome_id ?genome_seq WHERE {
        # genome
        #genome_seq_triple#
        ?genome_id ONTOAVIDA:00000122 ?genome_seq .
    }")

  # Replace params
  query <- replace_data(param = "genome_seq", value = genome_seq, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
  }

  # Return response
  return(response)
}

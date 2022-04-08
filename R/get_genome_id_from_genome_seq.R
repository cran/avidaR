#' Get genome from genome sequence
#'
#' @description Get the genome of a digital organism from the linear string of
#' letters representing the instruction codes that make up its genome.
#'
#' @param genome_seq String of letters or a list of strings
#' 
#' @return Data frame. Columns: "genome_id" "genome_seq"
#' 
#' @examples
#' # Get sequence for genome_1
#' sequence <- get_genome_seq_from_genome_id(genome_id = 1)$genome_seq[1]
#' 
#' #
#' get_genome_id_from_genome_seq(genome_seq = sequence)
#' 
#' @export

get_genome_id_from_genome_seq <- function(genome_seq) {
  # validata params
  validate_param(param = "genome_seq", value = genome_seq, types = 3)

  # Build query
  query <- paste0("
    PREFIX ONTOAVIDA: <", ontoavida_prefix, ">
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
    response <- remove_prefix(prefix = ontoavida_prefix, data = response)
  }

  # Return response
  return(response)
}

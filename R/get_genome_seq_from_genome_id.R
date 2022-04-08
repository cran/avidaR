#' Get genome sequence from genome
#'
#' @description Get the linear string of letters representing the instruction
#' codes that make up the genome of a digital organism from the id of the genome
#' of a digital organism.
#'
#' @param genome_id  Integer or a list of integer values.
#' 
#' @return Data frame. Columns: "genome_id" "genome_seq"
#' 
#' @examples 
#' 
#' # Single genome
#' get_genome_seq_from_genome_id(1)
#' 
#' # More than one genome
#' get_genome_seq_from_genome_id(genome_id = c(1, 2, 3, 4))
#'
#' @export

get_genome_seq_from_genome_id <- function(genome_id) {
  # Validate params
  validate_param(param = "genome_id", value = genome_id, types = 2)

  # Build query
  query <- paste0("
    PREFIX ONTOAVIDA: <", ontoavida_prefix, ">
    SELECT distinct ?genome_id ?genome_seq WHERE {
        # genome
        #genome#
        ?genome_id ONTOAVIDA:00000122 ?genome_seq .

    }")

  # Replace params
  query <- replace_data(param = "genome", value = genome_id, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix, data = response)
  }

  # Return response
  return(response)
}

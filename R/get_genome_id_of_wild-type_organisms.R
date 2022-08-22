#' Get genomes of wild-type organisms

#' @description Get the genome of the digital organisms that were used as
#' wild-type organisms to get their single-point mutants by calling the function
#' get_mutant_at_pos.
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#' 
#' @return Data frame: "genome_id_wild_type".
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
#' get_genome_id_of_wild_type_organisms(triplestore = triplestore)
#'
#' @export
get_genome_id_of_wild_type_organisms <- function(triplestore) {
  
  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "select distinct ?genome_id_wild_type from <http://www.ontotext.com/explicit> where {\n",
                  "  ?organism_id_mutant ONTOAVIDA:00000154 ?organism_id_wild_type .\n",
                  "  ?organism_id_wild_type RO:0002180 ?genome_id_wild_type .\n",
                  "}"
                 )

  # Submit query
  response <- triplestore$submit_query(query = query)
  
  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
  }
  
  # Return response
  return(response)
}
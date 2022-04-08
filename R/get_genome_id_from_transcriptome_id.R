#' Get genome from transcriptome
#'
#' @description Get the genome of a digital organism that executes a specific
#' transcriptome for a list of seeds used for starting the pseudo-random number
#' generator (i.e., a set of environments).
#' @param transcriptome_id Integer or a list of integer values.
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
#' @param genome_seq Logical value (TRUE/FALSE) to show/hide this column
#' ("FALSE" by default).
#' 
#' @return Data frame. Columns: "seed_id" (optional), "transcriptome_id",
#' "genome_seq" (optional). 
#' 
#' @examples 
#' 
#' # Single transcriptome
#' get_genome_id_from_transcriptome_id(transcriptome_id = 1)
#' 
#' # More than one transcriptome
#' get_genome_id_from_transcriptome_id(
#'   transcriptome_id = c(1, 2, 3),
#'   genome_seq = TRUE
#' )
#' 
#' # At seed_1 and seed_2
#' get_genome_id_from_transcriptome_id(
#'   transcriptome_id = 1,
#'   seed_id = c(1, 2)
#' )
#'
#' @export

get_genome_id_from_transcriptome_id <- function(transcriptome_id, seed_id = FALSE, genome_seq = FALSE) {
  # Validate params
  validate_param(param = "transcriptome_id", value = transcriptome_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "genome_seq", value = genome_seq, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix, ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix, ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #executes_at_seed_id# ?genome_id #genome_seq# ?transcriptome_id where {\n",

                  "    # transcriptomes \n",
                  "    #transcriptome#\n\n",
                  
                  "    # organism\n",
                  "    ?organism_id ONTOAVIDA:00000004 ?seq .\n\n",
                  
                  "    # container\n",
                  "    ?seq ?executes_at_seed_id ?transcriptome_id .\n\n",
                  
                  "    # seed\n",
                  "    #executes_at_seed#\n",
                  "    ?executes_at_seed rdfs:subPropertyOf rdfs:member .\n",
                  "    FILTER(?executes_at_seed_id != rdfs:member ) .\n\n",
                  
                  "    # has component \n",
                  "    ?organism_id RO:0002180 ?genome_id .\n\n",
                  
                  "    # genome seq\n",
                  "    ?genome_id ONTOAVIDA:00000122 ?genome_seq .\n\n",
                  
                  "}"
  )

  # Replace params
  query <- replace_data(param = "transcriptome", value = transcriptome_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "executes_at_seed", query = query)

  # Show/hide vars
  query <- show_hide_vars(vars = list(genome_seq = genome_seq), query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix, data = response)
    response <- remove_prefix(prefix = rdf_prefix, data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "executes_at_seed_id")
    
  }

  # Return response
  return(as.data.frame(response))
}

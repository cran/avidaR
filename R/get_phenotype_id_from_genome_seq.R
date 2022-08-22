#' Get phenotype from genome sequence
#'
#' @description Get the phenotype encoded by the instruction sequence
#' constituting the genome of a digital organism for a list of seeds used for
#' starting the pseudo-random number generator (i.e., a set of environments).
#'
#' @param genome_seq String of letters or a list of strings.
#' 
#' @param seed_id Integer (from 1 to 1000), a vector of integer
#' values, or a logical value. This integer is used for starting the
#' pseudo-random number generator that represents the environment experiencing a
#' digital organism. If a logical value is used, TRUE returns data found in all
#' environments and FALSE (by default) returns only distinct data regardless of
#' the seed.
#' 
#' @param genome_id Logical value (TRUE/FALSE) to show/hide genome_id (FALSE by
#' default).
#' 
#' @param phenotype_binary Logical value (TRUE/FALSE) to show/hide phenotype_id
#' in binary notation (FALSE by default).
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
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
#' # Get sequences for genomes_1 and genome_2
#' sequence1 <- get_genome_seq_from_genome_id(
#'   genome_id = 1,
#'   triplestore = triplestore
#' )$genome_seq[1]
#' 
#' sequence2 <- get_genome_seq_from_genome_id(
#'   genome_id = 2,
#'   triplestore = triplestore
#' )$genome_seq[1]
#' 
#' # Single genome
#' get_phenotype_id_from_genome_seq(
#'   genome_seq = sequence1,
#'   triplestore = triplestore
#' )
#' 
#' # More than one genome
#' get_phenotype_id_from_genome_seq(
#'   genome_seq = c(sequence1, sequence2),
#'   genome_id = TRUE,
#'   phenotype_binary = TRUE,
#'   triplestore = triplestore
#' )
#' 
#' # At seed_1 and seed_2
#' get_phenotype_id_from_genome_seq(
#'   genome_seq = sequence2,
#'   seed_id = c(1, 2),
#'   triplestore = triplestore
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional), "genome_id" (optional),
#' "genome_seq", "phenotype_id", "phenotype_binary" (optional).
#'
#' @export

get_phenotype_id_from_genome_seq <- function(genome_seq, seed_id = FALSE, genome_id = FALSE, phenotype_binary = FALSE, triplestore) {
  # Validate params
  validate_param(param = "genome_seq", value = genome_seq, types = 3)
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "genome_id", value = genome_id, types = 1)
  validate_param(param = "phenotype_binary", value = phenotype_binary, types = 1)
  
  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #encodes_at_seed_id# #genome_id# ?genome_seq  ?phenotype_id ('.' as ?phenotype_binary) where {\n",
                  "    # genome\n",
                  "    #genome_seq_triple#\n",
                  "    ?genome_id ONTOAVIDA:00000122 ?genome_seq .\n\n",

                  "    # encodes at seed\n",
                  "    ?genome_id ONTOAVIDA:00001198 ?phenotype_seq_id .\n",
                  "    ?phenotype_seq_id ?encodes_at_seed_id ?phenotype_id .\n",
                  "    ?encodes_at_seed_id rdfs:subPropertyOf rdfs:member .\n",
                  "    #encodes_at_seed#\n",
                  "    FILTER(?encodes_at_seed_id != rdfs:member ) .\n",
                  "}"
                )
  
  # Replace params
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "encodes_at_seed", query = query)
  query <- replace_data(param = "genome", value = genome_id, query = query)
  query <- replace_data(param = "genome_seq", value = genome_seq, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "encodes_at_seed_id")

    # show/hide columns
    response <- show_hide_columns(vars_list = list("phenotype_binary" = phenotype_binary), response)
  }

  # Return response
  return(as.data.frame(response))
}

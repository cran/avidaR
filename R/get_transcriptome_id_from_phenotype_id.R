#' Get transcriptome from phenotype
#'
#' @description Get the transcriptome of a digital organism whose genome encodes
#' a specific phenotype for a list of seeds used for starting the pseudo-random
#' number generator (i.e., a set of environments).
#'
#' @param phenotype_id Integer or list of integer values.
#' 
#' @param seed_id Integer (from 1 to 1000) or a vector of integer values. This
#' integer is used for starting the pseudo-random number generator that
#' represents the environment experiencing a digital organism. If seed_id value
#' is not specified, it returns data for a single randomly chosen seed_id value
#' (between 1 and 1000).
#' 
#' @param transcriptome_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @param transcriptome_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @param phenotype_binary Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame. Columns: "seed_id" (optional), "transcriptome_id",
#' "transcriptome_seq" (optional), "transcriptome_pos" (optional),
#' "phenotype_id", "phenotype_binary" (optional). 
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
#' # Single phenotype
#' get_transcriptome_id_from_phenotype_id(
#'   phenotype_id = 1,
#'   transcriptome_seq = TRUE,
#'   triplestore = triplestore
#' )
#' 
#' # More than one phenotype at seed_1
#' get_transcriptome_id_from_phenotype_id(
#'   phenotype_id = c(1, 2), seed_id = 1,
#'   triplestore = triplestore
#' )
#' 
#' # At seed_1 and seed_2
#' get_transcriptome_id_from_phenotype_id(
#'   phenotype_id = 1,
#'   seed_id = c(1, 2),
#'   transcriptome_pos = TRUE,
#'   triplestore = triplestore
#' )
#'
#' @export

get_transcriptome_id_from_phenotype_id <- function(phenotype_id, seed_id = sample(1:1000, 1), transcriptome_seq = FALSE, transcriptome_pos = FALSE, phenotype_binary = FALSE, triplestore) {
  # Validate params
  validate_param(param = "phenotype_id", value = phenotype_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = 2)
  validate_param(param = "transcriptome_seq", value = transcriptome_seq, types = 1)
  validate_param(param = "transcriptome_pos", value = transcriptome_pos, types = 1)
  validate_param(param = "phenotype_binary", value = phenotype_binary, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #encodes_at_seed_id# ?transcriptome_id #transcriptome_seq# #transcriptome_pos# ?phenotype_id ('.' as ?phenotype_binary) where { \n",
                  "    # phenotype\n",
                  "    #phenotype#\n\n",
                  
                  "    ?genome_id ONTOAVIDA:00001198 ?phenotype_seq_id .\n",
                  "    ?phenotype_seq_id ?encodes_at_seed_id ?phenotype_id .\n\n",
                  
                  "    # seed\n",
                  "    #encodes_at_seed#\n",
                  "    FILTER(?encodes_at_seed_id != rdfs:member ) .\n\n",
                  
                  "    # genome\n",
                  "    ?organism_id RO:0002180 ?genome_id .\n",
                  "    ?genome_id ONTOAVIDA:00000122 ?genome_seq .\n",
                  
                  "    # organism\n",
                  "    ?organism_id ONTOAVIDA:00000004 ?transcriptome_seq_id .\n\n",
                  
                  "    # container\n",
                  "    ?transcriptome_seq_id ?encodes_at_seed_id ?transcriptome_id .\n\n",
                  
                  "    # transcriptome\n",
                  "    ?transcriptome_id ONTOAVIDA:00000123 ?transcriptome_seq .\n",
                  "    ?transcriptome_id ONTOAVIDA:00000161 ?transcriptome_pos .\n\n",
                  "}"
                )

  # Replace params
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = c("encodes_at_seed", "executes_at_seed"), query = query)
  query <- replace_data(param = "phenotype", value = phenotype_id, query = query)
  query <- replace_data(param = "genome_seq", value = transcriptome_seq, query = query)
  query <- replace_data(param = "transcriptome_seq", value = transcriptome_seq, query = query)
  query <- replace_data(param = "transcriptome_pos", value = transcriptome_pos, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = c("encodes_at_seed_id", "executes_at_seed_id"))

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("phenotype_binary" = phenotype_binary, "transcriptome_seq" = transcriptome_seq, "transcriptome_pos" = transcriptome_pos), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

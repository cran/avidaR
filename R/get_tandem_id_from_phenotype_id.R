#' Get tandem repeat from phenotype
#'
#' @description Get the tandem repeat contained in the transcriptome of a
#' digital organism that encodes a specific phenotype for a list of seeds used
#' for starting the pseudo-random number generator (i.e., a set of environments).
#'
#' @param phenotype_id Integer or list of integer values.
#' 
#' @param seed_id Integer (from 1 to 1000) or a vector of integer values. This
#' integer is used for starting the pseudo-random number generator that
#' represents the environment experiencing a digital organism. If seed_id value
#' is not specified, it returns data for a single randomly chosen seed_id value
#' (between 1 and 1000).
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
#' @return Data frame. Column: "seed_id" (optional), "phenotype_id",
#' "tandem_id", "tandem_seq" (optional), "tandem_pos (optional)."
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
#' get_tandem_id_from_phenotype_id(
#'   phenotype_id = 8, tandem_seq = TRUE,
#'   triplestore = triplestore
#' )
#' 
#' # More than one phenotype at seed_1
#' get_tandem_id_from_phenotype_id(
#'   phenotype_id = c(2, 4, 8), seed_id = 1,
#'   triplestore = triplestore
#' )
#' 
#' # At seed_1 and seed_2
#' get_tandem_id_from_phenotype_id(
#'   phenotype_id = 1,
#'   seed_id = c(1, 2),
#'   tandem_pos = TRUE,
#'   triplestore = triplestore
#' )
#'
#' @export

get_tandem_id_from_phenotype_id <- function(phenotype_id, seed_id = sample(1:1000, 1), tandem_seq = FALSE, tandem_pos = FALSE, triplestore) {
  # Validate params
  validate_param(param = "phenotype_id", value = phenotype_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = 2)
  validate_param(param = "tandem_pos", value = tandem_pos, types = 1)
  validate_param(param = "tandem_seq", value = tandem_seq, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct  #encodes_at_seed_id# ?tandem_id #tandem_seq# #tandem_pos# ?phenotype_id {\n",
                  "    #phenotype\n",
                  "    #phenotype#\n\n",
                    
                  "    # encodes at seed\n",
                  "    #encodes_at_seed#\n",
                  "    ?genome_id ONTOAVIDA:00001198 ?phenotype_seq_id .\n",
                  "    ?phenotype_seq_id ?encodes_at_seed_id ?phenotype_id .\n",
                  "    FILTER(?encodes_at_seed_id != rdfs:member) .\n\n",
                    
                  "    # executes at seed\n",
                  "    ?organism_id RO:0002180 ?genome_id .\n",
                  "    ?organism_id ONTOAVIDA:00000004 ?transcriptome_seq_id .\n",
                  "    ?transcriptome_seq_id ?encodes_at_seed_id ?transcriptome_id .\n\n",
                    
                  "    # tandem\n",
                  "    ?transcriptome_id RO:0001019 ?tandem_id .\n",
                  "    ?tandem_id ONTOAVIDA:00000121 ?tandem_seq .\n",
                  "    ?transcriptome_id ONTOAVIDA:00000166 ?tandem_pos .\n\n",
                  "}"
                )

  # Replace params
  query <- replace_data(param = "phenotype", value = phenotype_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "encodes_at_seed", query = query)
  query <- replace_data(param = "tandem_seq", value = tandem_seq, query = query)
  query <- replace_data(param = "tandem_pos", value = tandem_pos, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "encodes_at_seed_id")
    
    # Show/hide columns
    response <- show_hide_columns(vars_list = list("tandem_seq" = tandem_seq, "tandem_pos" = tandem_pos), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

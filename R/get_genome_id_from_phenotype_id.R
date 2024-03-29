#' Get genome from phenotype
#'
#' @description Get the genome of a digital organism that encodes a specific
#' phenotype for a list of seeds used for starting the pseudo-random number
#' generator (i.e., a set of environments).
#' @param phenotype_id Integer or a list of integer values.
#' @param seed_id Integer (from 1 to 1000) or a vector of integer values. This
#' integer is used for starting the pseudo-random number generator that
#' represents the environment experiencing a digital organism. If seed_id value
#' is not specified, it returns data for a single randomly chosen seed_id value
#' (between 1 and 1000).
#' @param genome_seq Logical value (TRUE/FALSE) to show/hide this column
#' ("FALSE" by default).
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame. Columns: "seed_id" (optional), "phenotype_id",
#' "genome_seq" (optional).
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
#' # Single phenotype
#' get_genome_id_from_phenotype_id(phenotype_id = 1, triplestore = avidaDB)
#'
#' # More than one phenotype
#' get_genome_id_from_phenotype_id(
#'   phenotype_id = c(1, 2),
#'   genome_seq = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seeds_4 and seed_5
#' get_genome_id_from_phenotype_id(
#'   phenotype_id = c(1, 2),
#'   seed_id = c(4, 5),
#'   triplestore = avidaDB
#' )
#'
#' @export

get_genome_id_from_phenotype_id <- function(phenotype_id, seed_id = sample(1:1000, 1), genome_seq = FALSE, triplestore) {
  # Validate params
  validate_param(param = "phenotype_id", value = phenotype_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = 2)
  validate_param(param = "genome_seq", value = genome_seq, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #encodes_at_seed_id# ?genome_id #genome_seq# ?phenotype_id where {\n",
                  "    # phenotypes\n",
                  "    #phenotype#\n\n",

                  "    # encodes at seed\n",
                  "    ?genome_id ONTOAVIDA:00001198 ?phenotype_seq_id .\n",
                  "    ?phenotype_seq_id ?encodes_at_seed_id ?phenotype_id .\n",
                  "    ?encodes_at_seed_id rdfs:subPropertyOf rdfs:member .\n",
                  "    #encodes_at_seed#\n",
                  "    FILTER(?encodes_at_seed_id != rdfs:member ) .\n\n",

                  "    # genome seq\n",
                  "    ?genome_id ONTOAVIDA:00000122 ?genome_seq .\n",
                  "}"
  )

  # Replace params
  query <- replace_data(param = "phenotype", value = phenotype_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "encodes_at_seed", query = query)

  # Show/hide vars
  query <- show_hide_vars(vars = list(genome_seq = genome_seq), query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "encodes_at_seed_id")
  }

  # Return response
  return(as.data.frame(response))
}

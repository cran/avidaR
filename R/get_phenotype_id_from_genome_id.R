#' Get phenotype from genome
#'
#' @description Get the phenotype encoded by the genome of a digital organism
#' for a list of seeds used for starting the pseudo-random number generator
#' (i.e., a set of environments).
#'
#' @param genome_id Integer or a list of integer values.
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
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
#' # Single genome
#' get_phenotype_id_from_genome_id(genome_id = 1, triplestore = avidaDB)
#'
#' # More than one genome at seed_1
#' get_phenotype_id_from_genome_id(
#'   genome_id = c(1, 2, 3),
#'   seed_id = 1,
#'   triplestore = avidaDB
#' )
#'
#' # More than one genome at more than one seed (e.g., seed_3 and seed_4)
#' get_phenotype_id_from_genome_id(
#'   genome_id = 1,
#'   seed_id = c(3, 4),
#'   phenotype_binary = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional), "genome_id",
#' "phenotype_id" "phenotype_binary" (optional).
#'
#' @export

get_phenotype_id_from_genome_id <- function(genome_id, seed_id = FALSE, phenotype_binary = FALSE, triplestore) {
  # Validate params
  validate_param(param = "genome_id", value = genome_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "phenotype_binary", value = phenotype_binary, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #encodes_at_seed_id# ?genome_id ?phenotype_id ('.' as ?phenotype_binary) where {\n",
                  "    # genome\n",
                  "    #genome#\n\n",

                  "    # encodes at seed\n",
                  "    ?genome_id ONTOAVIDA:00001198 ?phenotype_seq_id .\n",
                  "    ?phenotype_seq_id ?encodes_at_seed_id ?phenotype_id .\n",
                  "    ?encodes_at_seed_id rdfs:subPropertyOf rdfs:member .\n",
                  "    #encodes_at_seed#\n",
                  "    FILTER(?encodes_at_seed_id != rdfs:member ) .\n",
                  "}"
  )

  # Replace params
  query <- replace_data(param = "genome", value = genome_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "encodes_at_seed", query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "encodes_at_seed_id")

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("phenotype_binary" = phenotype_binary), response)
  }

  # Return response
  return(as.data.frame(response))
}

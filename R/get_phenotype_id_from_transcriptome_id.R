#' Get phenotype from transcriptome
#'
#' @description Get the phenotype encoded by the genome of a digital organism
#' that executes a specific transcriptome for a list of seeds used for starting
#' the pseudo-random number generator (i.e., a set of environments).
#'
#' @param transcriptome_id Integer or list of integer values.
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
#'
#' @param phenotype_binary Logical value (TRUE/FALSE) to show/hide phenotype in
#' binary notation (FALSE by default).
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
#' # Single transcriptome
#' get_phenotype_id_from_transcriptome_id(
#'   transcriptome_id = 53674,
#'   triplestore = avidaDB
#' )
#'
#' # More than one transcriptome
#' get_phenotype_id_from_transcriptome_id(
#'   transcriptome_id = c(53674, 1666099),
#'   phenotype_binary = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seed_1 and seed_3
#' get_phenotype_id_from_transcriptome_id(
#'   transcriptome_id = 53674, seed_id = c(1,3),
#'   triplestore = avidaDB
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional),	"transcriptome_id",	"phenotype_id", "phenotype_binary" (optional)
#'
#' @export

get_phenotype_id_from_transcriptome_id <- function(transcriptome_id, seed_id = FALSE, phenotype_binary = FALSE, triplestore) {
  # Validate params
  validate_param(param = "transcriptome_id", value = transcriptome_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "phenotype_binary", value = phenotype_binary, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #executes_at_seed_id# ?transcriptome_id ?phenotype_id where {\n",
                  "    # transcriptome\n",
                  "    #transcriptome#\n\n",

                  "    # organism\n",
                  "    ?organism_id ONTOAVIDA:00000004 ?transcriptome_seq_id .\n\n",

                  "    # container\n",
                  "    ?transcriptome_seq_id ?executes_at_seed_id ?transcriptome_id .\n\n",

                  "    # seed\n",
                  "    #executes_at_seed#\n",
                  "    ?executes_at_seed_id rdfs:subPropertyOf rdfs:member .\n",
                  "    FILTER(?executes_at_seed_id != rdfs:member ) .\n\n",

                  "    # genome\n",
                  "    ?organism_id RO:0002180 ?genome_id .\n\n",

                  "    # encodes at executes_at_seed_id\n",
                  "    ?genome_id ONTOAVIDA:00001198 ?phenotype_seq_id .\n",

                  "    # phenotype\n",
                  "    ?phenotype_seq_id ?executes_at_seed_id ?phenotype_id .\n\n",

                  "}"
                )

  # Replace params
  query <- replace_data(param = "transcriptome", value = transcriptome_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = c("encodes_at_seed", "executes_at_seed"), query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)


  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = c("encodes_at_seed_id", "executes_at_seed_id"))

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("phenotype_binary" = phenotype_binary), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

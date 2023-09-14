#' Get tandem repeat from genome
#'
#' @description Get the tandem repeat contained in the transcriptome of a
#' digital organism having a specific genome.
#'
#' @param genome_id Integer or a list of integer values.
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
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
#' @return Data frame. Columns: "seed_id" (optional), "genome_id", "tandem_id",
#' "tandem_seq" (optional), "tandem_pos" (optional).
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
#' get_tandem_id_from_genome_id(genome_id = 1, triplestore = avidaDB)
#'
#' # More than one genome
#' get_tandem_id_from_genome_id(
#'   genome_id = c(1, 2, 3),
#'   tandem_seq = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seed_1, seed_3 and seed_5
#' get_tandem_id_from_genome_id(
#'   genome_id = 2,
#'   seed_id = c(1, 3, 5),
#'   tandem_pos = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' @export

get_tandem_id_from_genome_id <- function(genome_id, seed_id = FALSE, tandem_seq = FALSE, tandem_pos = FALSE, triplestore) {
  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "genome_id", value = genome_id, types = 2)
  validate_param(param = "tandem_seq", value = tandem_seq, types = 1)
  validate_param(param = "tandem_pos", value = tandem_pos, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct  #executes_at_seed_id# ?genome_id ?tandem_id #tandem_seq# #tandem_pos# where {\n\n",

                  "  # genome id\n",
                  "  #genome#\n\n",

                  "  # genome\n",
                  "  ?organism_id RO:0002180 ?genome_id .\n",

                  "  # executes at seed\n",
                  "  #executes_at_seed#\n",
                  "  ?organism_id ONTOAVIDA:00000004 ?transcriptome_seq_id .\n",
                  "  ?transcriptome_seq_id ?executes_at_seed_id ?transcriptome_id .\n",
                  "  FILTER(?executes_at_seed_id != rdfs:member) .\n\n",

                  "  # tandem\n",
                  "  ?transcriptome_id RO:0001019 ?tandem_id .\n\n",

                  "  # tandem seq\n",
                  "  ?tandem_id ONTOAVIDA:00000121 ?tandem_seq .\n\n",

                  "  # tandem pos\n",
                  "  ?transcriptome_id ONTOAVIDA:00000166 ?tandem_pos .\n",

                  "}"
                )

  # Replace params
  query <- replace_data(param = "genome", value = genome_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "executes_at_seed", query = query)
  query <- replace_data(param = "tandem_seq", value = tandem_seq, query = query)
  query <- replace_data(param = "tandem_pos", value = tandem_pos, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "executes_at_seed_id")

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("tandem_seq" = tandem_seq, "tandem_pos" = tandem_pos), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

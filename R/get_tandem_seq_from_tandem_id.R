#' Get the tandem repeat sequence from tandem repeat
#'
#' @description Get the tandem sequence from the tandem repeat contained in the
#' transcriptome of a digital organism for a list of seeds used for starting
#' the pseudo-random number generator (i.e., a set of environments).
#'
#' @param tandem_id Integer or a list of integer values.
#' 
#' @param seed_id Integer (from 1 to 1000), a vector of integer
#' values, or a logical value. This integer is used for starting the
#' pseudo-random number generator that represents the environment experiencing a
#' digital organism. If a logical value is used, TRUE returns data found in all
#' environments and FALSE (by default) returns only distinct data regardless of
#' the seed.
#' 
#' @param genome_id Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @param tandem_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#' 
#' @return Data frame. Columns: "seed_id" (optional), "tandem_id", "tandem_seq",
#' "tandem_pos" (optional), genome_id" (optional).
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
#' # Single tandem
#' get_tandem_seq_from_tandem_id(
#'   tandem_id = 6336945,
#'   triplestore = triplestore
#' )
#' 
#' # More than one tandem at seed_1
#' get_tandem_seq_from_tandem_id(
#'   tandem_id = c(6336945, 2520963, 2520963),
#'   seed_id = 1,
#'   triplestore = triplestore
#' )
#' 
#' # At seed_3 and seed_5
#' get_tandem_seq_from_tandem_id(
#'   tandem_id = 6336945,
#'   seed_id = c(1, 3),
#'   tandem_pos = TRUE,
#'   genome_id = TRUE,
#'   triplestore = triplestore
#' )
#'
#' @export

get_tandem_seq_from_tandem_id <- function(tandem_id, seed_id = FALSE, genome_id = FALSE, tandem_pos = FALSE, triplestore) {
  # Validate params
  validate_param(param = "tandem_id", value = tandem_id, types = 2)
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "genome_id", value = genome_id, types = 1)
  validate_param(param = "tandem_pos", value = tandem_pos, types = 1)

  # Build query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "SELECT distinct #executes_at_seed_id# ?tandem_id ?tandem_seq #tandem_pos# #genome_id# WHERE {\n",
                  "  # tandem\n",
                  "  #tandem#\n\n",
                  
                  "  # tandem seq\n",
                  "  ?tandem_id ONTOAVIDA:00000121 ?tandem_seq .\n\n",
                  
                  "  # tandem pos\n",
                  "  ?transcriptome_id RO:0001019 ?tandem_id .\n\n",

                  "  ?transcriptome_id ONTOAVIDA:00000166 ?tandem_pos .\n\n",
                  
                  "  # executes at seed\n",
                  "  #executes_at_seed#\n",
                  "  ?organism_id ONTOAVIDA:00000004 ?transcriptome_seq_id .\n",
                  "  ?transcriptome_seq_id ?executes_at_seed_id ?transcriptome_id .\n",
                  "  FILTER(?executes_at_seed_id != rdfs:member) .\n\n",
                  
                  "  # genome\n",
                  "  ?organism_id RO:0002180 ?genome_id .\n",
                  
                  "}"
                )
  
  # Replace params
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "executes_at_seed", query = query)
  query <- replace_data(param = "tandem", value = tandem_id, query = query)
  query <- replace_data(param = "genome", value = genome_id, query = query)
  query <- replace_data(param = "tandem_pos", value = tandem_pos, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Replace prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "executes_at_seed_id")

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("genome_id" = genome_id, "tandem_pos" = tandem_pos), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

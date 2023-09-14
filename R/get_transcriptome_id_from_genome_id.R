#' Get transcriptome from genome
#'
#' @description Get the transcriptome of a digital organism having a specific
#' genome for a list of seeds used for starting the pseudo-random number
#' generator (i.e., a set of environments).
#'
#' @param genome_id Integer or list of integer values.
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer
#' values, or a logical value. This integer is used for starting the
#' pseudo-random number generator that represents the environment experiencing a
#' digital organism. If a logical value is used, TRUE returns data found in all
#' environments and FALSE (by default) returns only distinct data regardless of
#' the seed.
#'
#' @param transcriptome_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#'
#' @param transcriptome_pos Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
#'
#' @param genome_seq Logical value (TRUE/FALSE) to show/hide this column
#' (FALSE by default).
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
#' # Singel genome
#' get_transcriptome_id_from_genome_id(
#'   genome_id = 1,
#'   triplestore = avidaDB
#' )
#'
#' # More than one genome
#' get_transcriptome_id_from_genome_id(
#'   genome_id = c(1, 2),
#'   transcriptome_seq = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' # At seed_1 and seed_3
#' get_transcriptome_id_from_genome_id(
#'   genome_id = 2,
#'   seed_id = c(1, 3),
#'   transcriptome_pos = TRUE,
#'   triplestore = avidaDB
#' )
#'
#' @return Data frame. Columns: "seed_id" (optional), "genome_id", "genome_seq"
#' (optional), "transcriptome_id", "transcriptome_seq" (optional),
#' "transcriptome_pos" (optional).
#'
#' @export

get_transcriptome_id_from_genome_id <- function(genome_id, seed_id = FALSE, transcriptome_seq = FALSE, transcriptome_pos = FALSE, genome_seq = FALSE, triplestore) {
  # Validate params
  validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "genome_id", value = genome_id, types = 2)
  validate_param(param = "genome_seq", value = genome_seq, types = 1)
  validate_param(param = "transcriptome_seq", value = transcriptome_seq, types = 1)

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
                  "select distinct #executes_at_seed_id# ?genome_id #genome_seq# ?transcriptome_id #transcriptome_seq# #transcriptome_pos# where {\n",
                  "    # genome\n",
                  "    #genome#\n",
                  "    ?organism_id RO:0002180 ?genome_id .\n",
                  "    ?genome_id ONTOAVIDA:00000122 ?genome_seq .\n",

                  "    # organism\n",
                  "    ?organism_id ONTOAVIDA:00000004 ?seq .\n\n",

                  "    # container\n",
                  "    ?seq ?executes_at_seed_id ?transcriptome_id .\n\n",

                  "    # seed\n",
                  "    #executes_at_seed#\n",
                  "    ?executes_at_seed rdfs:subPropertyOf rdfs:member .\n",
                  "    FILTER(?executes_at_seed_id != rdfs:member ) .\n\n",

                  "    # transcriptome\n",
                  "    ?transcriptome_id ONTOAVIDA:00000123 ?transcriptome_seq .\n",
                  "    ?transcriptome_id ONTOAVIDA:00000161 ?transcriptome_pos .\n\n",

                  "}"
                )

  # Replace params
  query <- replace_data(param = "genome", value = genome_id, query = query)
  query <- replace_at_seed_id(seed_id = seed_id, at_seed_vars = "executes_at_seed", query = query)
  query <- replace_data(param = "genome_seq", value = TRUE, query = query)
  query <- replace_data(param = "transcriptome_seq", value = transcriptome_seq, query = query)
  query <- replace_data(param = "transcriptome_pos", value = transcriptome_pos, query = query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefix
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
    response <- remove_prefix(prefix = rdf_prefix(), data = response)
    response <- clean_at_seed_id (data = response, seed_id = seed_id, at_seed_vars = "executes_at_seed_id")

    # Show/hide columns
    response <- show_hide_columns(vars_list = list("transcriptome_seq" = transcriptome_seq, "transcriptome_pos" = transcriptome_pos, "genome_seq" = genome_seq), data = response)
  }

  # Return response
  return(as.data.frame(response))
}

#' Get single-point mutants of wild-type organisms
#'
#' @description Get the genome sequence of a digital organism (i.e., wild-type)
#' and its single-point mutants.
#'
#' @param genome_id Integer or a list of integer values. If not specified, the
#' function will return the single-point mutants of a randomly chosen wild-type
#' organism.
#' @param inst_replaced A letter representing the instruction of the genome
#' sequence of the wild-type organism to be mutated. If not specified,
#' the function will return the single-point mutants that have replaced the
#' letter that the genome of the wild-type organism carried at that position
#' (if the position is specified, otherwise it will return the mutations
#' located on all positions) by the letter indicated in the argument
#' inst_replaced_by (if specified, otherwise it will return all the mutants at
#' that position on the genome of the wild type organism).

#' @param inst_replaced_by A letter representing the instruction of the genome
#' of the single-point mutant that have replaced the instruction of the genome
#' of the wild-type organism. If not specified, the function will return all
#' single-point mutants that have replaced the letter indicated in the argument
#' inst_replaced (if specified, otherwise it will return all the mutants at that
#' position on the genome of the wild type organism) of the genome of the
#' wild-type organism at that position (if the position is specified, otherwise
#' it will return the mutations located on all positions).
#'
#' @param pos Integer representing the position of the single-point mutation
#' along the genome of a digital organism (from 1 to 100 for a genome length of
#' 100 instructions). If not specified, the function will return all
#' single-point mutants of the genome of the wild-type organism.
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame: Columns: "genome_id_wild_type", "genome_seq_wild_type",
#' "genome_id_mutant", genome_seq_mutant", "pos".
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
#' get_mutant_at_pos(
#'   genome_id = 582,
#'   inst_replaced = 'o',
#'   inst_replaced_by = 'a',
#'   pos = 1,
#'   triplestore = avidaDB)
#'
#' @export

get_mutant_at_pos <- function(genome_id = NULL, inst_replaced = NULL, inst_replaced_by = NULL, pos = NULL, triplestore) {
  # Generate genome_id sample
  if (is.null(genome_id)){
    genome_ids <- get_genome_id_of_wild_type_organisms(triplestore = triplestore)
    if (genome_ids %>% nrow() >0)
      genome_id <- as.numeric(gsub("genome_", "", (genome_ids %>% sample_n(1))[1]$genome_id_wild_type))
  }

  # Get genome sequence
  genome_seq <- get_genome_seq_from_genome_id(genome_id, triplestore = triplestore)$genome_seq[1]

  # Validate params
  validate_param(param = "genome_id", value = genome_id, types = 2)
  if (!is.null(inst_replaced)) {
    validate_param(param = "inst_replaced", value = inst_replaced, types = 3)
  }
  if (!is.null(inst_replaced_by)) {
    validate_param(param = "inst_replaced_by", value = inst_replaced_by, 3)
  }
  if (!is.null(pos)) {
    validate_param(param = "pos", value = pos, types = 2)
  } else {
    pos <- paste0(seq(1, nchar(genome_seq)), collapse = " ")
  }

  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix(), ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX GSSO: <http://purl.obolibrary.org/obo/GSSO_>\n",
                  "PREFIX rdf: <", rdf_prefix(), ">\n",
                  "SELECT distinct ?genome_id_wild_type ?genome_seq_wild_type ?genome_id_mutant ?genome_seq_mutant ?pos WHERE {\n",
                  "    # organism mutant of ancestor\n",
                  "    ?organism_id_mutant rdf:type GSSO:009994 .\n",
                  "    ?organism_id_wild_type rdf:type GSSO:009994 .\n\n",
                  "    ?organism_id_mutant ONTOAVIDA:00000154 ?organism_id_wild_type .\n",

                  "    # genome\n",
                  "    ?organism_id_mutant RO:0002180 ?genome_id_mutant .\n",
                  "    ?organism_id_wild_type RO:0002180 ?genome_id_wild_type .\n",
                  "    values ?genome_id_wild_type { ONTOAVIDA:genome_", paste(genome_id, sep = " ", collapse = " "), " } .\n\n",

                  "    # instruction sequence\n",
                  "    ?genome_id_mutant ONTOAVIDA:00000122 ?genome_seq_mutant .\n",
                  "    ?genome_id_wild_type ONTOAVIDA:00000122 ?genome_seq_wild_type .\n\n",

                  "    # mutation\n",
                  "    values ?pos { ", pos, " } .\n",
                  "    #filter# .\n",
                  "}"
  )

  # Build filter
  filter <- "FILTER(substr(?genome_seq_wild_type,?pos,1) = '#inst1#' && substr(?genome_seq_mutant,?pos,1) = '#inst2#')"

  if (is.null(inst_replaced) && is.null(inst_replaced_by)) {
    filter <- "FILTER(substr(?genome_seq_wild_type,?pos,1) != substr(?genome_seq_mutant,?pos,1))"
  } else if (is.null(inst_replaced_by)) {
    filter <- "FILTER(substr(?genome_seq_wild_type,?pos,1) != substr(?genome_seq_mutant,?pos,1) && substr(?genome_seq_wild_type,?pos,1) = '#inst1#')"
  } else if (is.null(inst_replaced)) {
    filter <- "FILTER(substr(?genome_seq_wild_type,?pos,1) != substr(?genome_seq_mutant,?pos,1) && substr(?genome_seq_mutant,?pos,1) = '#inst2#')"
  }

  # Replace params
  if (!is.null(inst_replaced)) {
    filter <- gsub("#inst1#", inst_replaced, filter)
  }
  if (!is.null(inst_replaced_by)) {
    filter <- gsub("#inst2#", inst_replaced_by, filter)
  }
  query <- gsub("#filter#", filter, query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (is.null(response))
    return(invisible(NULL))
  
  if (nrow(response)>0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix(), data = response)
  }

  # Return response
  return(response)
}

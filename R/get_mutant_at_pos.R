#' Get a single-point mutant of a digital organism
#'
#' @description Get the genome sequences of a digital organism and one of its
#' single-point mutants.
#'
#' @param inst_replaced A letter representing the instruction of the genome
#' sequence of the organism to be mutated.
#' @param inst_replaced_by A letter representing the instruction replaced in the
#' genome of the single-point mutant.
#' @param pos Integer representing the position of the single-point mutation
#' along the genome of a digital organism (from 1 to 100 for a genome length of
#' 100 instructions).
#' 
#' @return Data frame: Columns: "genome_id_ancestor", "genome_seq_ancestor",
#' "genome_id_mutant", genome_seq_mutant".
#' 
#' @examples 
#' 
#' get_mutant_at_pos(inst_replaced = 'o', inst_replaced_by = 'a', pos = 1)
#'
#' @export

get_mutant_at_pos <- function(inst_replaced, inst_replaced_by, pos) {
  # Validate params
  validate_param(param = "inst_replaced", value = inst_replaced, types = 3)
  validate_param(param = "inst_replaced_by", value = inst_replaced_by, 3)
  validate_param(param = "pos", value = pos, types = 2)
  
  # Build sparql query
  query <- paste0("PREFIX ONTOAVIDA: <", ontoavida_prefix, ">\n",
                  "PREFIX RO: <http://purl.obolibrary.org/obo/RO_>\n",
                  "PREFIX GSSO: <http://purl.obolibrary.org/obo/GSSO_>\n",
                  "PREFIX rdf: <", rdf_prefix, ">\n",
                  "SELECT distinct ?genome_id_ancestor ?genome_seq_ancestor ?genome_id_mutant ?genome_seq_mutant WHERE {\n",
                  "    # organism mutant of ancestor\n",
                  "    ?organism_id_mutant rdf:type GSSO:009994 .\n",
                  "    ?organism_id_ancestor rdf:type GSSO:009994 .\n\n",
                  "    ?organism_id_mutant ONTOAVIDA:00000154 ?organism_id_ancestor .\n",

                  "    # genome\n",
                  "    #?genome_id_mutant rdf:type ONTOAVIDA:00000097 .\n",
                  "    #?genome_id_ancestor rdf:type ONTOAVIDA:00000097 .\n\n",
                  "    ?organism_id_mutant RO:0002180 ?genome_id_mutant .\n",
                  "    ?organism_id_ancestor RO:0002180 ?genome_id_ancestor .\n",
                  
                  "    # instruction sequence\n",
                  "    ?genome_id_mutant ONTOAVIDA:00000122 ?genome_seq_mutant .\n",
                  "    ?genome_id_ancestor ONTOAVIDA:00000122 ?genome_seq_ancestor .\n\n",
                  
                  "    # mutation\n",
                  "    FILTER(substr(?genome_seq_ancestor,#pos#,1) = \"#inst1#\") .\n",
                  "    FILTER(substr(?genome_seq_mutant,#pos#,1) = \"#inst2#\") .\n",
                  "}"
                )

  # Replace params
  query <- gsub("#pos#", pos, query)
  query <- gsub("#inst1#", inst_replaced, query)
  query <- gsub("#inst2#", inst_replaced_by, query)

  # Submit query
  response <- triplestore$submit_query(query = query)

  if (nrow(response) > 0) {
    # Remove prefixes
    response <- remove_prefix(prefix = ontoavida_prefix, data = response)
  }

  # Return response
  return(response)
}

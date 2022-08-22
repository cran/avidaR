#' Get the logic operations computed by a digital organism whose genome
#' encodes a specific phenotype
#'
#' @description Get the logic operations encoded by a digital organism having
#' the requested phenotype.
#'
#' @param phenotype_id Integer, a vector of integers (from 0 to 511), or a
#' logical value (if FALSE, the function returns the entire phenotype space).
#' @param phenotype_binary Logical value (TRUE/FALSE) to show/hide
#' phenotype_id in binary notation (FALSE by default).
#' 
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#' 
#' @return Data frame. Columns: "phenotype_id", "phenotype_binary" (optional),
#'"equals", "exclusive-or", "not-or", "and-not", "or", "orn-not", "and",
#'"not-and", "not"
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
#' # One phenotype
#' get_logic_operation_from_phenotype_id(
#'   phenotype_id = 1,
#'   phenotype_binary = TRUE,
#'   triplestore = triplestore
#' )
#'
#' # More than one phenotype
#' get_logic_operation_from_phenotype_id(
#'   phenotype_id = c(1,2,3),
#'   phenotype_binary = TRUE,
#'   triplestore = triplestore
#' )
#'
#' # All phenotypes
#' get_logic_operation_from_phenotype_id(triplestore = triplestore)
#' 
#' @export
#'
get_logic_operation_from_phenotype_id <- function(phenotype_id = FALSE, phenotype_binary = FALSE, triplestore) {
# Validate params
validate_param(param = "phenotype_id", value = phenotype_id, types = c(1, 2))
validate_param(param = "phenotype_binary", value = phenotype_binary, types = 1)

# Build sparql query
query <- paste0('
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX ONTOAVIDA: <', ontoavida_prefix(), '>
            PREFIX RO: <http://purl.obolibrary.org/obo/RO_>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            select distinct ?phenotype_id ("000000000" as ?phenotype_binary)
              (IF(BOUND(?b_equals), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?equals)
              (IF(BOUND(?b_exclusive_or), "true"^^xsd:boolean, "false"^^xsd:boolean) as ?exclusive_or)
              (IF(BOUND(?b_not_or), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?not_or)
              (IF(BOUND(?b_and_not), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?and_not)
              (IF(BOUND(?b_or), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?or)
              (IF(BOUND(?b_orn_not), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?orn_not)
              (IF(BOUND(?b_and), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?and)
              (IF(BOUND(?b_not_and), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?not_and)
              (IF(BOUND(?b_not), "true"^^xsd:boolean,"false"^^xsd:boolean) as ?not)
            
            where {
              # phenotype
              #phenotype#
          
              # boolean operations
              OPTIONAL {?phenotype_id RO:0002507 ?lo_and .
              ?lo_and rdfs:label ?label_and .
              filter(?label_and = "and"@en) .
              BIND("true"^^xsd:boolean as ?b_and) . }
              
              OPTIONAL {?phenotype_id RO:0002507 ?lo_and_not .
              ?lo_and_not rdfs:label ?label_and_not .
              filter(?label_and_not = "and-not"@en) .
              BIND("true"^^xsd:boolean as ?b_and_not) . }
              
              OPTIONAL {
              ?phenotype_id RO:0002507 ?lo_equals.
              ?lo_equals rdfs:label ?label_equals .
              filter(?label_equals = "equals"@en) .
              BIND("true"^^xsd:boolean as ?b_equals) . }
              
              OPTIONAL {
              ?phenotype_id RO:0002507 ?lo_exclusive_or.
              ?lo_exclusive_or rdfs:label ?label_exclusive_or .
              filter(?label_exclusive_or = "exclusive or"@en) .
              BIND("true"^^xsd:boolean as ?b_exclusive_or) . }
              
              OPTIONAL {?phenotype_id RO:0002507 ?lo_not .
              ?lo_not rdfs:label ?label_not .
              filter(?label_not = "not"@en) .
              BIND("true"^^xsd:boolean as ?b_not) . }
              
              OPTIONAL {?phenotype_id RO:0002507 ?lo_not_and .
              ?lo_not_and rdfs:label ?label_not_and .
              filter(?label_not_and = "not-and"@en) .
              BIND("true"^^xsd:boolean as ?b_not_and) . }
              
              OPTIONAL {?phenotype_id RO:0002507 ?lo_not_or .
              ?lo_not_or rdfs:label ?label_not_or .
              filter(?label_not_or = "not-or"@en) .
              BIND("true"^^xsd:boolean as ?b_not_or) . }
              
              OPTIONAL {?phenotype_id RO:0002507 ?lo_or .
              ?lo_or rdfs:label ?label_or .
              filter(?label_or = "or"@en) .
              BIND("true"^^xsd:boolean as ?b_or) . }
              
              OPTIONAL {?phenotype_id RO:0002507 ?lo_orn_not .
              ?lo_orn_not rdfs:label ?label_orn_not .
              filter(?label_orn_not = "orn-not"@en) .
              BIND("true"^^xsd:boolean as ?b_orn_not) . }

          }

          ')

# Replace params
query <- replace_data(param = "phenotype", value = phenotype_id, query = query)

# Submit query
response <- triplestore$submit_query(query = query)

if (nrow(response) > 0) {
# Remove prefixes
response <- remove_prefix(prefix = ontoavida_prefix(), data = response)

# Show hid/columns
response <- show_hide_columns(vars_list = list(phenotype_binary = phenotype_binary), data = response)
}

# Return response
return(response)
}

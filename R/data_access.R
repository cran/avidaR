#' @import R6
#' @import httr
#' @import xml2
#' 

# Class to manage triplestore access options
triplestore_access = R6::R6Class(
  classname = "triplestore_access",
  private = list(
    url = NULL,
    user = NULL,
    password = NULL,
    repository = NULL,
    authentication = NULL,
    protocol = NULL,
    ontology_title = NULL,
    ontology_description = NULL,
    ontology_versionIRI = NULL
  ),
  public = list(
    initialize = function() {
      private$url = NULL
      private$user = NULL
      private$password = NULL
      private$repository = NULL
      private$authentication = NULL
      private$ontology_title = NULL
      private$ontology_description = NULL
      private$ontology_versionIRI = NULL
    },
    access_options = function() {
      return(list(
        url = private$url,
        user = private$user,
        password = private$password,
        repository = private$repository,
        authentication = private$authentication,
        protocol = private$protocol
      )
      )
    },
    set_access_options = function(url = NULL, user = NULL, password = NULL, repository = NULL) {
      # Set private objects
      private$url = url
      private$user = user
      private$password = password
      private$repository = repository
      private$authentication = NULL
      
      # Set authentication
      if (!is.null(user) && !is.null(password)) {
        private$authentication = httr::authenticate(user = user,
                                                    password = password,
                                                    type = "basic"
        )
      }
      
      # Get protocol version
      if (!is.null(url) && !is.null(private$authentication)){
        # Target url
        protocol_url <-  paste0(url, "/protocol")
        
        # Response from server
        server_response = httr::content(httr::GET(url = protocol_url,
                                                  config = private$authentication
        )
        )
        
        # Set private protocol
        if (is.na(as.integer(server_response)))
          stop(server_response)
        else
          private$protocol <- as.integer(server_response)
      }      
    },
    submit_query = function(query)
    {
      if (!is.null(self$access_options()$url) && !is.null(self$access_options()$authentication)) {
        result = httr::POST(
          url = paste0(private$url, "/repositories/", private$repository),
          private$authentication,
          httr::add_headers(Accept = "text/csv, */*;q=0.5"),
          httr::add_headers('Content-Type' = "application/x-www-form-urlencoded; charset=utf-8"),
          body = list(query = query),
          encode = "form"
        )
        
        return (utils::read.csv(textConnection(httr::content(result, as = "text")), stringsAsFactors = FALSE))
      } else {
        stop("Triplestore is not accessible, pleas review access options.")
      }
    },
    ontology = function()
    {
      # get ontology info
      ontology <- self$submit_query("PREFIX owl: <http://www.w3.org/2002/07/owl#>
                                     PREFIX dc: <http://purl.org/dc/elements/1.1/>
                                     PREFIX obo: <http://purl.obolibrary.org/obo/>
                                     select * where { 
                                       obo:ontoavida.owl dc:title ?title ;
                                         dc:description ?description ;
                                         owl:versionIRI ?versionIRI .
                                     }")
      # set private properties
      private$ontology_title = ontology$title
      private$ontology_description = ontology$description
      private$ontology_versionIRI = ontology$versionIRI
      
      return(list(
        title = private$ontology_title,
        description = private$ontology_description,
        versionIRI = private$ontology_versionIRI
      ))
    }
  )
)

#' Set options to access the database
#' 
#' @description Set options to access a specific triple-store implemented in
#' GraphDB.
#' 
#' @examples 
#' 
#' # Set access options to graphddb
#' triplestore$set_access_options(
#'   url = "https://graphdb.fortunalab.org",
#'   user = "public_avida",
#'   password = "public_avida",
#'   repository = "avidaDB_tests"
#' )
#'
#'# Show current access options
#'triplestore$access_options()
#'
#'# Querying data with SPARQL
#'triplestore$submit_query('PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#'                          PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
#'                          select ?tandem_id where { 
#'                            ?digital_tandem_repeat rdfs:label "digital tandem repeat"@en .
#'                            ?tandem_id a ?digital_tandem_repeat .
#'                          } limit 10')
#'
#'# Show ontology info
#'triplestore$ontology()
#'
#' @export
triplestore <- triplestore_access$new()
triplestore$set_access_options(url = "https://graphdb.fortunalab.org",
                               user = "public_avida",
                               password = "public_avida",
                               repository = "avidaDB_test")
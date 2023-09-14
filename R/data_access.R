#' Class to manage triplestore access options
#'
#' @description Class to manage triplestore access options
#' 
#' @examples 
#' 
#' # Create object triplestore
#' 
#' triplestore <- triplestore_access$new()
#' 
#' 
#' # Set options to access a specific triple-store implemented in GraphDB
#' 
#' triplestore$set_access_options(
#'   url = "https://graphdb.fortunalab.org",
#'   user = "public_avida",
#'   password = "public_avida",
#'   repository = "avidaDB_test"
#' )
#' 
#' 
#'# Show current access options
#'
#'triplestore$get_access_options()
#'
#'
#'# Querying data with SPARQL
#'
#'triplestore$submit_query('PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#'                           PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
#'                           select ?tandem_id where { 
#'                             ?digital_tandem_repeat rdfs:label "digital tandem repeat"@en .
#'                             ?tandem_id a ?digital_tandem_repeat .
#'                           } limit 10')
#'
#'
#'# Show ontology info
#'
#'triplestore$ontology()
#'
#' 
#' @import R6
#' @import httr
#' @import xml2
#'
#' @export
triplestore_access <- R6::R6Class(
  classname = "triplestore_access",
  private = list(
    url = NULL,
    user = NULL,
    password = NULL,
    repository = NULL,
    authentication = NULL,
    protocol = NULL,
    timeout = NULL,
    ontology_title = NULL,
    ontology_description = NULL,
    ontology_versionIRI = NULL
  ),
  public = list(
    #'
    #' @description Create and initialize the object.
    #' 
    #' @return Object of class triplestore_access.
    #' 
    initialize = function() {
      private$url = NULL
      private$user = NULL
      private$password = NULL
      private$repository = NULL
      private$authentication = NULL
      private$protocol = NULL
      private$timeout = NULL
      private$ontology_title = NULL
      private$ontology_description = NULL
      private$ontology_versionIRI = NULL
    },
    #'
    #' @description Get access options
    #' 
    #' @return list containing URL of the API server, user credentials,
    #' repository name, authentication status, and SPARQL protocol version
    #' 
    get_access_options = function() {
      return(
        list(
          url = private$url,
          user = private$user,
          password = private$password,
          repository = private$repository,
          authentication = private$authentication,
          protocol = private$protocol,
          timeout = private$timeout
        )
      )
    },
    #'
    #' @description Set authentication access options for graphdb triplestore
    #' 
    #' @param url String containing the URL of the triplestore server
    #' @param repository String containing the ID of the repository to which you want to connect to
    #' @param user String containing the username if authentication is needed
    #' @param password String containing the password if authentication is needed
    #' @param timeout Connection timeout limit in seconds used for queries
    #' 
    set_access_options = function(url = NULL, user = NULL, password = NULL, repository = NULL, timeout = 100) {
      # Set private objects
      private$url = url
      private$user = user
      private$password = password
      private$repository = repository
      private$authentication = NULL
      private$timeout = timeout
      
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

        server_response = server_api_get(
          url = private$url,
          resource = "protocol",
          authentication = private$authentication
        )
        
        # Set private protocol
        if (is.null(server_response))
          message(server_response) # stop()
        else
          private$protocol <- as.integer(server_response)
      }   
    },
    #'
    #' @description Submit a SPARQL query to the triplestore to obtain data
    #' 
    #' @param query String containing the SPARQL query to retrieve data
    #' 
    #' 
    submit_query = function(query)
    {
      return(server_api_post(
        url = private$url,
        repository = private$repository,
        authentication = private$authentication,
        query = query,
        max_seconds = private$timeout
      ))
    },
    #' 
    #' @description Show ontology information
    #' 
    #' @return List containing title, description and versionIRI of the ontology
    #' 
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

#' Get database summary
#'
#' @description Get a summary of the data stored.
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @return Data frame: Columns: "data type", "value".
#'
#' @examples
#'
#' avidaDB <- triplestore_access$new()
#'
#' avidaDB$set_access_options(
#'   url = "https://graphdb.fortunalab.org",
#'   user = "public_avida",
#'   password = "public_avida",
#'   repository = "avidaDB_test"
#' )
#'
#' get_db_summary(triplestore = avidaDB)
#'
#' @export
#'
get_db_summary <- function(triplestore)
{
  # Get summary
  abstract <- triplestore$submit_query("PREFIX terms: <http://purl.org/dc/terms/>
                                        PREFIX ONTOAVIDA: <http://purl.obolibrary.org/obo/ONTOAVIDA_>
                                        select ?abstract where {
                                          ONTOAVIDA:11111111 terms:abstract ?abstract .
                                        } ORDER BY DESC(?abstract) LIMIT 1")$abstract

  if (is.null(abstract))
    return(invisible(NULL))
  
  # Convert to data frame
  df <- data.frame()
  if (length(abstract) > 0) {
    ul <- unlist(strsplit(abstract, split = "\n"))
    for(i in 1:length(ul))
    {
      if (i > 1) {
        data <- unlist(strsplit(ul[i], ","))
        df[i-1,1] <- data[1]
        df[i-1,2] <- as.numeric(data[2])
      }
    }
    colnames(df) <- unlist(strsplit(ul[1], ","))
  }
  return(df)
}

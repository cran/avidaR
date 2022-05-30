#' Get database summary
#'
#' @description Get a summary of the data stored.
#'
#' @return Data frame: Columns: "data type", "value".
#' 
#' @examples 
#' 
#' get_db_summary()
#'
#' @export
#' 
get_db_summary <- function()
{
  # Get summary
  abstract <- triplestore$submit_query("PREFIX terms: <http://purl.org/dc/terms/>
                                        PREFIX ONTOAVIDA: <http://purl.obolibrary.org/obo/ONTOAVIDA_>
                                        select ?abstract where { 
                                          ONTOAVIDA:11111111 terms:abstract ?abstract .
                                        }")$abstract
  
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
#'
#' @noRd
#'
all_at_seed <- function() {
  seeds <- c(0, 0)
  names(seeds) <- c("encodes_at_seed_id", "executes_at_seed_id")
  return(seeds)
}

#' validate access_options
#'
#'
#'
#' @noRd
#'
validate_access_options <- function(access_options = NULL) {
  if (is.null(access_options)) {
    stop("graphdb access_options is NULL")
  }
}

#' validate_logic_operation
#'
#' @param logic_operation a subset of logic_operation_default
#'
#' @noRd
#'
validate_logic_operation <- function(logic_operation) {
  if (get_data_type(value = logic_operation) == 3) {
    if (logic_operation[1] != "") {
        length_intersec <- length(Reduce(intersect, list(logic_operation, logic_operation_default())))
        length_default  <- length(logic_operation)
        if (length_intersec != length_default) {
          stop("Boolean operation not valid")
        }
      }
    }
}

#' validate_param
#'
#' @param param a string of character with the name of the param
#' @param value locgical, string, or integer
#' @param types a integer value
#'
#' @noRd
#'
validate_param <- function(param, value, types) {
  if (!(get_data_type(value = value) %in% types)) {
    message(paste0("Data type is not supported for ", param))
  }
}

#' get_data_type
#'
#' @param param a string of character with the name of the param
#' @param value locgical, string, or integer
#'
#' @return
#'  1: when value is a single logical value
#'  2: when value is numeric or a vector of integer
#'  3: when value is character of a verctor of character
#'  4: when value is of class triplestore_access
#'
#' @noRd
#'
get_data_type <- function(value) {
  if (is.logical(value) && length(value) == 1) {
    return(1)
  } else if (is.numeric(value) && length(value) > 0) {
    return(2)
  } else if (is.character(value) && length(value) > 0) {
    return(3)
  } else if ("triplestore_access" %in% class(value)) {
    return(4)
  } else {
    return(0)
  }
}

#' replace_at_seed_id
#' @param seed_id logical, or integer
#' @param at_seed_vars a vector of character
#' @param query a string of character
#'
#' @return query a string of character
#'
#' @noRd
#'
replace_at_seed_id <- function(seed_id, at_seed_vars, query) {

  all_at_seed_vars <- all_at_seed()
  names(all_at_seed_vars) <- gsub("_id","", names(all_at_seed_vars))

  if(isTRUE(seed_id)) {
    seed_id <- c(1:1000)
  }

  for(this_at_seed in at_seed_vars) {
    if(this_at_seed %in% names(all_at_seed_vars)) {
      if(get_data_type(seed_id) == 2) {
        base_index <- as.integer(all_at_seed_vars[this_at_seed][[1]])
        query <- replace_data(param = this_at_seed, value = base_index + seed_id, query = query)
      }
    }
  }

  if(get_data_type(value = seed_id) == 1 || isFALSE(seed_id)) {
    query <- gsub("#encodes_at_seed_id#", "", query)
    query <- gsub("#encodes_at_seed#", "", query)
    #query <- gsub("[?]encodes_at_seed_id", "ONTOAVIDA:00001198", query)
    query <- gsub("#encodes#", "ONTOAVIDA:00001198", query)

    query <- gsub("#executes_at_seed_id#", "", query)
    query <- gsub("#executes_at_seed#", "", query)
    #query <- gsub("[?]executes_at_seed_id", "ONTOAVIDA:00000004", query)
    query <- gsub("#executes#", "ONTOAVIDA:00000004", query)
  }

  return(query)
}
#' replace_data
#'
#' @param param a string of character with the name of the param
#' @param value locgical, string, or integer
#' @param query a string of character
#'
#' @return the query once the param has been replaced
#'
#' @noRd
#'
replace_data <- function(param, value, query) {
  # Set vars
  search_param <- paste0("#", param, "#")
  search_id <- paste0("#", param, "_id#")
  replace_id <- paste0("?", param, "_id")
  replace_value <- if(isTRUE(grepl("seed", param)))
    paste0("rdf:_", value)
  else
    paste0(paste0("ONTOAVIDA:", param, "_", format(value, scientific = FALSE, trim = TRUE)), collapse = " ")

  # Id vars
  id_vars <- c("encodes_at_seed", "executes_at_seed", "seed", "genome", "transcriptome", "tandem", "phenotype", "avida_experiment", "organism", "docker_image")

  # Triple vars
  triple_vars <- c("genome_seq", "transcriptome_seq", "transcriptome_pos", "tandem_seq", "tandem_pos", "doi", "description")

  # Replace identifier
  if (param %in% id_vars) {
    v <- get_data_type(value = value)

    if ((v == 1 && isTRUE(value)) || v == 2) {
      query <- gsub(search_id, replace_id, query)
    } else {
      query <- gsub(search_id, "", query)
    }

    if ((v == 1 && isTRUE(value) && param == "seed")) {
      value <- seq(1:1000)
      replace_value <- paste0(paste0("ONTOAVIDA:", formatC(value, width = 8, format = "d", flag = "0")), collapse = " ")
      v <- 2
    }

    if (v == 2) {
      query <- gsub(search_param, paste0("VALUES ", replace_id, " { ", paste0(replace_value, collapse = " "), " } ."), query)
    }
  }

  # Replace boolean operations
  if (param == "logic_operation") {
    logic_operation <- value
    boolean_value <- "TRUE"
    if (length(value) == 1) {
      if (value == "") {
        logic_operation <- logic_operation_default()
        boolean_value <- "FALSE"
      }
    }
    logic_operation_uri <- paste0("ONTOAVIDA:", formatC(c(112:120), width = 8, format = "d", flag = "0"))
    names(logic_operation_uri) <-  c("and", "and-not", "equals", "not-and", "not-or", "not", "or", "orn-not", "exclusive or")
    logic_operation_triple <- paste(paste0("?phenotype_id RO:0002507 ", logic_operation_uri[logic_operation], " ."), collapse = "\n")
    query <- gsub("#logic_operation_triple#", logic_operation_triple, query)
  }

  # Replace genome sequence
  if (param == "genome_seq") {
    query <- gsub("#genome_seq#", "?genome_seq", query)
    query <- gsub("#genome_seq_triple#", paste0("VALUES ?genome_seq { ", paste(paste0('"', value, '"'), collapse = " "), " } ."), query)
  }

  # Replace transcriptome sequence
  if (param == "transcriptome_seq") {
    query <- gsub("#transcriptome_seq#", "?transcriptome_seq", query)
    query <- gsub("#transcriptome_seq_triple#", paste0("VALUES ?transcriptome_seq { ", paste(paste0('"', value, '"'), collapse = " "), " } ."), query)
  }

  # Replace transcriptome pos
  if (param == "transcriptome_pos") {
    query <- gsub("#transcriptome_pos#", "?transcriptome_pos", query)
  }

  # Replace tangem sequence
  if (param == "tandem_seq") {
    query <- gsub("#tandem_seq#", "?tandem_seq", query)
    query <- gsub("#tandem_seq_triple#", paste0("VALUES ?tandem_seq { ", paste(paste0('"', value, '"'), collapse = " "), " } ."), query)
  }

  # Replace tandem pos
  if (param == "tandem_pos") {
    query <- gsub("#tandem_pos#", "?tandem_pos", query)
  }

  # Replace doi
  if (param == "doi") {
    value <- gsub("https://doi.org/", "", value)
    if((get_data_type(value) == 1 && isTRUE(value)) || get_data_type(value) != 1 ) {
      query <- gsub("#doi#", "?doi", query)
    } else {
      query <- gsub("#doi#", "", query)
    }
    if(get_data_type(value) != 1) {
      query <- gsub("#doi_triple#", paste0("VALUES ?doi { ", paste(paste0('"', value, '"'), collapse = " "), " } ."), query)
    }
  }

  # Replace description
  if (param == "description") {
    query <- if (isTRUE(value))
      gsub("#description#", "?description", query)
    else
      gsub("#description#", "", query)
  }



  # Return query
  return(query)
}

#' clean_at_seed_id
#'
#' @param data a data frame
#' @param seed_id logical, or integer
#' @param at_seed_vars a vector of character
#' @return a data frame
#'
#' @noRd
#'
clean_at_seed_id <- function(data, seed_id, at_seed_vars){
  if(get_data_type(value = seed_id) == 1 && isFALSE(seed_id)) {
    return(data)
  }

  for(this_at_seed in at_seed_vars) {
    if(this_at_seed %in% names(all_at_seed()) && this_at_seed %in% names(data)) {
      if(get_data_type(value = seed_id) == 2 || isTRUE(seed_id)) {
        data <- data %>% rename(seed_id = all_of(this_at_seed))
        data <- data %>% rowwise() %>% mutate(seed_id = paste0("seed_", as.integer(seed_id) - as.integer(all_at_seed()[this_at_seed][[1]])))
      }
    }
  }

  return(data)
}

#' show_hide_vars
#'
#' @param vars a list values with names
#' @param query a string of character
#'
#' @return a string of character
#'
#' @noRd
show_hide_vars <- function(vars, query) {
  for (v in names(vars)) {
    search_v <- paste0("#", v, "#")
    if (vars[v] == "TRUE") {
      query <- gsub(search_v, paste0("?", v), query)
    } else {
      query <- gsub(search_v, "", query)
    }
  }
  return(query)
}

#' remove_prefix
#'
#' @param prefix a string of character
#' @param data a data.frame
#' @param types a integer value
#'
#' @return data frame
#'
#' @noRd
remove_prefix <- function(prefix, data) {
  # add _ to rdf_prefix
  if(prefix == rdf_prefix())
    prefix = paste0(rdf_prefix(), "_")

  # Column names
  cols <- c("organism_id", "seed_id", "encodes_at_seed_id","executes_at_seed_id", "genome_id", "transcriptome_id", "tandem_id", "phenotype_id", "genome_id_ancestor", "genome_id_mutant", "organism_id_ancestor", "organism_id_mutant", "genome_id_wild_type", "genome_seq_wild_type", "avida_experiment_id", "docker_image_id")

  # Replace prefix with empty string
  for (col_name in cols) {
    if (col_name %in% colnames(data)) {
      data[, col_name] <- gsub(prefix, "", data[, col_name])
    }
  }

  # Return data frame
  return(data)
}

#' show_hide_columns
#'
#' @param vars_list a list values with names
#' @param data a data.frame
#'
#' @return data frame
#'
#' @noRd
show_hide_columns <- function(vars_list, data) {
  for (v in names(vars_list)) {
    # phenotype_binary
    if (isTRUE(as.logical(vars_list[v]))) {
      # Show var
      if (v == "phenotype_binary") {
        for (i in 1:nrow(data)) {
          data[i, "phenotype_binary"] <- paste(as.integer(intToBits(gsub("phenotype_", "", data[i, "phenotype_id"])))[9:1], collapse = "")
        }
      }

      if (v == "transcriptome_pos") {
        data <- data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(transcriptome_pos = decompress_sequence(.data$transcriptome_pos))
        data <- as.data.frame(data)
      }

      if (v == "transcriptome_seq") {
        data <- data %>%
          dplyr::rowwise() %>%
          dplyr::mutate(transcriptome_seq = decompress_sequence(.data$transcriptome_seq))
        data <- as.data.frame(data)
      }

    } else {
      data[, v] <- NULL
    }
  }

  # return data
  return(data)
}

#' decompress_sequence
#'
#' @param x a string of character
#' @param data a data.frame
#'
#' @return character
#'
#' @noRd
#'
decompress_sequence <- function(x) {
  # convert string to a vector of 1 character
  # s <- strsplit(x, "")[[1]]

  # converts the vector to vector of 2 characters: paste0(s[c(TRUE, FALSE)], s[c(FALSE, TRUE)])
  # then converts each element of the vector to integer (equivalent to convert from hex to int): strtoi(16L)
  # then converts it to raw format: as.raw(x)
  # decompress it with gzip: memDecompress(x,type="gzip")
  # and finally returns as string: rawToChar(x)
  # return(rawToChar(memDecompress(paste0(s[c(TRUE, FALSE)], s[c(FALSE, TRUE)]) %>% strtoi(16L) %>% as.raw(), type = "gzip")))
  return (memDecompress(base64enc::base64decode(what = x), "gzip", asChar=TRUE))
}

#' logic_operation_to_binary
#'
#' @param logic_operation List of logical functions withing the following:
#' "equals", "exclusive-or", "not-or", "and-not", "or", "orn-not", "and",
#' "not-and", "not".
#'
#' @return string of character
#'
#' @noRd
logic_operation_to_binary <- function(logic_operation) {
  phenotype_binary <- paste(as.integer(logic_operation_default() %in% logic_operation), collapse = "")
}

#' logic_operation_to_integer
#'
#' @param logic_operation List of logical functions withing the following:
#' "equals", "exclusive-or", "not-or", "and-not", "or", "orn-not", "and",
#' "not-and", "not".
#'
#' @return integer
#'
#' @noRd
logic_operation_to_integer <- function(logic_operation) {
  integer_value <- strtoi(logic_operation_to_binary(logic_operation), base = 2)
  return(integer_value)
}

capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
  tryCatch(
    list(result = code, error = NULL),
    error = function(e) {
      if (!quiet)
        message(e$message)

      list(result = otherwise, error = e)
    },
    interrupt = function(e) {
      stop("Terminated by user", call. = FALSE)
    }
  )
}

safely <- function(.f, otherwise = NULL, quiet = TRUE) {
  function(...) capture_error(.f(...), otherwise, quiet)
}

get_message_from_status_code <- function(status_code, url, repository) {

  status_message <- paste0(
    switch(
      as.character(status_code),
      "400" = "Bad request':\n- The server cannot process your request, check you provided a valid url and respository name.",
      "401" = "Not authorized. Please provide a valid url, user, password, and repository name for access options.",
      "403" = "You are trying to access a resource that doesn't exist or is forbidden. Please check the provided URL and repository name.",
      "404" = "Not found. Please double check that server url and repository name provided are valid and have a correct syntax.",
      "500" = "Sorry, we are having some interna issue with our service.",
      "522" = "Connection timed out reached. Please check your internet connection. Some requests may take longer to process, so consider requesting fewer items or increasing the timeout limit (in seconds).",
      "526" = "Sorry, we are having some internal issue with our service.",
      "Ups! Something went bad, try in a few minutes."
    )
  )

  if (status_code == "500" || status_code == "403") {
    if (endsWith(url, "/") == TRUE || grepl("/", repository) == TRUE) {
      status_message <- paste0(
        status_message,
        "\n- Please, remove any extra '/' at the end of the server url and/or in repository name that might be causing such type of error.",
        "\n- It could also be that you do not have the necessary privileges to access the requested resource."
      )
    }
  }

  status_message <- paste0(
    status_message,
    " If error persists, please contact server administrator."
  )

  return(status_message)
}

server_no_response <- function(resource_url) {
  return(
    paste0(
      "There is no response from service (", resource_url, "). Check that the url provided is correct. If error persists, please contact to server administrator."
    )
  )
}


#' @description Server API POST method
#'
#' @param url A single URL
#' @param repository Repository name
#' @param authentication what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param max_seconds Number of seconds to consider a timeout scenario.
#' #'
#' @return data frame
#'
#' @noRd
server_api_post <- function(url, repository, authentication, query, quiet = FALSE, max_seconds = 20) {
  
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  sPOST <- safely(httr::POST)

  resource_url <- paste0(url, "/repositories/", repository)

  res <- sPOST(
    url = resource_url,
    authentication,
    httr::add_headers(Accept = "text/csv, */*;q=0.5"),
    httr::add_headers('Content-Type' = "application/x-www-form-urlencoded; charset=utf-8"),
    body = list(query = query),
    encode = "form",
    timeout(max_seconds)
  )
  
  if (is.null(res$result)) {
    message("The service is not available in this moment, please try it later, if the problem persists contact the administrator.")
    return(invisible(NULL))
  }

  if (is.null(res$result) || ((httr::status_code(res$result) %/% 200) != 1)) {

    if (is.null(res$result)) {
      if (!is.null(res$error) && grepl("imeout", res$err)) {
        if (!quiet) message(get_message_from_status_code(522, url, repository)) #stop(call. = FALSE, get_message_from_status_code(522, url, repository))
      } else
          message(server_no_response(resource_url)) #stop(call. = FALSE, server_no_response(resource_url))
    }


    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) message(get_message_from_status_code(httr::status_code(res$result), url, repository)) # stop(call. = FALSE, get_message_from_status_code(httr::status_code(res$result), url, repository))
    }

  } else {
    return(utils::read.csv(textConnection(httr::content(res$result, as = "text")), stringsAsFactors = FALSE))
  }

}

#' @description Server API GET method.
#'
#' @param url A single URL.
#' @param resource Server resource to be requested.
#' @param authentication what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param max_seconds Number of seconds to consider a timeout scenario.
#' #'
#' @return String of character.
#'
#' @noRd
server_api_get <- function(url, resource, authentication, quiet = FALSE, max_seconds = 20) {

  sGET <- safely(httr::GET)

  resource_url <- ""

  if (resource == "protocol")
    resource_url <- paste0(url, "/protocol")

  res <- sGET(
    url = resource_url,
    config = authentication,
    timeout(max_seconds)
  )
  
  if (!curl::has_internet()) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  if (is.null(res$result) || ((httr::status_code(res$result) %/% 200) != 1)) {

    if (is.null(res$result)) {
      message(server_no_response(resource_url)) #stop(call. = FALSE, server_no_response(resource_url))
      return(invisible(NULL)) #return(res$result)
    }

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) {
        #stop(call. = FALSE, get_message_from_status_code(httr::status_code(res$result), url, resource))
        message(get_message_from_status_code(httr::status_code(res$result), url, resource))
        return(invisible(NULL)) #return(res$result)
      }
    }

  } else {
    return(httr::content(res$result))
  }

}

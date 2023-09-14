#' Converts a genome instruction sequence into a digital organism file
#'
#' @description Converts a digital organism instruction set into a digital
#' genome instruction sequence.
#'
#' @param org Data Frame. Column names: `instruction`.
#' @param inst_set Name of the instruction set. It must be one of the
#' following: `heads` (default), `heads-sex`", or `transsmt`. The names
#' correspond to the instruction set configuration files (e.g.,
#' `instset-heads.cfg` for `heads`.
#' @param save Logical value (TRUE/FALSE) indicating whether the output should
#' or should not be saved to a file ("FALSE" by default).
#' @param file_name String of characters representing the name of the file
#' without any extension ("genome.seq" by default).
#' @param save_path String of characters representing the name of the folder
#' where the digital organism file will be saved.
#' @param format String of characters representing the format of the file
#' ("seq" by default).
#' @param silent Logical value (TRUE/FALSE) to show/hide messages
#' ("FALSE" by default).
#'
#' @return String of characters.
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
#' sequence <- get_genome_seq_from_genome_id(
#'   genome_id = 1,
#'   triplestore = avidaDB
#' )$genome_seq[[1]]
#'
#' convert_seq_into_org(genome_seq = sequence)
#'
#' @export
convert_org_into_seq <- function(org, inst_set = "heads", save = FALSE, file_name = NULL, save_path = getwd(), format = "seq", silent = FALSE) {
  # verify output path
  if (isTRUE(save)) {
    if (!dir.exists(save_path)) {
      dir.create(save_path)
    }
  }

  # join
  seq <- dplyr::inner_join(org, instruction_set(inst_set = inst_set), by = "instruction") %>% dplyr::select("letter")

  # collapse
  seq <- paste(seq$letter, collapse = '')

  # save to file
  if (isTRUE(save)) {
    if (is.null(file_name)) {
      file_name <- "genome"
    }

    output_file <- paste0(save_path, "/", file_name, paste0(".", format))

    readr::write_csv(org, file = output_file, col_names = FALSE)

    if (isFALSE(silent)) {
      message(paste0("output saved to ", output_file))
    }
  }

  # return org
  return(seq)
}


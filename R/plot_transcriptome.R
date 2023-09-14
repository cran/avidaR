#' Get a plot of the transcriptome as a chord diagram
#'
#' @description Get a plot of the transcriptome executed by a digital organism
#' for a list of seeds used for starting the pseudo-random number generator
#' (i.e., a set of environments).
#'
#' @param transcriptome_id Integer
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
#'
#' @param inst_set Name of the instruction set. It must be one of the
#' following: `heads` (default), `heads-sex`, or `transsmt`. The names
#' correspond to the instruction set configuration files (e.g.,
#' `instset-heads.cfg` for `heads`).
#'
#' @param save Logical value (TRUE/FALSE) to save the plot (FALSE by default).
#'
#' @param file_name String of characters indicating the name of the file to be
#' saved (without extension).
#'
#' @param save_path String of characters indicating the name of the folder where
#' the file will be saved.
#'
#' @param format String of characters indicating the format of the file ("pdf"
#' and "svg" are currently supported).
#'
#' @param silent Logical value (TRUE/FALSE) to show/hide messages (TRUE by
#' default).
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @examples
#' \donttest{
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
#' # plot transcriptome 53674 at seed_1 and save to disk in pdf format
#' plot_transcriptome(
#'   transcriptome_id = 53674,
#'   seed_id = 1,
#'   save = FALSE,
#'   save_path = getwd(),
#'   format = "pdf",
#'   triplestore = avidaDB
#' )
#' }
#' @importFrom grDevices dev.off graphics.off pdf svg
#'
#' @noRd
plot_single_transcriptome <- function(transcriptome_id, seed_id = NULL, inst_set = "heads", save = FALSE, file_name = NULL, save_path = "~/transcriptome@chords", format = "svg", silent = FALSE, triplestore) {
  # Validate params
  validate_param(param = "transcriptome_id", value = transcriptome_id, types = 2)
  if(!is.null(seed_id)) validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "save", value = save, types = 1)
  if(!is.null(file_name)) validate_param(param = "file_name", value = file_name, types = 3)
  validate_param(param = "save_path", value = save_path, types = 3)
  validate_param(param = "format", value = format, types = 3)
  validate_param(param = "silent", value = silent, types = 1)
  validate_param(param = "triplestore", value = triplestore, types = 4)
  validate_param(param = "inst_set", value = inst_set, types = 3)

  # Verify output directory
  if (isTRUE(save)) {
    if (!dir.exists(save_path)) {
      dir.create(save_path)
    }
  }

  # Validate format
  formats <- c("pdf", "svg")
  if (isFALSE(format %in% formats)) {
    stop(paste0('Output format "', format, '" is not supported! Please, try one of these: ', paste0(formats, collapse = " ")))
  }

  # Get data
  if (is.null(seed_id)) {
    data <- get_transcriptome_seq_from_transcriptome_id(transcriptome_id = transcriptome_id, seed_id = FALSE, transcriptome_pos = TRUE, genome_seq=TRUE, triplestore = triplestore)[1, ]
  } else {
    data <- get_transcriptome_seq_from_transcriptome_id(transcriptome_id = transcriptome_id, seed_id = seed_id, transcriptome_pos = TRUE, genome_seq=TRUE, triplestore = triplestore)
  }

  if (is.null(data))
    return(invisible(NULL))
  
  if (nrow(data)>0) {
    # Change genome_seq format to a column vector
    genome_vector <- dplyr::tibble(letter = strsplit(as.character(data$genome_seq), "")[[1]])

    # Get instruction and color of genome_seq letters
    inst_genome <- dplyr::inner_join(genome_vector, instruction_set(inst_set = inst_set), by = "letter") %>% dplyr::mutate(pos = dplyr::row_number() - 1)

    # Change transcript_pos format to a column vector
    transcript_vector <- dplyr::tibble(letter = strsplit(as.character(data$transcriptome_pos), "[|]")[[1]])

    # get all pairs of positions in the transcript that are linked by the execution flow
    links <- cbind(transcript_vector %>% utils::head(-1), transcript_vector %>% utils::tail(-1))
    colnames(links) <- c("from", "to")
    links <- links %>%
      dplyr::group_by(.data$from, .data$to) %>%
      dplyr::summarise(value = dplyr::n(), .groups = "drop")
    links$from <- as.integer(links$from)
    links$to <- as.integer(links$to)

    # Get pairs of genome's positions
    all_pairs <- expand.grid(0:99, 0:99)
    colnames(all_pairs) <- c("from", "to")

    # Get all genome's positions linked by the execution flow (weighted):
    edge_list <- dplyr::left_join(all_pairs, links, by = c("from", "to")) %>% dplyr::mutate(value = tidyr::replace_na(as.double(.data$value), 0.001))
    edge_list <- edge_list %>% dplyr::mutate(from = as.character(.data$from), to = as.character(.data$to))

    graphics.off()

    # Save the chord
    if (isTRUE(save)) {

      if (is.null(file_name)) {
        file_name <- paste0("transcriptome_", transcriptome_id)
      }

      output_file <- paste0(save_path, "/", file_name, ".", format)

      switch(format,
             "pdf" = {
               pdf(output_file,
                   width = 8,
                   height = 8,
                   pointsize = 12
               )
             },
             "svg" = {
               svg(output_file,
                   width = 8,
                   height = 8,
                   pointsize = 12
               )
             }
      )
    }

    # Plot the chord
    circos.clear()
    circos.par(start.degree = 90, points.overflow.warning = FALSE)
    arrow_color <- "black"
    sector_color <- inst_genome$color
    names(sector_color) <- inst_genome$pos
    chordDiagram(edge_list,
      grid.col = sector_color, # color of the sectors
      directional = 1, # directed links
      link.sort = TRUE, link.decreasing = FALSE, # sorting links, first the smaller
      diffHeight = uh(5, "mm"), # from first column to second column (the position of starting end of the link is shorter than the other end)
      direction.type = c("diffHeight", "arrows"), link.arr.col = arrow_color, link.arr.length = 0.5, # combine both
      link.lwd = 1, link.lty = 1, link.border = "black", # width, style, and color of the link border
      link.visible = edge_list[[3]] >= 1, # highlight links with values larger than a cutoff (I have used value=0.001 to plot sectors with no links)
      link.zindex = rank(edge_list[[3]]), # wide links more forward and small links more backward (larger values)
      annotationTrack = "grid", annotationTrackHeight = 0.15, # only one track is defined
    )

    # Customizing the labels: put them on the grid (defined only one track: annotationTrack = "grid")
    for (si in get.all.sector.index()) {
      xlim <- get.cell.meta.data("xlim", sector.index = si, track.index = 1)
      ylim <- get.cell.meta.data("ylim", sector.index = si, track.index = 1)
      xplot <- get.cell.meta.data("xplot", sector.index = si, track.index = 1)
      if (abs(xplot[2] - xplot[1]) > 2) { # print only the labels of sectors with more than one link
        circos.text(mean(xlim), mean(ylim),
          labels = inst_genome$instruction[as.numeric(si) + 1], sector.index = si, track.index = 1,
          facing = "bending.inside", niceFacing = TRUE, col = "white"
        )
      }
    }

    if (isTRUE(save)) {
      dev.off()
      if (isFALSE(silent)) {
        message(paste0("Plot will be saved to ", output_file))
      }
    }

  } else {
    message("No data found!")
  }
  
  return(TRUE)
}

#' Get a plot of the transcriptome as a chord diagram
#'
#' @description Get a plot of the transcriptome executed by a digital organism
#' for a list of seeds used for starting the pseudo-random number generator
#' (i.e., a set of environments).
#'
#' @param transcriptome_id Integer
#'
#' @param seed_id Integer (from 1 to 1000), a vector of integer values, or a
#' logical value. This integer is used for starting the pseudo-random number
#' generator that represents the environment experiencing a digital organism.
#' If a logical value is used, TRUE returns data found in all environments and
#' FALSE (by default) returns only distinct data regardless of the seed.
#'
#' @param inst_set Name of the instruction set. It must be one of the
#' following: `heads` (default), `heads-sex`, or `transsmt`. The names
#' correspond to the instruction set configuration files (e.g.,
#' `instset-heads.cfg` for `heads`).
#'
#' @param save Logical value (TRUE/FALSE) to save the plot (FALSE by default).
#'
#' @param file_name String of characters indicating the name of the file to be
#' saved (without extension).
#'
#' @param save_path String of characters indicating the name of the folder where
#' the file will be saved.
#'
#' @param format String of characters indicating the format of the file ("pdf"
#' and "svg" are currently supported).
#'
#' @param silent Logical value (TRUE/FALSE) to show/hide messages (TRUE by
#' default).
#'
#' @param triplestore Object of class triplestore_access which manages database
#' access.
#'
#' @examples
#' \donttest{
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
#' # plot transcriptome 53674 at seed_1 and save to disk in pdf format
#' plot_transcriptome(
#'   transcriptome_id = 53674,
#'   seed_id = 1,
#'   save = FALSE,
#'   save_path = getwd(),
#'   format = "pdf",
#'   triplestore = triplestore
#' )
#' }
#' @importFrom grDevices dev.off graphics.off pdf svg
#'
#' @export
plot_transcriptome <- function(transcriptome_id, seed_id = NULL, inst_set = "heads", save = FALSE, file_name = NULL, save_path = "~/transcriptome@chords", format = "svg", silent = FALSE, triplestore) {
  # Validate params
  validate_param(param = "transcriptome_id", value = transcriptome_id, types = 2)
  if(!is.null(seed_id)) validate_param(param = "seed_id", value = seed_id, types = c(1, 2))
  validate_param(param = "save", value = save, types = 1)
  if(!is.null(file_name)) validate_param(param = "file_name", value = file_name, types = 3)
  validate_param(param = "save_path", value = save_path, types = 3)
  validate_param(param = "format", value = format, types = 3)
  validate_param(param = "silent", value = silent, types = 1)
  validate_param(param = "triplestore", value = triplestore, types = 4)
  validate_param(param = "inst_set", value = inst_set, types = 3)

  for(tr_id in transcriptome_id) {
    response <- plot_single_transcriptome(transcriptome_id = tr_id,
                              seed_id = seed_id,
                              inst_set = inst_set,
                              save = save,
                              file_name = file_name,
                              save_path = save_path,
                              format = format,
                              silent = silent,
                              triplestore = triplestore
                            )
    if (is.null(response))
      return(invisible(NULL))
  }
  return(TRUE)
}

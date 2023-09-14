test_that("plot_transcriptome", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")
  
  suppressMessages({
    triplestore <- triplestore_access$new()
    
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    plot1 <- plot_transcriptome(
      transcriptome_id = 53674,
      format="svg",
      silent = TRUE,
      triplestore = triplestore
    )
  
    plot2 <- plot_transcriptome(
      transcriptome_id = 1666099,
      save = TRUE,
      save_path = tempdir(),
      format="pdf",
      silent = TRUE,
      triplestore = triplestore
    )
    
    skip_if(is.null(response) || is.null(plot1) || is.null(plot2))
  
    expect_true(isFALSE(file.exists(paste0(tempdir(), "/transcriptome_53674.svg"))))
    expect_true(isTRUE(file.exists(paste0(tempdir(), "/transcriptome_1666099.pdf"))))    
  })
})
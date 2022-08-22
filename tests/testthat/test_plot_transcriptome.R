test_that("plot_transcriptome", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  plot_transcriptome(
    transcriptome_id = 53674,
    format="svg",
    silent = TRUE,
    triplestore = triplestore
  )

  plot_transcriptome(
    transcriptome_id = 1666099,
    save = TRUE,
    save_path = tempdir(),
    format="pdf",
    silent = TRUE,
    triplestore = triplestore
  )

  expect_true(isFALSE(file.exists(paste0(tempdir(), "/transcriptome_53674.svg"))))
  expect_true(isTRUE(file.exists(paste0(tempdir(), "/transcriptome_1666099.pdf"))))
})
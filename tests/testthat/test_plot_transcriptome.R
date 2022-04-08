test_that("plot_transcriptome", {

  plot_transcriptome(transcriptome_id = 53674,
                     format="svg",
                     silent = TRUE
  )

  plot_transcriptome(transcriptome_id = 1666099,
                     save = TRUE,
                     save_path = tempdir(),
                     format="pdf",
                     silent = TRUE
  )

  expect_true(isFALSE(file.exists(paste0(tempdir(), "/transcriptome_53674.svg"))))
  expect_true(isTRUE(file.exists(paste0(tempdir(), "/transcriptome_1666099.pdf"))))
})
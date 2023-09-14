test_that("convert_seq_into_org.", {
  
  seq <- "abcdefghijklmnopqrstuvwxyz"
  save_path <- "~/test_avidaR/"

  org <- avidaR::convert_seq_into_org(seq)

  expect_match(class(org), "data.frame")
  expect_equal(as.vector(org$instruction), as.vector(avidaR::instruction_set()$instruction))
  expect_equal(nrow(org), nrow(avidaR::instruction_set()))

  org <- avidaR::convert_seq_into_org(seq, save = TRUE, save_path = save_path, silent = TRUE)

  expect_true(dir.exists(save_path))

  unlink(save_path, recursive = TRUE)

})
test_that("convert_org_into_seq", {
  
  seq1 <- "abcdefghijklmnopqrstuvwxyz"
  save_path <- "~/test_avidaR/"

  org1 <- avidaR::convert_seq_into_org(seq1)

  seq2 <- convert_org_into_seq(org = org1)

  expect_match(class(seq2), "character")
  expect_equal(seq1, seq2)

  seq3 <- avidaR::convert_org_into_seq(org = org1, save = TRUE, save_path = save_path, silent = TRUE)

  expect_true(dir.exists(save_path))

  unlink(save_path, recursive = TRUE)

})

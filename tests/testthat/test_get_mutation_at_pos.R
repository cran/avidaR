test_that("get_mutant_at_pos", {

  found1 <- get_mutant_at_pos(
    inst_replaced = "a",
    inst_replaced_by = "b",
    pos = 1
  ) %>% dplyr::filter(genome_id_ancestor == "genome_582")

  expect_match(class(found1), "data.frame")
  expect_equal(colnames(found1), c("genome_id_ancestor", "genome_seq_ancestor", "genome_id_mutant", "genome_seq_mutant"))
  if (nrow(found1) == 1) {
    expect_equal(found1$genome_id_mutant[1] , "genome_934501")
  }

})
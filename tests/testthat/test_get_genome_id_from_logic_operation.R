test_that("get_genome_id_from_logic_operation", {

  found1 <- get_genome_id_from_logic_operation(
    logic_operation = c("not", "and"),
    seed_id = 1
  )

  found2 <- get_genome_id_from_logic_operation(
    logic_operation = c("not", "and"),
    seed_id = c(2, 1),
    genome_seq = TRUE
  )

  #expect_match(class(found1), "data.frame")
  expect_true(nrow(found1) > 0)
  expect_equal(colnames(found1), c("seed_id", "genome_id"))
  expect_equal(found1$seed_id[1], "seed_1")

  #expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "genome_id", "genome_seq"))
  expect_equal(found2$seed_id[1], "seed_2")

})
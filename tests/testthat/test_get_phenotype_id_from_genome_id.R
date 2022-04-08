test_that("get_phenotype_id_from_genome_id", {

  found1 <- get_phenotype_id_from_genome_id(
    genome_id = 1
  )

  found2 <- get_phenotype_id_from_genome_id(
    genome_id = c(1, 2),
    seed_id = c(2, 1),
    phenotype_binary = TRUE
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1), 1)
  expect_equal(colnames(found1), c("genome_id", "phenotype_id"))
  expect_equal(found1$phenotype_id[1], "phenotype_0")

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "genome_id", "phenotype_id", "phenotype_binary"))
  expect_equal(found2$seed_id[1], "seed_2")
  expect_equal(found2$phenotype_binary[1], "000000000")

})
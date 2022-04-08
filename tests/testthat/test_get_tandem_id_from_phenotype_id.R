test_that("get_tandem_id_from_phenotype", {

  found1 <- get_tandem_id_from_phenotype_id(
    phenotype_id = 1
  )

  found2 <- get_tandem_id_from_phenotype_id(
    phenotype_id = c(1, 2),
    seed_id = c(2,1),
    tandem_seq = TRUE,
    tandem_pos = FALSE
  )

  found3 <- get_tandem_id_from_phenotype_id(
    phenotype_id = c(1, 2),
    seed_id = c(2,1),
    tandem_seq = FALSE,
    tandem_pos = TRUE
  )

  expect_match(class(found1), "data.frame")
  expect_true(nrow(found1) > 0)
  expect_equal(colnames(found1), c("tandem_id", "phenotype_id"))

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "tandem_id", "tandem_seq", "phenotype_id"))
  expect_equal(found2$seed_id[1], "seed_2")

  expect_match(class(found3), "data.frame")
  expect_true(nrow(found3) > 0)
  expect_equal(colnames(found3), c("seed_id", "tandem_id", "tandem_pos", "phenotype_id"))
  expect_equal(grepl("|", found3$tandem_pos[1]), TRUE)

})
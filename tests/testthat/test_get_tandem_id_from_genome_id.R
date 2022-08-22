test_that("get_tandem_id_from_genome_id", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  found1 <- get_tandem_id_from_genome_id(
    genome_id = 1,
    triplestore = triplestore
  )

  found2 <- get_tandem_id_from_genome_id(
    genome_id = c(1, 2),
    seed_id = c(2,1),
    tandem_seq = TRUE,
    tandem_pos = FALSE,
    triplestore = triplestore
  )

  found3 <- get_tandem_id_from_genome_id(
    genome_id = c(1, 2),
    seed_id = c(2,1),
    tandem_seq = FALSE,
    tandem_pos = TRUE,
    triplestore = triplestore
  )

  expect_match(class(found1), "data.frame")
  expect_true(nrow(found1) > 0)
  expect_equal(colnames(found1), c("genome_id", "tandem_id"))
  expect_equal(found1$tandem_id[1], "tandem_4909824")

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "genome_id", "tandem_id", "tandem_seq"))
  expect_equal(found2$seed_id[1], "seed_2")

  expect_match(class(found3), "data.frame")
  expect_true(nrow(found3) > 0)
  expect_equal(colnames(found3), c("seed_id", "genome_id", "tandem_id", "tandem_pos"))
  expect_equal(grepl("|", found3$tandem_pos[1]), TRUE)

})
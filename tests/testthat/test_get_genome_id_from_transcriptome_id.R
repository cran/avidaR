test_that("get_genome_id_from_transcriptome_id", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  found1 <- get_genome_id_from_transcriptome_id(
    transcriptome_id = 53674,
    triplestore = triplestore
  )

  found2 <- get_genome_id_from_transcriptome_id(
    transcriptome_id = c(53674, 1666099),
    seed_id = c(2, 1),
    genome_seq = TRUE,
    triplestore = triplestore
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) > 0 , TRUE)
  expect_equal(colnames(found1), c("genome_id", "transcriptome_id"))
  expect_equal(found1$genome_id[1], "genome_1002")

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "genome_id", "genome_seq", "transcriptome_id"))
  expect_equal(found2$seed_id[1], "seed_2")

})
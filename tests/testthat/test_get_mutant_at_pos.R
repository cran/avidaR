test_that("get_mutant_at_pos", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  found1 <- get_mutant_at_pos(
    genome_id = 582,
    inst_replaced = "o",
    inst_replaced_by = "a",
    pos = 1,
    triplestore = triplestore
  )

  expect_match(class(found1), "data.frame")
  expect_equal(colnames(found1), c("genome_id_wild_type", "genome_seq_wild_type", "genome_id_mutant", "genome_seq_mutant", "pos"))
  expect_true(found1 %>% nrow() > 0)

})
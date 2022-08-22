test_that("get_genome_id_of_wild_type_organisms", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  found1 <- get_genome_id_of_wild_type_organisms(triplestore = triplestore)


  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) > 0 , TRUE)
  expect_equal(colnames(found1), c("genome_id_wild_type"))

})
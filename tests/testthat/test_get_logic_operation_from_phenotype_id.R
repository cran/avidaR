test_that("get_logic_operation_from_phenotype", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  found1 <- get_logic_operation_from_phenotype_id(
    phenotype_id = 511,
    triplestore = triplestore
  )

  found2 <- get_logic_operation_from_phenotype_id(
    phenotype_id = c(1, 3),
    phenotype_binary = TRUE,
    triplestore = triplestore
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1), 1)
  expect_equal(colnames(found1), c("phenotype_id", "equals", "exclusive_or", "not_or", "and_not", "or", "orn_not", "and", "not_and", "not"))
  expect_equal(found1$phenotype_id[1], "phenotype_511")

  expect_match(class(found2), "data.frame")
  expect_equal(nrow(found2), 2)
  expect_equal(colnames(found2), c("phenotype_id", "phenotype_binary", "equals", "exclusive_or", "not_or", "and_not", "or", "orn_not", "and", "not_and", "not"))
  expect_equal(found2$phenotype_id[1], "phenotype_1")
  expect_equal(found2$phenotype_id[2], "phenotype_3")
  expect_equal(found2$phenotype_binary[1], "000000001")
  expect_equal(found2$phenotype_binary[2], "000000011")

})
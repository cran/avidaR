test_that("get_db_summary", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    #engine = "graphdb",
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  summary <- get_db_summary(triplestore = triplestore)

  expect_true(length(summary$value)==17)

})
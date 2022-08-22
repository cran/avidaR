library(avidaR)

test_that("set_access_options", {
  
  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )

  expect_equal(triplestore$get_access_options()$protocol, 12)
  
})
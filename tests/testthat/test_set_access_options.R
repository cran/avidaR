library(avidaR)

test_that("set_access_options", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")
  
  suppressMessages({
    triplestore <- triplestore_access$new()
    
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    skip_if(is.null(response))
  
    expect_equal(triplestore$get_access_options()$protocol, 12)    
  })

  
})
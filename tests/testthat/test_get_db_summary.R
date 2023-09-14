test_that("get_db_summary", {

  skip_if_offline(host = "graphdb.fortunalab.org")

  suppressMessages({  
    triplestore <- triplestore_access$new()
  
    response <- triplestore$set_access_options(
      #engine = "graphdb",
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    summary <- get_db_summary(triplestore = triplestore)
    
    skip_if(is.null(response) || is.null(summary))
    
    expect_true(length(summary$value)==17)
  })
})
test_that("ongology", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")
  
  suppressMessages({
    triplestore <- triplestore_access$new()
    
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    ontology <- triplestore$ontology()
    
    skip_if(is.null(response) || is.null(ontology))
  
    expect_true(grepl("ontoavida", tolower(ontology$versionIRI)))
    expect_true(grepl("avida", tolower(ontology$title)))
    expect_true(grepl("avida", tolower(ontology$description)))
  })
  
})
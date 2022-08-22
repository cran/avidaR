test_that("ongology", {
  
  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  ontology <- triplestore$ontology()

  expect_true(grepl("ontoavida", tolower(ontology$versionIRI)))
  expect_true(grepl("avida", tolower(ontology$title)))
  expect_true(grepl("avida", tolower(ontology$description)))

})
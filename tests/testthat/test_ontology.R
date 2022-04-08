test_that("ongology", {

  ontology <- triplestore$ontology()

  expect_true(grepl("ontoavida", tolower(ontology$versionIRI)))
  expect_true(grepl("avida", tolower(ontology$title)))
  expect_true(grepl("avida", tolower(ontology$description)))

})
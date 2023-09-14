test_that("get_experiment_id_from_organism_id", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")

  suppressMessages({
    triplestore <- triplestore_access$new()
  
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
  
    found1 <- get_experiment_id_from_organism_id(
      organism_id = 1,
      triplestore = triplestore
    )
  
    found2 <- get_experiment_id_from_organism_id(
      organism_id = c(1, 2),
      triplestore = triplestore
    )
  
    found3 <- get_experiment_id_from_organism_id(
      organism_id = 1,
      description = TRUE,
      triplestore = triplestore
    )
    
    skip_if(is.null(response) || is.null(found1)  || is.null(found2))
  
    expect_match(class(found1), "data.frame")
    expect_true(nrow(found1) > 0)
    expect_equal(colnames(found1), c("organism_id", "avida_experiment_id"))
  
    expect_match(class(found2), "data.frame")
    expect_true(nrow(found2) > 0)
    expect_equal(colnames(found2), c("organism_id", "avida_experiment_id"))
  
    expect_match(class(found3), "data.frame")
    expect_true(nrow(found3) > 0)
    expect_equal(colnames(found3), c("organism_id", "avida_experiment_id", "description"))    
  })


})

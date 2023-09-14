test_that("get_genome_id_from_phenotype_id", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")
  
  suppressMessages({
    triplestore <- triplestore_access$new()
    
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    found1 <- get_genome_id_from_phenotype_id(
      phenotype_id = 0,
      seed_id = 1,
      triplestore = triplestore
    )
  
    found2 <- get_genome_id_from_phenotype_id(
      phenotype_id = c(1, 2),
      seed_id = c(2, 1),
      genome_seq = TRUE,
      triplestore = triplestore
    )
    
    skip_if(is.null(response) || is.null(found1) || is.null(found2))
  
    expect_match(class(found1), "data.frame")
    expect_equal(nrow(found1) > 0 , TRUE)
    expect_equal(colnames(found1), c("seed_id", "genome_id", "phenotype_id"))
    expect_equal(found1$genome_id[1], "genome_1")
  
    expect_match(class(found2), "data.frame")
    expect_true(nrow(found2) > 0)
    expect_equal(colnames(found2), c("seed_id", "genome_id", "genome_seq", "phenotype_id"))
    expect_equal(found2$seed_id[1], "seed_2")    
  })
})
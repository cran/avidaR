test_that("get_genome_seq_from_genome_id", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")

  suppressMessages({
    triplestore <- triplestore_access$new()
    
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    found1 <- get_genome_seq_from_genome_id(
      genome_id = 1,
      triplestore = triplestore
    )
    
    skip_if(is.null(response) || is.null(found1))
  
    expect_match(class(found1), "data.frame")
    expect_equal(nrow(found1), 1)
    expect_equal(colnames(found1), c("genome_id", "genome_seq"))
    expect_equal(found1$genome_seq[1], "acksdblwxnlhuxpjfmhcowivenwrcepkpwhcsdxzobuicloefcpsxpmovktqodgtpzursjelhdfxayfcvtdsqbloizzfgxypwoza")    
  })

})
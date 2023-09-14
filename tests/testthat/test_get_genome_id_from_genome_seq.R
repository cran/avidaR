test_that("get_genome_id_from_genome_seq", {
  
  skip_if_offline(host = "graphdb.fortunalab.org")

  suppressMessages({
    triplestore <- triplestore_access$new()
    
    response <- triplestore$set_access_options(
      url = "https://graphdb.fortunalab.org",
      user = "public_avida",
      password = "public_avida",
      repository = "avidaDB_test"
    )
    
    sequence1 <- "acksdblwxnlhuxpjfmhcowivenwrcepkpwhcsdxzobuicloefcpsxpmovktqodgtpzursjelhdfxayfcvtdsqbloizzfgxypwoza"
    sequence2 <- "wakkoarhimcntsprxhcwpxvdvixzqkeezetmyvuvnwlxatoldiimqgtslvutvnquxefucqmkearisdaogqzbdywpbenlhdtwawtd"
  
    found1 <- get_genome_id_from_genome_seq(
      genome_seq = sequence1,
      triplestore = triplestore
    )
  
    found2 <- get_genome_id_from_genome_seq(
      genome_seq = c(sequence2, sequence1),
      triplestore = triplestore
    )
    
    skip_if(is.null(response) || is.null(found1) || is.null(found2))
  
    expect_match(class(found1), "data.frame")
    expect_equal(nrow(found1) > 0 , TRUE)
    expect_equal(colnames(found1), c("genome_id", "genome_seq"))
    expect_equal(found1$genome_id[1], "genome_1")
  
    expect_match(class(found2), "data.frame")
    expect_true(nrow(found2) > 0)
    expect_equal(colnames(found2), c("genome_id", "genome_seq"))
    expect_equal(found2$genome_id[1], "genome_2")    
  })


})
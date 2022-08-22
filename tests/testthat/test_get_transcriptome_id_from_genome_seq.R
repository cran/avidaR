test_that("get_transcriptome_id_from_genome_seq", {

  triplestore <- triplestore_access$new()
  
  triplestore$set_access_options(
    url = "https://graphdb.fortunalab.org",
    user = "public_avida",
    password = "public_avida",
    repository = "avidaDB_test"
  )
  
  sequence1 <- "acksdblwxnlhuxpjfmhcowivenwrcepkpwhcsdxzobuicloefcpsxpmovktqodgtpzursjelhdfxayfcvtdsqbloizzfgxypwoza"
  sequence2 <- "wakkoarhimcntsprxhcwpxvdvixzqkeezetmyvuvnwlxatoldiimqgtslvutvnquxefucqmkearisdaogqzbdywpbenlhdtwawtd"

  found1 <- get_transcriptome_id_from_genome_seq(
    genome_seq = sequence1,
    triplestore = triplestore
  )

  found2 <- get_transcriptome_id_from_genome_seq(
    genome_seq = c(sequence1, sequence2),
    seed_id = c(2,1),
    transcriptome_seq = TRUE,
    transcriptome_pos = FALSE,
    triplestore = triplestore
  )

  found3 <- get_transcriptome_id_from_genome_seq(
    genome_seq = c(sequence1, sequence2),
    seed_id = c(2,1),
    transcriptome_seq = FALSE,
    transcriptome_pos = TRUE,
    triplestore = triplestore
  )

  found4 <- get_transcriptome_id_from_genome_seq(
    genome_seq = sequence2,
    genome_id = TRUE,
    transcriptome_seq = TRUE,
    transcriptome_pos = TRUE,
    triplestore = triplestore
  )

  expect_match(class(found1), "data.frame")
  expect_true(nrow(found1) > 0)
  expect_equal(colnames(found1), c("genome_seq", "transcriptome_id"))
  expect_equal(found1$transcriptome_id[1], "transcriptome_32406326")

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "genome_seq", "transcriptome_id", "transcriptome_seq"))
  expect_equal(found2$seed_id[1], "seed_2")

  expect_match(class(found3), "data.frame")
  expect_true(nrow(found3) > 0)
  expect_equal(colnames(found3), c("seed_id", "genome_seq", "transcriptome_id", "transcriptome_pos"))
  expect_equal(grepl("|", found3$transcriptome_pos[1]), TRUE)

  expect_match(class(found4), "data.frame")
  expect_true(nrow(found4) > 0)
  expect_equal(colnames(found4), c("genome_id", "genome_seq", "transcriptome_id", "transcriptome_seq", "transcriptome_pos"))
  expect_equal(found4$transcriptome_id[1], "transcriptome_13267653")

})
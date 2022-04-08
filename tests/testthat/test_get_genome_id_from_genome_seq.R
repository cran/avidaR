test_that("get_genome_id_from_genome_seq", {

  sequence1 <- "acksdblwxnlhuxpjfmhcowivenwrcepkpwhcsdxzobuicloefcpsxpmovktqodgtpzursjelhdfxayfcvtdsqbloizzfgxypwoza"
  sequence2 <- "wakkoarhimcntsprxhcwpxvdvixzqkeezetmyvuvnwlxatoldiimqgtslvutvnquxefucqmkearisdaogqzbdywpbenlhdtwawtd"

  found1 <- get_genome_id_from_genome_seq(
    genome_seq = sequence1
  )

  found2 <- get_genome_id_from_genome_seq(
    genome_seq = c(sequence2, sequence1)
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) > 0 , TRUE)
  expect_equal(colnames(found1), c("genome_id", "genome_seq"))
  expect_equal(found1$genome_id[1], "genome_1")

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("genome_id", "genome_seq"))
  expect_equal(found2$genome_id[1], "genome_2")

})
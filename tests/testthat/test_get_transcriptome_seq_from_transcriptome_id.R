test_that("get_transcriptome_seq_from_transcriptome_id", {

  found1 <- get_transcriptome_seq_from_transcriptome_id(
    transcriptome_id = 53674
  )

  found2 <- get_transcriptome_seq_from_transcriptome_id(
    transcriptome_id = c(53674, 1666099),
    seed_id = c(2,1),
    transcriptome_pos = TRUE
  )

  found3 <- get_transcriptome_seq_from_transcriptome_id(
    transcriptome_id = 53674,
    genome_seq = TRUE
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1), 1)
  expect_equal(colnames(found1), c("transcriptome_id", "transcriptome_seq"))

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "transcriptome_id", "transcriptome_seq", "transcriptome_pos"))
  expect_equal(grepl("|", found2$transcriptome_pos[1]), TRUE)

  expect_match(class(found3), "data.frame")
  expect_equal(nrow(found3), 1)
  expect_equal(colnames(found3), c("transcriptome_id", "transcriptome_seq", "genome_seq"))
  expect_equal(found3$genome_seq[1], "eirlxtxqauwatcknjayqgcufvylzdjpokplcvbzamnyyfqesoktrvxgauuqfufmjszbzwtdunnpvfmpplfuuvcmpvyftkgvxxhpp")
  expect_equal(nchar(found3$transcriptome_seq[1]), 1490)

})
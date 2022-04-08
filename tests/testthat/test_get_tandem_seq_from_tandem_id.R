test_that("get_tandem_seq_from_tandem_id", {

    found1 <- get_tandem_seq_from_tandem_id(
    tandem_id = 87873
  )

  found2 <- get_tandem_seq_from_tandem_id(
    tandem_id = c(87873, 388401),
    seed_id = c(2,1),
    tandem_pos = TRUE
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1), 1)
  expect_equal(colnames(found1), c("tandem_id", "tandem_seq"))

  expect_match(class(found2), "data.frame")
  expect_true(nrow(found2) > 0)
  expect_equal(colnames(found2), c("seed_id", "tandem_id", "tandem_seq", "tandem_pos"))
  expect_equal(grepl("|", found2$tandem_pos[1]), TRUE)

})
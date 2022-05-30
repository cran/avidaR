test_that("get_db_summary", {

  summary <- get_db_summary()

  expect_true(length(summary$value)==17)

})
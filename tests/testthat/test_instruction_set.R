test_that("instruction_set", {

  found1 <- instruction_set()

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) == 26 , TRUE)
  expect_equal(colnames(found1), c("instruction", "letter", "color"))
  expect_equal(found1$letter[1], "a")
  expect_equal(found1$instruction[26], "h-search")

})
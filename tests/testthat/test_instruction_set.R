test_that("instruction_set", {

  found1 <- instruction_set()

  found2 <- instruction_set(inst_set = "heads")

  found3 <- instruction_set(inst_set = "heads-sex")

  found4 <- instruction_set(inst_set = "transsmt")

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) == 26 , TRUE)
  expect_equal(colnames(found1), c("instruction", "letter", "color"))
  expect_equal(found1$letter[1], "a")
  expect_equal(found1$instruction[24], "h-divide")

  expect_match(class(found2), "data.frame")
  expect_equal(nrow(found2) == 26 , TRUE)
  expect_equal(colnames(found2), c("instruction", "letter", "color"))
  expect_equal(found2$letter[1], "a")
  expect_equal(found2$instruction[24], "h-divide")

  expect_match(class(found3), "data.frame")
  expect_equal(nrow(found3) == 26 , TRUE)
  expect_equal(colnames(found3), c("instruction", "letter", "color"))
  expect_equal(found3$letter[1], "a")
  expect_equal(found3$instruction[24], "divide-sex")

  expect_match(class(found4), "data.frame")
  expect_equal(nrow(found4) == 33 , TRUE)
  expect_equal(colnames(found4), c("instruction", "letter", "color"))
  expect_equal(found4$letter[1], "a")
  expect_equal(found4$instruction[33], "Divide-Erase")

})

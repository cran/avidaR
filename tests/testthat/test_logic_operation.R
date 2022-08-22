test_that("logic_operation", {
  
  found1 <- logic_operation()

  expect_match(class(found1), "character")
  expect_true(length(found1) == 9)
  expect_equal(found1, logic_operation_default())
  
})
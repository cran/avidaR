test_that("get_phenotype_id_from_logic_operation", {

  found1 <- get_phenotype_id_from_logic_operation(
    logic_operation = ""
  )

  found2 <- get_phenotype_id_from_logic_operation(
    logic_operation = c("equals", "exclusive or", "not-or", "and-not", "or", "orn-not", "and", "not-and", "not"),
    phenotype_binary = TRUE
  )

  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) , 1)
  expect_equal(colnames(found1), c("phenotype_id"))
  expect_equal(found1$phenotype_id[1], "phenotype_0")

  expect_match(class(found2), "data.frame")
  expect_equal(nrow(found2), 1)
  expect_equal(colnames(found2), c("phenotype_id", "phenotype_binary"))
  expect_equal(found2$phenotype_id[1], "phenotype_511")
  expect_equal(found2$phenotype_binary[1], "111111111")

})
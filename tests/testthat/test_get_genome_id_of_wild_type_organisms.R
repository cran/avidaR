test_that("get_genome_id_of_wild_type_organisms", {

  found1 <- get_genome_id_of_wild_type_organisms()


  expect_match(class(found1), "data.frame")
  expect_equal(nrow(found1) > 0 , TRUE)
  expect_equal(colnames(found1), c("genome_id_wild_type"))

})
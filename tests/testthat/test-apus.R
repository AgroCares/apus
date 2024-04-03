test_that("Apus is created", {

  farm_name <- 'my_farm'
  apus <- apus::Apus$new(farm_name = farm_name)

  expect_contains(class(apus), 'Apus')
  expect_equal(apus$farm_name, farm_name)
})

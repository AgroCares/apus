test_that("Create training dataset", {
  dataset <- apus::createApusDataset(fields = NULL, device = 'cpu')
  expect_contains(class(dataset), 'apus_dataset')
  expect_equal(dataset$.length(), 5)
  expect_contains(class(dataset$.getitem(1)$fields), 'torch_tensor')
  expect_equal(dim(dataset$.getitem(1)$fields), c(5,8))
  expect_false(identical(dataset$.getitem(1), dataset$.getitem(2)))
  expect_setequal(names(dataset$.getitem(3)), c('fields', 'fertilizers'))
})

# test_that("Create validation/test dataset", {
#  # TODO
# })
#
# test_that("Create interference dataset", {
#   #TODO
# })

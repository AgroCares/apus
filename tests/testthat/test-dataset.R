test_that("Create training dataset", {
  dataset <- apus::createApusDataset(farms = NULL, device = 'cpu')
  expect_contains(class(dataset), 'apus_dataset')
  expect_equal(dataset$.length(), dataset$farms_count)
  expect_contains(class(dataset$.getitem(1)$farms), 'torch_tensor')
  expect_equal(dim(dataset$.getitem(1)$farms), c(10,5,9))
})

# test_that("Create validation/test dataset", {
#  # TODO
# })
#
# test_that("Create interference dataset", {
#   #TODO
# })

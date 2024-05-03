test_that("Create training dataset", {
  fields_max <- 5
  dataset <- apus::createApusDataset(farms = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fines = apus::fines, fields_max = fields_max, device = 'cpu')
  expect_contains(class(dataset), 'apus_dataset')
  expect_equal(dataset$.length(), dataset$farms_count)
  expect_contains(class(dataset$.getitem(1)$fields), 'torch_tensor')
  expect_setequal(names(dataset$.getitem(1)), c('fields', 'fertilizers', 'fines'))
  expect_equal(dim(dataset$.getitem(1)$fields), c(dataset$fields_max, 9))
  expect_false(identical(dataset$.getitem(1), dataset$.getitem(2)))
})

test_that("Create validation/test dataset", {
  farms_count <- 10
  fields_max <- 5
  farms <- createSyntheticFarms(farms_count = farms_count, fields_max = fields_max)

  expect_contains(class(farms), 'data.table')
  expect_equal(nrow(farms), farms_count * fields_max)

  dataset.valid <- apus::createApusDataset(farms = farms, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fines = apus::fines, fields_max = fields_max, device = 'cpu')
  expect_contains(class(dataset.valid), 'apus_dataset')
  expect_equal(dataset.valid$.length(), dataset.valid$farms_count)
  expect_contains(class(dataset.valid$.getitem(1)$fields), 'torch_tensor')
  expect_setequal(names(dataset.valid$.getitem(3)), c('fields', 'fertilizers', 'fines'))
  expect_equal(dim(dataset.valid$.getitem(1)$fields), c(fields_max, 9))

  dl <- torch::dataloader(dataset.valid, batch_size = farms_count)
  batch <- dl$.iter()
  batch <- batch$.next()

  expect_contains(class(batch$fields), 'torch_tensor')
  expect_equal(dim(batch$fields), c(farms_count, fields_max, 9))
  expect_equal(dim(batch$fertilizers), c(farms_count, nrow(apus::fertilizers), 10))
})


# test_that("Create interference dataset", {
#   #TODO
# })

test_that("Create model and run a forward pass", {

  dataset <- apus::createApusDataset(fields = NULL, device = 'cpu')
  model <- createApusModel(dataset, device = 'cpu')
  expect_contains(class(model), 'nn_module')

  batch <- dataset$.getitem(1)
  pass <- model(batch$fields, batch$fertilizers)
  expect_contains(class(pass), 'torch_tensor')
  expect_equal(dim(pass), c(dataset$fields_max, nrow(dataset$fertilizers)))

  # Do not allow negative dose advice
  expect_gte(min(as.matrix(pass)), 0)
})

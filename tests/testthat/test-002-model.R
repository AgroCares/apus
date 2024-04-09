
# Test creating model and forward pass ------------------------------------

dataset <- apus::createApusDataset(fields = NULL, device = 'cpu')
model <- createApusModel(dataset, device = 'cpu', epochs = 2)
dl <- torch::dataloader(dataset, batch_size = 1)

batch <- dl$.iter()
batch <- batch$.next()
fields <- batch$fields
fertilizers <- batch$fertilizers
doses <- model(fields, fertilizers)

test_that("Create model and run a forward pass", {

  expect_contains(class(model), 'nn_module')
  expect_contains(class(doses), 'torch_tensor')
  expect_equal(dim(doses), c(1, dataset$fields_max, nrow(dataset$fertilizers)))

  # Do not allow negative dose advice
  expect_gte(min(as.matrix(doses)), 0)
})

# Test cost functions of modules ------------------------------------------

module1 <- calculateCostModule1(doses, fertilizers)

test_that("Calculate cost for module 1: Purchase of fertilizers", {
  expect_contains(class(module1), 'torch_tensor')
  expect_length(as.numeric(module1), 1)
})

cost <- calculateCost(doses, fields, fertilizers)

test_that("Calculate overall cost", {
  expect_contains(class(cost), 'torch_tensor')
  expect_length(as.numeric(module1), 1)
})

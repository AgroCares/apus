
# Test creating model and forward pass ------------------------------------
farms_count <- 10
fields_max <- 5

dataset.train <- apus::createApusDataset(farms = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max = fields_max, device = 'cpu')
farms.valid <-  apus:::createSyntheticFarms(farms_count = farms_count, fields_max = fields_max)
dataset.valid<- apus::createApusDataset(farms = farms.valid, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max = fields_max, device = 'cpu')

model <- apus::createApusModel(dataset.train, dataset.valid, device = 'cpu', epochs = 2)
dl <- torch::dataloader(dataset.train, batch_size = farms_count)

batch <- dl$.iter()
batch <- batch$.next()
fields <- batch$fields
fertilizers <- batch$fertilizers
doses <- model(fields, fertilizers)

test_that("Create model and run a forward pass", {

  expect_contains(class(model), 'nn_module')
  expect_contains(class(doses), 'torch_tensor')
  expect_equal(dim(doses), c(farms_count, dataset.train$fields_max, nrow(dataset.train$fertilizers)))

  # Do not allow negative dose advice
  expect_gte(min(as.matrix(doses)), 0)
})

# Test cost functions of modules ------------------------------------------

module1 <- calculateCostModule1(doses, fertilizers)

test_that("Calculate cost for module 1: Purchase of fertilizers", {
  expect_contains(class(module1), 'torch_tensor')
  expect_length(as.numeric(module1), farms_count)
})

module4 <- calculateRevenueModule4(doses, fields, fertilizers)

test_that("Calculate revenue for module 4: Revenue of harvested crops", {
  expect_contains(class(module4), 'torch_tensor')
  expect_length(as.numeric(module4), farms_count)
})



cost <- calculateCost(doses, fields, fertilizers)

test_that("Calculate overall cost", {
  expect_contains(class(cost), 'torch_tensor')
  expect_length(as.numeric(module1), farms_count)
})

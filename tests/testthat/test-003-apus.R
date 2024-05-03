

# Create apus object ------------------------------------------------------
farm_name <- 'my_farm'
apus <- apus::Apus$new(farm_name = farm_name)

test_that("Apus is created", {
  expect_contains(class(apus), 'Apus')
  expect_equal(apus$farm_name, farm_name)
  expect_identical(apus$cultivations, apus::cultivations)
  expect_identical(apus$fertilizers, apus::fertilizers)
  expect_identical(apus$fines, apus::fines)
})


# Add first field to apus -------------------------------------------------
field1 <- data.frame(
  b_id_field = 1L,
  b_area = 10000,
  b_lu = 'nl_2014',
  d_n_req = 270,
  d_p_req = 120,
  d_k_req = 50,
  d_n_norm = 230,
  d_n_norm_man = 170,
  d_p_norm = 75
)

apus$addField(
  b_id_field = field1$b_id_field,
  b_area = field1$b_area,
  b_lu = field1$b_lu,
  d_n_req = field1$d_n_req,
  d_p_req = field1$d_p_req,
  d_k_req = field1$d_k_req,
  d_n_norm = field1$d_n_norm,
  d_n_norm_man = field1$d_n_norm_man,
  d_p_norm = field1$d_p_norm
)

test_that("First field is added", {
  expect_contains(class(apus$fields), 'data.table')
  expect_equal(nrow(apus$fields), 1)
  expect_equal(apus$fields$b_id_field[1], field1$b_id_field)
  expect_equal(apus$fields$b_area[1], field1$b_area)
  expect_equal(apus$fields$b_lu[1], field1$b_lu)
  expect_equal(apus$fields$d_n_req[1], field1$d_n_req)
  expect_equal(apus$fields$d_p_req[1], field1$d_p_req)
  expect_equal(apus$fields$d_k_req[1], field1$d_k_req)
  expect_equal(apus$fields$d_n_norm[1], field1$d_n_norm)
  expect_equal(apus$fields$d_n_norm_man[1], field1$d_n_norm_man)
  expect_equal(apus$fields$d_p_norm[1], field1$d_p_norm)
})

# Add second field --------------------------------------------------------
field2 <- data.frame(
  b_id_field = 2L,
  b_area = 10000,
  b_lu = 'nl_265',
  d_n_req = 270,
  d_p_req = 120,
  d_k_req = 50,
  d_n_norm = 230,
  d_n_norm_man = 170,
  d_p_norm = 75
)

apus$addField(
  b_id_field = field2$b_id_field,
  b_area = field2$b_area,
  b_lu = field2$b_lu,
  d_n_req = field2$d_n_req,
  d_p_req = field2$d_p_req,
  d_k_req = field2$d_k_req,
  d_n_norm = field2$d_n_norm,
  d_n_norm_man = field2$d_n_norm_man,
  d_p_norm = field2$d_p_norm
)

test_that("Second field is added", {
  expect_contains(class(apus$fields), 'data.table')
  expect_equal(nrow(apus$fields), 2)
  expect_equal(apus$fields$b_id_field[2], field2$b_id_field)
  expect_equal(apus$fields$b_area[2], field2$b_area)
  expect_equal(apus$fields$b_lu[2], field2$b_lu)
  expect_equal(apus$fields$d_n_req[2], field2$d_n_req)
  expect_equal(apus$fields$d_p_req[2], field2$d_p_req)
  expect_equal(apus$fields$d_k_req[2], field2$d_k_req)
  expect_equal(apus$fields$d_n_norm[2], field2$d_n_norm)
  expect_equal(apus$fields$d_n_norm_man[2], field2$d_n_norm_man)
  expect_equal(apus$fields$d_p_norm[2], field2$d_p_norm)
})


# Add third field --------------------------------------------------------
field3 <- data.frame(
  b_id_field = 3L,
  b_area = 5000,
  b_lu = 'nl_372',
  d_n_req = 370,
  d_p_req = 130,
  d_k_req = 50,
  d_n_norm = 330,
  d_n_norm_man = 200,
  d_p_norm = 80
)

apus$addField(
  b_id_field = field3$b_id_field,
  b_area = field3$b_area,
  b_lu = field3$b_lu,
  d_n_req = field3$d_n_req,
  d_p_req = field3$d_p_req,
  d_k_req = field3$d_k_req,
  d_n_norm = field3$d_n_norm,
  d_n_norm_man = field3$d_n_norm_man,
  d_p_norm = field3$d_p_norm
)

test_that("Third field is added", {
  expect_contains(class(apus$fields), 'data.table')
  expect_equal(nrow(apus$fields), 3)
  expect_equal(apus$fields$b_id_field[3], field3$b_id_field)
  expect_equal(apus$fields$b_area[3], field3$b_area)
  expect_equal(apus$fields$b_lu[3], field3$b_lu)
  expect_equal(apus$fields$d_n_req[3], field3$d_n_req)
  expect_equal(apus$fields$d_p_req[3], field3$d_p_req)
  expect_equal(apus$fields$d_k_req[3], field3$d_k_req)
  expect_equal(apus$fields$d_n_norm[3], field3$d_n_norm)
  expect_equal(apus$fields$d_n_norm_man[3], field3$d_n_norm_man)
  expect_equal(apus$fields$d_p_norm[3], field3$d_p_norm)
})


# Update a fertilizer -----------------------------------------------------

apus$updateFertilizer(
  p_id = 17,
  p_price = -5,
  p_stored = 15000,
  p_storage_available = 1
)

test_that("Fertilizer is updated", {
  expect_contains(class(apus$fertilizers), 'data.table')
  expect_equal(nrow(apus$fertilizers), nrow(apus::fertilizers))
  expect_equal(apus$fertilizers[p_id == 17]$p_price, -5)
  expect_equal(apus$fertilizers[p_id == 17]$p_stored, 15000)
  expect_equal(apus$fertilizers[p_id == 17]$p_storage_available, 1)
})

# Train a model -----------------------------------------------------------

apus$trainModel()

test_that("Model is trained and available", {
  expect_contains(class(apus$model), 'nn_module')
})


# Optimize fertilizer choice ----------------------------------------------

hoi <- apus$optimizeFertilizerChoice()

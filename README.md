# apus

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/AgroCares/apus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AgroCares/apus/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/AgroCares/apus/branch/main/graph/badge.svg)](https://app.codecov.io/gh/AgroCares/apus?branch=main)
<!-- badges: end -->

The R package `apus` optimizes the fertilizer mix for a farm based on the nutrient requirements, legal norms, cultivation response curves, available fertilizers and tries to achieve the best economic result within the provided boundaries.

> [!NOTE]  
> Currently this package is in an early stage of development and not suitable to be used in the real world yet. 


## Installation
Currently the `apus` package is in development and only available to install via GitHub. Use the following command to install the latest development version:

`remotes::install_github('AgroCares/apus')`

## Getting started
`apus` makes use of [R6](https://cran.r-project.org/web/packages/R6/index.html) to keep track of data of a farm.
First step is to initialize an `apus` object
```
library(apus)
apus <- apus::Apus$new(farm_name = 'my farm')
```
Then you can add fields to your farm with the `addField()` function. Multiple fields can be added by using again the `addField()` function

```
apus$addField(
  b_id_field = 1L,
  b_area = 10000,
  b_lu = 'nl_2014',
  d_n_req = 270,
  d_p_req = 120,
  d_k_req = 50,
  d_n_norm = 230,
  d_n_norm_man = 170,
  d_p_norm = 75,
  b_lu_yield = 40000,
  b_lu_price = 0.3
)

# Add a second field
apus$addField(
  b_id_field = 2L,
  b_area = 5000,
  b_lu = 'nl_262',
  d_n_req = 270,
  d_p_req = 120,
  d_k_req = 50,
  d_n_norm = 230,
  d_n_norm_man = 170,
  d_p_norm = 75,
  b_lu_yield = 66000,
  b_lu_price = 0.1
)
```

To get a fertilizer advice you need a model first. A model can be trained with with the function `trainModel()`

`apus$trainModel()` 

After the model has been trained the advice can be generated with the `optimizeFertilizerChoice()` function of the `apus` object. 

`advice = apus$optimizeFertilizerChoice()`

The `advice` is a table with adviced dose per fertilizer and field in kg /ha.

## Roadmap

For the v1 version of `apus` we plan to develop to following features:

* [ ] Import and export trained models
* [ ] Include a trained base model 
* [x] Add function to train model
* [ ] Enable fine-tuning of (base) models
* [x] Include cost function for module 1: Purchase of fertilizers
* [ ] Include cost function for module 2: Disposal of manure
* [ ] Include cost function for module 3: Cost of storing fertilizers
* [ ] Include cost function for module 4: Cost of applying fertilizers
* [x] Include cost function for module 5: Revenue of harvest
* [x] Include cost function for module 6: Penalties in case of exceeding legal limits
* [ ] Include cost function for module 7: Cost of greenhouse gas emissions
* [ ] Include realistic cultivation response curves from module 5
* [ ] Add other nutrients than NPK to module 5
* [ ] Add custom fertilizers
* [ ] Add custom cultivations
* [ ] Add details of the optimization to the result

## Contributing
If you would like to contribute to `apus` you are very welcome! Please create an issue with your idea or a pull request with your contribution.

## Contact
* Maintainer: @SvenVw

## Made possible by
The package `apus` is developed by the [Nutriënten Management Instituut](https://www.nmi-agro.nl/) as part of the Horizon Europe project: [NutriBudget](https://www.nutribudget.eu/)

![Logo of NutriBudget](https://www.nutribudget.eu/wp-content/themes/nutribudget/images/logo-nutribudget.png)

![Logo of EU](https://ec.europa.eu/regional_policy/images/information-sources/logo-download-center/eu_funded_en.jpg)

![Logo of NMI](https://media.licdn.com/dms/image/C560BAQEYGcm4HjNnxA/company-logo_200_200/0?e=2159024400&v=beta&t=u40rJ7bixPWB2SAqaj3KCKzJRoKcqf0wUXCdmsTDQvw)


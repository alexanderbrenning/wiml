# wiml: Interpreting Machine-Learning Models in Transformed Space

<!-- badges: start -->
[![R-CMD-check](https://github.com/alexanderbrenning/wiml/workflows/R-CMD-check/badge.svg)](https://github.com/alexanderbrenning/wiml/actions)
<!-- badges: end -->

Main effects plots (such as partial dependence and ALE plots) can be confusing and even misleading when dealing with large numbers of highly correlated features. Example applications include land cover classification using multitemporal satellite remote-sensing data or texture features derived from such imagery. This package introduces a simple and pragmatic approach to dealing with this problem. This approach can be especially beneficial in situations where features tend to be linearly dependent, or in other words, where principal components analysis seems like a reasonable approach.

### Reference

Brenning, A. (2023). Interpreting machine-learning models in transformed feature space with an application to remote-sensing classification. *Machine Learning*, **112**, 3455--3471, <https://doi.org/10.1007/s10994-023-06327-8>

### My Personal Wishlist

1. Write a package vignette. - DONE (12 Apr 2021)

2. Look into [DALEX](https://github.com/ModelOriented/DALEX) and `modelStudio` connectivity in more detail. Changes to DALEX may be necessary to support `warped_model` objects and recognize "the model inside". First attempts were successful and look promising.

3. Create a more general framework for structuring feature space than through `strucpca_wrapper`. E.g. `c()` multiple wrappers to create a structured wrapper. Does `c()` have methods? Then write a `c()` method that creates a `structured_wrapper` object, doing some validity checks.

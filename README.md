# wiml: Interpreting Machine-Learning Models in Transformed Space

Main effects plots (such as partial dependence and ALE plots) can be confusing and even misleading when dealing with large numbers of highly correlated features. Example applications include land cover classification using multitemporal satellite remote-sensing data or texture features derived from such imagery. This package introduces a simple and pragmatic approach to dealing with this problem. This approach can be especially beneficial in situations where features tend to be linearly dependent, or in other words, where principal components analysis seems like a reasonable approach.

### Reference

Brenning, A. (2021). Transforming Feature Space to Interpret Machine Learning Models. arXiv preprint, arXiv:2104.04295, <https://arxiv.org/abs/2104.04295>.

### My Personal Wishlist

1. Write a package vignette.

2. Look into [DALEX](https://github.com/ModelOriented/DALEX) and `modelStudio` connectivity in more detail. Changes to DALEX may be necessary to support `warped_model` objects and recognize "the model inside".

3. Create a more general framework for structuring feature space than through `strucpca_wrapper`. E.g. `c()` multiple wrappers to create a structured wrapper. Does `c()` have methods? Then write a `c()` method that creates a `structured_wrapper` object, doing some validity checks.

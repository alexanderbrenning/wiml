#' wiml: Interpreting machine-learning models in transformed space
#'
#' Main effects plots (such as partial dependence and ALE plots) can be
#' confusing and even misleading when dealing with large numbers of highly
#' correlated features. Example applications include land cover classification
#' using multitemporal satellite remote-sensing data or texture features derived
#' from such imagery. This package introduces a simple and pragmatic approach to
#' dealing with this problem. This approach can be especially beneficial in
#' situations where features tend to be linearly dependent, or in other words,
#' where principal component analysis seems like a reasonable approach.
#' @references Brenning, A. (2021). Transforming Feature Space to Interpret Machine Learning Models. arXiv preprint, arXiv:2104.04295, <https://arxiv.org/abs/2104.04295>.
#'
#' @docType package
#' @name wiml
NULL

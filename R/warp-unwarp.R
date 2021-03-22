#' Transform data from feature space into a transformed space
#'
#' These methods apply the transformation \eqn{T} to data
#' in feature space - the result is, for example, the principal components
#' representation of the data, if a [pca_warper] is used.
#'
#' @param x The object to be transformed, normally a data frame.
#' @param ... Additional arguments to be passed to the `warp` method.
#' @return A data frame with features in the transformed (e.g., PCA) space.
#' @seealso [unwarp()] for the inverse transformation, and
#'   [pca_warper()] for an example of a simple warper function based
#'   on the principal component transformation.
#' @export
warp <- function(x, ...) {
  UseMethod("warp", x)
}

#' Transform data from feature space into a transformed space
#'
#' `warp` methods apply the transformation \eqn{T} to data
#' in feature space - the result is, for example, the principal components
#' representation of the data, if a [pca_warper] is used.
#'
#' @param x The object to be transformed, normally a data frame.
#' @param warper A `warper` object as, for example, created by [pca_warper()] for principal components transformations.
#' @param ... Additional arguments to be passed to the \code{warp} method.
#' @return A data frame with features in the transformed (e.g., PCA) space.
#' @seealso [unwarp()] for the inverse transformation, and
#'   [pca_warper()] for an example of a simple warper function based
#'   on the principal component transformation.
#' @export
warp.data.frame <- function(x, warper, ...) {
  chkDots(...)
  x <- outbound(warper, xdata = x)
  class(x) <- c("warped_df", class(x))
  x
}

#' @describeIn warp.data.frame Does nothing - `warped_df` has already been transformed
#' @export
warp.warped_df <- function(x, warper, ...) {
  chkDots(...)
  x
}

#' Backtransform from transformed ('warped') to original feature space
#'
#' These methods apply the inverse transformation \eqn{T^{-1}} to data
#' in transformed space - e.g. a feature vector in principal components
#' space back to the original feature space, in the case of a PCA
#' transformation.
#'
#' @param x The object to be backtransformed, for example a data frame or a `warped_df` object.
#' @param ... Additional arguments to be passed to the [unwarp()] method.
#' @return A data frame with features in the original feature space.
#' @seealso [warp()] for the forward transformation, and
#'   [pca_warper()] for an example of a simple warper function based
#'   on the principal components transformation.
#' @example examples/pca_warper.R
#' @export
unwarp <- function(x, ...) {
  UseMethod("unwarp")
}

#' Backtransform from transformed ('warped') to original feature space
#'
#' These methods apply the inverse transformation \eqn{T^{-1}} to data
#' in transformed space - e.g. a feature vector in principal components
#' space back to the original feature space, in the case of a PCA
#' transformation.
#'
#' @param x The object to be backtransformed, for example a data frame or a \code{warped_df} object.
#' @param warper A `warper` object as, for example, created by [pca_warper()] for principal components transformations.
#' @param force_unwarp If `TRUE`, backtransform even if it's not a `warped_df` object. (Ignored by \code{unwarp.warped_df}.)
#' @param ... Additional arguments to be passed to the [unwarp()] method.
#' @return A data frame with features in the original feature space.
#' @seealso [warp()] for the forward transformation, and
#'   [pca_warper()] for an example of a simple warper function based
#'   on the principal components transformation.
#' @example examples/pca_warper.R
#' @export
unwarp.warped_df <- function(x, warper, force_unwarp = FALSE, ...) {
  chkDots(...)
  class(x) <- class(x)[class(x) != "warped_df"]
  x <- inbound(warper, wdata = x)
  x
}

#' @describeIn unwarp.warped_df Backtransform a data frame from transformed to original feature space
#' @export
unwarp.data.frame <- function(x, warper = NULL, force_unwarp = FALSE, ...) {
  chkDots(...)
  if (force_unwarp)
    x <- unwarp.warped_df(x, warper = warper)
  x
}


#' Create a warped version of a model fitting function
#'
#' Use [warp_fitted_model()] instead, which provides a more
#' elegant way to create a warped perspective on a model. This function may
#' be omitted in the future, unless it turns out to be instrumental
#' in drop/permute and re-learn techniques that require a trainable
#' model, not a fitted one.
#'
#' @param x A model fitting function such as [stats::lm()] or [randomForest::randomForest]
#' @param warper A `warper` object, for example a [pca_warper].
#' @param ... Additional arguments to be passed to the model fitting function.
#' @return A function representing the composition of the learning function \eqn{F:L\mapsto\hat{f}}
#'   with the inverse transformation function \eqn{T^{-1}}. In other words,
#'   this function first (back)transformed its input data from the transformed space
#'   to the original feature space, and then fits a model using these features.
#' @seealso [warp_fitted_model()]
#' @export
warp.function <- function(x, warper, ...) {
  function(formula, data, ...) {
    data <- unwarp(data, warper = warper)
    fit <- x(formula, data = data, ...)
    res <- list(
      fit = fit,
      warper = warper
    )
    chkDots(...)
    class(res) <- "warped_model"
    res
  }
}


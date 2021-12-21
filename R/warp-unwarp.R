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
#'
#' @details Models fitted using the resulting object currently don't seem to work properly
#'   inside function calls that are parallelized with `future`; at least that's
#'   what happens with `sperrorest`.
#'
#' @export
warp.function <- function (x, warper, ...)
{
  function(formula, data, ...) {
    data <- unwarp(data, warper = warper)
    uwformula <- unwarp(formula, warper = warper)
    fit <- x(uwformula, data = data, ...)
    res <- list(fit = fit, warper = warper,
                unwarped_formula = uwformula,
                formula = formula)
    class(res) <- "warped_model"
    res
  }
}


#' Formula method for warped models
#'
#' @param x Warped model, of class `warped_model`.
#' @param warped Return the formula involving the warped predictors
#'   or the original untransformed predictor names. Defaults to `TRUE`,
#'   i.e. warped names, since the warped model wants to be talked to
#'   in the language of warped predictors.
#' @param ... Not used, ignored with a warning.
#'
#' @return Formula
#' @export
formula.warped_model <- function(x, warped = TRUE, ...) {
  chkDots(...)
  ifelse(warped, x$formula, x$unwarped_formula)
}


#' Warp model formula
#'
#' @param x Formula object.
#' @param warper Warper object.
#' @param ... Not used, ignored with a warning.
#'
#' @return The warped formula (e.g., based on principal components), or the
#'   unwarped formula (using the original, untransformed variable names).
#' @importFrom stats as.formula
#' @importFrom purrr flatten
#' @export
#'
#' @details Currently only "simple" formulas of the form `y ~ x1 + x2 + x3` etc.
#'   are allowed - no `*`, `:`, `s()`, `I()`, `^2` or other operations that
#'   are supported by some modeling functions.
warp.formula <- function(x, warper, ...) {
  chkDots(...)
  xvars <- unlist(purrr::flatten(warper$xvars))
  # check if all predictor variables are included in the Xvars / uvars set:
  if (!all(xvars %in% all.vars(x)[-1]))
    stop("Warper inconsistent with formula: not all Xvars are included in formula.")
  if (!all(all.vars(x)[-1] %in% c(xvars, warper$uvars)))
    stop("Warper inconsistent with formula: not all predictor variables are included in Xvars or uvars.")
  # Only use uvars that are also in the formula since warper might contain
  # additional metadata variables such as x/y coordinates:
  sel_uvars <- warper$uvars[ warper$uvars %in% all.vars(x)[-1] ]
  wvars <- warper$Wvars[ !(warper$Wvars %in% warper$uvars) ]

  stats::as.formula(paste(all.vars(x)[1], "~",
                   paste(c(wvars, sel_uvars), collapse = "+")))
}


#' @describeIn warp.formula Unwarp a model formula.
#' @importFrom stats as.formula
#' @importFrom purrr flatten
#' @export
unwarp.formula <- function(x, warper, ...) {
  wvars <- warper$Wvars[ !(warper$Wvars %in% warper$uvars) ]
  # check if all predictor variables are included in the Wvars / uvars set:
  if (!all(wvars %in% all.vars(x)[-1]))
    stop("Warper inconsistent with formula: not all Xvars are included in formula.")
  if (!all(all.vars(x)[-1] %in% c(wvars, warper$uvars)))
    stop("Warper inconsistent with formula: not all predictor variables are included in Xvars or uvars.")
  # Only use uvars that are also in the formula since warper might contain
  # additional metadata variables such as x/y coordinates:
  sel_uvars <- warper$uvars[ warper$uvars %in% all.vars(x)[-1] ]
  xvars <- unlist(purrr::flatten(warper$xvars))

  stats::as.formula(paste(all.vars(x)[1], "~",
                   paste(c(xvars, sel_uvars), collapse = "+")))
}

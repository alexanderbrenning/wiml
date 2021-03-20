
#' Create a warped view of a fitted machine-learning model
#'
#' This function creates the composition function \eqn{\hat{f}\circ T^{-1}}
#' from a fitted machine-learning model \eqn{\hat{f}} and a warper object
#' representing the transformation function \eqn{T}.
#'
#' @param x A fitted model, for example from a \code{randomForest} call.
#'     This model was fitted to the original, untransformed data.
#' @param warper A \code{warper} object, representing an (invertible)
#'     transformation from the model's feature space to a transformed space
#'     whose axes (or base vectors) define a new perspective on the
#'     feature space and model.
#' @return An object of class \code{warped_model}, which can be used to
#'   make predictions using input data from the transformed space
#'   thanks to the \code{predict} method for this object type.
#' @details This function does not re-fit the model, it simply creates a
#'   data structure that jointly stores the fitted model and the warper
#'   object. The \code{warped_model}'s \code{predict} (and other) methods
#'   allow the user to use a \code{warped_model} as if it had been fitted
#'   using the transformed data.
#' @example examples/warp_fitted_model.R
#' @export
warp_fitted_model <- function(x, warper) {
  res <- list(fit = x, warper = warper)
  class(res) <- "warped_model"
  res
}


#' Predict from a warped fitted machine-learning model
#'
#' This function implements the prediction \eqn{\textbf{w}\mapsto\hat{y}:=(\hat{f}\circ T^{-1})(\textbf{w})}
#' by (back)transforming a feature vector \eqn{w} from the transformed space (e.g.,
#' principal components space) to the original feature space and then applying
#' the fitted machine learning model's predict method to make predictions.
#'
#' @param object A \code{warped_model} object, i.e. a warped fitted machine-learning model as created by \code{\link{warp_fitted_model}}.
#' @param newdata A data frame with features in transformed space. This can be a \code{warped_df} object or just a plain \code{data.frame}. See \code{force_unwarp}.
#' @param force_unwarp If \code{TRUE}, \code{newdata} will always be (back)transformed
#'   into feature space using the \code{warper}'s inverse transformation.
#'   If \code{FALSE}, only objects of class \code{warped_df} will be backtransformed.
#' @param ... Additional arguments for the predict method of \code{object$fit}.
#' @return The function's result depends on the behaviour of the invoked
#'   predict method.
#' @details \code{newdata} will normally be a data frame, not \code{NULL}; not all
#'   fitted models accept \code{NULL}. The most common behaviour is to return
#'   model predictions for the training data.
#' @seealso \code{\link{warp_fitted_model}} for creating a warped machine-learning
#'   model, and the \code{\link{predict}} method of your machine-learning model
#'   for details on additional arguments and the returned values.
#' @example examples/warp_fitted_model.R
#' @export
predict.warped_model <- function(object, newdata = NULL,
                                 force_unwarp = TRUE, ...) {
  # Note on force_unwarp = TRUE:
  # Default value for force_unwarp must be TRUE because calling functions
  # such as sperrorest or functions in the iml package are unaware of
  # the existence of transformed and untransformed feature space representations.
  newdata <- unwarp(newdata, warper = object$warper,
                    force = force_unwarp)
  predict(object$fit, newdata = newdata, ...)
}


#' Summary of a warped model
#'
#' These methods try to create meaningful summaries for warped model objects
#' and their components (warper, model).
#'
#' @param object,x A warped fitted model object (or a summary object, in the `print` method).
#' @param ... Additional arguments.
#' @return Summary object.
#' @export
summary.warped_model <- function(object, ...) {
  x <- list(
    summary_warper = summary(object$warper),
    summary_model = summary(object$fit, ...)
  )
  class(x) <- "print.summary.warped_model"
  x
}


#' @describeIn summary.warped_model Print summary of warped model
#'
#' @export
print.summary.warped_model <- function(x, ...) {
  cat("Warped machine-learning model:\n\n")
  cat("Warper:\n")
  print(x$summary_warper)
  cat("\nModel summary:\n\n")
  print(x$summary_model, ...)
  invisible(x)
}


#' @describeIn summary.warped_model Attempt to plot warped model
#' @export
plot.warped_model <- function(x, ...) {
  plot(x$fit, ...)
}


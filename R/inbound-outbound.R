#' Generic methods for forward (outbound) and backward (inbound) feature space transformation using warper objects
#'
#' Outbound or forward transformation refers to the mapping \eqn{T}
#' from feature space into a transformed space (e.g., principal components space),
#' and inbound or backward transformation is its inverse transformation \eqn{T^{1}}
#' back into feature space.
#'
#' @param object A \code{warper} object, such as a \code{\link{pca_warper}}
#'     for principal components transformations.
#' @param ... Additional arguments for the corresponding method.
#' @return A transformed data frame of class \code{warped_df} (for outbound)
#'   or \code{data.frame} (for inbound transformations).
#' @details These functions are mainly intended for internal use.
#'   The \code{\link{warp}} and \code{\link{unwarp}} methods are more
#'   user-friendly.
#' @export
outbound <- function(object, ...) {
  UseMethod("outbound", object)
}


#' @describeIn outbound Generic method for inbound
#' @export
inbound <- function(object, ...) {
  UseMethod("inbound", object)
}


#' Methods for forward (outbound) and backward (inbound) feature space transformation using warper objects
#'
#' Outbound or forward transformation refers to the mapping \eqn{T}
#' from feature space into a transformed space (e.g., principal components space),
#' and inbound or backward transformation is its inverse transformation \eqn{T^{1}}
#' back into feature space. This is an abstract parent class for [pca_warper]
#' and other related objects that can be expressed using rotation matrices.
#'
#' @param object A \code{warper} object.
#' @param xdata A data frame with features in the original, untransformed
#'     feature space.
#' @param wdata A data frame containing data in transformed space.
#' @param ... Currently not used.
#' @return A transformed data frame of class \code{warped_df} (for outbound)
#'   or \code{data.frame} (for inbound transformations).
#' @details These functions are mainly intended for internal use.
#'   The \code{\link{warp}} and \code{\link{unwarp}} methods are more
#'   user-friendly.
#' @export
inbound.rotation_warper <- function(object, wdata = object$wdata, ...) {
  chkDots(...)

  # Rotate the data using the inverse rotation matrix:
  res <- as.data.frame(as.matrix(wdata[, object$Wvars]) %*%
                         solve(object$full_rotation))
  colnames(res) <- object$Xvars

  # Scale the data, and adds its mean, if applicable:
  if (any(names(object) == "scale"))
    if (!is.logical(object$scale))
      res[, names(object$scale)] <-
    res[, names(object$scale)] *
    matrix(rep(object$scale, nrow(wdata)),
           nrow = nrow(wdata), byrow = TRUE)
  if (any(names(object) == "center"))
    if (!is.logical(object$center))
      res[, names(object$center)] <-
    res[, names(object$center)] +
    matrix(rep(object$center, nrow(wdata)),
           nrow = nrow(wdata), byrow = TRUE)

  wdata[object$Wvars] <- NULL
  wdata[, object$Xvars] <- res

  wdata
}


#' Methods for forward (outbound) and backward (inbound) feature space transformation using warper objects
#'
#' Outbound or forward transformation refers to the mapping \eqn{T}
#' from feature space into a transformed space (e.g., principal components space),
#' and inbound or backward transformation is its inverse transformation \eqn{T^{1}}
#' back into feature space.
#'
#' @param object A \code{warper} object created by \code{\link{pca_warper}}
#'     for principal components transformations.
#' @param wdata A data frame containing data in transformed space, i.e.
#'     coordinates with respect to principal components axes.
#' @param ... Additional arguments to be passed to `inbound.rotation_warper`
#' @return A transformed data frame of class \code{warped_df} (for outbound)
#'   or \code{data.frame} (for inbound transformations).
#' @details These functions are mainly intended for internal use.
#'   The \code{\link{warp}} and \code{\link{unwarp}} methods are more
#'   user-friendly.
#' @export
inbound.pca_warper <- function(object, wdata = object$pca$x, ...) {
  inbound.rotation_warper(object = object, wdata = wdata, ...)
}


#' @describeIn inbound.rotation_warper Forward (outbound) transformation using a rotation warper
#' @export
outbound.rotation_warper <- function(object, xdata = NULL, ...) {
  chkDots(...)
  # Center and then scale the variables:
  res <- as.matrix(xdata[, object$Xvars])
  if (any(names(object) == "center"))
    if (!is.logical(object$center))
      res[, names(object$center)] <-
    res[, names(object$center)] -
    matrix(rep(object$center, nrow(xdata)),
           nrow = nrow(xdata), byrow = TRUE)
  if (any(names(object) == "scale"))
    if (!is.logical(object$scale))
      res[, names(object$scale)] <-
    res[, names(object$scale)] *
    matrix(rep(1 / object$scale, nrow(xdata)),
           nrow = nrow(xdata), byrow = TRUE)
  # Apply rotation:
  res <- as.data.frame(res %*% object$full_rotation)

  xdata[object$Xvars] <- NULL
  xdata[, object$Wvars] <- res

  xdata
}



#' Generic methods for forward (outbound) and backward (inbound) feature space transformation using warper objects
#'
#' Outbound or forward transformation refers to the mapping \eqn{T}
#' from feature space into a transformed space (e.g., principal components space),
#' and inbound or backward transformation is its inverse transformation \eqn{T^{1}}
#' back into feature space.
#'
#' @param object A \code{warper} object, such as a \code{\link{pca_warper}}
#'     for principal components transformations.
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
#' @return A transformed data frame of class \code{warped_df} (for outbound)
#'   or \code{data.frame} (for inbound transformations).
#' @details These functions are mainly intended for internal use.
#'   The \code{\link{warp}} and \code{\link{unwarp}} methods are more
#'   user-friendly.
#' @export
inbound.rotation_warper <- function(object, wdata = object$wdata) {
  res <- as.data.frame(as.matrix(wdata[, object$wvars]) %*%
                         solve(object$full_rotation))
  if (any(names(object) == "scale"))
    if (!is.logical(object$scale))
      res <- res * matrix(rep(object$scale, nrow(wdata)),
                          nrow = nrow(wdata), byrow = TRUE)
  if (any(names(object) == "center"))
    if (!is.logical(object$center))
      res <- res + matrix(rep(object$center, nrow(wdata)),
                          nrow = nrow(wdata), byrow = TRUE)
  if (any(colnames(wdata) == object$yvar))
    res[object$yvar] <- wdata[, object$yvar]
  return(res)
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
#' @return A transformed data frame of class \code{warped_df} (for outbound)
#'   or \code{data.frame} (for inbound transformations).
#' @details These functions are mainly intended for internal use.
#'   The \code{\link{warp}} and \code{\link{unwarp}} methods are more
#'   user-friendly.
#' @export
inbound.pca_warper <- function(object, wdata = object$pca$x) {
  inbound.rotation_warper(object = object, wdata = wdata)
}


#' @describeIn inbound.rotation_warper Forward (outbound) transformation using a rotation warper
#' @export
outbound.rotation_warper <- function(object, xdata = NULL) {
  # Center and then scale the variables:
  res <- as.matrix(xdata[, object$xvars])
  if (any(names(object) == "center"))
    if (!is.logical(object$center))
      res <- res - matrix(rep(object$center, nrow(xdata)),
                          nrow = nrow(xdata), byrow = TRUE)
  if (any(names(object) == "scale"))
    if (!is.logical(object$scale))
      res <- res * matrix(rep(1 / object$scale, nrow(xdata)),
                          nrow = nrow(xdata), byrow = TRUE)
  # Apply rotation:
  res <- as.data.frame(res %*% object$full_rotation)
  # }
  if (any(colnames(xdata) == object$yvar))
    res[object$yvar] <- xdata[, object$yvar]
  return(res)
}



#' Principal components transformation of feature space
#'
#' This function generates a \code{warper} object based on a principal components
#' analysis of the features (or more precisely, of selected features \code{xvars}).
#'
#' @param xdata A data frame containing the observations in the original feature space.
#' @param xvars A character vector with the column names of features in \code{xdata}
#'     that should be transformed.
#' @param wvars A character vector with the names of all transformed predictors,
#'     or (more often) a simple character string to which the numbers 1, ...
#'     will be appended as needed.
#' @param yvar Name of the response variable (not to be transformed)
#' @param uvars Names of additional variables that should remain untouched.
#' @param scale,center Logical arguments indicating whether the data should
#'     be centered and then scales. Both should be turned on (`TRUE`), which is
#'     the default behaviour.
#' @param positive Logical argument (default: `TRUE`) indicating whether
#'     the signs of the loadings should be adjusted so that the most
#'     strongly weighted PC gets a positive sign.
#' @param title Optional name of the transformation, may be used for printing summaries
#'     or for plotting.
#' @return An object of class \code{warper}, \code{rotation_warper} and \code{pca_warper}.
#' @details The function will make an effort to ensure that there's no overlap
#'     between \code{xvars}, \code{uvars} and \code{yvar}; however, ideally
#'     these arguments should not overlap.
#' @example examples/pca_warper.R
#' @export
pca_warper <- function(xdata, xvars, wvars = "PC", yvar, uvars = NULL,
                       center = TRUE, scale = TRUE,
                       positive = TRUE,
                       title = wvars) {
  if (missing(xdata))
    stop("data frame 'xdata' must be specified")
  if (missing(xvars))
    stop("predictor variables 'xvars' must be specified")
  if (missing(yvar))
    stop("response variable 'yvar' must be specified")

  xvars <- xvars[ !(xvars %in% uvars) ]
  wvars <- wvars[ !(wvars %in% uvars) ]

  # Center and scale x variables:
  if (center) {
    center <- colMeans(xdata[, xvars], na.rm = TRUE)
    xdata[, xvars] <- sweep(xdata[, xvars], 2L, center,
                            check.margin = FALSE)
  }
  if (scale) {
    f <- function(v) {
      v <- v[!is.na(v)]
      sqrt(sum(v^2)/max(1, length(v) - 1L))
    }
    scale <- apply(xdata[, xvars], 2L, f)
    xdata[, xvars] <- sweep(xdata[, xvars], 2L, scale, "/",
                            check.margin = FALSE)
  }

  # Perform PCA on x variables:
  fo <- as.formula(paste0("~", paste(xvars, collapse = " + ")))
  pca <- prcomp(formula = fo, data = xdata[,xvars],
                center = FALSE, scale. = FALSE)

  # Make loadings 'more positive':
  if (positive) {
    for (i in 1:length(xvars)) {
      ldg <- pca$rotation[,i]
      wh <- which.max(abs(ldg))
      if (ldg[wh] < 0) {
        pca$rotation[,i] <- ldg * (-1)
        pca$x[,i] <- pca$x[,i] * (-1)
      }
    }
  }

  # Assign user-defined names to transformed features:
  if (length(wvars) < ncol(pca$x))
    wvars <- paste0(wvars[1], 1:ncol(pca$x))
  wvars <- wvars[1:ncol(pca$x)]
  colnames(pca$x) <- wvars
  colnames(pca$rotation) <- wvars

  # Overall rotation matrix, including untransformed features, if present:
  uid <- diag(length(uvars)) # works with length == 0
  rownames(uid) <- colnames(uid) <- uvars
  full_rotation <- lava::blockdiag(pca$rotation, uid)

  # Overall model formula (I think this can be removed):
  xfo <- as.formula(paste0(yvar, "~", paste(xvars, collapse = " + ")))

  # Set up warper object:
  x <- list(
    pca = pca,
    full_rotation = full_rotation,
    center = center,
    scale = scale,
    xformula = xfo,
    xvars = rownames(full_rotation),
    wvars = colnames(full_rotation),
    yvar = yvar,
    uvars = uvars,
    title = title
  )
  class(x) <- c("pca_warper", "rotation_warper", "warper")
  return(x)
}




#' Structured principal components transformation of feature space
#'
#' This function offers a more structured approach to feature space transformation
#' by allowing the user to transform different groups of predictor variables
#' separately. It generates a \code{warper} object based on a principal components
#' analyes applied to the feature subsets.
#'
#' @param xdata A data frame containing the observations in the original feature space.
#' @param xvars A list of character vectors with the column names of features in \code{xdata}
#'     that should be transformed. Variables named in each list component are jointly
#'     transformed using PCA. These groups shouldn't be too correlated amongst
#'     each other.
#' @param wvars A character vector of same length as \code{xvars} giving the prefixes
#'     to be used to generate the names of transformed predictors. If of length 1,
#'     use prefix \code{"<wvars>[1]"} for first feature subset, i.e. \code{"PC[1]2"} for
#'     second principal compoment from first feature subset. If \code{NULL}, use
#'     \code{"PC"} as the prefix.
#' @param uvars Optional list of same length as \code{xvars} with names of additional
#'     variables that should remain untouched.
#' @inheritParams pca_warper
#' @return An object of class \code{warper}, \code{rotation_warper} and \code{strucpca_warper}.
#' @export
strucpca_warper <- function(xdata, xvars, wvars = NULL, yvar, uvars = NULL,
                            center = TRUE, scale = TRUE,
                            positive = TRUE,
                            title = NULL) {
  if (missing(xdata))
    stop("data frame 'xdata' must be specified")
  if (missing(xvars))
    stop("predictor variables 'xvars' must be specified")
  if (missing(yvar))
    stop("response variable 'yvar' must be specified")

  wrp <- list()
  nwrp <- length(xvars)

  if (length(center) == 1)
    ctr <- rep(center, length(xvars))
  if (length(scale) == 1)
    scl <- rep(scale, length(xvars))
  if (length(positive) == 1)
    positive <- rep(positive, length(xvars))

  for (i in 1:nwrp) {
    if (is.list(uvars)) {
      the_uvars <- uvars[[i]]
    } else {
      the_uvars <- NULL
      if (i == 1) the_uvars <- uvars
    }
    if (length(wvars) == nwrp) {
      the_wvars <- wvars[i]
    } else if (length(wvars) == 1) {
      the_wvars <- paste0(wvars, "[", i, "]")
    } else if (is.null(wvars)) {
      the_wvars <- paste0("PC[", i, "]")
    }
    wrp[[i]] <- pca_warper(xdata = xdata, xvars = xvars[[i]],
                           yvar = yvar, uvars = the_uvars,
                           wvars = the_wvars,
                           center = ctr[i],
                           scale = scl[i],
                           positive = positive[i])
    if (i == 1) {
      full_rotation <- wrp[[i]]$full_rotation
      ctr <- wrp[[i]]$pca$center
      scl <- wrp[[i]]$pca$scale
    } else {
      full_rotation <- lava::blockdiag(full_rotation, wrp[[i]]$full_rotation)
      ctr <- c(ctr, wrp[[i]]$pca$center)
      scl <- c(scl, wrp[[i]]$pca$scale)
    }
  }

  if (is.null(title)) {
    title <- "PCs"
    if (!is.null(wvars)) {
      title <- paste0(wvars, collapse = " / ")
    }
  }

  x <- list(
    warpers = wrp,
    full_rotation = full_rotation,
    center = ctr,
    scale = scl,
    xvars = rownames(full_rotation),
    wvars = colnames(full_rotation),
    yvar = yvar,
    uvars = unlist(uvars),
    title = title
  )
  class(x) <- c("strucpca_warper", "pca_warper", "rotation_warper", "warper")

  return(x)
}





#' Partial least squares transformation of feature space
#'
#' This function generates a \code{warper} object based on a partial least squares
#' transformation with respect to a selected feature.
#'
#' @param xdata A data frame containing the observations in the original feature space.
#' @param xvars A character string identifying one feature in \code{xdata}
#'     that should be transformed.
#' @param wvars A character string giving a prefix for partial residual features.
#' @inheritParams pca_warper
#' @return An object of class \code{warper}, \code{rotation_warper} and \code{pls_warper}.
#' @details The function will make an effort to ensure that there's no overlap
#'     between \code{xvars}, \code{uvars} and \code{yvar}; however, ideally
#'     these arguments should not overlap.
#' @export
pls_warper <- function(xdata, xvars, pvars, wvars = "resid", yvar, uvars = NULL,
                       title = wvars) {
  if (missing(xdata))
    stop("data frame 'xdata' must be specified")
  if (missing(xvars))
    stop("predictor variables 'xvars' must be specified")
  if (missing(yvar))
    stop("response variable 'yvar' must be specified")
  if (missing(pvars))
    stop("selected predictor variable 'pvars' must be specified (length 1)")
  if (length(pvars) > 1) {
    warning("length of 'pvars' must be 1 at present; using first element")
    pvars <- pvars[1]
  }

  xvars <- xvars[ !(xvars %in% uvars) ]
  xvars <- xvars[ !(xvars %in% pvars) ]
  wvars <- wvars[ !(wvars %in% uvars) ]
  wvars <- wvars[ !(wvars %in% pvars) ]

  if (is.null(uvars))
    uvars <- dplyr::setdiff(colnames(xdata), c(yvar, pvars, xvars))

  if (length(wvars) < length(xvars))
    wvars <- paste0(wvars[1], xvars)

  # Re-order columns, remove unused variables:
  xdata <- xdata[, c(yvar, pvars, xvars, uvars)]

  stopifnot(length(pvars) == 1)

  # Identity rotation matrix for untransformed features:
  uid <- diag(length(uvars)) # works with length == 0
  rownames(uid) <- colnames(uid) <- uvars

  betas <- xvars %>% map_dbl(~ cor(xdata[,.x], xdata[,pvars[1]]))

  # xdata[,xvars] <- xvars %>% map(~ xdata[,.x] - xdata[,pvars] * betas[.x]) %>%
  #   as.data.frame()

  rotation <- rbind(betas*(-1), diag(length(xvars)))
  rotation <- cbind(c(1, rep(0, length(xvars))), rotation)
  rownames(rotation) <- c(pvars, xvars)
  colnames(rotation) <- c(pvars, wvars)

  # Compile full rotation matrix from identity matrices for pvars and uvars,
  # and orthogonalization matrix for xvars -> wvars transformation:
  full_rotation <- lava::blockdiag(rotation, uid)

  # Transform xvars -> wvars:
  xdata[,c(pvars,xvars)] <- as.matrix(xdata[,c(pvars,xvars)]) %*% rotation
  # Full rotation would be a waste of computing time:
  # xdata[,c(pvars,xvars,uvars)] <- as.matrix(xdata[,c(pvars,xvars,uvars)]) %*% full_rotation

  # Assign user-defined names to transformed features:
  colnames(xdata) <- c(yvar, pvars, wvars, uvars)

  # Set up warper object:
  x <- list(
    full_rotation = full_rotation,
    center = FALSE,
    scale = FALSE,
    wdata = xdata,
    xvars = rownames(full_rotation),
    wvars = colnames(full_rotation),
    pvars = pvars,
    yvar = yvar,
    uvars = uvars,
    title = title
  )
  class(x) <- c("pls_warper", "rotation_warper", "warper")
  return(x)
}




#' Create a warped version of a model fitting function
#'
#' Use \code{\link{warp_fitted_model}} instead, which provides a more
#' elegant way to create a warped perspective on a model. This function may
#' be omitted in the future.
#'
#' @param x A model fitting function such as \code{\link{lm}} or \code{\link[randomForest]{randomForest}}
#' @param warper A \code{warper} object, for example a \code{\link{pca_warper}}.
#' @param ... Additional arguments to be passed to the model fitting function.
#' @return A function representing the composition of the learning function \eqn{F:L\mapsto\hat{f}}
#'   with the inverse transformation function \eqn{T^{-1}}. In other words,
#'   this function first (back)transformed its input data from the transformed space
#'   to the original feature space, and then fits a model using these features.
#' @seealso \code{\link{warp_fitted_model}}
#' @export
warp.function <- function(x, warper) {
  function(formula, data, ...) {
    data <- unwarp(data, warper = warper)
    fit <- x(formula, data = data, ...)
    res <- list(
      fit = fit,
      warper = warper
    )
    class(res) <- "warped_model"
    return(res)
  }
}


#' Transform data from feature space into a transformed space
#'
#' These methods apply the transformation \eqn{T} to data
#' in feature space - the result is, for example, the principal components
#' representation of the data, if a \code{\link{pca_warper}} is used.
#'
#' @param x The object to be transformed, normally a data frame.
#' @param ... Additional arguments to be passed to the \code{warp} method.
#' @return A data frame with features in the transformed (e.g., PCA) space.
#' @seealso \code{\link{unwarp}} for the inverse transformation, and
#'   \code{\link{pca_warper}} for an example of a simple warper function based
#'   on the principal components transformation.
#' @export
warp <- function(x, ...) {
  UseMethod("warp", x)
}

#' Transform data from feature space into a transformed space
#'
#' \code{warp} methods apply the transformation \eqn{T} to data
#' in feature space - the result is, for example, the principal components
#' representation of the data, if a \code{\link{pca_warper}} is used.
#'
#' @param x The object to be transformed, normally a data frame.
#' @param warper A \code{warper} object as, for example, created by \code{\link{pca_warper}} for principal components transformations.
#' @param ... Additional arguments to be passed to the \code{warp} method.
#' @return A data frame with features in the transformed (e.g., PCA) space.
#' @seealso \code{\link{unwarp}} for the inverse transformation, and
#'   \code{\link{pca_warper}} for an example of a simple warper function based
#'   on the principal components transformation.
#' @export
warp.data.frame <- function(x, warper) {
  x <- outbound(warper, xdata = x)
  class(x) <- c("warped_df", class(x))
  return(x)
}

#' @describeIn warp.data.frame Does nothing - \code{warped_df} has already been transformed
#' @export
warp.warped_df <- function(x, warper) {
  x
}

#' Backtransform from transformed ('warped') to original feature space
#'
#' These methods apply the inverse transformation \eqn{T^{-1}} to data
#' in transformed space - e.g. a feature vector in principal components
#' space back to the original feature space, in the case of a PCA
#' transformation.
#'
#' @param x The object to be backtransformed, for example a data frame or a \code{warped_df} object.
#' @param ... Additional arguments to be passed to the \code{unwarp} method.
#' @return A data frame with features in the original feature space.
#' @seealso \code{\link{warp}} for the forward transformation, and
#'   \code{\link{pca_warper}} for an example of a simple warper function based
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
#' @param warper A \code{warper} object as, for example, created by \code{\link{pca_warper}} for principal components transformations.
#' @param force_unwarp If \code{TRUE}, backtransform even if it's not a \code{warped_df} object. (Ignored by \code{unwarp.warped_df}.)
#' @param ... Additional arguments to be passed to the \code{unwarp} method.
#' @return A data frame with features in the original feature space.
#' @seealso \code{\link{warp}} for the forward transformation, and
#'   \code{\link{pca_warper}} for an example of a simple warper function based
#'   on the principal components transformation.
#' @example examples/pca_warper.R
#' @export
unwarp.warped_df <- function(x, warper, force_unwarp = FALSE) {
  class(x) <- class(x)[class(x) != "warped_df"]
  x <- inbound(warper, wdata = x)
  return(x)
}

#' @describeIn unwarp.warped_df Backtransform a data frame from transformed to original feature space
#' @export
unwarp.data.frame <- function(x, warper = NULL, force_unwarp = FALSE) {
  if (force_unwarp)
    x <- unwarp.warped_df(x, warper = warper)
  return(x)
}


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
  return(res)
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
#' @param object A warped fitted model object.
#' @param ... Additional arguments.
#' @return summary or plot objects
#' @export
summary.warped_model <- function(object, ...) {
  list(
    summary_warper = summary(object$warper),
    summary_model = summary(object$fit, ...)
  )
}


#' @describeIn summary.warped_model Attempt to plot warped model
#' @export
plot.warped_model <- function(object, ...) {
  plot(object$fit, ...)
}


#' Plot a PCA warper transformation object
#'
#' This method plots standard PCA summary diagrams.
#'
#' @param object A \code{\link{pca_warper}} object.
#' @param col,bp.col Colours of \code{biplot} labels and \code{barplot} bars.
#' @return Invisibly returns the result of the biplot call (this may change)
#' @example examples/pca_warper.R
#' @export
plot.pca_warper <- function(object, col = c("white", "black"), bp.col = "lightblue") {
  # check out ggbiplot:
  # https://rpubs.com/ciwhite/585948
  par(mfrow = c(1,2))
  plot(object$pca, col = bp.col, xlab = "PC #", main = "PC Variances")
  biplot(object$pca, col = col, main = "Screeplot")
}

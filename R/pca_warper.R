
#' Principal components transformation of feature space
#'
#' This function generates a `warper` object based on a principal component
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
#' @return An object of class `warper`, `rotation_warper` and `pca_warper`.
#' @details There should be no overlap
#'     between `xvars`, `uvars` and `yvar`.
#'     The rotation matrix `$full_rotation` in the results object
#'     is of size `length(xvars)+length(uvars)` and includes an
#'     identity transformation `diag(length(uvars))` for the
#'     features in `uvars`.
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

  # xvars <- xvars[ !(xvars %in% uvars) ]
  # wvars <- wvars[ !(wvars %in% uvars) ]

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
  pca <- stats::prcomp(formula = fo, data = xdata[, xvars],
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
    Xvars = rownames(full_rotation),
    Wvars = colnames(full_rotation),
    xvars = xvars,
    wvars = wvars,
    yvar = yvar,
    uvars = uvars,
    title = title
  )
  class(x) <- c("pca_warper", "rotation_warper", "warper")
  x
}



#' Plot a PCA warper transformation object
#'
#' This method plots standard PCA summary diagrams for a `pca_warper`, i.e. a principal component transformation of feature space.
#'
#' @param object A \code{\link{pca_warper}} object.
#' @param which Which plots to plot: screeplot (`1`), biplot (`2`).
#' @param col,bp.col Colours of \code{biplot} labels and \code{barplot} bars.
#' @param ... Currently not used.
#' @return Returns `NULL`
#' @example examples/pca_warper.R
#' @export
plot.pca_warper <- function(x,
                            which = c(1:2),
                            col = c("white", "black"),
                            bp.col = "lightblue",
                            ...) {
  # check out ggbiplot:
  # https://rpubs.com/ciwhite/585948
  chkDots(...)
  if (any(which == 1))
    plot(x$pca, col = bp.col, xlab = "PC #", main = "PC Variances")
  if (any(which == 2))
    biplot(x$pca, col = col, main = "Screeplot")
  invisible(NULL)
}



#' Summary of a `pca_warper` object
#'
#' @param object A `pca_warper` object.
#' @param ... Additional arguments to [stats::summary.prcomp()]
#'     (currently not used).
#'
#' @return A `summary.pca_warper` object.
#' @seealso [stats::summary.prcomp()]
#' @export
#'
#' @example examples/summary.pca_warper.R
summary.pca_warper <- function(object, ...) {
  x <- list(
    spca = summary(object$pca, ...),
    nxvars = length(object$xvars),
    nuvars = length(object$uvars)
  )
  class(x) <- "summary.pca_warper"
  x
}


#' Print summary of `pca_warper` object
#'
#' @param x `summary.pca_warper` object for principal component transformation of feature space
#' @param ... Additional arguments for [stats::print.summary.prcomp()],
#'
#' @return Returns the `summary.pca_warper` object.
#' @seealso [summary.pca_warper()], [stats::print.summary.prcomp()]
#' @export
#'
#' @example examples/summary.pca_warper.R
print.summary.pca_warper <- function(x, ...) {
  cat("Principal components transformation for ",
      x$nxvars,
      " transformed features\n(and ",
      x$nuvars,
      " untransformed ones)\n\n", sep = "")
  print(x$spca, ...)
  invisible(x)
}


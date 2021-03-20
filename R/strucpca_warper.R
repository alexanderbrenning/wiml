
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
#'     second principal component from first feature subset. If \code{NULL}, use
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

  if (is.null(wvars)) wvars <- "PC"
  if ((length(wvars) == 1) & (nwrp > 1))
    wvars <- paste0(wvars, "[", c(1:nwrp), "]")

  for (i in 1:nwrp) {
    if (is.list(uvars)) {
      the_uvars <- uvars[[i]]
    } else {
      the_uvars <- NULL
      if (i == 1) the_uvars <- uvars
    }
    wrp[[i]] <- pca_warper(xdata = xdata, xvars = xvars[[i]],
                           yvar = yvar, uvars = the_uvars,
                           wvars = wvars[i],
                           center = ctr[i],
                           scale = scl[i],
                           positive = positive[i])
    if (i == 1) {
      full_rotation <- wrp[[i]]$full_rotation
      ctr <- wrp[[i]]$center
      scl <- wrp[[i]]$scale
    } else {
      full_rotation <- lava::blockdiag(full_rotation, wrp[[i]]$full_rotation)
      ctr <- c(ctr, wrp[[i]]$center)
      scl <- c(scl, wrp[[i]]$scale)
      # ...not $pca$center and $pca$scale, which are
      # unused and FALSE!
    }
  }

  if (is.null(title)) {
    title <- "PCs"
    if (!is.null(wvars)) {
      title <- paste0(wvars, collapse = " / ")
    }
  }

  names(wrp) <- wvars

  x <- list(
    warpers = wrp,
    full_rotation = full_rotation,
    center = ctr,
    scale = scl,
    Xvars = rownames(full_rotation),
    Wvars = colnames(full_rotation),
    xvars = xvars,
    wvars = wvars,
    yvar = yvar,
    uvars = unlist(uvars),
    title = title
  )
  class(x) <- c("strucpca_warper", "pca_warper", "rotation_warper", "warper")

  x
}


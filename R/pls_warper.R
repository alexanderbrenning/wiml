
#' Partial least squares transformation of feature space
#'
#' This function generates a `warper` object based on a partial least squares
#' transformation with respect to a selected feature.
#'
#' @param xdata A data frame containing the observations in the original feature space.
#' @param pvars A character string identifying one feature in `xdata`
#'     that is used as the selected feature; the other features in
#'     `xvars` will be transformed by orthogonalization, while
#'     this one will remain unchanged.
#' @param wvars A character string giving a prefix for partial residual features.
#' @inheritParams pca_warper
#' @return An object of class `warper`, `rotation_warper` and `pls_warper`.
#' @details The arguments
#'     `pvars`, `xvars`, `uvars` and `yvar` should not overlap.
#' @example examples/pls_warper.R
#' @importFrom magrittr %>%
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
  if (any(xvars == pvars)) {
    warning("'pvars' cannot be included in 'xvars'; removing it.")
    xvars <- xvars[ xvars != pvars ]
  }
  if ((length(wvars) != 1) & (length(wvars) != length(xvars))) {
    warning("length of 'wvars' must be 1 or equal to length of 'xvars';\n  using 'wvars[1]' (or otherwise \"resid\") as prefix")
    if (length(wvars) >= 1) {
      wvars <- wvars[1]
    } else {
      wvars <- "resid"
    }
  }

  # FIXME: do some checks instead:
  # xvars <- xvars[ !(xvars %in% uvars) ]
  # xvars <- xvars[ !(xvars %in% pvars) ]
  # wvars <- wvars[ !(wvars %in% uvars) ]
  # wvars <- wvars[ !(wvars %in% pvars) ]

  # if (is.null(uvars))
  #   uvars <- dplyr::setdiff(colnames(xdata), c(yvar, pvars, xvars))

  if (length(wvars) < length(xvars))
    wvars <- paste0(wvars[1], xvars)

  # Ignored variables:
  ivars <- dplyr::setdiff(colnames(xdata), c(yvar, pvars, xvars, uvars))

  # Re-order columns:
  xdata <- xdata[, c(yvar, pvars, xvars, uvars, ivars)]

  # Identity rotation matrix for untransformed features:
  uid <- diag(length(uvars)) # works with length == 0
  rownames(uid) <- colnames(uid) <- uvars

  cf_fun <- function(x) {
    fo <- stats::as.formula(paste(x, "~", pvars))
    cf <- stats::coef(stats::lm(fo, data = xdata))
    names(cf) <- c("alpha", "beta")
    cf
  }

  coefs <- xvars %>% purrr::map_dfr(cf_fun) %>% as.matrix()

  center <- t(coefs[, 1])
  names(center) <- xvars

  rotation <- rbind(t(coefs[, 2])*(-1), diag(length(xvars)))
  rotation <- cbind(c(1, rep(0, length(xvars))), rotation)
  rownames(rotation) <- c(pvars, xvars)
  colnames(rotation) <- c(pvars, wvars)

  # Compile full rotation matrix from identity matrices for pvars and uvars,
  # and orthogonalization matrix for xvars -> wvars transformation:
  full_rotation <- lava::blockdiag(rotation, uid)

  # Transform xvars -> wvars:
  xdata[, names(center)] <- xdata[, names(center)] -
    matrix(rep(center, nrow(xdata)),
           nrow = nrow(xdata),
           byrow = TRUE)
  xdata[, colnames(rotation)] <- as.matrix(xdata[, rownames(rotation)]) %*% rotation
  # Full rotation would be a waste of computing time:
  # xdata[,c(pvars,xvars,uvars)] <- as.matrix(xdata[,c(pvars,xvars,uvars)]) %*% full_rotation

  # Assign user-defined names to transformed features:
  colnames(xdata) <- c(yvar, pvars, wvars, uvars, ivars)

  # Set up warper object:
  x <- list(
    full_rotation = full_rotation,
    center = center,
    scale = FALSE,
    wdata = xdata,
    Xvars = rownames(full_rotation),
    Wvars = colnames(full_rotation),
    xvars = xvars,
    wvars = wvars,
    pvars = pvars,
    yvar = yvar,
    uvars = uvars,
    title = title
  )
  class(x) <- c("pls_warper", "rotation_warper", "warper")
  x
}


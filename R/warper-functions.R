
pca2_warper <- function(xdata, xvars, wvars = NULL, yvar, uvars = NULL, title = NULL) {
  wrp <- list()
  nwrp <- length(xvars)
  
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
                            wvars = the_wvars)
    if (i == 1) {
      full_rotation <- wrp[[i]]$full_rotation
    } else {
      full_rotation <- lava::blockdiag(full_rotation, wrp[[i]]$full_rotation)
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
    xvars = rownames(full_rotation),
    wvars = colnames(full_rotation),
    yvar = yvar,
    uvars = unlist(uvars),
    title = title
  )
  class(x) <- c("pca2_warper", "pca_warper", "rotation_warper", "warper")
  
  return(x)
}


pca_warper <- function(xdata, xvars, wvars = "PC", yvar, uvars = NULL, title = wvars) {
  xvars <- xvars[ !(xvars %in% uvars) ]
  wvars <- wvars[ !(wvars %in% uvars) ]
  
  # Perform PCA on x variables:
  fo <- as.formula(paste0("~", paste(xvars, collapse = " + ")))
  pca <- prcomp(formula = fo, data = xdata[,xvars], center = FALSE, scale. = FALSE)
  
  # Make loadings 'more positive':
  for (i in 1:length(xvars)) {
    ldg <- pca$rotation[,i]
    wh <- which.max(abs(ldg))
    if (ldg[wh] < 0) {
      pca$rotation[,i] <- ldg * (-1)
      pca$x[,i] <- pca$x[,i] * (-1)
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


pls_warper <- function(xdata, xvars, pvars, wvars = "resid", yvar, uvars = NULL, title = wvars) {
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


inbound.rotation_warper <- function(object, wdata = object$wdata) {
  res <- as.data.frame(as.matrix(wdata[, object$wvars]) %*% 
                         solve(object$full_rotation))
  if (any(colnames(wdata) == object$yvar))
    res[object$yvar] <- wdata[, object$yvar]
  return(res)
}



inbound.pca_warper <- function(object, wdata = object$pca$x) {
  inbound.rotation_warper(object = object, wdata = wdata)
}

outbound.rotation_warper <- function(object, xdata = NULL) {
  # if (is.null(xdata)) {
  #   # won't work when there are untransformed variables!
  #   res <- object$pca$x
  # } else {
  res <- as.data.frame(as.matrix(xdata[, object$xvars]) %*% 
                         object$full_rotation)
  # }
  if (any(colnames(xdata) == object$yvar))
    res[object$yvar] <- xdata[, object$yvar]
  return(res)
}

inbound <- function(object, ...) {
  UseMethod("inbound", object)
}

outbound <- function(object, ...) {
  UseMethod("outbound", object)
}

warp <- function(object, ...) {
  UseMethod("warp", object)
}

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

warp_fitted_model <- function(x, warper) {
  res <- list(fit = x, warper = warper)
  class(res) <- "warped_model"
  return(res)
}

warp.data.frame <- function(x, warper) {
  x <- outbound(warper, xdata = x)
  class(x) <- c("warped_df", class(x))
  return(x)
}

warp.warped_df <- function(x, warper) {
  return(x)
}

unwarp <- function(x, ...) {
  UseMethod("unwarp")
}

unwarp.warped_df <- function(x, warper, force_unwarp = FALSE) {
  class(x) <- class(x)[class(x) != "warped_df"]
  x <- inbound(warper, wdata = x)
  return(x)
}

unwarp.data.frame <- function(x, warper = NULL, force_unwarp = FALSE) {
  if (force_unwarp)
    x <- unwarp.warped_df(x, warper = warper)
  return(x)
}

predict.warped_model <- function(object, newdata = NULL, 
                                 force_unwarp = TRUE, ...) {
  newdata <- unwarp(newdata, warper = object$warper,
                    force = force_unwarp)
  predict(object$fit, newdata = newdata, ...)
}

summary.warped_model <- function(object, ...) {
  list(
    summary_warper = summary(object$warper),
    summary_model = summary(object$fit, ...)
  )
}

plot.warped_model <- function(object, ...) {
  plot(object$fit, ...)
}

plot.pca_warper <- function(object, col = c("white", "black"), bp.col = "lightblue") {
  par(mfrow = c(1,2))
  plot(object$pca, col = bp.col, xlab = "PC #", main = "PC Variances")
  biplot(object$pca, col = col, main = "Screeplot")
}



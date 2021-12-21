### Create PLS warper for late-season mean NDVI
### in the Maipo data set to explore the combined
### effect of these variables:
xvars <- c(paste("ndvi0", 1:8, sep = ""),
           paste("ndwi0", 1:8, sep = ""))
fo <- as.formula(paste("class ~", paste(xvars, collapse=" +" )))
d <- maipofields
fit <- randomForest::randomForest(fo, data = d)

# Late-season NDVI and NDWI features:
late_ndis <- c(paste("ndvi0", 4:7, sep = ""),
               paste("ndwi0", 4:7, sep = ""))
# Note that they are strongly correlated,
# especially for same or adjacent image dates,
# e.g. ndvi04 and ndwi04, or ndvi06 and ndvi07:
round(cor(d[,late_ndis]), 2)
# PC #1 explains 91% of the variance:
late_pca <- stats::prcomp(d[,late_ndis],
                          scale. = TRUE,
                          rank. = 3)
summary(late_pca)

# Mean late-season NDVI + NDWI:
# (we can average them since they have a comparable scale,
# otherwise we'd want to standardize them first)
d$late_ndi <- rowMeans(d[, late_ndis])
# Note that this variable was not in the RF's feature set!

wrp <- pls_warper(d, xvars = xvars,
                  pvars = "late_ndi",
                  yvar = "class")

# Warp the model and the feature data:
wd <- warp(d, warper = wrp)
wfit <- warp_fitted_model(fit, warper = wrp)

# Use iml package to create partial dependence plot:
if (require("iml")) {
  wprd <- Predictor$new(wfit, data = wd, y = "class",
                       type = "prob", class = "crop1")
  weff <- FeatureEffect$new(wprd, feature = "late_ndi",
                            method = "pdp", grid.size = 100)
  plot(weff)
}

# For comparison, the traditional, untransformed
# perspective:
if (require("iml")) {
  prd <- Predictor$new(fit, data = d, y = "class",
                       type = "prob", class = "crop1")
  eff <- FeatureEffects$new(prd, features = late_ndis,
                            method = "pdp", grid.size = 100)
  plot(eff, ncol = 4)
}

# ...and remember that this feature set has 64 features,
# and four classes, therefore our transformed perspective
# is much tidier as it allows you to represent a combined
# effect using only one figure.

# Backtransform it, should be identical to d:
d2 <- unwarp(wd, warper = wrp)
all.equal(d[,xvars], d2[,xvars])
# Default tolerance works for this data set, but you may have to use
# e.g. tol = 10^(-6) for less well conditioned data sets and
# transformations.

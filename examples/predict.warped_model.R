### Create principal components warper for Maipo data set
### from Peña et al. (2015):
xvars <- c(paste("ndvi0", 1:8, sep = ""),
           paste("ndwi0", 1:8, sep = ""))
fo <- as.formula(paste("class ~", paste(xvars, collapse=" +" )))

sel <- seq(1, nrow(maipofields), by = 2)
dtrain <- maipofields[sel, ]
dtest <- maipofields[-sel, ]

wrp <- pca_warper(dtrain, xvars = xvars, yvar = "class")

fit <- randomForest::randomForest(formula = fo, data = dtrain)
fit
warped_fit <- warp_fitted_model(fit, warper = wrp)
class(warped_fit) # warped_model

# Create data frame with transformed test data:
wdtest <- warp(dtest, warper = wrp)

# Prediction using the transformed data and the warped model
# using the predict.warped_model method:
wpred <- predict(warped_fit, newdata = wdtest)
# ...should give the same results as prediction using the
# untransformed features and the original model using
# randomForest's predict method:
pred <- predict(fit, newdata = dtest)
all.equal(wpred, pred)

# Misclassification error rate on the test set:
mean(wdtest$class != wpred)

# Create profile plot along the first PC while
# keeping the other PCs = 0:
ngrid <- 100
newd <- matrix(0, ncol = length(wrp$Wvars), nrow = ngrid)
colnames(newd) <- wrp$Wvars
newd <- as.data.frame(newd)
newd$PC1 <- seq(min(wdtest$PC1), max(wdtest$PC1),
                length.out = ngrid)
# Using the predict.warped_model method:
prd <- predict(warped_fit, newdata = newd, type = "prob")

# Plot the predicted probabilities for varying PC1:
profile_plots <- function(pc = 1) {
  pc_name <- paste0("PC", pc)
  for (i in 1:4) {
    crop <- paste0("crop", i)
    if (i == 1) {
      plot(newd[, pc_name], prd[, crop],
           type = "l", lwd = 2,
           xlab = paste0("PC #", pc),
           ylab = "Predicted probability",
           main = "RF profile plots for crops 1-4",
           ylim = c(0, 1))
    } else {
      lines(newd[, pc_name], prd[, crop],
            lwd = 2, col = c("blue", "red", "magenta")[i - 1])
    }
  }
}

profile_plots(1)


### Repeat this for a linear discriminant analysis:

fit <- MASS::lda(formula = fo, data = dtrain)
warped_fit <- warp_fitted_model(fit, warper = wrp)

# Prediction using the transformed data and the warped model:
wdtest <- warp(dtest, warper = wrp)
wpred <- predict(warped_fit, newdata = wdtest)$class
mean(wdtest$class != wpred) # better than RF!

# Create profile plot along the first PC:
prd <- predict(warped_fit, newdata = newd)$posterior
profile_plots(1)

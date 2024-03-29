% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/warped_model.R
\name{predict.warped_model}
\alias{predict.warped_model}
\title{Predict from a warped fitted machine-learning model}
\usage{
\method{predict}{warped_model}(object, newdata = NULL, force_unwarp = TRUE, ...)
}
\arguments{
\item{object}{A \code{warped_model} object, i.e. a warped fitted machine-learning model as created by \code{\link{warp_fitted_model}}.}

\item{newdata}{A data frame with features in transformed space. This can be a \code{warped_df} object or just a plain \code{data.frame}. See \code{force_unwarp}.}

\item{force_unwarp}{If \code{TRUE}, \code{newdata} will always be (back)transformed
into feature space using the \code{warper}'s inverse transformation.
If \code{FALSE}, only objects of class \code{warped_df} will be backtransformed.}

\item{...}{Additional arguments for the predict method of \code{object$fit}.}
}
\value{
The function's result depends on the behaviour of the invoked
predict method.
}
\description{
This function implements the prediction \eqn{\textbf{w}\mapsto\hat{y}:=(\hat{f}\circ T^{-1})(\textbf{w})}
by (back)transforming a feature vector \eqn{w} from the transformed space (e.g.,
principal components space) to the original feature space and then applying
the fitted machine learning model's predict method to make predictions.
}
\details{
\code{newdata} will normally be a data frame, not \code{NULL}; not all
fitted models accept \code{NULL}. The most common behaviour is to return
model predictions for the training data.
}
\examples{
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

library("MASS")
fit <- MASS::lda(formula = fo, data = dtrain)
warped_fit <- warp_fitted_model(fit, warper = wrp)

# Prediction using the transformed data and the warped model:
wdtest <- warp(dtest, warper = wrp)
wpred <- predict(warped_fit, newdata = wdtest)$class
mean(wdtest$class != wpred) # better than RF!

# Create profile plot along the first PC:
prd <- predict(warped_fit, newdata = newd)$posterior
profile_plots(1)
}
\seealso{
\code{\link[=warp_fitted_model]{warp_fitted_model()}} for creating a warped machine-learning
model, and the \code{predict} method of your machine-learning model
for details on additional arguments and the returned values.
}

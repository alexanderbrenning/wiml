### Create principal components warper for Maipo data set
### from Peña et al. (2015):
xvars <- c(paste("ndvi0", 1:8, sep = ""), paste("ndwi0", 1:8, sep = ""),
           paste("b", outer(1:8,2:7,paste,sep = ""), sep = ""))
fo <- as.formula(paste("class ~", paste(xvars, collapse=" +" )))
d <- maipofields

sel <- seq(1, nrow(d), by = 2)
#sel <- sample(1:nrow(d), size = floor(nrow(d)/2))
dtrain <- d[sel, ]
dtest <- d[-sel, ]

wrp <- pca_warper(dtrain, xvars = xvars, yvar = "class")

fit <- randomForest::randomForest(formula = fo, data = dtrain)
fit
warped_fit <- warp_fitted_model(fit, warper = wrp)
warped_fit$fit # just a copy of fit

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

# Use this warped model to visualize main effects in a tidy way:
library(iml)
predictor <- Predictor$new(warped_fit, data = wdtest, y = "class",
                           type = "prob", class = "crop3")
effs <- FeatureEffects$new(predictor, features = c("PC1","PC2","PC3","PC4"),
                           method = "ale")
plot(effs)
# What's the meaning of these principal components?
# Interpretation aid:
# https://landsat.gsfc.nasa.gov/landsat-8/landsat-8-bands
round(wrp$pca$rotation[,1:4], digits = 2)
# PC1: mid- to late season vegetation vigour and moisture
# PC2: late minus early season vegetation vigour and moisture contrast
# PC3: earliest season vegetation vigour (image date 1)
# PC4: all-season SWIR (moisture) signal (except last image date)
# See help(strucpca_warper) for a more structured approach!


# For comparison: fit RF to transformed data -> often not a good idea,
# Repeat this several times to see that OOB error is not only larger
# but especially more variable than using the untransformed data:
pcfo <- as.formula(paste("class ~", paste(wrp$wvars, collapse="+" )))
wdtrain <- warp(dtrain, warper = wrp)
pcfit <- randomForest::randomForest(formula = pcfo, data = wdtrain)
pcfit
mean(wdtest$class != predict(pcfit, newdata = wdtest))
# (but the rotation forest method makes more intelligent use of transformed
# predictors...)

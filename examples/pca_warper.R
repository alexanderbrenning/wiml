### Create principal components warper for Maipo data set:
xvars <- c(paste("ndvi0", 1:8, sep = ""), paste("ndwi0", 1:8, sep = ""),
           paste("b", outer(1:8,2:7,paste,sep = ""), sep = ""))
fo <- as.formula(paste("class ~", paste(xvars, collapse=" +" )))
d <- maipo

wrp <- pca_warper(d, xvars = xvars, yvar = "class")
plot(wrp)
round(wrp$pca$rotation[,1:3], digits = 2)
summary(wrp)

# Create a data frame with transformed data:
wd <- warp(d, warper = wrp)
summary(wd)

# Backtransform it, should be identical to d:
d2 <- unwarp(wd, warper = wrp)
all.equal(d, d2[,colnames(d)])
# Default tolerance works for this data set, but you may have to use
# e.g. tol = 10^(-6) for less well conditioned data sets and
# transformations.

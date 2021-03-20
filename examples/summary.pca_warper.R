xvars <- c(paste("ndvi0", 1:8, sep = ""),
           paste("ndwi0", 1:8, sep = ""))
fo <- as.formula(paste("class ~", paste(xvars, collapse=" +" )))
wrp <- pca_warper(maipo, xvars = xvars, yvar = "class")
summary(wrp)

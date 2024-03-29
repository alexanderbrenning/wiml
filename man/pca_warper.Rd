% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca_warper.R
\name{pca_warper}
\alias{pca_warper}
\title{Principal components transformation of feature space}
\usage{
pca_warper(
  xdata,
  xvars,
  wvars = "PC",
  yvar,
  uvars = NULL,
  center = TRUE,
  scale = TRUE,
  positive = TRUE,
  title = wvars
)
}
\arguments{
\item{xdata}{A data frame containing the observations in the original feature space.}

\item{xvars}{A character vector with the column names of features in \code{xdata}
that should be transformed.}

\item{wvars}{A character vector with the names of all transformed predictors,
or (more often) a simple character string to which the numbers 1, ...
will be appended as needed.}

\item{yvar}{Name of the response variable (not to be transformed)}

\item{uvars}{Names of additional variables that should remain untouched.}

\item{scale, center}{Logical arguments indicating whether the data should
be centered and then scales. Both should be turned on (\code{TRUE}), which is
the default behaviour.}

\item{positive}{Logical argument (default: \code{TRUE}) indicating whether
the signs of the loadings should be adjusted so that the most
strongly weighted PC gets a positive sign.}

\item{title}{Optional name of the transformation, may be used for printing summaries
or for plotting.}
}
\value{
An object of class \code{warper}, \code{rotation_warper} and \code{pca_warper}.
}
\description{
This function generates a \code{warper} object based on a principal component
analysis of the features (or more precisely, of selected features \code{xvars}).
}
\details{
There should be no overlap
between \code{xvars}, \code{uvars} and \code{yvar}.
The rotation matrix \verb{$full_rotation} in the results object
is of size \code{length(xvars)+length(uvars)} and includes an
identity transformation \code{diag(length(uvars))} for the
features in \code{uvars}.
}
\examples{
### Create principal components warper for Maipo data set:
xvars <- c(paste("ndvi0", 1:8, sep = ""), paste("ndwi0", 1:8, sep = ""),
           paste("b", outer(1:8,2:7,paste,sep = ""), sep = ""))
fo <- as.formula(paste("class ~", paste(xvars, collapse=" +" )))
d <- maipofields

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
}

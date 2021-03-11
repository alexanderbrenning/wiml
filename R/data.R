#' Maipo fruit-tree crop data
#'
#' Maipo data - a subset of the data used by Pena and Brenning (2015), aggregated
#' to the field level, i.e. each field or parcel is one observation.
#'
#' @format A data frame with 400 rows and 67 variables:
#' \describe{
#'   \item{class}{Croptype \code{"crop1"} to \code{"crop4"} as a factor variable.}
#'   \item{utmx,utmy}{UTM x and y coordinates}
#'   \item{b12,...,b87}{Landsat reflectances: variable \code{b}ij corresponds to
#'       band j from image date i, where larger values of i indicate a date later
#'       in the season. See referenced paper for details.}
#'   \item{ndvi01,...,ndvi08}{NDVI values for each image date.}
#'   \item{ndwi01,...,ndwi}{NDWI values, i.e. a moisture index.}
#' }
#' @source Peña, M.A., and Brenning, A. 2015. Assessing Fruit-Tree Crop
#'   Classification from Landsat-8 Time Series for the Maipo Valley, Chile.
#'   Remote Sensing of Environment 171: 234-44. \url{https://doi.org/10.1016/j.rse.2015.10.029}
"Maipo"


#' Rock glacier remote sensing using Gabor texture filters
#'
#' This is a subset of the data used by Brenning et al. (2012).
#'
#' @format A data frame with 3403 rows and 49 variables.
#' \describe{
#'   \item{class}{Factor variable indicating that the observation represents
#'       flow structures on a rock glacier (\code{"TRUE"}) or not
#'       (\code{"FALSE"}).}
#'  \item{...}{Terrain attributes and Gabor texture features as predictors.}
#' }
#' @source Brenning, A., Long, S. and Fieguth, P. 2012. Detecting rock glacier
#'   flow structures using Gabor filters and IKONOS imagery. Remote Sensing
#'   of Environment, 125: 227-237. https://doi.org/10.1016/j.rse.2012.07.005
"Gabor"

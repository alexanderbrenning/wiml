#' Remote sensing of fruit trees
#'
#' This data set for multi-class classification contains inventoried fruit-tree
#' crop types and multitemporal Landsat satellite data from the Maipo river
#' basin in central Chile. And Landsat data is available for nine image dates
#' from one growing season. This is a subset of the data used by Pena et al.
#' (2015), who provide more detailed information on the data set (e.g., image
#' dates).
#'
#' @details **Objective:** To predict crop type as the response variable based
#'   on the available optical remote sensing data.
#'
#' @format A data frame with 400 rows and 67 variables:
#' \describe{
#'   \item{class}{Croptype \code{"crop1"} to \code{"crop4"} as a factor
#'   variable.}
#'   \item{utmx,utmy}{UTM x and y coordinates - not to be used as
#'   predictors.}
#'   \item{b*ij*}{value of Landsat band *j* in image *i*; images are
#'   numbered from early season = 1 to late season = 9; see Peña et al. (2017)
#'   for details, e.g. image dates.}
#'   \item{ndvi0*i*}{NDVI values (representing vegetation vigour) for
#'   each image date.}
#'   \item{ndwi0*i*}{NDWI values, i.e. a moisture index (representing moisture
#'   content of plants).} }
#' @source Pena, M.A., and Brenning, A. 2015. Assessing fruit-tree crop
#'   classification from Landsat-8 time series for the Maipo Valley, Chile.
#'   *Remote Sensing of Environment*, 171: 234-44.
#'   <https://doi.org/10.1016/j.rse.2015.10.029>
"maipo"


#' Rock glacier remote sensing using Gabor texture filters
#'
#' This data set for binary classification tasks contains information on the
#' presence/absence of flow structures related to the deformation of rock
#' glaciers in the Andes of Santiago, and corresponding remotely-sensed texture
#' attributes and terrain attributes as predictors. Rock glaciers are ice-debris
#' landforms - the visible expression of creeping mountain permafrost.
#'
#' @details Texture attributes derived from high-resolution panchromatic IKONOS
#'   imagery form the largest group of features in this study, and terrain
#'   attributes are used as additional predictors. A ‘filter bank’ of Gabor
#'   filters is used since Gabor features are capable of detecting ‘zebra
#'   stripe’ type patterns that relate to the troughs and ridges typically found
#'   on rock glaciers. The second group of features represent topographic
#'   conditions, e.g. elevation, slope angle and size of the upslope
#'   contributing area. These are proxies for topoclimatic conditions that may
#'   relate to the formation and conservation of permafrost, and for talus
#'   supply to these ice-debris landforms.
#'
#'   This data set is a subset of the data used by Brenning et al. (2012),
#'   specifically a subset of the Laguna Negra area. Note that areas that can
#'   “obviously” not present rock glaciers have been masked out (i.e. removed
#'   from the data set), e.g. steep slopes, in order to allow the classifier to
#'   focus on the “difficult” areas; see Brenning et al. (2012) for details.
#'
#'   **Objective:** To identify rock-glacier flow patterns based on the available
#'   texture and terrain attribute data.
#'
#' @format A data frame with 4656 grid cells (1290
#'   from flow patterns and 3366 from other terrain outside of
#'   rock glaciers) in two study areas (`area = "LN"`: 2499 grid cells;
#'   `area = "CAT"`: 2157 cells).
#'   (The flow patterns are from approximately 50 individual rock glaciers
#'   of varying size.) \describe{
#'   \item{class}{Factor variable (levels: `"TRUE"`, `"FALSE"`) representing the
#'   presence (`"TRUE"`) and absence (`"FALSE"`) of rock glacier flow patterns.}
#'   \item{x,y}{UTM x and y coordinates (not to be used as predictors)}
#'   \item{area}{Factor variable identifying one of the two study
#'   areas, `"LN"` or `"CAT"`.}
#'   \item{dem}{Elevation in metres above sea level (m a.s.l.)}
#'   \item{slope}{Local slope angle in degrees}
#'   \item{cslope}{Slope angle of the upslope contributing area in degrees}
#'   \item{log.carea}{Logarithm (to the base 10) of the size of the upslope
#'   contributing area in m²}
#'   \item{log.cheight}{Logarithm (to the base 10) of the height of the upslope contributing area in m}
#'   \item{pisr}{Annual potential incoming solar radiation}
#'   \item{m30e*i*g*j*x}{Gabor feature with the following settings (see Brenning et al. 2012 for details): *i* = axis ratio (1 or 2) of Gabor filter; *j* = wavelength of Gabor filter (5, 10, 20, 30, or 50 m); **x** = aggregation scheme (“min” = minimum; “max” = maximum; “rg” = range; “med” = median}}
#' @source Brenning, A., Long, S. and Fieguth, P. 2012. Detecting rock glacier
#'   flow structures using Gabor filters and IKONOS imagery. *Remote Sensing of
#'   Environment*, 125: 227-237. <https://doi.org/10.1016/j.rse.2012.07.005>
"gabor"

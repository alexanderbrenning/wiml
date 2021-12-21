#' Remote sensing of fruit trees
#'
#' This data set for multi-class classification contains inventoried fruit-tree
#' crop types and multitemporal Landsat satellite data from the Maipo river
#' basin in central Chile. And Landsat data is available for nine image dates
#' from one growing season. This is a subset of the data used by Pena et al.
#' (2015), who provide more detailed information on the data set (e.g., image
#' dates). Note that this is different from the [sperrorest::maipo] data set shipped with
#' the `sperrorest` package, which contains pixel-level data; this data set
#' contains field-level data, i.e. one (averaged) observation per field (n=400).
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
#'   \item{b12,b13,b14,b15,b16,b17,b22,b23,b24,b25,b26,b27,b32,b33,b34,b35,b36,b37,b42,b43,b44,b45,b46,b47,b52,b53,b54,b55,b56,b57,b62,b63,b64,b65,b66,b67,b72,b73,b74,b75,b76,b77,b82,b83,b84,b85,b86,b87}{`b`*ij*
#'   is the value of Landsat band *j* in image *i*; images are
#'   numbered from early season = 1 to late season = 9; see Peña et al. (2017)
#'   for details, e.g. image dates.}
#'   \item{ndvi01,ndvi02,ndvi03,ndvi04,ndvi05,ndvi06,ndvi07,ndvi08}{`ndvi0`*i*
#'   is the NDVI value (representing vegetation vigour) for
#'   each image date *i*.}
#'   \item{ndwi01,ndwi02,ndwi03,ndwi04,ndwi05,ndwi06,ndwi07,ndwi08}{`ndwi0`*i*
#'   is the NDWI value for image date *i*, i.e. a moisture index (representing moisture
#'   content of plants).} }
#' @source Pena, M.A., and Brenning, A. 2015. Assessing fruit-tree crop
#'   classification from Landsat-8 time series for the Maipo Valley, Chile.
#'   *Remote Sensing of Environment*, 171: 234-44.
#'   <https://doi.org/10.1016/j.rse.2015.10.029>
"maipofields"


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
#'   \item{m30e1g5rg,m30e1g5max,m30e1g5min,m30e1g5med,m30e1g10rg,m30e1g10max,m30e1g10min,m30e1g10med,m30e1g20rg,m30e1g20max,m30e1g20min,m30e1g20med,m30e1g30rg,m30e1g30max,m30e1g30min,m30e1g30med,m30e1g50rg,m30e1g50max,m30e1g50min,m30e1g50med,m30e2g5rg,m30e2g5max,m30e2g5min,m30e2g5med,m30e2g10rg,m30e2g10max,m30e2g10min,m30e2g10med,m30e2g20rg,m30e2g20max,m30e2g20min,m30e2g20med,m30e2g30rg,m30e2g30max,m30e2g30min,m30e2g30med,m30e2g50rg,m30e2g50max,m30e2g50min,m30e2g50med}{Gabor features of the form `m30e*i*g*j*x` with the following settings (see Brenning et al. 2012 for details): *i* = axis ratio (1 or 2) of Gabor filter; *j* = wavelength of Gabor filter (5, 10, 20, 30, or 50 m); **x** = aggregation scheme (“min” = minimum; “max” = maximum; “rg” = range; “med” = median}}
#' @source Brenning, A., Long, S. and Fieguth, P. 2012. Detecting rock glacier
#'   flow structures using Gabor filters and IKONOS imagery. *Remote Sensing of
#'   Environment*, 125: 227-237. <https://doi.org/10.1016/j.rse.2012.07.005>
"gabor"

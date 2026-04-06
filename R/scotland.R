#' Lip Cancer in Scotland
#'
#' County-level (n=56) data for lip cancer among males in Scotland between 1975-1980
#'
#'
#' @format
#' List containing:
#' \describe{
#'   \item{geo}{a table of county IDs, x-coordinates (eastings) and y-coordinates (northings) of the geographic centroid of each county.}
#'   \item{data}{a table of county IDs, number of cases, population and strata information}
#'   \item{spatial.polygon}{a Spatial Polygons class map of Scotland (deprecated, use scotland_sf instead)}
#'   \item{polygon}{a polygon map of Scotland (see [polygon2sf()])}
#' }
#' @source Kemp I., Boyle P., Smans M. and Muir C. (1985) Atlas of cancer in Scotland, 1975-1980, incidence and epidemiologic perspective *International Agency for Research on Cancer* **72**.
#'
#' @references Clayton D. and Kaldor J. (1987) Empirical Bayes estimates of age-standardized relative risks for use in disease mapping.  *Biometrics*, **43**, 671--681.
#'
#' @examples
#' # Use scotland_sf for modern sf-based workflows
#' data(scotland_sf)
#' SMR <- scotland_sf$cases / scotland_sf$expected
#' mapvariable(SMR, scotland_sf)
#'
#'
#'
#'
#'
"scotland"

#' Upstate New York Leukemia Data
#'
#'
#' Census tract level (`n=281`) leukemia data for the 8 counties in upstate New York from 1978-1982, paired with population data from the 1980 census.
#' Note that 4 census tracts were completely surrounded by another unique census tract;
#' when applying the Bayesian cluster detection model in [bayes_cluster()],
#' we merge them with the surrounding census tracts yielding `n=277` areas.
#'
#'
#' @format
#' List with 5 items:
#' \describe{
#'   \item{geo}{table of the FIPS code, longitude, and latitude of the geographic centroid of each census tract}
#'   \item{data}{table of the FIPS code, number of cases, and population of each census tract}
#'   \item{spatial.polygon}{object of class SpatialPolygons (deprecated, use NYleukemia_sf instead)}
#'   \item{surrounded}{row IDs of the 4 census tracts that are completely surrounded by the}
#'   \item{surrounding}{census tracts}
#'
#' }
#'
#' @references  Turnbull, B. W. et al (1990) Monitoring for clusters of disease: application to leukemia incidence in upstate New York *American Journal of Epidemiology*, **132**, 136--143
#'
#' @examples
#' # Use NYleukemia_sf for modern sf-based workflows
#' data(NYleukemia_sf)
#' population <- NYleukemia_sf$population
#' cases <- NYleukemia_sf$cases
#'
#' ## Plot incidence
#' plotmap(cases/population, NYleukemia_sf, log=TRUE, nclr=5)
#'
#'
#'
"NYleukemia"

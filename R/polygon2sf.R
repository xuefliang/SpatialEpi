#' Convert a Polygon to an sf Object
#'
#' @description Converts a polygon (a matrix of coordinates with NA values to separate subpolygons) into an sf object.
#' @param poly a 2-column matrix of coordinates, where each complete subpolygon is separated by NA's
#' @param crs the coordinate reference system, either as EPSG code (e.g., 4326) or proj4 string
#' @param area.names names of all areas
#' @param nrepeats number of sub polygons for each area
#'
#' @details Just as when plotting with the [graphics::polygon()] function, it is assumed that each subpolygon is to be closed by joining the last point to the first point. In the matrix `poly`, NA values separate complete subpolygons.
#' In the case with an area consists of more than one separate closed polygon, `nrepeats` specifies the number of closed polygons associated with each area.
#'
#' @references Pebesma, E. (2018) Simple Features for R: Standardized Support for Spatial Vector Data. *The R Journal*, **10**, 439--446.
#' @author Albert Y. Kim
#' @return
#' An object of class `sf` with POLYGON or MULTIPOLYGON geometries.
#' @export
#'
#' @examples
#' data(scotland)
#'
#' polygon <- scotland$polygon$polygon
#' names <- scotland$data$county.names
#' nrepeats <- scotland$polygon$nrepeats
#'
#' # Create sf object with WGS84 CRS
#' sf_polygon <- polygon2sf(polygon, crs = 4326, names, nrepeats)
#'
#' par(mfrow=c(1,2))
#' # plot using polygon function
#' plot(polygon, type='n', xlab="Eastings (km)", ylab="Northings (km)", main="Polygon File")
#' polygon(polygon)
#'
#' # plot as sf object
#' plot(sf::st_geometry(sf_polygon), axes=TRUE)
#' title(xlab="Eastings (km)", ylab="Northings (km)", main="SF Object")
#'
#' # Note that area 23 (argyll-bute) consists of 8 separate polygons
#' nrepeats[23]
#' plot(sf::st_geometry(sf_polygon[23, ]), add=TRUE, col="red")
polygon2sf <- function(poly, crs, area.names = NULL, nrepeats = NULL) {

  if (missing(crs)) {
    stop("Coordinate reference system (crs) must be specified")
  }

  if (is.null(nrepeats)) {
    nrepeats <- rep(1, sum(is.na(poly[, 1])) + 1)
  }

  if (is.null(area.names)) {
    area.names <- as.character(1:length(nrepeats))
  }

  # Find NA separators
  na.index <- which(is.na(poly[, 1]))
  n.areas <- length(nrepeats)

  # Extract all ring coordinates as a list
  rings <- list()

  # First ring
  ring_coords <- poly[1:(na.index[1] - 1), , drop = FALSE]
  # Close the ring if not already closed
  if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
    ring_coords <- rbind(ring_coords, ring_coords[1, ])
  }
  rings[[1]] <- ring_coords

  # Middle rings
  for (i in 1:(length(na.index) - 1)) {
    ring_coords <- poly[(na.index[i] + 1):(na.index[i + 1] - 1), , drop = FALSE]
    if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
      ring_coords <- rbind(ring_coords, ring_coords[1, ])
    }
    rings[[i + 1]] <- ring_coords
  }

  # Last ring
  ring_coords <- poly[(na.index[length(na.index)] + 1):nrow(poly), , drop = FALSE]
  if (!all(ring_coords[1, ] == ring_coords[nrow(ring_coords), ])) {
    ring_coords <- rbind(ring_coords, ring_coords[1, ])
  }
  rings[[length(na.index) + 1]] <- ring_coords

  # Group rings into polygons by area according to nrepeats
  geometries <- vector("list", n.areas)
  ring_idx <- 1

  for (i in 1:n.areas) {
    n_rings <- nrepeats[i]

    if (n_rings == 1) {
      # Single polygon
      geometries[[i]] <- sf::st_polygon(list(rings[[ring_idx]]))
    } else {
      # Multipolygon - each ring is a separate polygon part
      poly_list <- lapply(ring_idx:(ring_idx + n_rings - 1), function(j) {
        list(rings[[j]])
      })
      geometries[[i]] <- sf::st_multipolygon(poly_list)
    }

    ring_idx <- ring_idx + n_rings
  }

  # Create sfc geometry column
  geom_col <- sf::st_sfc(geometries, crs = crs)

  # Create sf object with area names

result <- sf::st_sf(
    name = area.names,
    geometry = geom_col
  )

  return(result)
}

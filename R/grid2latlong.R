#' Convert Coordinates from Grid to Latitude/Longitude
#'
#' @description Convert geographic coordinates from kilometer-based grid system to Latitude/Longitude.
#'
#' @param input A data frame with columns named `x` and `y` of the grid coordinates to convert or an `n x 2` matrix of grid coordinates or an sf/sfc object
#'
#' @author Lance A. Waller
#'
#' @note Rough conversion of US lat/long to km (used by GeoBUGS):  (see
#'   also forum.swarthmore.edu/dr.math/problems/longandlat.html).
#'   Radius of earth: r = 3963.34 (equatorial) or 3949.99 (polar) mi =
#'   6378.2 or 6356.7 km, which implies: km per mile  = 1.609299 or
#'   1.609295 a change of 1 degree of latitude corresponds to the same
#'   number of km, regardless of longitude.  arclength=r*theta, so the
#'   multiplier for coord y should probably be just the radius of
#'   earth. On the other hand, a change of 1 degree in longitude
#'   corresponds to a different distance, depending on latitude.  (at N
#'   pole, the change is essentially 0.  at the equator, use equatorial
#'   radius.  Perhaps for U.S., might use an "average" latitude, 30 deg
#'   is roughly Houston, 49deg is most of N bdry of continental 48
#'   states.  0.5(30+49)=39.5 deg.  so use r approx 6378.2*sin(51.5)
#'
#' @details Longitude/latitudes are not a grid-based coordinate system:  latitudes are equidistant but the distance between longitudes varies.
#' @return
#' Either a data frame with the corresponding longitude and latitude, or an sf/sfc object with the coordinates transformed.
#' @export
#'
#' @examples
#' coord <- data.frame(rbind(
#' # Montreal, QC
#' c(-6414.30, 5052.849),
#' # Vancouver, BC
#' c(-122.6042, 45.6605)
#' ))
#'
#' grid2latlong(coord)
#'
grid2latlong <- function(input) {

  toradians <- atan(1) / 45
  radiusearth <- 0.5 * (6378.2 + 6356.7)
  sine51 <- sin(51.5 * toradians)

  # Inverse transform function for coordinates
  transform_coords <- function(x, y) {
    new_x <- x / (toradians * radiusearth * sine51)
    new_y <- y / (toradians * radiusearth)
    cbind(new_x, new_y)
  }

  # If sf or sfc object
  if (inherits(input, c("sf", "sfc"))) {
    geom <- sf::st_geometry(input)

    # Transform each geometry
    new_geoms <- lapply(geom, function(g) {
      transform_geometry_inverse(g, transform_coords)
    })

    new_sfc <- sf::st_sfc(new_geoms, crs = 4326)  # WGS84 lat/long

    if (inherits(input, "sf")) {
      return(sf::st_set_geometry(input, new_sfc))
    }
    return(new_sfc)
  }

  # Otherwise assume matrix/data.frame of coordinates
  output <- data.frame(
    x = input[, 1] / (toradians * radiusearth * sine51),
    y = input[, 2] / (toradians * radiusearth)
  )

  return(output)
}


#' Transform geometry coordinates (inverse)
#' @noRd
transform_geometry_inverse <- function(geom, transform_fn) {
  geom_type <- sf::st_geometry_type(geom)

  if (geom_type == "POLYGON") {
    rings <- lapply(geom, function(ring) {
      new_coords <- transform_fn(ring[, 1], ring[, 2])
      matrix(new_coords, ncol = 2)
    })
    return(sf::st_polygon(rings))

  } else if (geom_type == "MULTIPOLYGON") {
    polys <- lapply(geom, function(poly) {
      rings <- lapply(poly, function(ring) {
        new_coords <- transform_fn(ring[, 1], ring[, 2])
        matrix(new_coords, ncol = 2)
      })
      rings
    })
    return(sf::st_multipolygon(polys))

  } else if (geom_type == "POINT") {
    coords <- sf::st_coordinates(geom)
    new_coords <- transform_fn(coords[1], coords[2])
    return(sf::st_point(new_coords))

  } else if (geom_type == "MULTIPOINT") {
    coords <- sf::st_coordinates(geom)
    new_coords <- transform_fn(coords[, 1], coords[, 2])
    return(sf::st_multipoint(new_coords))

  } else if (geom_type == "LINESTRING") {
    coords <- sf::st_coordinates(geom)
    new_coords <- transform_fn(coords[, 1], coords[, 2])
    return(sf::st_linestring(new_coords))

  } else if (geom_type == "MULTILINESTRING") {
    lines <- lapply(geom, function(line) {
      new_coords <- transform_fn(line[, 1], line[, 2])
      matrix(new_coords, ncol = 2)
    })
    return(sf::st_multilinestring(lines))

  } else {
    stop("Unsupported geometry type: ", geom_type)
  }
}

#' @title Convert Coordinates from Latitude/Longitude to Grid
#'
#' @description Convert geographic latitude/longitude coordinates to kilometer-based grid coordinates.
#'
#' @param input either an `n x 2` matrix of longitude and latitude coordinates in decimal format or an sf/sfc object
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
#'   radius.
#'
#' @details Longitude/latitudes are not a grid-based coordinate system:  latitudes are equidistant but the distance between longitudes varies.
#'
#' @author Lance A. Waller
#'
#' @return Either a data frame with the corresponding (x,y) kilometer-based grid coordinates, or an sf/sfc object with the coordinates transformed.
#'
#' @export
#'
#' @examples
#' ## Convert coordinates
#' coord <- data.frame(rbind(
#'  # Montreal, QC:  Latitude: 45deg 28' 0" N (deg min sec), Longitude: 73deg 45' 0" W
#'  c(-73.7500, 45.4667),
#'  # Vancouver, BC:  Latitude: 45deg 39' 38" N (deg min sec), Longitude: 122deg 36' 15" W
#'  c(-122.6042, 45.6605)
#' ))
#' latlong2grid(coord)
#'
#' ## Convert sf object
#' data(pennLC_sf)
#' # Get unique county geometries
#' county_sf <- sf::st_as_sf(aggregate(pennLC_sf["geometry"],
#'   by = list(county = pennLC_sf$county), FUN = head, n = 1))
#' new <- latlong2grid(county_sf)
#' par(mfrow=c(1,2))
#' plot(sf::st_geometry(county_sf), axes=TRUE)
#' title("Lat/Long")
#' plot(sf::st_geometry(new), axes=TRUE)
#' title("Grid (in km)")
latlong2grid <- function(input) {

  toradians <- atan(1) / 45
  radiusearth <- 0.5 * (6378.2 + 6356.7)
  sine51 <- sin(51.5 * toradians)

  # Transform function for coordinates
  transform_coords <- function(x, y) {
    new_x <- (x * toradians) * radiusearth * sine51
    new_y <- (y * toradians) * radiusearth
    cbind(new_x, new_y)
  }

  # If sf or sfc object
 if (inherits(input, c("sf", "sfc"))) {
    geom <- sf::st_geometry(input)

    # Transform each geometry
    new_geoms <- lapply(geom, function(g) {
      transform_geometry(g, transform_coords)
    })

    # Use projected CRS for grid coordinates
    new_sfc <- sf::st_sfc(new_geoms, crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

    if (inherits(input, "sf")) {
      return(sf::st_set_geometry(input, new_sfc))
    }
    return(new_sfc)
  }

  # Otherwise assume matrix/data.frame of coordinates
  output <- data.frame(
    x = (input[, 1] * toradians) * radiusearth * sine51,
    y = (input[, 2] * toradians) * radiusearth
  )

  return(output)
}


#' Transform geometry coordinates
#' @noRd
transform_geometry <- function(geom, transform_fn) {
  geom_type <- sf::st_geometry_type(geom)

  if (geom_type == "POLYGON") {
    # POLYGON is a list of rings (first is exterior, rest are holes)
    rings <- lapply(geom, function(ring) {
      new_coords <- transform_fn(ring[, 1], ring[, 2])
      matrix(new_coords, ncol = 2)
    })
    return(sf::st_polygon(rings))

  } else if (geom_type == "MULTIPOLYGON") {
    # MULTIPOLYGON is a list of polygons, each polygon is a list of rings
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

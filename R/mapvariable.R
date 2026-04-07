globalVariables(c(
  "palette","gray","par","layout","axis","rect"
))

#' Plot Levels of a Variable in a Colour-Coded Map
#'
#' @description Plot levels of a variable in a colour-coded map along with a legend.
#'
#' @param data an sf object containing the variable to plot
#' @param y column name (unquoted) or character string specifying the variable to plot.
#'   Must be a column in \code{data}.
#' @param ncut number of cuts in colour levels to plot
#' @param nlevels number of levels to include in legend
#' @param lower lower bound of levels
#' @param upper upper bound of levels
#' @param main an overall title for the plot
#' @param xlab a title for the x axis
#' @param ylab a title for the y axis
#'
#' @references Pebesma, E. (2018) Simple Features for R: Standardized Support for Spatial Vector Data. *The R Journal*, **10**, 439--446.
#'
#' @author Jon Wakefield, Nicky Best, Sebastien Haneuse, and Albert Y. Kim
#'
#' @return A map colour-coded to indicate the different levels of `y`. Returns `data` invisibly
#'   for use in pipelines.
#'
#' @export
#'
#' @examples
#' data(scotland_sf)
#'
#' # Using pipe syntax (recommended)
#' scotland_sf |> mapvariable(cases)
#'
#' # With transformations
#' scotland_sf$SMR <- scotland_sf$cases / scotland_sf$expected
#' scotland_sf |> mapvariable(SMR, main="Scotland SMR")
#'
#' # Using character column name
#' scotland_sf |> mapvariable("cases", main="Scotland", xlab="Eastings (km)", ylab="Northings (km)")
mapvariable <- function(data, y, ncut=1000, nlevels=10, lower=NULL, upper=NULL,
                        main=NULL, xlab=NULL, ylab=NULL) {

  # Validate data is an sf object

  if (!inherits(data, "sf")) {
    stop("'data' must be an sf object")
  }

  # Handle y using non-standard evaluation

  y_expr <- substitute(y)
  if (is.character(y_expr)) {
    # y is a character string
    col_name <- y
  } else if (is.name(y_expr)) {
    # y is an unquoted column name
    col_name <- as.character(y_expr)
  } else {
    stop("'y' must be a column name (unquoted) or a character string")
  }

  # Validate column exists in data
  if (!col_name %in% names(data)) {
    stop(paste0("Column '", col_name, "' not found in data. Available columns: ",
                paste(setdiff(names(data), attr(data, "sf_column")), collapse = ", ")))
  }

  # Extract the variable
  y_values <- data[[col_name]]

  #-------------------------------------------------------------------------------
  # Create id indicators for coloring scheme
  #-------------------------------------------------------------------------------
  if (is.null(lower))
    lower <- min(y_values)
  if (is.null(upper))
    upper <- max(y_values)

  id <- cut(y_values, breaks=seq(from=lower, to=upper, length=(ncut+1)))
  id <- as.numeric(id)
  id[is.na(id)] <- 0
  id <- id + 1

  # Set colours to grey scale
  palette(gray(seq(1, 0, len=(ncut+1))))


  #-------------------------------------------------------------------------------
  # Make the scale of the two axes the same
  #-------------------------------------------------------------------------------
  bbox <- sf::st_bbox(data)
  xrnge <- c(bbox["xmin"], bbox["xmax"])
  yrnge <- c(bbox["ymin"], bbox["ymax"])

  xd <- xrnge[2] - xrnge[1]
  yd <- yrnge[2] - yrnge[1]

  if (xd > yd) {
    xplot <- xrnge
    yplot <- NULL
    yplot[1] <- ((yrnge[2] + yrnge[1])/2) - xd/2
    yplot[2] <- ((yrnge[2] + yrnge[1])/2) + xd/2
  }

  if (xd <= yd) {
    yplot <- yrnge
    xplot <- NULL
    xplot[1] <- ((xrnge[2] + xrnge[1])/2) - yd/2
    xplot[2] <- ((xrnge[2] + xrnge[1])/2) + yd/2
  }


  #-------------------------------------------------------------------------------
  # Plots
  #-------------------------------------------------------------------------------
  def.par <- par(no.readonly = TRUE)
  layout(matrix(c(1,2), ncol=2, nrow=1), heights=c(.3, .3), widths=c(.4, .1))

  # plot variable
  plot(sf::st_geometry(data), axes=TRUE, col=id)

  # plot title and axis labels
  if (!is.null(main)) {
    title(main=main)
  }
  if (!is.null(xlab)) {
    title(xlab=xlab)
  }
  if (!is.null(ylab)) {
    title(ylab=ylab)
  }

  # plot legend
  plot(c(0,1), c(0,1), type="n", axes=FALSE, xlab="", ylab="")
  xlims <- rep(0, nlevels)
  atpts <- rep(0, nlevels)
  for (i in 1:nlevels) {
    xlims[i] <- format(lower + (i-1)*(upper-lower)/(nlevels-1), digits=2)
    atpts[i] <- (i-1)/(nlevels-1)
  }

  axis(2, at=c(atpts[1:nlevels]), labels=c(xlims[1:nlevels]))
  yb <- seq(0, (nlevels-2)/(nlevels-1), 1/(nlevels-1))
  yt <- seq(1/(nlevels-1), 1, 1/(nlevels-1))
  xl <- rep(0, nlevels-1)
  xr <- rep(1, nlevels-1)
  gr <- seq(0, 1, 1/nlevels)
  gr <- max(gr) - gr
  rect(xleft=xl, ybottom=yb, xright=xr, ytop=yt, col=gray(gr), border=TRUE)

  # Reset colours to default
  palette("default")
  par(def.par)

  # Return data invisibly for pipe chains
  invisible(data)
}
